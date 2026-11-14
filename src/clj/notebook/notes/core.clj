(ns notebook.notes.core
  "Note operations. Every mutation goes through `with-write-lock` so the API
  and the file watcher never interleave a file write with its re-indexing."
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [notebook.links :as links]
            [notebook.notes.index :as index]
            [notebook.notes.parse :as parse]
            [notebook.notes.store :as store]
            [notebook.sse :as sse]))

;; ctx = {:root notes-dir :db conn :hub sse-hub :lock (Object.)}

(defmacro with-write-lock [ctx & body]
  `(locking (:lock ~ctx) ~@body))

(defn- catalog-entry [doc]
  (select-keys doc [:slug :type :title :mtime :created :lang :excerpt :tags :hash]))

(defn- broadcast! [{:keys [hub]} event data]
  (when hub (sse/broadcast! hub event data)))

(defn index-note!
  "(Re)index one note from disk. Returns the doc, or nil if the file is gone.
  Skips work (and the broadcast) when the content hash is unchanged."
  [{:keys [root db] :as ctx} type slug & {:keys [force?]}]
  (if-let [note (store/read-note root type slug)]
    (let [known (:hash (index/note-meta db slug))]
      (when (or force? (not= known (:hash note)))
        (let [doc (parse/index-doc note)]
          (index/upsert! db doc)
          (broadcast! ctx :note-upserted (catalog-entry doc))
          doc)))
    (when (index/note-meta db slug)
      (when-not (store/find-type root slug)
        (index/delete! db slug)
        (broadcast! ctx :note-deleted {:slug slug}))
      nil)))

(defn reconcile!
  "Bring the index in line with the files on disk (startup)."
  [{:keys [root db] :as ctx}]
  (with-write-lock ctx
    (let [files  (store/list-note-files root)
          known  (index/hashes db)
          on-disk (set (map :slug files))]
      (doseq [slug (keys known) :when (not (on-disk slug))]
        (index/delete! db slug))
      (doseq [{:keys [type slug]} files]
        (let [note (store/read-note root type slug)]
          (when (not= (known slug) (:hash note))
            (index/upsert! db (parse/index-doc note)))))
      (log/infof "Index reconciled: %d notes" (count files)))))

;; ---------------------------------------------------------------------------
;; Queries

(defn get-note [{:keys [root db]} slug]
  (when-let [type (store/find-type root slug)]
    (let [note (store/read-note root type slug)
          meta (index/note-meta db slug)]
      (merge meta
             {:slug      slug
              :type      (name type)
              :content   (:content note)
              :hash      (:hash note)
              :backlinks (index/backlinks db slug)
              :outgoing  (index/outgoing db slug)}))))

;; ---------------------------------------------------------------------------
;; Mutations

(defn create! [{:keys [root] :as ctx} {:keys [type title content tags lang slug]}]
  (let [type  (if (= (some-> type name) "snippet") :snippet :article)
        title (or (some-> title str str/trim not-empty) "Untitled")]
    (with-write-lock ctx
      (let [slug    (store/unique-slug root (links/slugify (or (not-empty slug) title)))
            content (or content (store/new-note-content {:type type :title title
                                                         :tags (or tags []) :lang lang}))]
        (store/write-atomic! (store/note-file root type slug) content)
        (index-note! ctx type slug :force? true)
        (get-note ctx slug)))))

(defn save!
  "Write `content` if the file on disk still has `base-hash`.
  Returns {:ok hash} or {:conflict {:content :hash}} or {:missing true}."
  [{:keys [root] :as ctx} slug content base-hash]
  (with-write-lock ctx
    (if-let [type (store/find-type root slug)]
      (let [current (store/read-note root type slug)]
        (cond
          (= (:hash current) (store/sha1 content)) {:ok (:hash current)}
          (and base-hash (not= base-hash (:hash current)))
          {:conflict {:content (:content current) :hash (:hash current)}}
          :else
          (let [h (store/write-atomic! (store/note-file root type slug) content)]
            (index-note! ctx type slug)
            {:ok h})))
      {:missing true})))

(defn delete! [{:keys [root db] :as ctx} slug]
  (with-write-lock ctx
    (when-let [type (store/find-type root slug)]
      (.delete (store/note-file root type slug))
      (index/delete! db slug)
      (broadcast! ctx :note-deleted {:slug slug})
      true)))

(defn rename!
  "Rename `slug` to a new title (and slug), moving the file and rewriting
  every [[link]] that points at it.

  `content` (optional) is the editor's current text, used instead of the file
  so unsaved edits survive the rename; it is only accepted when the file still
  has `base-hash`. Returns {:slug :changed [...] :content :hash} or
  {:error ...} (with :conflict true when base-hash is stale)."
  [{:keys [root db] :as ctx} slug {:keys [title new-slug content base-hash]}]
  (with-write-lock ctx
    (if-let [type (store/find-type root slug)]
      (let [note   (store/read-note root type slug)
            title  (or (some-> title str str/trim not-empty)
                       (:title (index/note-meta db slug)))
            target (links/slugify (or (some-> new-slug str/trim not-empty) title))]
        (cond
          (and content base-hash (not= base-hash (:hash note)))
          {:error "The note changed on disk; resolve the conflict before renaming"
           :conflict true}

          (and (not= target slug) (store/exists? root target))
          {:error (str "A note named \"" target "\" already exists")}

          :else
          (let [content  (-> (or content (:content note))
                             (store/set-title title)
                             (links/rewrite-links slug target))
                referers (remove #{slug} (index/referencing db slug))
                ;; 1. write the renamed note at its new path
                hash     (store/write-atomic! (store/note-file root type target) content)]
            (when (not= target slug)
              (.delete (store/note-file root type slug))
              (index/delete! db slug))
            (index-note! ctx type target :force? true)
            ;; 2. rewrite references in other notes
            (let [changed (vec
                           (for [src referers
                                 :let [stype (store/find-type root src)
                                       sn    (when stype (store/read-note root stype src))]
                                 :when sn
                                 :let [new (links/rewrite-links (:content sn) slug target)]
                                 :when (not= new (:content sn))]
                             (do (store/write-atomic! (store/note-file root stype src) new)
                                 (index-note! ctx stype src)
                                 src)))]
              (when (not= target slug)
                (broadcast! ctx :note-renamed {:from slug :to target}))
              {:slug target :changed changed :content content :hash hash}))))
      {:error "Not found"})))
