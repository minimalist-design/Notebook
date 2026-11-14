(ns notebook.notes.store
  "The notes directory on disk is the source of truth:
     <dir>/articles/<slug>.md
     <dir>/snippets/<slug>.md
     <dir>/assets/<file>"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-yaml.core :as yaml]
            [notebook.links :as links])
  (:import (java.nio.file Files StandardCopyOption CopyOption Path)
           (java.security MessageDigest)
           (java.time LocalDate)))

(def types {:article "articles" :snippet "snippets"})
(def dir->type {"articles" :article "snippets" :snippet})

(defn ensure-dirs! [root]
  (doseq [d ["articles" "snippets" "assets"]]
    (.mkdirs (io/file root d))))

(defn note-file ^java.io.File [root type slug]
  (io/file root (types type) (str slug ".md")))

(defn assets-dir ^java.io.File [root] (io/file root "assets"))

(defn sha1 [^String s]
  (let [d (.digest (MessageDigest/getInstance "SHA-1") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(defn path->id
  "File -> {:type :slug} when it is a note file inside root, else nil."
  [root ^java.io.File f]
  (let [rootp  (.toPath (.getCanonicalFile (io/file root)))
        p      (.toPath (.getAbsoluteFile f))
        p      (if (.startsWith p rootp) p (.toPath (.getCanonicalFile f)))
        rel    (when (.startsWith p rootp) (.relativize rootp p))]
    (when (and rel (= 2 (.getNameCount ^Path rel)))
      (let [dir  (str (.getName ^Path rel 0))
            name (str (.getName ^Path rel 1))]
        (when-let [type (dir->type dir)]
          (when (and (str/ends-with? name ".md") (not (str/starts-with? name ".")))
            {:type type :slug (subs name 0 (- (count name) 3))}))))))

(defn list-note-files [root]
  (for [[type dir] types
        ^java.io.File f (sort (.listFiles (io/file root dir)))
        :when (and (.isFile f) (str/ends-with? (.getName f) ".md")
                   (not (str/starts-with? (.getName f) ".")))]
    {:type type :slug (subs (.getName f) 0 (- (count (.getName f)) 3)) :file f}))

(defn parse-yaml [s]
  (try
    (let [m (yaml/parse-string s :keywords true)]
      (if (map? m) m {}))
    (catch Exception _ {})))

(defn read-note
  "Read a note file -> {:type :slug :content :hash :mtime :meta :body} or nil."
  [root type slug]
  (let [f (note-file root type slug)]
    (when (.isFile f)
      (let [content (slurp f :encoding "UTF-8")
            {:keys [frontmatter body]} (links/split-frontmatter content)]
        {:type    type
         :slug    slug
         :file    f
         :content content
         :hash    (sha1 content)
         :mtime   (.lastModified f)
         :meta    (if frontmatter (parse-yaml frontmatter) {})
         :body    body}))))

(defn write-atomic!
  "Write via a temp file in the same dir + atomic move, so watchers and
  readers never see a half-written file."
  [^java.io.File f ^String content]
  (.mkdirs (.getParentFile f))
  (let [tmp (io/file (.getParentFile f) (str "." (.getName f) "." (System/nanoTime) ".tmp"))]
    (spit tmp content :encoding "UTF-8")
    (try
      (Files/move (.toPath tmp) (.toPath f)
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (catch java.nio.file.AtomicMoveNotSupportedException _
        (Files/move (.toPath tmp) (.toPath f)
                    (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))))
    (sha1 content)))

(defn move-file! [^java.io.File from ^java.io.File to]
  (.mkdirs (.getParentFile to))
  (Files/move (.toPath from) (.toPath to) (into-array CopyOption [])))

(defn valid-slug?
  "A slug names a file directly inside articles/ or snippets/. Hand-made files
  may have any name, so this only refuses what could escape that directory."
  [slug]
  (boolean (and (string? slug)
                (not (str/blank? slug))
                (<= (count slug) 255)
                (not (str/starts-with? slug "."))
                (not (re-find #"[/\\\x00]" slug)))))

(defn exists? [root slug]
  (and (valid-slug? slug)
       (some #(.isFile (note-file root % slug)) (keys types))))

(defn find-type
  "The type of the note file for `slug`, or nil (also for invalid slugs, so
  every API lookup that goes through here is confined to the notes dirs)."
  [root slug]
  (when (valid-slug? slug)
    (some #(when (.isFile (note-file root % slug)) %) (keys types))))

(defn unique-slug [root base]
  (if-not (exists? root base)
    base
    (some #(let [s (str base "-" %)] (when-not (exists? root s) s)) (iterate inc 2))))

(defn yaml-str [v]
  (let [s (str v)]
    (if (re-find #"^[\w][\w .,'()\-/]*$" s)
      s
      (str "\"" (-> s (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) "\""))))

(defn new-note-content [{:keys [type title tags lang body]}]
  (str "---\n"
       "title: " (yaml-str title) "\n"
       "tags: [" (str/join ", " (map yaml-str tags)) "]\n"
       (when (and (= type :snippet) lang) (str "lang: " (yaml-str lang) "\n"))
       "created: " (LocalDate/now) "\n"
       "---\n\n"
       (or body (if (= type :snippet) "" ""))))

(defn set-title
  "Replace (or insert) the `title:` line of the frontmatter."
  [content title]
  (let [{:keys [frontmatter body]} (links/split-frontmatter content)
        line (str "title: " (yaml-str title))]
    (if frontmatter
      (let [fm (if (re-find #"(?m)^title:.*$" frontmatter)
                 (str/replace-first frontmatter #"(?m)^title:.*$"
                                    (str/re-quote-replacement line))
                 (str line "\n" frontmatter))]
        (str "---\n" fm "\n---\n" body))
      (str "---\n" line "\n---\n\n" body))))

;; ---------------------------------------------------------------------------
;; Assets

(defn safe-asset-name [name]
  (let [name (-> (or name "file") (str/replace #".*[/\\]" ""))
        [_ base ext] (or (re-matches #"(.*?)(\.[A-Za-z0-9]{1,8})?" name) [nil name nil])
        base (-> (links/slugify base))]
    (str base (some-> ext str/lower-case))))

(defn unique-asset-name [root name]
  (let [dir (assets-dir root)
        [_ base ext] (re-matches #"(.*?)(\.[^.]*)?" name)]
    (if-not (.exists (io/file dir name))
      name
      (some #(let [n (str base "-" % ext)] (when-not (.exists (io/file dir n)) n))
            (iterate inc 2)))))

(defn list-assets [root]
  (->> (.listFiles (assets-dir root))
       (filter #(and (.isFile ^java.io.File %) (not (str/starts-with? (.getName ^java.io.File %) "."))))
       (sort-by #(- (.lastModified ^java.io.File %)))
       (mapv (fn [^java.io.File f] {:name (.getName f) :size (.length f) :mtime (.lastModified f)}))))
