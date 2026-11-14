(ns notebook.events
  (:require [clojure.string :as str]
            [notebook.api]
            [notebook.links :as links]
            [re-frame.core :as rf]))

(def default-db
  {:catalog  {}     ; slug -> {:slug :type :title :mtime :tags :excerpt :lang}
   :notes    {}     ; slug -> full note (content, hash, backlinks, outgoing)
   :loading  #{}    ; slugs being fetched
   :assets   []
   :tags     []
   :route    nil
   :editor   nil
   :palette  {:open? false :query "" :sel 0 :mode :go}
   :search   {:query "" :results [] :loading? false}
   :tag-view {:tag nil :notes []}})

(defn- index-catalog [rows]
  (into {} (map (juxt :slug identity)) rows))

;; ---------------------------------------------------------------------------
;; Boot

(rf/reg-event-fx
 :app/init
 (fn [_ _]
   {:db (assoc default-db :booting? true)
    :fx [[:http {:url "/api/notes" :on-ok [:catalog/loaded]}]
         [:http {:url "/api/assets" :on-ok [:assets/loaded]}]
         [:http {:url "/api/tags" :on-ok [:tags/loaded]}]]}))

(rf/reg-event-db
 :catalog/loaded
 (fn [db [_ rows]]
   (assoc db :catalog (index-catalog rows) :booting? false)))

(rf/reg-event-db :assets/loaded (fn [db [_ rows]] (assoc db :assets rows)))
(rf/reg-event-db :tags/loaded (fn [db [_ rows]] (assoc db :tags rows)))

(rf/reg-event-fx
 :catalog/refresh
 (fn [_ _]
   {:fx [[:http {:url "/api/notes" :on-ok [:catalog/loaded]}]
         [:http {:url "/api/tags" :on-ok [:tags/loaded]}]]}))

;; ---------------------------------------------------------------------------
;; Routing

(defn- snapshot-fx
  "Save text left behind by an editor that is no longer open."
  [slug text base-hash]
  [:http {:method :put :url (str "/api/notes/" (js/encodeURIComponent slug))
          :body {:content text :base_hash base-hash}
          :on-err [:editor/snapshot-failed slug]}])

(rf/reg-event-fx
 :route/changed
 (fn [{:keys [db]} [_ match]]
   (let [name   (get-in match [:data :name])
         slug   (get-in match [:path-params :slug])
         prev   (:route db)
         db     (assoc db :route {:name name :params (:path-params match)
                                  :query (:query-params match)}
                       :palette (:palette default-db))
         same-note? (and slug (= slug (get-in prev [:params :slug])))
         {ed-slug :slug :keys [text saved-text base-hash saving sid renaming]} (:editor db)
         keep-editor? (and ed-slug (= ed-slug slug) (#{:read :edit} name))
         ;; text already sent with an in-flight rename is saved by that rename
         unsaved? (and ed-slug (not keep-editor?) text (not= text saved-text)
                       (not= text renaming))
         ;; leaving an editor with unsaved text: save a snapshot of it. If a
         ;; save is still in flight, its response carries the hash the
         ;; snapshot must be based on, so wait for it (see :editor/saved).
         leave-fx (when (and unsaved? (not saving))
                    (snapshot-fx ed-slug text base-hash))
         db (cond-> db
              (and unsaved? saving) (assoc-in [:deferred-saves sid] {:slug ed-slug :text text})
              (not keep-editor?) (assoc :editor nil))
         res
     (case name
       :read   {:db db
                :fx [(when (and keep-editor? (= :edit (:name prev))) [:dispatch [:editor/flush]])
                     [:dispatch [:note/fetch slug]]
                     (when-not same-note? [:scroll-to-hash (some-> js/location.hash (subs 1) js/decodeURIComponent not-empty)])]}
       :edit   {:db db :fx [[:dispatch [:editor/open slug]]]}
       :search {:db (assoc-in db [:search :query] (get-in match [:query-params :q] ""))
                :fx [[:dispatch [:search/run (get-in match [:query-params :q] "")]]]}
       :tag    {:db db :fx [[:dispatch [:tag/fetch (get-in match [:path-params :tag])]]
                            [:scroll-to-hash nil]]}
       :tags   {:db db :fx [[:http {:url "/api/tags" :on-ok [:tags/loaded]}]]}
       {:db db :fx [[:scroll-to-hash nil]]})]
     (cond-> res leave-fx (update :fx conj leave-fx)))))

(rf/reg-event-fx
 :editor/snapshot-failed
 (fn [_ [_ slug {:keys [status]}]]
   (js/alert (str "Unsaved changes to \"" slug "\" could not be saved"
                  (when (= status 409) " (the file changed on disk)") "."))
   {}))

(rf/reg-event-fx
 :nav/go
 (fn [_ [_ & args]] {:nav (vec args)}))

;; ---------------------------------------------------------------------------
;; Notes cache

(rf/reg-event-fx
 :note/fetch
 (fn [{:keys [db]} [_ slug]]
   (when (and slug (not (contains? (:loading db) slug)))
     {:db (update db :loading conj slug)
      :http {:url (str "/api/notes/" (js/encodeURIComponent slug))
             :on-ok [:note/loaded slug] :on-err [:note/failed slug]}})))

(rf/reg-event-fx
 :note/ensure
 (fn [{:keys [db]} [_ slug]]
   (when (and (get-in db [:catalog slug])
              (not (get-in db [:notes slug]))
              (not (contains? (:loading db) slug)))
     {:dispatch [:note/fetch slug]})))

(rf/reg-event-fx
 :note/ensure-many
 (fn [_ [_ slugs]]
   {:fx (mapv (fn [s] [:dispatch [:note/ensure s]]) (distinct slugs))}))

(rf/reg-event-fx
 :note/loaded
 (fn [{:keys [db]} [_ slug note]]
   {:db (-> db
            (update :loading disj slug)
            (assoc-in [:notes slug] note))
    ;; side cards in the rail show snippet bodies
    :fx [[:dispatch [:note/ensure-many (->> (:outgoing note)
                                            (filter #(= "snippet" (:type %)))
                                            (map :slug))]]]}))

(rf/reg-event-db
 :note/failed
 (fn [db [_ slug {:keys [status]}]]
   (-> db
       (update :loading disj slug)
       (assoc-in [:notes slug] {:slug slug :missing? (= status 404)}))))

(rf/reg-event-fx
 :note/create
 (fn [_ [_ {:keys [type title open?]}]]
   {:http {:method :post :url "/api/notes"
           :body {:type (name (or type :article)) :title title}
           :on-ok [:note/created open?]}}))

(rf/reg-event-fx
 :note/created
 (fn [{:keys [db]} [_ open? note]]
   {:db (-> db
            (assoc-in [:catalog (:slug note)] (dissoc note :content :backlinks :outgoing))
            (assoc-in [:notes (:slug note)] note))
    :fx [(when open? [:nav [:edit {:slug (:slug note)}]])]}))

(rf/reg-event-fx
 :note/create-from-link
 (fn [{:keys [db]} [_ raw-title]]
   (let [slug (links/slugify raw-title)]
     (if (get-in db [:catalog slug])
       {:nav [:read {:slug slug}]}
       {:dispatch [:note/create {:type :article :title raw-title :open? true}]}))))

(rf/reg-event-fx
 :note/rename
 ;; When the note is open in the editor, its current text travels with the
 ;; rename (checked against base_hash on the server), so unsaved edits are
 ;; neither lost nor raced by an autosave to the old slug.
 (fn [{:keys [db]} [_ slug title]]
   (let [{ed-slug :slug :keys [text base-hash saving status]} (:editor db)
         editing? (and (= ed-slug slug) (some? text))]
     (cond
       (and editing? (= status :conflict))
       (do (js/alert "Resolve the conflict with the file on disk before renaming.") {})

       ;; let the in-flight save land first; :editor/saved resumes the rename
       (and editing? saving)
       {:db (assoc-in db [:editor :pending-rename] title)}

       :else
       {:db (cond-> db editing? (update :editor assoc :renaming text :pending-rename nil))
        :cancel-debounce :editor-save
        :http {:method :post :url (str "/api/notes/" (js/encodeURIComponent slug) "/rename")
               :body (cond-> {:title title}
                       editing? (assoc :content text :base_hash base-hash))
               :on-ok [:note/renamed slug] :on-err [:note/rename-failed slug]}}))))

(rf/reg-event-fx
 :note/renamed
 (fn [{:keys [db]} [_ old {:keys [slug changed content hash]}]]
   (let [mode   (get-in db [:route :name])
         editor (:editor db)
         sent   (:renaming editor)
         ;; carry the open editor over to the new slug instead of dropping it
         editor (when (and (= old (:slug editor)) (some? sent))
                  (let [e (assoc editor :slug slug :base-hash hash :saved-text content
                                 :renaming nil :saving nil)]
                    (if (= (:text editor) sent)
                      ;; take the server's version (new title, rewritten self-links)
                      (-> e (assoc :text content :status :saved) (update :reload inc))
                      ;; typed during the request: let the user choose
                      (assoc e :status :conflict :conflict {:content content :hash hash}))))]
     {:db (-> db
              (update :notes #(apply dissoc % old changed))
              (update :catalog dissoc old)
              (assoc :editor editor))
      :fx [[:dispatch [:catalog/refresh]]
           [:nav [(if (= mode :edit) :edit :read) {:slug slug} nil true]]]})))

(rf/reg-event-fx
 :note/rename-failed
 (fn [{:keys [db]} [_ slug {:keys [status body]}]]
   (js/alert (or (:error body) "Rename failed"))
   (when (= slug (get-in db [:editor :slug]))
     {:db (update db :editor assoc :renaming nil :pending-rename nil)
      :fx [(when (= status 409) [:dispatch [:editor/external-check]])
           [:dispatch [:editor/save]]]})))

(rf/reg-event-fx
 :note/delete
 (fn [_ [_ slug]]
   {:http {:method :delete :url (str "/api/notes/" (js/encodeURIComponent slug))
           :on-ok [:note/deleted slug]}}))

(rf/reg-event-fx
 :note/deleted
 (fn [{:keys [db]} [_ slug]]
   {:db (-> db (update :catalog dissoc slug) (update :notes dissoc slug) (assoc :editor nil))
    :fx [[:nav [:home]] [:dispatch [:catalog/refresh]]]}))

;; ---------------------------------------------------------------------------
;; Live updates (SSE)

(rf/reg-event-fx
 :sse/open
 (fn [{:keys [db]} _]
   ;; after a reconnect anything may have changed
   (when-not (:booting? db) {:dispatch [:catalog/refresh]})))

(rf/reg-event-fx
 :sse/upserted
 (fn [{:keys [db]} [_ {:keys [slug hash] :as entry}]]
   (let [cached  (get-in db [:notes slug])
         editing (= slug (get-in db [:editor :slug]))
         db      (assoc-in db [:catalog slug] (dissoc entry :hash))]
     {:db db
      :fx [(when (and cached (not= hash (:hash cached)) (not editing))
             [:dispatch [:note/fetch slug]])
           (when (and editing (not= hash (get-in db [:editor :base-hash])))
             [:dispatch [:editor/external-check]])
           ;; links/backlinks of the open note may have changed
           (let [cur (get-in db [:route :params :slug])]
             (when (and cur (not= cur slug) (not= cur (get-in db [:editor :slug]))
                        (get-in db [:notes cur]))
               [:dispatch-later {:ms 50 :dispatch [:note/refresh-links cur]}]))
           [:dispatch-later {:ms 200 :dispatch [:tags/refresh]}]]})))

(rf/reg-event-fx
 :note/refresh-links
 (fn [{:keys [db]} [_ slug]]
   {:http {:url (str "/api/notes/" (js/encodeURIComponent slug))
           :on-ok [:note/links-refreshed slug] :on-err [:noop]}}))

(rf/reg-event-fx :noop (fn [_ _] {}))

(rf/reg-event-db
 :note/links-refreshed
 (fn [db [_ slug note]]
   (update-in db [:notes slug] merge (select-keys note [:backlinks :outgoing]))))

(rf/reg-event-fx :tags/refresh (fn [_ _] {:http {:url "/api/tags" :on-ok [:tags/loaded]}}))

(rf/reg-event-fx
 :sse/deleted
 (fn [{:keys [db]} [_ {:keys [slug]}]]
   {:db (-> db (update :catalog dissoc slug) (update :notes dissoc slug))}))

(rf/reg-event-fx
 :sse/renamed
 (fn [{:keys [db]} [_ {:keys [from to]}]]
   (let [{:keys [name params]} (:route db)
         ;; our own rename: :note/renamed moves the editor and navigates
         own? (and (= from (get-in db [:editor :slug]))
                   (some? (get-in db [:editor :renaming])))]
     {:db (-> db (update :catalog dissoc from) (update :notes dissoc from))
      :fx [(when (and (not own?) (= from (:slug params)) (#{:read :edit} name))
             [:nav [name {:slug to} nil true]])]})))

;; ---------------------------------------------------------------------------
;; Editor

(rf/reg-event-fx
 :editor/open
 (fn [{:keys [db]} [_ slug]]
   (if (= slug (get-in db [:editor :slug]))
     {}
     ;; :sid identifies this editor session, so responses to requests made by
     ;; an earlier session for the same slug are never applied to this one
     (let [sid (str (random-uuid))]
       {:db (assoc db :editor {:slug slug :sid sid :loading? true})
        :http {:url (str "/api/notes/" (js/encodeURIComponent slug))
               :on-ok [:editor/loaded slug sid] :on-err [:note/failed slug]}}))))

(rf/reg-event-db
 :editor/loaded
 (fn [db [_ slug sid note]]
   (if (= sid (get-in db [:editor :sid]))
     (-> db
         (assoc-in [:notes slug] note)
         (assoc :editor {:slug       slug
                         :sid        sid
                         :text       (:content note)
                         :saved-text (:content note)
                         :base-hash  (:hash note)
                         :status     :saved
                         :reload     0}))
     db)))

(def save-delay-ms 800)

(rf/reg-event-fx
 :editor/changed
 (fn [{:keys [db]} [_ text]]
   (when (:editor db)
     (let [dirty? (not= text (get-in db [:editor :saved-text]))]
       {:db (-> db
                (assoc-in [:editor :text] text)
                (update-in [:editor :status] #(cond (= % :conflict) :conflict
                                                    dirty? :dirty
                                                    :else :saved)))
        :dispatch-debounce {:id :editor-save :ms save-delay-ms :event [:editor/save]}}))))

(rf/reg-event-fx
 :editor/save
 (fn [{:keys [db]} _]
   (let [{:keys [slug sid text saved-text base-hash saving status renaming]} (:editor db)]
     (cond
       (or (nil? slug) (nil? text) (= status :conflict) (= text saved-text)) {}
       ;; the rename request carries the text; :note/renamed takes over
       (some? renaming) {}
       saving {:dispatch-debounce {:id :editor-save :ms 300 :event [:editor/save]}}
       :else
       {:db (-> db (assoc-in [:editor :saving] text) (assoc-in [:editor :status] :saving))
        :http {:method :put :url (str "/api/notes/" (js/encodeURIComponent slug))
               :body {:content text :base_hash base-hash}
               :on-ok [:editor/saved slug sid text] :on-err [:editor/save-failed slug sid]}}))))

(rf/reg-event-fx
 :editor/saved
 (fn [{:keys [db]} [_ slug sid text {:keys [hash]}]]
   (if (= sid (get-in db [:editor :sid]))
     (let [pending-rename (get-in db [:editor :pending-rename])
           db (-> db
                  (update :editor assoc :saved-text text :base-hash hash :saving nil
                          :pending-rename nil)
                  (update-in [:notes slug] assoc :content text :hash hash))
           dirty? (not= text (get-in db [:editor :text]))]
       {:db (assoc-in db [:editor :status] (if dirty? :dirty :saved))
        :fx [(cond pending-rename [:dispatch [:note/rename slug pending-rename]]
                   dirty?         [:dispatch [:editor/save]])]})
     ;; the editor was closed while this save was in flight
     (when-let [{d-slug :slug d-text :text} (get-in db [:deferred-saves sid])]
       {:db (update db :deferred-saves dissoc sid)
        :fx [(when (not= d-text text) (snapshot-fx d-slug d-text hash))]}))))

(def save-retry-ms 3000)

(rf/reg-event-fx
 :editor/save-failed
 (fn [{:keys [db]} [_ slug sid {:keys [status body] :as res}]]
   (if (= sid (get-in db [:editor :sid]))
     (let [pending-rename (get-in db [:editor :pending-rename])
           db (cond-> (update db :editor assoc :saving nil :pending-rename nil)
                (= status 409) (update :editor assoc :status :conflict :conflict body)
                (not= status 409) (assoc-in [:editor :status] :error))]
       {:db db
        :fx [(cond
               ;; :note/rename re-checks for a conflict; otherwise it sends the text itself
               pending-rename  [:dispatch [:note/rename slug pending-rename]]
               ;; transient failure (server restarting, network): keep trying.
               ;; 404 means the file is gone; retrying cannot help.
               (not (#{404 409} status))
               [:dispatch-debounce {:id :editor-save :ms save-retry-ms :event [:editor/save]}])]})
     (when-let [{d-slug :slug} (get-in db [:deferred-saves sid])]
       {:db (update db :deferred-saves dissoc sid)
        :fx [[:dispatch [:editor/snapshot-failed d-slug res]]]}))))

(rf/reg-event-fx
 :editor/external-check
 (fn [{:keys [db]} _]
   (when-let [slug (get-in db [:editor :slug])]
     {:http {:url (str "/api/notes/" (js/encodeURIComponent slug))
             :on-ok [:editor/external-loaded slug]}})))

(rf/reg-event-db
 :editor/external-loaded
 (fn [db [_ slug {:keys [content hash] :as note}]]
   (let [{ed-slug :slug :keys [text saved-text saving base-hash]} (:editor db)]
     (cond
       (not= slug ed-slug) db
       (= hash base-hash) db
       ;; our own save, echoed back
       (or (= content saving) (= content saved-text) (= content text))
       (update db :editor assoc :base-hash hash :saved-text content)
       ;; no local edits -> silently take the new version
       (and (= text saved-text) (not saving))
       (-> db
           (assoc-in [:notes slug] note)
           (update :editor assoc :text content :saved-text content :base-hash hash
                   :status :saved)
           (update-in [:editor :reload] inc))
       :else
       (update db :editor assoc :status :conflict :conflict {:content content :hash hash})))))

(rf/reg-event-fx
 :editor/resolve
 (fn [{:keys [db]} [_ choice]]
   (let [{:keys [conflict]} (:editor db)]
     (case choice
       :theirs {:db (-> db
                        (update :editor assoc :text (:content conflict) :saved-text (:content conflict)
                                :base-hash (:hash conflict) :status :saved :conflict nil)
                        (update-in [:editor :reload] inc))}
       :mine   {:db (update db :editor assoc :base-hash (:hash conflict) :status :dirty
                        :saved-text (:content conflict) :conflict nil)
                :dispatch [:editor/save]}))))

(rf/reg-event-fx
 :editor/flush
 (fn [_ _]
   {:cancel-debounce :editor-save
    :dispatch [:editor/save]}))

(rf/reg-event-db
 :assets/added
 (fn [db [_ name]]
   (update db :assets #(into [{:name name}] (remove (fn [a] (= name (:name a)))) %))))

;; ---------------------------------------------------------------------------
;; Search & tags

(rf/reg-event-fx
 :search/run
 (fn [{:keys [db]} [_ q]]
   (if (str/blank? q)
     {:db (update db :search assoc :query "" :results [] :loading? false)}
     {:db (update db :search assoc :query q :loading? true)
      :http {:url (str "/api/search?q=" (js/encodeURIComponent q))
             :on-ok [:search/loaded q]}})))

(rf/reg-event-db
 :search/loaded
 (fn [db [_ q results]]
   (if (= q (get-in db [:search :query]))
     (update db :search assoc :results results :loading? false)
     db)))

(rf/reg-event-fx
 :search/input
 (fn [{:keys [db]} [_ q]]
   {:db (assoc-in db [:search :query] q)
    :dispatch-debounce {:id :search :ms 180
                        :event [:nav/go :search nil (when-not (str/blank? q) {:q q}) true]}}))

(rf/reg-event-fx
 :tag/fetch
 (fn [{:keys [db]} [_ tag]]
   {:db (assoc db :tag-view {:tag tag :notes [] :loading? true})
    :http {:url (str "/api/tags/" (js/encodeURIComponent tag)) :on-ok [:tag/loaded tag]}}))

(rf/reg-event-db
 :tag/loaded
 (fn [db [_ tag notes]]
   (assoc db :tag-view {:tag tag :notes notes})))

;; ---------------------------------------------------------------------------
;; Palette

(rf/reg-event-db
 :palette/open
 (fn [db [_ mode]]
   (assoc db :palette {:open? true :query "" :sel 0 :mode (or mode :go)})))

(rf/reg-event-db :palette/close (fn [db _] (assoc-in db [:palette :open?] false)))

(rf/reg-event-db
 :palette/query
 (fn [db [_ q]] (update db :palette assoc :query q :sel 0)))

(rf/reg-event-db
 :palette/mode
 (fn [db [_ mode]] (update db :palette assoc :mode mode :sel 0)))

(rf/reg-event-db
 :palette/move
 (fn [db [_ delta n]]
   (update-in db [:palette :sel] #(if (pos? n) (mod (+ % delta) n) 0))))
