(ns notebook.api
  "HTTP + SSE effects."
  (:require [re-frame.core :as rf]))

(defn json->clj [s]
  (when (and s (not= s ""))
    (js->clj (.parse js/JSON s) :keywordize-keys true)))

(defn request
  "Promise of {:ok? :status :body}."
  [{:keys [method url body form-data]}]
  (let [opts (cond-> {:method (or (some-> method name .toUpperCase) "GET")
                      :headers (if form-data {} {"Accept" "application/json"})}
               body      (-> (assoc :body (.stringify js/JSON (clj->js body)))
                             (assoc-in [:headers "Content-Type"] "application/json"))
               form-data (assoc :body form-data))]
    (-> (js/fetch url (clj->js opts))
        (.then (fn [^js r]
                 (-> (.text r)
                     (.then (fn [t]
                              {:ok?    (.-ok r)
                               :status (.-status r)
                               :body   (try (json->clj t) (catch :default _ t))})))))
        (.catch (fn [e] {:ok? false :status 0 :body {:error (str e)}})))))

(rf/reg-fx
 :http
 (fn [{:keys [on-ok on-err] :as req}]
   (-> (request req)
       (.then (fn [{:keys [ok? body] :as res}]
                (if ok?
                  (when on-ok (rf/dispatch (conj on-ok body)))
                  (if on-err
                    (rf/dispatch (conj on-err res))
                    (js/console.warn "HTTP error" (clj->js req) (clj->js res)))))))))

(defn upload-asset!
  "Upload a File; promise of the stored name."
  [file]
  (let [fd (js/FormData.)]
    (.append fd "file" file (.-name file))
    (-> (request {:method :post :url "/api/assets" :form-data fd})
        (.then (fn [{:keys [ok? body]}]
                 (if ok? (:name body) (throw (js/Error. (str "Upload failed: " (:error body))))))))))

;; ---------------------------------------------------------------------------
;; Server-sent events

(defonce ^:private source (atom nil))

(defn connect-sse! []
  (when-let [^js s @source] (.close s))
  (let [s (js/EventSource. "/api/events")]
    (doseq [[ev k] [["note-upserted" :sse/upserted]
                    ["note-deleted" :sse/deleted]
                    ["note-renamed" :sse/renamed]]]
      (.addEventListener s ev (fn [^js e] (rf/dispatch [k (json->clj (.-data e))]))))
    (.addEventListener s "open" (fn [_] (rf/dispatch [:sse/open])))
    (reset! source s)))

;; ---------------------------------------------------------------------------
;; Debounced dispatch

(defonce ^:private timers (atom {}))

(rf/reg-fx
 :dispatch-debounce
 (fn [{:keys [id ms event]}]
   (js/clearTimeout (get @timers id))
   (swap! timers assoc id (js/setTimeout #(rf/dispatch event) ms))))

(rf/reg-fx
 :cancel-debounce
 (fn [id] (js/clearTimeout (get @timers id))))
