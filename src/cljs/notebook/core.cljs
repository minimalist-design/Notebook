(ns notebook.core
  (:require [notebook.api :as api]
            [notebook.events]
            [notebook.routes :as routes]
            [notebook.subs]
            [notebook.views.edit :as edit]
            [notebook.views.layout :as layout]
            [notebook.views.pages :as pages]
            [notebook.views.palette :as palette]
            [notebook.views.read :as read]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [reagent.dom.client :as rdc]))

(defn page []
  (let [{:keys [name params]} @(rf/subscribe [:route])]
    (case name
      :home   [pages/home]
      :read   ^{:key (:slug params)} [read/read-page (:slug params)]
      :edit   ^{:key (:slug params)} [edit/edit-page (:slug params)]
      :search [pages/search-page]
      :tags   [pages/tags-page]
      :tag    [pages/tag-page]
      [:div.loading ""])))

(defn app []
  (let [{:keys [name]} @(rf/subscribe [:route])]
    [:div {:class (str "app app--" (some-> name clojure.core/name))}
     [layout/sidebar]
     [:main.main [page]]
     [palette/palette]]))

(defn- on-keydown [^js e]
  (let [mod? (or (.-ctrlKey e) (.-metaKey e))
        ;; some synthetic keydowns (autofill, IME) carry no .key
        k    (some-> (.-key e) .toLowerCase)]
    (cond
      (and mod? (= k "k"))
      (do (.preventDefault e) (rf/dispatch [:palette/open :go]))

      (and mod? (= k "e"))
      (let [{:keys [name params]} (:route @rfdb/app-db)]
        (when (#{:read :edit} name)
          (.preventDefault e)
          (rf/dispatch [:nav/go (if (= name :read) :edit :read) params])))

      (and mod? (= k "s"))
      (do (.preventDefault e) (rf/dispatch [:editor/flush])))))

(defonce root (atom nil))

(defn mount! []
  (rf/clear-subscription-cache!)
  (when-not @root
    (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [app]))

(defn after-load [] (mount!))

(defn init []
  (rf/dispatch-sync [:app/init])
  (routes/start!)
  (api/connect-sse!)
  (.addEventListener js/document "keydown" on-keydown)
  (.addEventListener js/window "beforeunload"
                     (fn [^js e]
                       (let [{:keys [text saved-text]} (:editor @rfdb/app-db)]
                         (when (and text (not= text saved-text))
                           (rf/dispatch-sync [:editor/flush])
                           (.preventDefault e)
                           (set! (.-returnValue e) "")))))
  (mount!))
