(ns notebook.routes
  (:require [re-frame.core :as rf]
            [reitit.frontend :as rfront]
            [reitit.frontend.easy :as rfe]))

(def routes
  [["/" :home]
   ["/n/:slug" :read]
   ["/n/:slug/edit" :edit]
   ["/search" :search]
   ["/tags" :tags]
   ["/tags/:tag" :tag]])

(def router (rfront/router routes))

(defn href
  ([name] (rfe/href name))
  ([name params] (rfe/href name params))
  ([name params query] (rfe/href name params query)))

(defn start! []
  (rfe/start! router
              (fn [match _history] (rf/dispatch [:route/changed match]))
              {:use-fragment false}))

(rf/reg-fx
 :nav
 (fn [[name params query replace?]]
   (if replace?
     (rfe/replace-state name params query)
     (rfe/push-state name params query))))

(rf/reg-fx
 :scroll-to-hash
 (fn [hash]
   (js/requestAnimationFrame
    (fn []
      (if-let [el (and hash (js/document.getElementById hash))]
        (.scrollIntoView el #js {:block "start"})
        (js/window.scrollTo 0 0))))))
