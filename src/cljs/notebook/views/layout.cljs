(ns notebook.views.layout
  (:require [notebook.routes :refer [href]]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn- nav-section [_title _items]
  (let [open? (r/atom true)]
    (fn [title items current]
      [:section.nav__section
       [:button.nav__heading {:on-click #(swap! open? not) :aria-expanded @open?}
        [:span title] [:span.nav__count (count items)]]
       (when @open?
         (into [:ul.nav__list]
               (for [{:keys [slug title]} items]
                 [:li {:key slug}
                  [:a {:class (str "nav__link" (when (= slug current) " is-active"))
                       :href (href :read {:slug slug})}
                   title]])))])))

(defn sidebar []
  (let [articles @(rf/subscribe [:by-type :article])
        snippets @(rf/subscribe [:by-type :snippet])
        tags     @(rf/subscribe [:tags])
        route    @(rf/subscribe [:route])
        current  (get-in route [:params :slug])]
    [:nav.nav
     [:a.nav__brand {:href (href :home)}
      [:span.nav__mark "¶"] [:span "Notebook"]]
     [:button.nav__search {:on-click #(rf/dispatch [:palette/open :go])}
      [:span "Jump to…"] [:kbd "Ctrl K"]]
     [:div.nav__actions
      [:button.btn.btn--small {:on-click #(rf/dispatch [:palette/open :new-article])} "+ Article"]
      [:button.btn.btn--small {:on-click #(rf/dispatch [:palette/open :new-snippet])} "+ Snippet"]]
     [:div.nav__scroll
      [nav-section "Articles" articles current]
      [nav-section "Snippets" snippets current]
      (when (seq tags)
        [:section.nav__section
         [:a.nav__heading {:href (href :tags)} [:span "Tags"] [:span.nav__count (count tags)]]
         [:div.nav__tags
          (for [{:keys [tag count]} (take 30 tags)]
            ^{:key tag} [:a.nav__tag {:href (href :tag {:tag tag})} "#" tag [:sup count]])]])]
     [:a.nav__footer {:href (href :search)} "Full-text search"]]))
