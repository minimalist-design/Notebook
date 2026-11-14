(ns notebook.views.pages
  "Home, search and tag pages."
  (:require [notebook.routes :refer [href]]
            [notebook.views.common :as c]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn home []
  (let [articles @(rf/subscribe [:by-type :article])
        snippets @(rf/subscribe [:by-type :snippet])
        booting? @(rf/subscribe [:booting?])]
    [:div.home
     [:header.home__header
      [:h1.home__title "Notebook"]
      [:p.home__lede (count articles) " articles · " (count snippets) " snippets"]]
     (cond
       booting? [:div.loading "Loading…"]
       (and (empty? articles) (empty? snippets))
       [:div.empty
        [:p "Nothing here yet."]
        [:button.btn.btn--primary {:on-click #(rf/dispatch [:palette/open :new-article])} "Write the first article"]]
       :else
       [:div.home__grid
        [:section.home__articles
         [:h2.section-title "Articles"]
         [:ol.article-list
          (for [{:keys [slug title excerpt mtime tags]} articles]
            ^{:key slug}
            [:li.article-list__item
             [:a.article-list__title {:href (href :read {:slug slug})} title]
             (when (seq excerpt) [:p.article-list__excerpt excerpt])
             [:div.article-list__meta [:span (c/fmt-date mtime)] [c/tag-list tags]]])]]
        [:section.home__snippets
         [:h2.section-title "Snippets"]
         [:div.card-grid
          (for [s snippets] ^{:key (:slug s)} [c/note-card s])]]])]))

(defn search-page []
  (let [{:keys [query results loading?]} @(rf/subscribe [:search])]
    [:div.search-page
     [:h1.page-title "Search"]
     [:input.input.input--large
      {:type "search" :placeholder "Search all notes…" :auto-focus true
       :default-value query :on-change #(rf/dispatch-sync [:search/input (.. % -target -value)])}]
     (cond
       (and loading? (empty? results)) [:p.muted "Searching…"]
       (and (seq query) (empty? results)) [:p.muted "No matches."]
       :else
       [:ol.results
        (for [{:keys [slug title type snippet lang]} results]
          ^{:key slug}
          [:li.result
           [:a.result__link {:href (href :read {:slug slug})}
            [:span.result__head [c/type-badge type] [:span.result__title title]
             (when lang [:span.result__lang lang])]
            [:span.result__snippet {:dangerouslySetInnerHTML (r/unsafe-html snippet)}]]])])]))

(defn tags-page []
  (let [tags @(rf/subscribe [:tags])]
    [:div.tags-page
     [:h1.page-title "Tags"]
     [:div.tag-cloud
      (for [{:keys [tag count]} tags]
        ^{:key tag} [:a.tag-cloud__tag {:href (href :tag {:tag tag})} "#" tag [:span count]])]]))

(defn tag-page []
  (let [{:keys [tag notes]} @(rf/subscribe [:tag-view])]
    [:div.tag-page
     [:h1.page-title [:span.muted "#"] tag]
     [:div.card-grid
      (for [n notes] ^{:key (:slug n)} [c/note-card n])]]))
