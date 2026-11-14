(ns notebook.views.common
  (:require [notebook.fuzzy :as fuzzy]
            [notebook.routes :refer [href]]))

(defn fmt-date [ms]
  (when ms
    (.toLocaleDateString (js/Date. ms) "en-GB" #js {:day "numeric" :month "short" :year "numeric"})))

(defn type-badge [type]
  [:span {:class (str "badge badge--" type)} (if (= type "snippet") "Snippet" "Article")])

(defn tag-list [tags]
  (when (seq tags)
    (into [:span.tags]
          (for [t tags] [:a.tag {:key t :href (href :tag {:tag t})} "#" t]))))

(defn highlighted
  "Render `text` with fuzzy-matched positions wrapped in <mark>."
  [text positions]
  (if (empty? positions)
    [:<> text]
    (let [rs (fuzzy/ranges positions)]
      (into [:<>]
            (loop [i 0, [[s e] & more] rs, acc []]
              (if (nil? s)
                (conj acc (subs text i))
                (recur e more (conj acc (subs text i s) ^{:key s} [:mark (subs text s e)]))))))))

(defn note-card [{:keys [slug title type excerpt tags mtime lang]}]
  [:a {:class (str "card card--" type) :href (href :read {:slug slug})}
   [:div.card__head
    [:span.card__title title]
    (when lang [:span.card__lang lang])]
   (when (seq excerpt)
     [:div {:class (if (= type "snippet") "card__excerpt card__excerpt--code" "card__excerpt")} excerpt])
   [:div.card__meta
    [:span (fmt-date mtime)]
    (when (seq tags) [:span.card__tags (for [t tags] ^{:key t} [:span "#" t])])]])
