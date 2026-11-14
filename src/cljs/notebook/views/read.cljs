(ns notebook.views.read
  (:require [clojure.string :as str]
            [notebook.links :as links]
            [notebook.md.render :as md]
            [notebook.routes :refer [href]]
            [notebook.views.common :as c]
            [notebook.views.markdown :as mdv]
            [re-frame.core :as rf]
            [reagent.core :as r]))

;; ---------------------------------------------------------------------------
;; Table of contents with scroll-spy

(defn toc [_items]
  (let [active (r/atom nil)
        observer (atom nil)
        observe! (fn [items]
                   (when-let [^js o @observer] (.disconnect o))
                   (let [visible (atom {})
                         o (js/IntersectionObserver.
                            (fn [entries]
                              (doseq [^js e entries]
                                (swap! visible assoc (.. e -target -id) (.-isIntersecting e)))
                              ;; first heading on screen, else the last one scrolled past
                              (let [ids  (map :id items)
                                    vis  (first (filter #(get @visible %) ids))
                                    past (last (filter (fn [id]
                                                         (when-let [el (js/document.getElementById id)]
                                                           (< (.. el getBoundingClientRect -top) 120)))
                                                       ids))]
                                (reset! active (or vis past))))
                            #js {:rootMargin "-64px 0px -55% 0px"})]
                     (doseq [{:keys [id]} items]
                       (when-let [el (and id (js/document.getElementById id))] (.observe o el)))
                     (reset! observer o)))]
    (r/create-class
     {:component-did-mount (fn [this] (js/requestAnimationFrame #(observe! (second (r/argv this)))))
      :component-did-update (fn [this] (js/requestAnimationFrame #(observe! (second (r/argv this)))))
      :component-will-unmount (fn [] (when-let [^js o @observer] (.disconnect o)))
      :reagent-render
      (fn [items]
        (when (seq items)
          [:nav.toc
           [:h2.rail__title "Contents"]
           [:ol.toc__list
            (doall
             (for [{:keys [id text level]} items]
              ^{:key (str id level)}
              [:li {:class (str "toc__item toc__item--" level (when (= id @active) " is-active"))}
               [:a {:href (str "#" id)
                    :on-click (fn [e]
                                (.preventDefault e)
                                (when-let [el (js/document.getElementById id)]
                                  (.scrollIntoView el #js {:behavior "smooth" :block "start"})
                                  (js/history.replaceState nil "" (str "#" id))))}
                text]]))]]))})))

;; ---------------------------------------------------------------------------
;; Rail

(defn- side-snippet [{:keys [slug title lang embeds]}]
  (let [content @(rf/subscribe [:note slug])]
    [:details.side-snippet {:open (not= 1 embeds)}
     [:summary
      [:a.side-snippet__title {:href (href :read {:slug slug})} title]
      (when lang [:span.side-snippet__lang lang])]
     (if-let [text (:content content)]
       [mdv/markdown {:src (:body (links/split-frontmatter text)) :class "prose prose--side" :depth 1}]
       [:div.side-snippet__loading "…"])]))

(defn- link-list [title items]
  (when (seq items)
    [:section.rail__section
     [:h2.rail__title title]
     [:ul.linklist
      (for [{:keys [slug title type excerpt]} items]
        ^{:key slug}
        [:li
         [:a {:class (str "linklist__item linklist__item--" (or type "dangling"))
              :href (href :read {:slug slug})}
          [:span.linklist__title (or title slug)]
          (when (seq excerpt) [:span.linklist__excerpt excerpt])]])]]))

(defn rail [{:keys [toc-items note]}]
  (let [outgoing  (:outgoing note)
        snippets  (filter #(= "snippet" (:type %)) outgoing)
        articles  (filter #(= "article" (:type %)) outgoing)
        backlinks (:backlinks note)]
    [:aside.rail
     [:div.rail__inner
      [toc toc-items]
      (when (seq snippets)
        [:section.rail__section
         [:h2.rail__title "Snippets referenced"]
         (for [s snippets] ^{:key (:slug s)} [side-snippet s])])
      [link-list "Linked articles" articles]
      [link-list "Referenced by" backlinks]]]))

;; ---------------------------------------------------------------------------
;; Pages

(defn- note-header [{:keys [title type tags mtime created lang slug]} show-title?]
  [:header.note-header
   [:div.note-header__meta
    [c/type-badge type]
    (when lang [:span.note-header__lang lang])
    [:span.note-header__date (or (some-> created str (subs 0 10)) (c/fmt-date mtime))]
    [c/tag-list tags]
    [:a.note-header__edit {:href (href :edit {:slug slug}) :title "Edit (Ctrl+E)"} "Edit"]]
   (when show-title? [:h1.note-header__title title])])

(defn read-page [slug]
  (let [note     @(rf/subscribe [:note slug])
        entry    @(rf/subscribe [:catalog-entry slug])
        catalog  @(rf/subscribe [:catalog])
        contents @(rf/subscribe [:notes-content])]
    (cond
      (:missing? note)
      [:div.empty
       [:h1 "No note called “" slug "”"]
       [:p "It may have been renamed or deleted. "
        [:button.btn {:on-click #(rf/dispatch [:note/create {:type :article :title slug :open? true}])}
         "Create it"]]]

      (nil? (:content note))
      [:div.loading "Loading…"]

      :else
      (let [meta     (merge note entry)
            {:keys [body]} (links/split-frontmatter (:content note))
            snippet? (= "snippet" (:type meta))
            {:keys [html toc missing]} (md/render (:content note)
                                                  {:catalog catalog :contents contents
                                                   :depth 0 :seen #{slug}})
            show-title? (not= (some-> (links/first-heading body) str/trim) (:title meta))]
        [:div {:class (str "read " (if snippet? "read--snippet" "read--article"))}
         [:article {:class (if snippet? "snippet-page" "article")}
          [note-header meta show-title?]
          [mdv/html-view {:html html :missing missing
                          :class (str "prose " (if snippet? "prose--snippet" "prose--article"))}]]
         [rail {:toc-items (when-not snippet? toc) :note note}]]))))
