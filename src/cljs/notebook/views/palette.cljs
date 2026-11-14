(ns notebook.views.palette
  "Ctrl+K: jump to any note (same fuzzy ranking as the [[ autocomplete),
  create notes, or run a full-text search."
  (:require [clojure.string :as str]
            [notebook.fuzzy :as fuzzy]
            [notebook.views.common :as c]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]))

(defn- items [{:keys [mode query]} results]
  (let [q (str/trim query)]
    (case mode
      :new-article [{:kind :create :type :article :label (if (seq q) (str "Create article “" q "”") "Type a title…")}]
      :new-snippet [{:kind :create :type :snippet :label (if (seq q) (str "Create snippet “" q "”") "Type a title…")}]
      (concat
       (map #(assoc % :kind :note) results)
       (when (seq q)
         [{:kind :search :label (str "Search all notes for “" q "”")}
          {:kind :create :type :article :label (str "New article “" q "”")}
          {:kind :create :type :snippet :label (str "New snippet “" q "”")}])))))

(defn- choose! [{:keys [kind slug type]} query]
  (let [q (str/trim query)]
    (rf/dispatch [:palette/close])
    (case kind
      :note   (rf/dispatch [:nav/go :read {:slug slug}])
      :search (rf/dispatch [:nav/go :search nil {:q q}])
      :create (when (seq q) (rf/dispatch [:note/create {:type type :title q :open? true}])))))

(defn palette []
  (let [{:keys [open? query sel mode] :as p} @(rf/subscribe [:palette])
        results @(rf/subscribe [:palette/results])]
    (when open?
      (let [its (vec (items p results))
            n   (count its)]
        [:div.palette-backdrop {:on-click #(rf/dispatch [:palette/close])}
         [:div.palette {:on-click #(.stopPropagation %) :role "dialog" :aria-label "Command palette"}
          [:div.palette__mode
           (case mode
             :new-article "New article — title"
             :new-snippet "New snippet — title"
             "Go to note")]
          [:input.palette__input
           {:auto-focus true
            :default-value query
            :placeholder (if (= mode :go) "Type to search titles…" "Title")
            :on-change #(rf/dispatch-sync [:palette/query (.. % -target -value)])
            :on-key-down (fn [e]
                           (case (.-key e)
                             "ArrowDown" (do (.preventDefault e) (rf/dispatch [:palette/move 1 n]))
                             "ArrowUp"   (do (.preventDefault e) (rf/dispatch [:palette/move -1 n]))
                             "Enter"     (do (.preventDefault e)
                                             ;; use the live input value, not the last render
                                             (let [q   (.. e -target -value)
                                                   {:keys [catalog palette]} @rfdb/app-db
                                                   its (vec (items (assoc palette :query q)
                                                                   (fuzzy/rank q (vals catalog) 40)))]
                                               (when-let [it (get its (:sel palette))] (choose! it q))))
                             "Escape"    (rf/dispatch [:palette/close])
                             nil))}]
          (into [:ul.palette__list]
                (map-indexed
                 (fn [i {:keys [kind slug title type label tags] :as it}]
                   [:li {:key (or slug (str (name kind) i))
                         :class (str "palette__item" (when (= i sel) " is-selected"))
                         :on-mouse-down #(do (.preventDefault %) (choose! it query))}
                    (if (= kind :note)
                      [:<>
                       [:span {:class (str "palette__icon palette__icon--" type)}]
                       [:span.palette__title [c/highlighted title (:fuzzy/positions it)]]
                       [:span.palette__meta (when (seq tags) (str "#" (str/join " #" (take 3 tags))))]]
                      [:<>
                       [:span {:class (str "palette__icon palette__icon--" (name kind))}]
                       [:span.palette__title label]])])
                 its))]]))))
