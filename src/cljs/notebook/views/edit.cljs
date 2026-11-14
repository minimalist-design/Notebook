(ns notebook.views.edit
  (:require [clojure.string :as str]
            [notebook.editor.cm :as cm]
            [notebook.links :as links]
            [notebook.md.render :as md]
            [notebook.routes :refer [href]]
            [notebook.views.common :as c]
            [notebook.views.markdown :as mdv]
            [re-frame.core :as rf]
            [re-frame.db :as rfdb]
            [reagent.core :as r]))

(defn- status-label [status]
  (case status
    :saved    [:span.status.status--saved "Saved"]
    :dirty    [:span.status.status--dirty "Edited"]
    :saving   [:span.status.status--saving "Saving…"]
    :conflict [:span.status.status--conflict "Conflict"]
    :error    [:span.status.status--error "Save failed"]
    nil))

(defn- rename-dialog [_slug _title on-close]
  (let [value (r/atom nil)]
    (fn [slug title on-close]
      (let [v (if (nil? @value) title @value)
            submit #(do (when-not (str/blank? v)
                          (rf/dispatch [:note/rename slug v]))
                        (on-close))]
        [:div.dialog-backdrop {:on-click on-close}
         [:form.dialog {:on-click #(.stopPropagation %)
                        :on-submit #(do (.preventDefault %) (submit))}
          [:h2.dialog__title "Rename note"]
          [:p.dialog__hint "References in other notes are rewritten to the new name."]
          [:input.input {:auto-focus true :default-value title :on-change #(reset! value (.. % -target -value))
                         :on-key-down #(when (= "Escape" (.-key %)) (on-close))}]
          [:p.dialog__slug "/n/" (links/slugify v)]
          [:div.dialog__actions
           [:button.btn {:type "button" :on-click on-close} "Cancel"]
           [:button.btn.btn--primary {:type "submit"} "Rename"]]]]))))

(defn- conflict-banner [conflict]
  (when conflict
    [:div.banner.banner--conflict
     [:span "This note changed on disk while you were editing."]
     [:button.btn.btn--small {:on-click #(rf/dispatch [:editor/resolve :theirs])} "Load disk version"]
     [:button.btn.btn--small.btn--primary {:on-click #(rf/dispatch [:editor/resolve :mine])} "Keep mine & overwrite"]]))

(defn- sync-preview!
  "Scroll the preview so the block at editor `line` (1-based) sits at the top."
  [^js preview line]
  (when preview
    (let [blocks (array-seq (.querySelectorAll preview "[data-line]"))
          target (dec line)
          [before after] (reduce (fn [[b a] ^js el]
                                   (let [l (js/parseInt (.getAttribute el "data-line"))]
                                     (if (<= l target) [[l el] a] (reduced [b [l el]]))))
                                 [nil nil] blocks)]
      (if-let [[bl ^js bel] before]
        (let [top  (.-offsetTop bel)
              next (if-let [[al ^js ael] after]
                     (+ top (* (- (.-offsetTop ael) top) (/ (- target bl) (max 1 (- al bl)))))
                     top)]
          (set! (.-scrollTop preview) (- next 24)))
        (set! (.-scrollTop preview) 0)))))

(defn edit-page [_slug]
  (let [preview-text (r/atom nil)
        preview-node (atom nil)
        timer (atom nil)
        rename? (r/atom false)
        get-state (fn [] {:catalog (:catalog @rfdb/app-db) :assets (:assets @rfdb/app-db)})
        on-create (fn [{:keys [type title]}]
                    (rf/dispatch [:note/create {:type type :title title}]))
        on-change (fn [text]
                    (rf/dispatch [:editor/changed text])
                    (js/clearTimeout @timer)
                    (reset! timer (js/setTimeout #(reset! preview-text text) 150)))]
    (r/create-class
     {:component-will-unmount (fn [] (js/clearTimeout @timer) (rf/dispatch [:editor/flush]))
      :reagent-render
      (fn [slug]
        (let [{:keys [text status reload conflict loading?] ed-slug :slug} @(rf/subscribe [:editor])
              entry    @(rf/subscribe [:catalog-entry slug])
              catalog  @(rf/subscribe [:catalog])
              contents @(rf/subscribe [:notes-content])]
          (if (or loading? (not= ed-slug slug) (nil? text))
            [:div.loading "Loading…"]
            (let [src (or @preview-text text)
                  {:keys [html missing]} (md/render src {:catalog catalog :contents contents
                                                         :depth 0 :seen #{slug}})]
              [:div.edit
               [:header.edit__bar
                [c/type-badge (:type entry)]
                [:h1.edit__title (:title entry)]
                [status-label status]
                [:div.edit__actions
                 [:a.btn.btn--small {:href (href :read {:slug slug}) :title "Read (Ctrl+E)"} "Done"]
                 [:button.btn.btn--small {:on-click #(reset! rename? true)} "Rename"]
                 [:button.btn.btn--small.btn--danger
                  {:on-click #(when (js/confirm (str "Delete “" (:title entry) "”? This removes the file."))
                                (rf/dispatch [:note/delete slug]))}
                  "Delete"]]]
               [conflict-banner conflict]
               [:div.edit__panes
                [:div.edit__editor
                 ^{:key slug}
                 [cm/editor {:doc text :reload reload
                             :on-change on-change
                             :on-scroll (fn [line _] (sync-preview! @preview-node line))
                             :get-state get-state
                             :on-create on-create}]]
                [:div.edit__preview {:ref #(reset! preview-node %)}
                 [mdv/html-view {:html html :missing missing :class "prose prose--article prose--preview"}]]]
               (when @rename?
                 [rename-dialog slug (:title entry) #(reset! rename? false)])]))))})))
