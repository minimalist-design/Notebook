(ns notebook.views.markdown
  "Reagent component that shows rendered markdown and wires up the bits the
  HTML string can't do on its own: SPA navigation for wiki-links, creating
  dangling notes, copy buttons, Mermaid, and fetching embed contents."
  (:require [notebook.md.render :as md]
            [re-frame.core :as rf]
            [reagent.core :as r]))

;; ---------------------------------------------------------------------------
;; Mermaid (lazy-loaded from /vendor, cached by source text)

(defonce ^:private mermaid-promise (atom nil))
(defonce ^:private mermaid-cache (atom {}))

(defn- load-mermaid []
  (or @mermaid-promise
      (reset! mermaid-promise
              (js/Promise.
               (fn [resolve reject]
                 (let [s (js/document.createElement "script")]
                   (set! (.-src s) "/vendor/mermaid/mermaid.min.js")
                   (set! (.-onload s)
                         (fn []
                           (let [^js m (.-mermaid js/window)]
                             (.initialize m #js {:startOnLoad false
                                                 :securityLevel "strict"
                                                 :theme "base"
                                                 :fontFamily "IBM Plex Sans, sans-serif"
                                                 :themeVariables #js {:fontFamily "IBM Plex Sans, sans-serif"
                                                                      :primaryColor "#f3eee4"
                                                                      :primaryBorderColor "#8a7f6c"
                                                                      :primaryTextColor "#1d1b17"
                                                                      :lineColor "#6b6253"
                                                                      :secondaryColor "#e9f0f6"
                                                                      :tertiaryColor "#faf8f3"}})
                             (resolve m))))
                   (set! (.-onerror s) reject)
                   (.appendChild js/document.head s)))))))

(defn- cache-svgs! [sources nodes]
  (doseq [[src ^js n] (map vector sources nodes)]
    (when (.querySelector n "svg")
      (swap! mermaid-cache assoc src (.-innerHTML n)))))

(defn- render-pending! [pending]
  (let [sources (mapv #(.-textContent ^js %) pending)]
    (-> (load-mermaid)
        (.then (fn [^js m] (.run m #js {:nodes (into-array pending) :suppressErrors true})))
        (.then (fn [] (cache-svgs! sources pending)))
        (.catch #(js/console.warn "mermaid" %)))))

(defn- run-mermaid! [^js root]
  (let [nodes   (array-seq (.querySelectorAll root "pre.mermaid:not([data-processed])"))
        pending (vec (remove (fn [^js n]
                               (when-let [svg (get @mermaid-cache (.-textContent n))]
                                 (set! (.-innerHTML n) svg)
                                 (.setAttribute n "data-processed" "true")
                                 true))
                             nodes))]
    (when (seq pending)
      (render-pending! pending))))

;; ---------------------------------------------------------------------------
;; Click handling

(defn- on-click [^js e]
  (let [^js t (.-target e)]
    (cond
      (.closest t ".codeblock__copy")
      (let [^js btn (.closest t ".codeblock__copy")
            ^js code (some-> (.closest btn ".codeblock") (.querySelector "code"))]
        (when code
          (-> (js/navigator.clipboard.writeText (.-textContent code))
              (.then (fn []
                       (set! (.-textContent btn) "Copied")
                       (js/setTimeout #(set! (.-textContent btn) "Copy") 1200))))))

      (.closest t "a.wikilink--dangling")
      (let [^js a (.closest t "a.wikilink--dangling")]
        (when-not (or (.-metaKey e) (.-ctrlKey e))
          (.preventDefault e)
          (.stopPropagation e)
          (rf/dispatch [:note/create-from-link (.getAttribute a "data-create")]))))))

;; ---------------------------------------------------------------------------

(defn html-view
  "props: :html :missing :class :on-rendered (fn [dom-node])"
  [_props]
  (let [node (atom nil)
        after (fn [this]
                (let [{:keys [missing on-rendered]} (r/props this)]
                  (when-let [n @node]
                    (run-mermaid! n)
                    (when (seq missing) (rf/dispatch [:note/ensure-many missing]))
                    (when on-rendered (on-rendered n)))))]
    (r/create-class
     {:display-name "html-view"
      :component-did-mount after
      :component-did-update after
      :reagent-render
      (fn [{:keys [html class style]}]
        [:div {:class class
               :style style
               :ref #(reset! node %)
               :on-click on-click
               :dangerouslySetInnerHTML (r/unsafe-html html)}])})))

(defn markdown
  "Render markdown text with the current catalog/content cache."
  [{:keys [src class depth on-rendered]}]
  (let [catalog  @(rf/subscribe [:catalog])
        contents @(rf/subscribe [:notes-content])
        {:keys [html missing]} (md/render src {:catalog catalog :contents contents
                                               :depth (or depth 0) :seen #{}})]
    [html-view {:html html :missing missing :class class :on-rendered on-rendered}]))
