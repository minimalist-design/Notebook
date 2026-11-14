(ns notebook.editor.cm
  "CodeMirror 6 markdown editor as a Reagent component."
  (:require ["@codemirror/autocomplete" :refer [autocompletion completionKeymap closeBrackets closeBracketsKeymap]]
            ["@codemirror/commands" :refer [defaultKeymap history historyKeymap indentWithTab]]
            ["@codemirror/lang-markdown" :refer [markdown markdownLanguage]]
            ["@codemirror/language" :refer [syntaxHighlighting HighlightStyle indentOnInput bracketMatching]]
            ["@codemirror/language-data" :refer [languages]]
            ["@codemirror/search" :refer [searchKeymap highlightSelectionMatches]]
            ["@codemirror/state" :refer [EditorState StateEffect StateField]]
            ["@codemirror/view" :refer [EditorView keymap drawSelection dropCursor highlightActiveLine
                                        placeholder rectangularSelection]]
            ["@lezer/highlight" :refer [tags]]
            [clojure.string :as str]
            [notebook.api :as api]
            [notebook.editor.autocomplete :as ac]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def ^js highlight-style
  (.define HighlightStyle
           (clj->js
            [{:tag (.-heading1 tags) :class "md-h md-h1"}
             {:tag (.-heading2 tags) :class "md-h md-h2"}
             {:tag #js [(.-heading3 tags) (.-heading4 tags) (.-heading5 tags) (.-heading6 tags)] :class "md-h"}
             {:tag (.-strong tags) :class "md-strong"}
             {:tag (.-emphasis tags) :class "md-em"}
             {:tag (.-strikethrough tags) :class "md-strike"}
             {:tag #js [(.-link tags) (.-url tags)] :class "md-link"}
             {:tag (.-monospace tags) :class "md-code"}
             {:tag (.-quote tags) :class "md-quote"}
             {:tag #js [(.-processingInstruction tags) (.-contentSeparator tags) (.-meta tags)] :class "md-mark"}
             {:tag (.-list tags) :class "md-list"}
             ;; embedded code
             {:tag #js [(.-keyword tags) (.-operatorKeyword tags) (.-modifier tags)] :class "tok-kw"}
             {:tag #js [(.-string tags) (.-regexp tags) ((.-special tags) (.-string tags))] :class "tok-str"}
             {:tag #js [(.-number tags) (.-bool tags) (.-null tags) (.-atom tags)] :class "tok-num"}
             {:tag #js [(.-comment tags) (.-lineComment tags) (.-blockComment tags)] :class "tok-comment"}
             {:tag #js [((.-function tags) (.-variableName tags)) ((.-function tags) (.-propertyName tags))] :class "tok-fn"}
             {:tag #js [(.-typeName tags) (.-className tags)] :class "tok-type"}
             {:tag #js [((.-definition tags) (.-variableName tags))] :class "tok-def"}])))

(defn- image-files [^js data-transfer]
  (when data-transfer
    (->> (array-seq (.-files data-transfer))
         (filter #(re-find #"^(image|video|audio)/|application/pdf" (.-type ^js %))))))

;; Uploads take a while and the user keeps typing meanwhile, so the insert
;; position of each pending upload lives in editor state and is mapped
;; through every change until the upload finishes.

(def ^:private add-upload (.define StateEffect))     ; value: #js [id pos]
(def ^:private remove-upload (.define StateEffect))  ; value: id

(def ^:private upload-positions
  (.define StateField
           #js {:create (fn [_] {})
                :update (fn [m ^js tr]
                          (let [m (if (.-docChanged tr)
                                    (update-vals m #(.mapPos (.-changes tr) % -1))
                                    m)]
                            (reduce (fn [m ^js e]
                                      (cond
                                        (.is e add-upload)    (let [v (.-value e)] (assoc m (aget v 0) (aget v 1)))
                                        (.is e remove-upload) (dissoc m (.-value e))
                                        :else m))
                                    m
                                    (.-effects tr))))}))

(defonce ^:private upload-seq (atom 0))

(defn- upload-and-insert!
  "Upload all `files`, then insert their embeds, in order, wherever `pos`
  has moved to. Failed uploads are reported and skipped."
  [^js view files pos]
  (let [id (swap! upload-seq inc)]
    (.dispatch view #js {:effects (.of add-upload #js [id pos])})
    (-> (js/Promise.allSettled (into-array (map api/upload-asset! files)))
        (.then (fn [results]
                 (let [results (array-seq results)
                       names   (keep #(when (= "fulfilled" (.-status ^js %)) (.-value ^js %)) results)
                       errors  (keep #(when (= "rejected" (.-status ^js %)) (str (.-reason ^js %))) results)]
                   (doseq [n names] (rf/dispatch [:assets/added n]))
                   (when (seq errors) (js/alert (str/join "\n" errors)))
                   ;; the editor may have been closed while uploading
                   (when (.. view -dom -isConnected)
                     (let [at (get (.. view -state (field upload-positions)) id)]
                       (.dispatch view
                                  (clj->js
                                   (cond-> {:effects (.of remove-upload id)}
                                     (and at (seq names))
                                     (assoc :changes {:from at
                                                      :insert (str/join "\n" (map #(str "![[" % "]]") names))}
                                            :userEvent "input.paste")))))))))
        (.catch #(js/alert (str %))))))

(defn create-view
  [{:keys [parent doc on-change get-state on-create on-scroll]}]
  (let [extensions
        #js [(history)
             upload-positions
             (drawSelection)
             (dropCursor)
             (indentOnInput)
             (bracketMatching)
             (closeBrackets)
             (rectangularSelection)
             (highlightActiveLine)
             (highlightSelectionMatches)
             (.. EditorState -allowMultipleSelections (of true))
             (.-lineWrapping EditorView)
             (markdown #js {:base markdownLanguage :codeLanguages languages})
             (syntaxHighlighting highlight-style)
             (placeholder "Start writing…  Type [[ to link a note, ![[ to embed one.")
             (autocompletion #js {:override #js [(ac/source get-state on-create)]
                                  :icons true
                                  :maxRenderedOptions 60
                                  :closeOnBlur true
                                  :activateOnTypingDelay 0})
             (.of keymap (.concat closeBracketsKeymap defaultKeymap searchKeymap historyKeymap
                                  completionKeymap #js [indentWithTab]))
             (.. EditorView -updateListener
                 (of (fn [^js u]
                       (when (.-docChanged u)
                         (on-change (.. u -state -doc toString))))))
             (.domEventHandlers EditorView
                                #js {:paste (fn [^js e ^js view]
                                              (let [files (image-files (.-clipboardData e))]
                                                (when (seq files)
                                                  (.preventDefault e)
                                                  (upload-and-insert! view files (.. view -state -selection -main -head))
                                                  true)))
                                     :drop  (fn [^js e ^js view]
                                              (let [files (image-files (.-dataTransfer e))]
                                                (when (seq files)
                                                  (.preventDefault e)
                                                  (let [pos (or (.posAtCoords view #js {:x (.-clientX e) :y (.-clientY e)})
                                                                (.. view -state -selection -main -head))]
                                                    (upload-and-insert! view files pos))
                                                  true)))})]
        view (EditorView. #js {:parent parent
                               :state (.create EditorState #js {:doc doc :extensions extensions})})]
    (when on-scroll
      (.addEventListener (.-scrollDOM view) "scroll"
                         (fn [_]
                           (let [top   (.. view -scrollDOM -scrollTop)
                                 block (.lineBlockAtHeight view top)
                                 line  (.-number (.lineAt (.. view -state -doc) (.-from block)))]
                             (on-scroll line view)))
                         #js {:passive true}))
    view))

(defn editor
  "props: :doc :reload (int; bump to replace the doc) :on-change :on-scroll
  :get-state :on-create :on-view"
  [_props]
  (let [view (atom nil)
        node (atom nil)
        last-reload (atom nil)]
    (r/create-class
     {:display-name "codemirror"
      :component-did-mount
      (fn [this]
        (let [{:keys [doc reload on-view] :as props} (r/props this)
              v (create-view (assoc props :parent @node :doc (or doc "")))]
          (reset! view v)
          (reset! last-reload reload)
          (when on-view (on-view v))
          (.focus v)))
      :component-did-update
      (fn [this _]
        (let [{:keys [doc reload]} (r/props this)
              ^js v @view]
          (when (and v (not= reload @last-reload))
            (reset! last-reload reload)
            (.dispatch v #js {:changes #js {:from 0 :to (.. v -state -doc -length) :insert (or doc "")}}))))
      :component-will-unmount
      (fn [_] (when-let [^js v @view] (.destroy v)))
      :reagent-render
      (fn [_] [:div.cm-host {:ref #(reset! node %)}])})))
