(ns notebook.editor.autocomplete
  "CodeMirror completion source for [[wiki-links]] and ![[embeds]].
  `filter: false` hands ranking to notebook.fuzzy, which re-ranks the whole
  catalog on every keystroke."
  (:require [clojure.string :as str]
            [notebook.fuzzy :as fuzzy]
            [notebook.links :as links]))

(defn- el [tag cls text]
  (let [e (js/document.createElement tag)]
    (when cls (set! (.-className e) cls))
    (when text (set! (.-textContent e) text))
    e))

(defn- info-node [{:keys [type excerpt tags slug lang]}]
  (let [root (el "div" "cm-ac-info" nil)]
    (.appendChild root (el "div" "cm-ac-info__meta"
                           (str (if (= type "snippet") "Snippet" "Article")
                                (when lang (str " · " lang))
                                "  ·  " slug)))
    (when (seq excerpt)
      (.appendChild root (el "div" (str "cm-ac-info__excerpt"
                                        (when (= type "snippet") " cm-ac-info__excerpt--code"))
                             excerpt)))
    (when (seq tags)
      (let [t (el "div" "cm-ac-info__tags" nil)]
        (doseq [tag tags] (.appendChild t (el "span" "cm-ac-tag" (str "#" tag))))
        (.appendChild root t)))
    root))

(defn- insert-link!
  "Insert `target` at [from,to] and close the link, skipping an existing ]]."
  [^js view from to target]
  (let [doc   (.. view -state -doc)
        after (.sliceString doc to (min (.-length doc) (+ to 2)))
        closed? (= after "]]")
        ins   (if closed? target (str target "]]"))]
    (.dispatch view #js {:changes   #js {:from from :to to :insert ins}
                         :selection #js {:anchor (+ from (count ins) (if closed? 2 0))}
                         :userEvent "input.complete"})))

(defn- flat-ranges [positions]
  (clj->js (mapcat identity (fuzzy/ranges positions))))

(defn source
  "get-state: (fn [] {:catalog {slug entry} :assets [{:name}]})
  on-create: (fn [{:keys [type title]}])"
  [get-state on-create]
  (fn [^js ctx]
    (when-let [^js m (.matchBefore ctx #"!?\[\[[^\[\]\n|#]*")]
      (let [txt    (.-text m)
            embed? (str/starts-with? txt "!")
            from   (+ (.-from m) (if embed? 3 2))
            q      (subs txt (if embed? 3 2))
            {:keys [catalog assets]} (get-state)
            notes  (fuzzy/rank q (vals catalog) 60)
            note-opts
            (for [{:keys [slug title type] :as n} notes]
              #js {:label  title
                   :detail (if (= slug (links/slugify title)) "" slug)
                   :type   (str "nb-" type)
                   :hl     (when-let [ps (:fuzzy/positions n)] (flat-ranges ps))
                   :info   (fn [_] (info-node n))
                   :apply  (fn [view _ f t] (insert-link! view f t slug))})
            asset-opts
            (when embed?
              (for [{:keys [name]} (->> assets
                                        (keep #(when-let [r (fuzzy/match q (:name %))] (assoc % :s (:score r))))
                                        (sort-by #(- (:s %)))
                                        (take 20))]
                #js {:label name :type "nb-asset" :detail "asset"
                     :apply (fn [view _ f t] (insert-link! view f t name))}))
            slug   (links/slugify q)
            create-opts
            (when (and (not (str/blank? q)) (not (contains? catalog slug)) (not (links/asset-name? q)))
              (for [type ["article" "snippet"]]
                #js {:label (str "Create " type " “" (str/trim q) "”")
                     :type  "nb-create"
                     :detail slug
                     :boost -99
                     :apply (fn [view _ f t]
                              (insert-link! view f t slug)
                              (on-create {:type type :title (str/trim q)}))}))]
        #js {:from     from
             :options  (into-array (concat note-opts asset-opts create-opts))
             :filter   false
             :getMatch (fn [^js c] (or (.-hl c) #js []))}))))
