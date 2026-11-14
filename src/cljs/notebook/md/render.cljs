(ns notebook.md.render
  "One markdown-it pipeline for the editor preview and the read view:
  front-matter, heading anchors, KaTeX, highlight.js, Mermaid fences,
  [[wiki-links]] and ![[embeds]]."
  (:require ["markdown-it" :as markdown-it-mod]
            ["markdown-it-anchor" :as anchor-mod]
            ["markdown-it-front-matter" :as front-matter-mod]
            ["@vscode/markdown-it-katex" :as katex-mod]
            ["highlight.js" :as hljs-mod]
            [clojure.string :as str]
            [notebook.links :as links]))

(defn- default-export [m] (or (.-default ^js m) m))

(def ^js MarkdownIt (default-export markdown-it-mod))
(def ^js anchor (default-export anchor-mod))
(def ^js front-matter (default-export front-matter-mod))
(def ^js katex-plugin (default-export katex-mod))
(def ^js hljs (default-export hljs-mod))

(defn escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn- highlight [code lang]
  (let [lang (some-> lang str/trim str/lower-case not-empty)]
    (try
      (cond
        (and lang (.getLanguage hljs lang))
        (.-value (.highlight hljs code #js {:language lang :ignoreIllegals true}))
        :else (escape-html code))
      (catch :default _ (escape-html code)))))

;; ---------------------------------------------------------------------------
;; Render context
;;
;; Passed through markdown-it's `env` as env.ctx:
;;   {:catalog {slug entry} :contents {slug content} :depth n :seen #{slug}}
;; env.missing collects slugs whose content is needed for an embed.

(defn- ctx-of [^js env] (.-ctx env))

(defn- note-href [slug heading]
  (str "/n/" (js/encodeURIComponent slug)
       (when heading (str "#" (links/slugify heading)))))

(declare render-body)

(defn- render-link [{:keys [target raw-target heading alias]} ctx]
  (let [entry (get-in ctx [:catalog target])
        text  (or alias
                  (str (or (:title entry) raw-target)
                       (when heading (str " › " heading))))]
    (if entry
      (str "<a class=\"wikilink wikilink--" (:type entry) "\" href=\"" (note-href target heading)
           "\" data-slug=\"" (escape-html target) "\">" (escape-html text) "</a>")
      (str "<a class=\"wikilink wikilink--dangling\" href=\"" (note-href target nil)
           "\" data-create=\"" (escape-html raw-target) "\" title=\"Create “"
           (escape-html raw-target) "”\">" (escape-html text) "</a>"))))

(defn- render-asset [{:keys [target alias]}]
  (let [src (str "/assets/" (js/encodeURIComponent target))
        alt (escape-html (or alias target))]
    (cond
      (links/image-name? target)
      (str "<img class=\"embed-img\" src=\"" src "\" alt=\"" alt "\" loading=\"lazy\">")
      (re-find #"(?i)\.(mp4|webm)$" target)
      (str "<video class=\"embed-video\" src=\"" src "\" controls preload=\"metadata\"></video>")
      (re-find #"(?i)\.(mp3|ogg)$" target)
      (str "<audio src=\"" src "\" controls preload=\"metadata\"></audio>")
      :else
      (str "<a class=\"asset-link\" href=\"" src "\" target=\"_blank\" rel=\"noopener\">" alt "</a>"))))

(defn- render-embed [{:keys [target] :as link} ctx ^js env]
  (let [entry   (get-in ctx [:catalog target])
        content (get-in ctx [:contents target])]
    (cond
      (:asset? link) (render-asset link)
      (nil? entry) (render-link link ctx)
      (or (>= (:depth ctx 0) 2) (contains? (:seen ctx) target))
      (str "<span class=\"embed-cycle\" title=\"Embed depth/cycle limit\">↻ </span>"
           (render-link link ctx))
      (nil? content)
      (do (.push (.-missing env) target)
          (str "<div class=\"embed embed--" (:type entry) " embed--loading\">"
               "<div class=\"embed__head\">" (render-link link ctx) "</div>"
               "<div class=\"embed__body\">Loading…</div></div>"))
      :else
      (let [{:keys [body]} (links/split-frontmatter content)
            inner (render-body body (-> ctx (update :depth (fnil inc 0)) (update :seen (fnil conj #{}) target))
                               (.-missing env))]
        (str "<aside class=\"embed embed--" (:type entry) "\">"
             "<header class=\"embed__head\">" (render-link (dissoc link :alias) ctx)
             "<span class=\"embed__type\">" (if (= "snippet" (:type entry)) "Snippet" "Article")
             (when-let [l (:lang entry)] (str " · " (escape-html l))) "</span></header>"
             "<div class=\"embed__body\">" inner "</div></aside>")))))

;; ---------------------------------------------------------------------------
;; markdown-it plugins

(defn- wikilink-rule
  "Inline rule: `[[...]]` and `![[...]]`."
  [^js state silent]
  (let [src   (.-src state)
        pos   (.-pos state)
        bang? (= "!" (.charAt src pos))
        open  (if bang? (inc pos) pos)]
    (if (and (= "[[" (subs src open (+ open 2))))
      (let [close (.indexOf src "]]" (+ open 2))
            inner (when (> close open) (subs src (+ open 2) close))]
        (if (and inner (not (re-find #"[\[\]\n]" inner)) (not (str/blank? inner)))
          (do
            (when-not silent
              (let [^js tok (.push state "wikilink" "" 0)
                    parsed (assoc (links/parse-inner inner) :embed? bang?)]
                (set! (.-meta tok) parsed)
                (set! (.-content tok) inner)))
            (set! (.-pos state) (+ close 2))
            true)
          false))
      false)))

(defn- lone-embed-paragraphs
  "Core rule: a paragraph holding only a note embed renders without <p>, so
  the embedded <aside> is not nested inside a paragraph."
  [^js state]
  (let [^js tokens (.-tokens state)]
    (doseq [i (range (- (.-length tokens) 2))]
      (let [^js open (aget tokens i)
            ^js inl  (aget tokens (inc i))
            ^js close (aget tokens (+ i 2))]
        (when (and (= "paragraph_open" (.-type open))
                   (= "inline" (.-type inl))
                   (= "paragraph_close" (.-type close)))
          (let [kids (array-seq (or (.-children inl) #js []))
                embeds (filter #(and (= "wikilink" (.-type ^js %)) (:embed? (.-meta ^js %))
                                     (not (:asset? (.-meta ^js %))))
                               kids)
                others (remove #(or (= "wikilink" (.-type ^js %))
                                    (= "softbreak" (.-type ^js %))
                                    (and (= "text" (.-type ^js %)) (str/blank? (.-content ^js %))))
                               kids)]
            (when (and (seq embeds) (empty? others))
              (set! (.-hidden open) true)
              (set! (.-hidden close) true))))))))

(defn- source-lines
  "Core rule: top-level blocks get data-line (0-based) for scroll sync."
  [^js state]
  (doseq [^js t (array-seq (.-tokens state))]
    (when (and (.-map t) (zero? (.-level t)) (or (= 1 (.-nesting t)) (zero? (.-nesting t)))
               (.-block t) (not= "inline" (.-type t)))
      (.attrSet t "data-line" (str (aget (.-map t) 0))))))

(defn- fence-renderer [^js tokens idx ^js options env ^js self]
  (let [^js t (aget tokens idx)
        info  (str/trim (or (.-info t) ""))
        lang  (first (str/split info #"\s+"))
        line  (.attrGet t "data-line")
        line-attr (if line (str " data-line=\"" line "\"") "")]
    (if (= lang "mermaid")
      (str "<div class=\"mermaid-wrap\"" line-attr "><pre class=\"mermaid\">"
           (escape-html (.-content t)) "</pre></div>\n")
      (str "<div class=\"codeblock\"" line-attr ">"
           "<div class=\"codeblock__bar\"><span class=\"codeblock__lang\">" (escape-html (or (not-empty lang) "text")) "</span>"
           "<button type=\"button\" class=\"codeblock__copy\">Copy</button></div>"
           "<pre><code class=\"hljs" (when (seq lang) (str " language-" (escape-html lang))) "\">"
           (highlight (.-content t) lang)
           "</code></pre></div>\n"))))

(defn- make-md []
  (let [^js md (MarkdownIt. #js {:html false :linkify true :typographer true})]
    (.use md front-matter (fn [_]))
    (.use md katex-plugin #js {:throwOnError false :enableFencedBlocks true})
    (.use md anchor #js {:slugify links/slugify :tabIndex false})
    (.. md -inline -ruler (before "link" "wikilink" wikilink-rule))
    (.. md -core -ruler (push "lone_embeds" lone-embed-paragraphs))
    (.. md -core -ruler (push "source_lines" source-lines))
    (set! (.. md -renderer -rules -wikilink)
          (fn [^js tokens idx _ ^js env _]
            (let [^js t (aget tokens idx)
                  m     (.-meta t)
                  ctx   (ctx-of env)]
              (if (:embed? m) (render-embed m ctx env) (render-link m ctx)))))
    (set! (.. md -renderer -rules -fence) fence-renderer)
    ;; external links open in a new tab
    (let [default-open (or (.. md -renderer -rules -link_open)
                           (fn [tokens idx options _env ^js self]
                             (.renderToken self tokens idx options)))]
      (set! (.. md -renderer -rules -link_open)
            (fn [^js tokens idx options env ^js self]
              (let [^js t (aget tokens idx)
                    href (or (.attrGet t "href") "")]
                (when (re-find #"^https?://" href)
                  (.attrSet t "target" "_blank")
                  (.attrSet t "rel" "noopener noreferrer"))
                (default-open tokens idx options env self)))))
    md))

(defonce ^js md (make-md))

(defn- render-body [src ctx missing]
  (.render md src #js {:ctx ctx :missing missing}))

(defn- inline-text [^js t]
  (->> (array-seq (or (.-children t) #js []))
       (map (fn [^js c]
              (case (.-type c)
                "wikilink" (or (:alias (.-meta c)) (:raw-target (.-meta c)))
                ("text" "code_inline" "math_inline") (.-content c)
                "")))
       (apply str)))

(defn render
  "Render markdown `src`. Returns {:html :toc [{:level :id :text :line}] :missing [slugs]}."
  [src ctx]
  (let [missing #js []
        env     #js {:ctx ctx :missing missing}
        tokens  (.parse md (or src "") env)
        toc     (->> (range (.-length tokens))
                     (keep (fn [i]
                             (let [^js t (aget tokens i)]
                               (when (= "heading_open" (.-type t))
                                 (let [level (js/parseInt (subs (.-tag t) 1))]
                                   (when (<= 2 level 4)
                                     {:level level
                                      :id    (.attrGet t "id")
                                      :line  (some-> (.-map t) (aget 0))
                                      :text  (inline-text (aget tokens (inc i)))}))))))
                     vec)
        html    (.render (.-renderer md) tokens (.-options md) env)]
    {:html html :toc toc :missing (vec (distinct (array-seq missing)))}))
