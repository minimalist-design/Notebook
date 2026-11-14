(ns notebook.links
  "Shared (clj + cljs) parsing of note text: slugs, frontmatter boundaries and
  [[wiki-links]] / ![[embeds]].  Code fences, inline code and math are masked
  before scanning so links inside them are ignored."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Slugs

(defn- strip-accents [s]
  #?(:clj  (-> (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
               (str/replace #"[̀-ͯ]" ""))
     :cljs (-> (.normalize s "NFD")
               (str/replace #"[̀-ͯ]" ""))))

(defn slugify
  "\"Clojure REPL Tricks!\" -> \"clojure-repl-tricks\""
  [s]
  (let [slug (-> (or s "")
                 strip-accents
                 str/lower-case
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+|-+$" ""))
        slug (if (> (count slug) 80)
               (str/replace (subs slug 0 80) #"-+$" "")
               slug)]
    (if (str/blank? slug) "untitled" slug)))

(def asset-re #"(?i)\.(png|jpe?g|gif|svg|webp|avif|bmp|pdf|mp4|webm|mp3|ogg)$")

(defn asset-name? [s] (boolean (re-find asset-re (or s ""))))

(defn image-name? [s] (boolean (re-find #"(?i)\.(png|jpe?g|gif|svg|webp|avif|bmp)$" (or s ""))))

;; ---------------------------------------------------------------------------
;; Regex helpers (positions, portable)

(defn re-find-all
  "All matches of `re` in `s` as maps {:match :groups :start :end}."
  [re s]
  #?(:clj
     (let [m (re-matcher re s)]
       (loop [acc []]
         (if (.find m)
           (recur (conj acc {:match  (.group m)
                             :groups (mapv #(.group m (int %)) (range 1 (inc (.groupCount m))))
                             :start  (.start m)
                             :end    (.end m)}))
           acc)))
     :cljs
     (let [flags (str "g" (when (.-ignoreCase re) "i") (when (.-multiline re) "m"))
           r     (js/RegExp. (.-source re) flags)]
       (loop [acc []]
         (if-let [m (.exec r s)]
           (do (when (zero? (count (aget m 0)))
                 (set! (.-lastIndex r) (inc (.-lastIndex r))))
               (recur (conj acc {:match  (aget m 0)
                                 :groups (vec (rest (array-seq m)))
                                 :start  (.-index m)
                                 :end    (+ (.-index m) (count (aget m 0)))})))
           acc)))))

(defn- blank-out
  "Replace every non-newline char with a space (keeps offsets stable)."
  [s]
  (str/replace s #"[^\n]" " "))

(defn- mask-regex [s re]
  (reduce (fn [acc {:keys [start end]}]
            (str (subs acc 0 start) (blank-out (subs acc start end)) (subs acc end)))
          s
          (re-find-all re s)))

(defn- split-lines* [s]
  #?(:clj (vec (.split ^String s "\n" -1))
     :cljs (vec (.split s "\n"))))

;; ---------------------------------------------------------------------------
;; Frontmatter

(def ^:private frontmatter-re #"^---[ \t]*\r?\n([\s\S]*?)\r?\n---[ \t]*(?:\r?\n|$)")

(defn split-frontmatter
  "Returns {:frontmatter yaml-or-nil :body str :body-offset n}."
  [text]
  (let [text (or text "")]
    (if-let [{:keys [groups end]} (first (re-find-all frontmatter-re text))]
      {:frontmatter (first groups) :body (subs text end) :body-offset end}
      {:frontmatter nil :body text :body-offset 0})))

;; ---------------------------------------------------------------------------
;; Masking code + math

(defn- mask-fences [text]
  (let [lines (split-lines* text)
        out   (loop [ls lines, fence nil, acc (transient [])]
                (if-let [l (first ls)]
                  (if fence
                    (let [closing (re-find #"^ {0,3}(`{3,}|~{3,})[ \t]*$" l)
                          closes? (and closing
                                       (= (first (second closing)) (:ch fence))
                                       (>= (count (second closing)) (:n fence)))]
                      (recur (rest ls) (when-not closes? fence) (conj! acc (blank-out l))))
                    (if-let [[_ f] (re-find #"^ {0,3}(`{3,}|~{3,})" l)]
                      (recur (rest ls) {:ch (first f) :n (count f)} (conj! acc (blank-out l)))
                      (recur (rest ls) nil (conj! acc l))))
                  (persistent! acc)))]
    (str/join "\n" out)))

(defn mask-code
  "Blank out frontmatter, fenced code, inline code and $math$ so link scanning
  ignores them. Output has the same length/offsets as the input."
  [text]
  (let [{:keys [body-offset]} (split-frontmatter text)
        text (str (blank-out (subs text 0 body-offset)) (subs text body-offset))]
    (-> text
        mask-fences
        (mask-regex #"(`+)[^`][\s\S]*?\1")
        (mask-regex #"\$\$[\s\S]+?\$\$")
        (mask-regex #"\$[^\s$](?:[^$\n]*[^\s$\\])?\$"))))

;; ---------------------------------------------------------------------------
;; Wiki links

(def wikilink-re #"(!?)\[\[([^\[\]\n]+?)\]\]")

(defn parse-inner
  "\"slug#Heading|Alias\" -> {:raw-target :target :heading :alias :asset?}"
  [inner]
  (let [[target-part alias] (let [i (str/index-of inner "|")]
                              (if i [(subs inner 0 i) (subs inner (inc i))] [inner nil]))
        [raw heading]       (let [i (str/index-of target-part "#")]
                              (if i [(subs target-part 0 i) (subs target-part (inc i))] [target-part nil]))
        raw                 (str/trim raw)
        asset?              (asset-name? raw)]
    {:raw-target raw
     :target     (if asset? raw (slugify raw))
     :heading    (some-> heading str/trim not-empty)
     :alias      (some-> alias str/trim not-empty)
     :asset?     asset?}))

(defn find-links
  "All [[links]] and ![[embeds]] outside code/math.
  Each: {:embed? :target :raw-target :heading :alias :asset? :start :end}"
  [text]
  (let [text (or text "")
        masked (mask-code text)]
    (for [{:keys [groups start end]} (re-find-all wikilink-re masked)
          :let [[bang inner] groups
                parsed (parse-inner inner)]
          :when (not (str/blank? (:raw-target parsed)))]
      (assoc parsed :embed? (= bang "!") :start start :end end))))

(defn link-text
  "Rebuild the inner text of a link."
  [{:keys [target heading alias]}]
  (str target (when heading (str "#" heading)) (when alias (str "|" alias))))

(defn rewrite-links
  "Point every link/embed whose target is `from` at `to`. Code is untouched."
  [text from to]
  (let [hits (->> (find-links text)
                  (filter #(and (not (:asset? %)) (= (:target %) from)))
                  reverse)]
    (reduce (fn [acc {:keys [embed? start end] :as l}]
              (str (subs acc 0 start)
                   (if embed? "!" "") "[[" (link-text (assoc l :target to)) "]]"
                   (subs acc end)))
            text
            hits)))

;; ---------------------------------------------------------------------------
;; Misc text helpers

(defn first-heading [body]
  (some->> (re-find #"(?m)^#\s+(.+?)\s*#*\s*$" (mask-code body)) second))

(defn excerpt
  "First prose paragraph of `body`, stripped of markdown syntax, max n chars."
  ([body] (excerpt body 180))
  ([body n]
   (let [masked (mask-code body)
         para   (->> (str/split masked #"\n\s*\n")
                     (map str/trim)
                     (remove #(or (str/blank? %) (str/starts-with? % "#")
                                  (str/starts-with? % "|") (str/starts-with? % "![[")))
                     first)
         txt    (-> (or para "")
                    (str/replace #"!?\[\[([^\]|#]+)(?:#[^\]|]*)?(?:\|([^\]]+))?\]\]"
                                 (fn [[_ t a]] (or a t)))
                    (str/replace #"!\[[^\]]*\]\([^)]*\)" "")
                    (str/replace #"\[([^\]]+)\]\([^)]*\)" "$1")
                    (str/replace #"[*_~>#`]+" "")
                    (str/replace #"\s+" " ")
                    str/trim)]
     (if (> (count txt) n)
       (str (str/trimr (subs txt 0 n)) "…")
       txt))))
