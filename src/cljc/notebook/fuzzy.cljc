(ns notebook.fuzzy
  "Subsequence fuzzy matching used by the [[ autocomplete and the Ctrl+K
  palette. Rewards prefix, word-boundary, camelCase and consecutive matches;
  penalises gaps."
  (:require [clojure.string :as str]))

(defn- boundary-char? [c]
  (contains? #{\space \- \_ \/ \. \: \( \) \# \, \' \"} c))

(defn- upper? [c] (let [s (str c)] (and (= s (str/upper-case s)) (not= s (str/lower-case s)))))
(defn- lower? [c] (let [s (str c)] (and (= s (str/lower-case s)) (not= s (str/upper-case s)))))

(defn- bonus-at [^String text i]
  (cond
    (zero? i) 18
    (boundary-char? (nth text (dec i))) 14
    (and (upper? (nth text i)) (lower? (nth text (dec i)))) 10
    :else 0))

(defn- greedy-from
  "Greedily align query chars after a forced first match at `start`."
  [q lt start]
  (loop [qi 1, ti (inc start), acc [start]]
    (cond
      (= qi (count q)) acc
      (>= ti (count lt)) nil
      (= (nth q qi) (nth lt ti)) (recur (inc qi) (inc ti) (conj acc ti))
      :else (recur qi (inc ti) acc))))

(defn- score-positions [text positions]
  (loop [[p & more] positions, prev nil, score 0]
    (if (nil? p)
      score
      (let [s (+ 16
                 (bonus-at text p)
                 (if (and prev (= p (inc prev))) 12 0)
                 (if prev
                   (- (min 8 (- p prev 1)))
                   (- (min 12 p))))]
        (recur more p (+ score s))))))

(defn match
  "Match `query` against `text`. Returns {:score n :positions [i ...]} or nil.
  Whitespace in the query is ignored."
  [query text]
  (let [q  (str/lower-case (str/replace (or query "") #"\s+" ""))
        text (or text "")
        lt (str/lower-case text)]
    (cond
      (str/blank? q) {:score 0 :positions []}
      (> (count q) (count lt)) nil
      :else
      (let [starts (->> (range (count lt))
                        (filter #(= (nth lt %) (first q)))
                        (take 24))
            best (reduce (fn [best start]
                           (if-let [ps (greedy-from q lt start)]
                             (let [s (score-positions text ps)]
                               (if (or (nil? best) (> s (:score best)))
                                 {:score s :positions ps}
                                 best))
                             best))
                         nil starts)]
        (when best
          (let [plain (str/lower-case (str/trim (or query "")))]
            (update best :score +
                    (cond (= lt plain) 60
                          (str/starts-with? lt plain) 30
                          (str/includes? lt plain) 15
                          :else 0)
                    ;; slight preference for shorter strings
                    (- (/ (count lt) 20)))))))))

(defn ranges
  "positions [0 1 2 5] -> [[0 3] [5 6]] (half-open)."
  [positions]
  (reduce (fn [acc p]
            (let [[s e] (peek acc)]
              (if (and e (= p e))
                (conj (pop acc) [s (inc p)])
                (conj acc [p (inc p)]))))
          []
          (sort positions)))

(defn rank
  "Rank notes ({:title :slug :mtime ...}) for `query`. Each result gets
  :fuzzy/score and :fuzzy/positions (positions refer to :title, or nil if the
  best hit was on the slug). Empty query -> most recently edited first."
  ([query notes] (rank query notes nil))
  ([query notes limit]
   (let [ranked
         (if (str/blank? query)
           (sort-by (juxt #(- (or (:mtime %) 0)) :title) notes)
           (->> notes
                (keep (fn [n]
                        (let [mt (match query (:title n))
                              ms (match query (:slug n))]
                          (cond
                            (and mt (or (nil? ms) (>= (:score mt) (:score ms))))
                            (assoc n :fuzzy/score (:score mt) :fuzzy/positions (:positions mt))
                            ms
                            (assoc n :fuzzy/score (- (:score ms) 2) :fuzzy/positions nil)))))
                (sort-by (juxt #(- (:fuzzy/score %)) #(- (or (:mtime %) 0)) :title))))]
     (if limit (take limit ranked) ranked))))
