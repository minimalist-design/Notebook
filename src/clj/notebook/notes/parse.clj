(ns notebook.notes.parse
  "Turn a note read from disk into the document stored in the index."
  (:require [clojure.string :as str]
            [notebook.links :as links]))

(defn- ->tags [v]
  (->> (cond (string? v) (str/split v #"[,\s]+")
             (sequential? v) v
             :else [])
       (map #(-> (str %) str/trim (str/replace #"^#" "") str/lower-case))
       (remove str/blank?)
       distinct
       vec))

(defn- ->date-str [v]
  (cond (nil? v) nil
        (instance? java.util.Date v) (str (.toLocalDate (.atZone (.toInstant ^java.util.Date v)
                                                                  java.time.ZoneOffset/UTC)))
        :else (str v)))

(defn- code-excerpt [body]
  (when-let [[_ code] (re-find #"(?s)```[^\n]*\n(.*?)(?:\n```|$)" body)]
    (let [lines (->> (str/split-lines code) (remove str/blank?) (take 3))]
      (when (seq lines) (str/join "\n" lines)))))

(defn index-doc
  "note (from store/read-note) -> map for the index."
  [{:keys [type slug content hash mtime meta body]}]
  (let [title (or (some-> (:title meta) str str/trim not-empty)
                  (links/first-heading body)
                  slug)
        lnks  (links/find-links content)
        ex    (links/excerpt body)]
    {:slug    slug
     :type    (name type)
     :title   title
     :hash    hash
     :mtime   mtime
     :created (->date-str (:created meta))
     :lang    (some-> (:lang meta) str)
     :tags    (->tags (:tags meta))
     :excerpt (if (str/blank? ex) (or (code-excerpt body) "") ex)
     :body    body
     :links   (->> lnks
                   (remove :asset?)
                   (map (fn [l] {:dst (:target l) :kind (if (:embed? l) "embed" "link")
                                 :alias (:alias l)}))
                   distinct
                   vec)}))
