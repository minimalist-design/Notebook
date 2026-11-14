(ns notebook.render-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [notebook.fuzzy :as fuzzy]
            [notebook.md.render :as md]))

(def catalog
  {"alpha" {:slug "alpha" :type "article" :title "Alpha Article"}
   "handy-snippet" {:slug "handy-snippet" :type "snippet" :title "Handy Snippet" :lang "clojure"}
   "loop-a" {:slug "loop-a" :type "article" :title "Loop A"}
   "loop-b" {:slug "loop-b" :type "article" :title "Loop B"}})

(defn- html [src & [contents]]
  (:html (md/render src {:catalog catalog :contents (or contents {}) :depth 0 :seen #{}})))

(deftest wikilinks
  (let [h (html "See [[alpha]], [[Handy Snippet]], [[alpha#Some Heading|here]] and [[Nope Missing]].")]
    (is (str/includes? h "<a class=\"wikilink wikilink--article\" href=\"/n/alpha\" data-slug=\"alpha\">Alpha Article</a>"))
    (is (str/includes? h "wikilink--snippet\" href=\"/n/handy-snippet\"") "title-style targets are slugified")
    (is (str/includes? h "href=\"/n/alpha#some-heading\" data-slug=\"alpha\">here</a>"))
    (is (str/includes? h "wikilink--dangling\" href=\"/n/nope-missing\" data-create=\"Nope Missing\""))))

(deftest links-in-code-are-literal
  (let [h (html "`[[alpha]]`\n\n```\n[[alpha]]\n```")]
    (is (not (str/includes? h "wikilink")))))

(deftest embeds
  (testing "missing content is reported and a placeholder rendered"
    (let [{:keys [html missing]} (md/render "![[handy-snippet]]" {:catalog catalog :contents {} :depth 0 :seen #{}})]
      (is (= ["handy-snippet"] missing))
      (is (str/includes? html "embed--loading"))))
  (testing "embedded note renders without a wrapping <p>"
    (let [h (html "![[handy-snippet]]" {"handy-snippet" "---\ntitle: x\n---\n```clojure\n(+ 1 2)\n```"})]
      (is (str/starts-with? h "<aside class=\"embed embed--snippet\">"))
      (is (str/includes? h "Snippet · clojure"))
      (is (str/includes? h "hljs"))))
  (testing "cycles stop instead of recursing forever"
    (let [h (html "![[loop-a]]" {"loop-a" "A ![[loop-b]]" "loop-b" "B ![[loop-a]]"})]
      (is (str/includes? h "embed-cycle"))))
  (testing "image assets"
    (is (str/includes? (html "![[pic.png]]") "<img class=\"embed-img\" src=\"/assets/pic.png\""))))

(deftest math-mermaid-toc
  (let [{:keys [html toc]} (md/render "## One\n\n$x^2$\n\n### Two\n\n```mermaid\ngraph TD\n```"
                                      {:catalog {} :contents {} :depth 0 :seen #{}})]
    (is (str/includes? html "class=\"katex\""))
    (is (str/includes? html "<pre class=\"mermaid\">graph TD"))
    (is (= [{:level 2 :id "one" :text "One"} {:level 3 :id "two" :text "Two"}]
           (map #(dissoc % :line) toc)))))

(deftest fuzzy-in-js
  (is (= "Handy Snippet" (:title (first (fuzzy/rank "hsn" (vals catalog)))))))
