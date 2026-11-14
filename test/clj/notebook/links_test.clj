(ns notebook.links-test
  (:require [clojure.test :refer [deftest is testing]]
            [notebook.links :as links]))

(deftest slugify-test
  (is (= "clojure-repl-tricks" (links/slugify "Clojure REPL Tricks!")))
  (is (= "cafe-creme" (links/slugify "Café   Crème")))
  (is (= "untitled" (links/slugify "!!!"))))

(deftest find-links-test
  (let [text (str "---\ntitle: [[not-a-link]]\n---\n"
                  "See [[Clojure REPL Tricks]] and [[jvm-flags|flags]].\n"
                  "Heading link [[setup#Install Steps]] and embed ![[snippet-a]].\n"
                  "Image ![[diagram.png]]\n"
                  "Inline `[[in-code]]` and math $[[x]]$.\n"
                  "```clojure\n(str \"[[fenced]]\")\n```\n"
                  "~~~\n[[tilde-fenced]]\n~~~\n"
                  "$$\n[[display-math]]\n$$\n")
        ls   (links/find-links text)]
    (is (= ["clojure-repl-tricks" "jvm-flags" "setup" "snippet-a" "diagram.png"]
           (map :target ls)))
    (is (= "flags" (:alias (second ls))))
    (is (= "Install Steps" (:heading (nth ls 2))))
    (is (:embed? (nth ls 3)))
    (is (:asset? (nth ls 4)))))

(deftest rewrite-links-test
  (let [text "a [[old]] b [[Old|Alias]] c [[old#H]] d ![[old]] e `[[old]]` f [[other]]"]
    (is (= "a [[new]] b [[new|Alias]] c [[new#H]] d ![[new]] e `[[old]]` f [[other]]"
           (links/rewrite-links text "old" "new")))))

(deftest frontmatter-test
  (let [{:keys [frontmatter body]} (links/split-frontmatter "---\ntitle: X\n---\nBody")]
    (is (= "title: X" frontmatter))
    (is (= "Body" body)))
  (is (nil? (:frontmatter (links/split-frontmatter "No fm")))))

(deftest excerpt-test
  (is (= "Hello flags world." (links/excerpt "# Title\n\nHello [[jvm-flags|flags]] *world*.\n\nMore")))
  (is (= "Text" (links/excerpt "```\ncode\n```\n\nText"))))
