(ns notebook.fuzzy-test
  (:require [clojure.test :refer [deftest is]]
            [notebook.fuzzy :as fuzzy]))

(def notes
  [{:slug "clojure-repl-tricks" :title "Clojure REPL tricks" :mtime 1}
   {:slug "color-palette" :title "Color palette reference" :mtime 5}
   {:slug "replicant-notes" :title "Replicant notes" :mtime 3}
   {:slug "jvm-flags" :title "JVM flags" :mtime 2}])

(deftest match-basics
  (is (nil? (fuzzy/match "xyz" "Clojure")))
  (is (= [0 1 2] (:positions (fuzzy/match "clo" "Clojure"))))
  (is (> (:score (fuzzy/match "rt" "REPL tricks"))
         (:score (fuzzy/match "rt" "sorted")))))

(deftest ranking
  (is (= "clojure-repl-tricks" (:slug (first (fuzzy/rank "cljrpl" notes)))))
  (is (= "replicant-notes" (:slug (first (fuzzy/rank "repli" notes)))))
  (is (= "jvm-flags" (:slug (first (fuzzy/rank "jvm" notes)))))
  (is (= ["color-palette" "replicant-notes" "jvm-flags" "clojure-repl-tricks"]
         (map :slug (fuzzy/rank "" notes))))
  (is (= [[0 3] [5 6]] (fuzzy/ranges [0 1 2 5]))))
