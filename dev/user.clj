(ns user
  (:require [integrant.repl :as ir]
            [notebook.main :as main]))

(ir/set-prep! #(main/config {}))

(def go ir/go)
(def halt ir/halt)
(def reset ir/reset)

(comment
  (go)
  (reset)
  (halt))
