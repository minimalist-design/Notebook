(ns build
  "clojure -T:build uber   — builds target/notebook.jar
  Run `npm run release` first so resources/public holds the optimised JS,
  fonts and vendor files."
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def jar-file "target/notebook.jar")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_] (b/delete {:path "target"}))

(defn uber [_]
  (when-not (.exists (io/file "resources/public/js/main.js"))
    (throw (ex-info "resources/public/js/main.js missing — run `npm run release` first" {})))
  (clean nil)
  (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "resources"] :target-dir class-dir
               :ignores [#"^cljs-runtime$" #".*\.map$" #"^manifest\.edn$"]})
  (b/compile-clj {:basis @basis :ns-compile '[notebook.main] :class-dir class-dir})
  (b/uber {:class-dir class-dir :uber-file jar-file :basis @basis :main 'notebook.main})
  (println "Built" jar-file))
