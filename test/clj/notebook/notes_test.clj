(ns notebook.notes-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [notebook.notes.core :as notes]
            [notebook.notes.index :as index]
            [notebook.notes.store :as store])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:dynamic *ctx* nil)

(defn- tmp-dir []
  (str (Files/createTempDirectory "notebook-test" (make-array FileAttribute 0))))

(use-fixtures :each
  (fn [t]
    (let [root (tmp-dir)
          db   (index/open (str root "/.notebook/index.db"))]
      (store/ensure-dirs! root)
      (binding [*ctx* {:root root :db db :hub nil :lock (Object.)}]
        (try (t) (finally (index/close db)))))))

(defn- spit-note [type slug content]
  (spit (store/note-file (:root *ctx*) type slug) content))

(deftest create-and-index
  (let [n (notes/create! *ctx* {:type "snippet" :title "JVM Flags" :tags ["jvm" "Perf"]})]
    (is (= "jvm-flags" (:slug n)))
    (is (= "snippet" (:type n)))
    (is (= ["jvm" "perf"] (:tags n)))
    (is (.isFile (store/note-file (:root *ctx*) :snippet "jvm-flags"))))
  (testing "slugs are unique across types"
    (is (= "jvm-flags-2" (:slug (notes/create! *ctx* {:type "article" :title "JVM flags"}))))))

(deftest reconcile-picks-up-external-files
  (spit-note :article "alpha" "---\ntitle: Alpha\ntags: [x]\n---\nLinks to [[beta]] and ![[missing]].")
  (spit-note :snippet "beta" "# Beta snippet\n\n```sh\necho hi\n```")
  (notes/reconcile! *ctx*)
  (let [cat (index/catalog (:db *ctx*))]
    (is (= #{"alpha" "beta"} (set (map :slug cat))))
    (is (= "Beta snippet" (:title (first (filter #(= "beta" (:slug %)) cat))))))
  (is (= ["alpha"] (map :slug (index/backlinks (:db *ctx*) "beta"))))
  (is (= [["beta" "snippet"] ["missing" nil]]
         (map (juxt :slug :type) (index/outgoing (:db *ctx*) "alpha"))))
  (testing "deleted files leave the index"
    (.delete (store/note-file (:root *ctx*) :snippet "beta"))
    (notes/reconcile! *ctx*)
    (is (= ["alpha"] (map :slug (index/catalog (:db *ctx*)))))))

(deftest save-conflict
  (let [{:keys [slug hash]} (notes/create! *ctx* {:type "article" :title "Doc"})]
    (is (:ok (notes/save! *ctx* slug "v2" hash)))
    (testing "stale base hash -> conflict with current content"
      (let [r (notes/save! *ctx* slug "v3" hash)]
        (is (= "v2" (get-in r [:conflict :content])))))
    (testing "saving identical content is a no-op success"
      (is (:ok (notes/save! *ctx* slug "v2" hash))))))

(deftest rename-rewrites-references
  (spit-note :snippet "old-name" "---\ntitle: Old Name\n---\nSelf [[old-name]]")
  (spit-note :article "a" "See [[old-name]], [[Old Name|alias]], ![[old-name]]\n```\n[[old-name]]\n```")
  (spit-note :article "b" "Unrelated [[a]]")
  (notes/reconcile! *ctx*)
  (let [{:keys [slug changed]} (notes/rename! *ctx* "old-name" {:title "New Name"})]
    (is (= "new-name" slug))
    (is (= ["a"] changed))
    (is (nil? (store/find-type (:root *ctx*) "old-name")))
    (let [renamed (slurp (store/note-file (:root *ctx*) :snippet "new-name"))]
      (is (str/includes? renamed "title: New Name"))
      (is (str/includes? renamed "Self [[new-name]]")))
    (is (= "See [[new-name]], [[new-name|alias]], ![[new-name]]\n```\n[[old-name]]\n```"
           (slurp (store/note-file (:root *ctx*) :article "a"))))
    (is (= ["a"] (map :slug (index/backlinks (:db *ctx*) "new-name")))))
  (testing "renaming onto an existing slug fails"
    (is (:error (notes/rename! *ctx* "a" {:title "B"})))))

(deftest search-test
  (spit-note :article "fts" "---\ntitle: Persistent data structures\ntags: [clojure]\n---\nHash array mapped tries are <fast>.")
  (notes/reconcile! *ctx*)
  (let [[r] (index/search (:db *ctx*) "tri")]
    (is (= "fts" (:slug r)))
    (is (str/includes? (:snippet r) "<mark>tries</mark>"))
    (is (str/includes? (:snippet r) "&lt;fast&gt;")))
  (is (= [] (index/search (:db *ctx*) "\"\"")))
  (is (= ["fts"] (map :slug (index/search (:db *ctx*) "clojure")))))
