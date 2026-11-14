(ns notebook.api-test
  "Exercise the real Ring handler (routing, JSON, multipart) end to end."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [jsonista.core :as json]
            [notebook.notes.index :as index]
            [notebook.notes.store :as store]
            [notebook.server :as server])
  (:import (java.io ByteArrayInputStream)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def ^:dynamic *app* nil)
(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [root (str (Files/createTempDirectory "notebook-api" (make-array FileAttribute 0)))
          db   (index/open (str root "/.notebook/index.db"))]
      (store/ensure-dirs! root)
      (binding [*root* root
                *app*  (server/app {:root root :db db :hub (atom #{}) :lock (Object.)})]
        (try (t) (finally (index/close db)))))))

(defn- req [method uri & {:keys [body headers]}]
  (let [[path qs] (str/split uri #"\?" 2)
        bytes (cond (string? body) (.getBytes ^String body "UTF-8")
                    (bytes? body) body
                    body (.getBytes (json/write-value-as-string body) "UTF-8"))]
    (*app* (cond-> {:request-method method :uri path :query-string qs
                    :remote-addr "127.0.0.1" :scheme :http :server-name "localhost" :server-port 8080
                    :headers (merge {"accept" "application/json"}
                                    (when (and body (not (bytes? body)) (not (string? body)))
                                      {"content-type" "application/json"})
                                    headers)}
             bytes (assoc :body (ByteArrayInputStream. bytes))))))

(defn- json-body [{:keys [body]}]
  (let [s (if (string? body) body (slurp body))]
    (when-not (str/blank? s) (json/read-value s json/keyword-keys-object-mapper))))

(deftest notes-crud
  (let [created (json-body (req :post "/api/notes" :body {:type "article" :title "Hello World"}))
        slug    (:slug created)]
    (is (= "hello-world" slug))
    (testing "save with correct base hash"
      (let [r (req :put "/api/notes/hello-world" :body {:content "# Hi\n\nSee [[other]]" :base_hash (:hash created)})]
        (is (= 200 (:status r)))
        (is (string? (:hash (json-body r))))))
    (testing "stale base hash -> 409 with disk content"
      (let [r (req :put "/api/notes/hello-world" :body {:content "x" :base_hash (:hash created)})]
        (is (= 409 (:status r)))
        (is (= "# Hi\n\nSee [[other]]" (:content (json-body r))))))
    (testing "catalog, note detail, search"
      (is (= ["hello-world"] (map :slug (json-body (req :get "/api/notes")))))
      (is (= [{:slug "other" :type nil}] (map #(select-keys % [:slug :type])
                                               (:outgoing (json-body (req :get "/api/notes/hello-world"))))))
      (is (= ["hello-world"] (map :slug (json-body (req :get "/api/search?q=hi"))))))
    (testing "rename"
      (let [r (json-body (req :post "/api/notes/hello-world/rename" :body {:title "Bye World"}))]
        (is (= "bye-world" (:slug r)))
        (is (= 404 (:status (req :get "/api/notes/hello-world"))))))
    (testing "delete"
      (is (= 204 (:status (req :delete "/api/notes/bye-world"))))
      (is (= [] (json-body (req :get "/api/notes")))))))

(deftest asset-upload
  (let [boundary "----nbtest"
        body (str "--" boundary "\r\n"
                  "Content-Disposition: form-data; name=\"file\"; filename=\"My Pic.PNG\"\r\n"
                  "Content-Type: image/png\r\n\r\n"
                  "PNGDATA\r\n"
                  "--" boundary "--\r\n")
        r (req :post "/api/assets" :body body
               :headers {"content-type" (str "multipart/form-data; boundary=" boundary)})]
    (is (= 201 (:status r)))
    (is (= "my-pic.png" (:name (json-body r))))
    (is (= "PNGDATA" (slurp (io/file *root* "assets" "my-pic.png"))))
    (testing "served back with a content type"
      (let [r (req :get "/assets/my-pic.png")]
        (is (= 200 (:status r)))
        (is (str/starts-with? (get-in r [:headers "Content-Type"]) "image/png"))))
    (testing "path traversal is refused"
      (is (= 404 (:status (req :get "/assets/..%2F..%2Fetc%2Fpasswd")))))))

(defn- multipart [filename content-type data]
  (let [boundary "----nbtest"]
    {:body    (str "--" boundary "\r\n"
                   "Content-Disposition: form-data; name=\"file\"; filename=\"" filename "\"\r\n"
                   "Content-Type: " content-type "\r\n\r\n"
                   data "\r\n"
                   "--" boundary "--\r\n")
     :headers {"content-type" (str "multipart/form-data; boundary=" boundary)}}))

(defn- upload [filename content-type data & {:as extra-headers}]
  (let [{:keys [body headers]} (multipart filename content-type data)]
    (req :post "/api/assets" :body body :headers (merge headers extra-headers))))

(deftest asset-upload-is-restricted-to-media
  (testing "HTML and other active content is refused"
    (is (= 415 (:status (upload "evil.html" "text/html" "<script>alert(1)</script>"))))
    (is (= 415 (:status (upload "noext" "image/png" "x"))))
    (is (empty? (.list (io/file *root* "assets")))))
  (testing "SVG is served with a script-blocking sandbox CSP"
    (is (= 201 (:status (upload "pic.svg" "image/svg+xml" "<svg xmlns='http://www.w3.org/2000/svg'/>"))))
    (let [r (req :get "/assets/pic.svg")]
      (is (str/starts-with? (get-in r [:headers "Content-Type"]) "image/svg+xml"))
      (is (str/includes? (get-in r [:headers "Content-Security-Policy"]) "sandbox"))
      (is (= "nosniff" (get-in r [:headers "X-Content-Type-Options"])))))
  (testing "files dropped into assets/ by hand are downloaded, not rendered"
    (spit (io/file *root* "assets" "page.html") "<script>alert(1)</script>")
    (let [r (req :get "/assets/page.html")]
      (is (= "application/octet-stream" (get-in r [:headers "Content-Type"])))
      (is (= "attachment" (get-in r [:headers "Content-Disposition"]))))))

(deftest cross-origin-requests-are-refused
  (testing "foreign Host (DNS rebinding)"
    (is (= 403 (:status (req :get "/api/notes" :headers {"host" "attacker.example:8080"}))))
    (is (= 200 (:status (req :get "/api/notes" :headers {"host" "localhost:8080"}))))
    (is (= 200 (:status (req :get "/api/notes" :headers {"host" "[::1]:8080"})))))
  (testing "cross-origin writes (CSRF)"
    (is (= 403 (:status (upload "a.png" "image/png" "x"
                                "host" "127.0.0.1:8080" "origin" "https://evil.example"))))
    (is (= 403 (:status (req :post "/api/notes" :body {:title "x"}
                             :headers {"host" "127.0.0.1:8080" "origin" "null"}))))
    (is (= 403 (:status (req :post "/api/notes" :body {:title "x"}
                             :headers {"host" "127.0.0.1:8080" "sec-fetch-site" "cross-site"}))))
    (is (= [] (json-body (req :get "/api/notes")))))
  (testing "same-origin writes still work"
    (is (= 201 (:status (req :post "/api/notes" :body {:title "x"}
                             :headers {"host" "127.0.0.1:8080" "origin" "http://127.0.0.1:8080"
                                       "sec-fetch-site" "same-origin"}))))))

(deftest slugs-cannot-escape-the-notes-dir
  (spit (io/file *root* "outside.md") "secret")
  (is (= 404 (:status (req :get "/api/notes/..%2Foutside"))))
  (is (= 404 (:status (req :delete "/api/notes/..%2Foutside"))))
  (is (= 404 (:status (req :put "/api/notes/..%2Foutside" :body {:content "pwned"}))))
  (is (= "secret" (slurp (io/file *root* "outside.md")))))

(deftest rename-carries-editor-content
  (let [{:keys [hash]} (json-body (req :post "/api/notes" :body {:title "Draft"}))]
    (testing "unsaved editor text is written under the new name"
      (let [r (json-body (req :post "/api/notes/draft/rename"
                              :body {:title "Final" :content "---\ntitle: Draft\n---\nunsaved words"
                                     :base_hash hash}))]
        (is (= "final" (:slug r)))
        (is (str/includes? (:content r) "unsaved words"))
        (is (str/includes? (:content r) "title: Final"))
        (is (= (:content r) (slurp (io/file *root* "articles" "final.md"))))
        (is (= (store/sha1 (:content r)) (:hash r)))))
    (testing "a stale base hash is a conflict and changes nothing"
      (let [r (req :post "/api/notes/final/rename"
                   :body {:title "Other" :content "x" :base_hash hash})]
        (is (= 409 (:status r)))
        (is (:conflict (json-body r)))
        (is (.isFile (io/file *root* "articles" "final.md")))))))

(deftest spa-fallback-and-localhost-guard
  (is (= 200 (:status (req :get "/n/some-note"))))
  (is (= 403 (:status (*app* {:request-method :get :uri "/api/notes" :remote-addr "10.0.0.5"
                              :headers {} :scheme :http :server-name "x" :server-port 1})))))
