(ns notebook.api
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [notebook.links :as links]
            [notebook.notes.core :as notes]
            [notebook.notes.index :as index]
            [notebook.notes.store :as store]
            [ring.util.response :as resp]))

(defn- ok [body] {:status 200 :body body})
(defn- not-found [] {:status 404 :body {:error "Not found"}})

(defn list-notes [{:keys [db]}]
  (fn [_] (ok (index/catalog db))))

(defn get-note [ctx]
  (fn [{{:keys [slug]} :path-params}]
    (if-let [n (notes/get-note ctx slug)] (ok n) (not-found))))

(defn create-note [ctx]
  (fn [{:keys [body-params]}]
    {:status 201 :body (notes/create! ctx body-params)}))

(defn save-note [ctx]
  (fn [{{:keys [slug]} :path-params {:keys [content base_hash]} :body-params}]
    (if-not (string? content)
      {:status 400 :body {:error "content required"}}
      (let [{saved :ok :keys [conflict missing]} (notes/save! ctx slug content base_hash)]
        (cond
          saved    (ok {:hash saved})
          conflict {:status 409 :body conflict}
          missing  (not-found))))))

(defn rename-note [ctx]
  (fn [{{:keys [slug]} :path-params {:keys [title new_slug content base_hash]} :body-params}]
    (if (and (some? content) (not (string? content)))
      {:status 400 :body {:error "content must be a string"}}
      (let [{:keys [error] :as r} (notes/rename! ctx slug {:title title :new-slug new_slug
                                                           :content content :base-hash base_hash})]
        (if error
          {:status (if (= error "Not found") 404 409) :body (select-keys r [:error :conflict])}
          (ok r))))))

(defn delete-note [ctx]
  (fn [{{:keys [slug]} :path-params}]
    (if (notes/delete! ctx slug) {:status 204} (not-found))))

(defn search [{:keys [db]}]
  (fn [{{:strs [q]} :query-params}]
    (ok (try (index/search db q)
             (catch Exception _ [])))))

(defn tags [{:keys [db]}]
  (fn [_] (ok (index/tag-counts db))))

(defn tag-notes [{:keys [db]}]
  (fn [{{:keys [tag]} :path-params}]
    (ok (index/by-tag db (str/lower-case tag)))))

;; ---------------------------------------------------------------------------
;; Assets

(defn list-assets [{:keys [root]}]
  (fn [_] (ok (store/list-assets root))))

(defn upload-asset [{:keys [root] :as ctx}]
  (fn [{:keys [multipart-params]}]
    (let [{:keys [filename tempfile]} (get multipart-params "file")
          safe (some-> filename store/safe-asset-name)]
      (cond
        (not tempfile)
        {:status 400 :body {:error "file required"}}

        ;; only media we know how to embed; never HTML or other active content
        (not (links/asset-name? safe))
        {:status 415 :body {:error (str "Unsupported file type: " filename)}}

        :else
        (let [name (notes/with-write-lock ctx
                     (let [name (store/unique-asset-name root safe)
                           dest (io/file (store/assets-dir root) name)]
                       (.mkdirs (.getParentFile dest))
                       (io/copy tempfile dest)
                       name))]
          {:status 201 :body {:name name}})))))

(def ^:private svg-csp
  "SVG can carry script; render it as an inert image even when opened directly."
  "default-src 'none'; img-src 'self' data:; style-src 'unsafe-inline'; sandbox")

(defn serve-asset [{:keys [root]}]
  (fn [{{:keys [name]} :path-params}]
    (let [dir (.getCanonicalFile (store/assets-dir root))
          f   (.getCanonicalFile (io/file dir name))]
      (if (and (.isFile f) (= dir (.getParentFile f)))
        (let [fname (.getName f)
              r     (-> (resp/file-response (str f))
                        (resp/header "Cache-Control" "public, max-age=3600")
                        (resp/header "X-Content-Type-Options" "nosniff"))]
          (cond
            (re-find #"(?i)\.svg$" fname)
            (-> r (resp/content-type "image/svg+xml")
                (resp/header "Content-Security-Policy" svg-csp))

            ;; anything else dropped into assets/ by hand is downloaded, not rendered
            (not (links/asset-name? fname))
            (-> r (resp/content-type "application/octet-stream")
                (resp/header "Content-Disposition" "attachment"))

            :else r))
        {:status 404 :body "Not found"}))))
