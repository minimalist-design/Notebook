(ns notebook.main
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]
            [notebook.notes.index :as index]
            [notebook.notes.sync :as sync]
            [notebook.server :as server]
            [notebook.sse :as sse]))

(defn- usage! [code msg]
  (when msg (println msg))
  (println "Usage: notebook [--notes DIR] [--port N] [--host ADDR] [--db FILE]")
  (System/exit code))

(defn- ->port
  "Parse a TCP port, or throw with a readable message."
  [source s]
  (let [n (some-> s str str/trim parse-long)]
    (if (and n (<= 1 n 65535))
      n
      (throw (ex-info (str "Invalid port " (pr-str s) " (from " source "): expected 1-65535")
                      {:port s})))))

(defn config
  [{:keys [notes port host db]
    :or   {notes (or (System/getenv "NOTEBOOK_DIR") "notes")
           host  "127.0.0.1"}}]
  (let [port (if port
               (->port "--port" port)
               (->port "NOTEBOOK_PORT" (or (System/getenv "NOTEBOOK_PORT") "8080")))
        root (.getAbsolutePath (io/file notes))]
    {::db      {:path (or db (str (io/file root ".notebook" "index.db")))}
     ::hub     {}
     ::ctx     {:root root :db (ig/ref ::db) :hub (ig/ref ::hub)}
     ::watcher {:ctx (ig/ref ::ctx)}
     ::server  {:ctx (ig/ref ::ctx) :port port :host host :watcher (ig/ref ::watcher)}}))

(defmethod ig/init-key ::db [_ {:keys [path]}]
  (log/info "Index at" path)
  (index/open path))
(defmethod ig/halt-key! ::db [_ conn] (index/close conn))

(defmethod ig/init-key ::hub [_ _] (sse/hub))

(defmethod ig/init-key ::ctx [_ ctx] (assoc ctx :lock (Object.)))

(defmethod ig/init-key ::watcher [_ {:keys [ctx]}] (sync/start! ctx))
(defmethod ig/halt-key! ::watcher [_ w] (sync/stop! w))

(defmethod ig/init-key ::server [_ {:keys [ctx port host]}]
  (let [s (server/start! ctx {:port port :host host})]
    (log/infof "Notebook running at http://%s:%d  (notes: %s)" host port (:root ctx))
    s))
(defmethod ig/halt-key! ::server [_ s] (server/stop! s))

(def ^:private flags {"--notes" :notes "--port" :port "--host" :host "--db" :db})

(defn- parse-args [args]
  (loop [[k v & more] args, acc {}]
    (cond
      (nil? k) acc
      (str/includes? k "help") (usage! 0 nil)
      (not (flags k)) (usage! 1 (str "Unknown option: " k))
      (nil? v) (usage! 1 (str "Missing value for " k))
      :else (recur more (assoc acc (flags k) v)))))

(defn -main [& args]
  (let [cfg    (try (config (parse-args args))
                    (catch clojure.lang.ExceptionInfo e (usage! 1 (ex-message e))))
        system (ig/init cfg)]
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(ig/halt! system)))
    @(promise)))
