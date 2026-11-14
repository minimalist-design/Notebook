(ns notebook.notes.sync
  "Watches the notes directory and keeps the index (and connected clients)
  in sync with edits made outside the app."
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [nextjournal.beholder :as beholder]
            [notebook.notes.core :as notes]
            [notebook.notes.store :as store]))

(defn- on-event [{:keys [root] :as ctx} {:keys [path]}]
  (try
    (let [f (.toFile ^java.nio.file.Path path)]
      (when-let [{:keys [type slug]} (store/path->id root f)]
        (notes/with-write-lock ctx
          (notes/index-note! ctx type slug))))
    (catch Throwable t
      (log/warn t "watcher: failed to handle" (str path)))))

(defn start! [{:keys [root] :as ctx}]
  (store/ensure-dirs! root)
  (notes/reconcile! ctx)
  (let [dirs (mapv #(str (io/file root %)) ["articles" "snippets"])]
    (apply beholder/watch #(on-event ctx %) dirs)))

(defn stop! [watcher]
  (when watcher (beholder/stop watcher)))
