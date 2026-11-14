(ns notebook.sse
  "Minimal Server-Sent Events hub. Each connected client owns a queue; the
  response body loops, writing queued events and periodic pings until the
  client disconnects."
  (:require [jsonista.core :as json]
            [clojure.tools.logging :as log]
            [ring.core.protocols :as ring-protocols])
  (:import (java.util.concurrent LinkedBlockingQueue TimeUnit)
           (java.io OutputStream IOException)))

(defn hub [] (atom #{}))

(defn broadcast! [hub event data]
  (let [msg (str "event: " (name event) "\n"
                 "data: " (json/write-value-as-string data) "\n\n")]
    (doseq [^LinkedBlockingQueue q @hub]
      (.offer q msg))))

(defn- write! [^OutputStream out ^String s]
  (.write out (.getBytes s "UTF-8"))
  (.flush out))

(defn handler [hub]
  (fn [_req]
    {:status  200
     :headers {"Content-Type"      "text/event-stream; charset=utf-8"
               "Cache-Control"     "no-cache"
               "X-Accel-Buffering" "no"}
     :body    (reify ring-protocols/StreamableResponseBody
                (write-body-to-stream [_ _ out]
                  (let [q (LinkedBlockingQueue.)]
                    (swap! hub conj q)
                    (try
                      (write! out "retry: 2000\n: connected\n\n")
                      (loop []
                        (if-let [msg (.poll q 20 TimeUnit/SECONDS)]
                          (write! out msg)
                          (write! out ": ping\n\n"))
                        (recur))
                      (catch IOException _ nil)
                      (catch InterruptedException _ nil)
                      (catch Exception e (log/debug e "SSE stream closed"))
                      (finally
                        (swap! hub disj q)
                        (try (.close ^OutputStream out) (catch Exception _)))))))}))
