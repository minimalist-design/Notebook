(ns notebook.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [muuntaja.core :as m]
            [notebook.api :as api]
            [notebook.sse :as sse]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]))

(defn- index-html [_]
  (if-let [r (io/resource "public/index.html")]
    {:status  200
     :headers {"Content-Type" "text/html; charset=utf-8" "Cache-Control" "no-cache"}
     :body    (slurp r)}
    {:status 500 :body "index.html missing"}))

(defn- wrap-localhost-only
  "Defence in depth: this app has no auth, so refuse non-loopback clients."
  [handler]
  (fn [req]
    (let [addr (:remote-addr req)]
      (if (or (nil? addr) (#{"127.0.0.1" "0:0:0:0:0:0:0:1" "::1"} addr))
        (handler req)
        {:status 403 :body "Forbidden"}))))

(def ^:private loopback-hosts #{"localhost" "127.0.0.1" "[::1]"})

(defn- host-name
  "\"localhost:8080\" -> \"localhost\", \"[::1]:8080\" -> \"[::1]\"."
  [host]
  (when host
    (str/lower-case (second (re-matches #"(\[[^\]]*\]|[^:]*)(?::\d*)?" host)))))

(defn- wrap-same-origin
  "The browser runs on loopback too, so the remote-address check alone does
  not stop other web pages. Refuse:
   - any Host that is not a loopback name (DNS rebinding), and
   - state-changing requests sent by another origin (CSRF; a multipart form
     POST needs no CORS preflight)."
  [handler]
  (fn [{:keys [request-method headers] :as req}]
    (let [host   (get headers "host")
          origin (get headers "origin")
          site   (get headers "sec-fetch-site")
          safe?  (#{:get :head :options} request-method)]
      (cond
        (and host (not (loopback-hosts (host-name host))))
        {:status 403 :body "Forbidden host"}

        (and (not safe?) origin
             (not= (str/lower-case origin) (str "http://" (str/lower-case (or host "")))))
        {:status 403 :body "Cross-origin request refused"}

        (and (not safe?) (not origin) site (not (#{"same-origin" "none"} site)))
        {:status 403 :body "Cross-site request refused"}

        :else (handler req)))))

(defn router [ctx]
  (ring/router
   [["/api" {:middleware [muuntaja/format-middleware]}
     ["/notes" {:get  (api/list-notes ctx)
                :post (api/create-note ctx)}]
     ["/notes/:slug" {:get    (api/get-note ctx)
                      :put    (api/save-note ctx)
                      :delete (api/delete-note ctx)}]
     ["/notes/:slug/rename" {:post (api/rename-note ctx)}]
     ["/search" {:get (api/search ctx)}]
     ["/tags" {:get (api/tags ctx)}]
     ["/tags/:tag" {:get (api/tag-notes ctx)}]
     ["/assets" {:get  (api/list-assets ctx)
                 :post {:middleware [wrap-multipart-params]
                        :handler    (api/upload-asset ctx)}}]
     ["/events" {:get (sse/handler (:hub ctx))}]]
    ["/assets/:name" {:get (api/serve-asset ctx)}]]
   {:data {:muuntaja   m/instance
           :middleware [parameters/parameters-middleware]}}))

(defn app [ctx]
  (-> (ring/ring-handler
       (router ctx)
       (ring/routes
        (ring/create-resource-handler {:path "/" :root "public" :not-found-handler index-html})
        (ring/create-default-handler {:not-found index-html})))
      wrap-content-type
      wrap-not-modified
      wrap-same-origin
      wrap-localhost-only))

(defn start! [ctx {:keys [port host]}]
  (jetty/run-jetty (app ctx) {:port port :host host :join? false
                              :max-idle-time (* 10 60 1000)}))

(defn stop! [server]
  (when server (.stop ^org.eclipse.jetty.server.Server server)))
