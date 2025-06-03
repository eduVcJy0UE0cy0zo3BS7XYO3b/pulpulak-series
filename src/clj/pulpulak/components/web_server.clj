(ns pulpulak.components.web-server
  (:require [com.stuartsierra.component :as component]
            [org.httpkit.server :as http-kit]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.util.response :as response]
            [taoensso.timbre :as log]))

(defn create-routes [websocket]
  (defroutes app-routes
    (GET "/" [] (response/resource-response "index.html" {:root "public"}))
    (GET "/health" [] (response/response {:status "ok" :version "2.0"}))
    
    ;; WebSocket routes
    (GET  "/chsk" req ((get-in websocket [:socket :ajax-get-or-ws-handshake-fn]) req))
    (POST "/chsk" req ((get-in websocket [:socket :ajax-post-fn]) req))
    
    ;; API routes
    (GET "/api/games" []
      (response/response {:games (pulpulak.game.registry/list-games)}))
    
    ;; Static resources
    (route/resources "/")
    (route/not-found "Not Found")))

(defn create-app [websocket]
  (-> (create-routes websocket)
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
      wrap-json-response
      (wrap-json-body {:keywords? true})))

(defrecord WebServer [config websocket server]
  component/Lifecycle
  
  (start [this]
    (log/info "Starting web server on port" (:port config))
    (let [app (create-app websocket)
          server (http-kit/run-server app {:port (:port config)
                                           :thread 8})]
      (assoc this :server server)))
  
  (stop [this]
    (log/info "Stopping web server")
    (when server
      (server :timeout 100))
    (dissoc this :server)))

(defn new-web-server
  "Creates a new web server component"
  [config]
  (map->WebServer {:config config}))