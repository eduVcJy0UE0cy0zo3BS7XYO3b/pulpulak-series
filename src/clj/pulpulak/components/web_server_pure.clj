(ns pulpulak.components.web-server-pure
  "Pure functional web server implementation"
  (:require [com.stuartsierra.component :as component]
            [org.httpkit.server :as http-kit]
            [compojure.core :refer [defroutes GET POST]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.response :as response]
            [pulpulak.utils.logging-pure :as logging]
            [pulpulak.game.registry-pure :as registry]))

;; Pure request handling functions
(defn handle-root-request
  "Pure function to handle root requests"
  [request]
  {:type :static-resource
   :resource "index.html"
   :root "public"
   :logs [(logging/log-debug "Serving root page")]})

(defn handle-health-check
  "Pure function to handle health check requests"
  [request]
  {:type :json-response
   :data {:status "ok" :version "2.0" :timestamp (System/currentTimeMillis)}
   :logs [(logging/log-debug "Health check requested")]})

(defn handle-games-list
  "Pure function to handle games list requests"
  [request registry-state]
  {:type :json-response
   :data {:games (registry/list-games registry-state)}
   :logs [(logging/log-debug "Games list requested")]})

(defn handle-websocket-handshake
  "Pure function to prepare WebSocket handshake"
  [request websocket-config]
  {:type :websocket-handshake
   :handler-fn (get-in websocket-config [:socket :ajax-get-or-ws-handshake-fn])
   :logs [(logging/log-debug "WebSocket handshake requested")]})

(defn handle-websocket-post
  "Pure function to prepare WebSocket POST"
  [request websocket-config]
  {:type :websocket-post
   :handler-fn (get-in websocket-config [:socket :ajax-post-fn])
   :logs [(logging/log-debug "WebSocket POST requested")]})

(defn handle-static-resource
  "Pure function to handle static resource requests"
  [request]
  {:type :static-resources
   :logs [(logging/log-debug "Static resource requested" {:uri (:uri request)})]})

(defn handle-not-found
  "Pure function to handle 404 requests"
  [request]
  {:type :not-found
   :logs [(logging/log-warn "Resource not found" {:uri (:uri request)})]})

;; Pure route matching
(defn match-route
  "Pure function to match routes and return handler result"
  [request websocket registry-state]
  (let [method (:request-method request)
        uri (:uri request)]
    (cond
      (and (= method :get) (= uri "/"))
      (handle-root-request request)
      
      (and (= method :get) (= uri "/health"))
      (handle-health-check request)
      
      (and (= method :get) (= uri "/api/games"))
      (handle-games-list request registry-state)
      
      (and (= method :get) (= uri "/chsk"))
      (handle-websocket-handshake request websocket)
      
      (and (= method :post) (= uri "/chsk"))
      (handle-websocket-post request websocket)
      
      (and (= method :get) (.startsWith uri "/"))
      (handle-static-resource request)
      
      :else
      (handle-not-found request))))

;; Response builders
(defn build-response
  "Pure function to build HTTP response from handler result"
  [handler-result]
  (case (:type handler-result)
    :static-resource
    (response/resource-response (:resource handler-result) 
                                {:root (:root handler-result)})
    
    :json-response
    (response/response (:data handler-result))
    
    :static-resources
    (route/resources "/")
    
    :not-found
    (response/not-found "Not Found")
    
    ;; For WebSocket routes, we need to delegate to the actual handler
    :websocket-handshake
    nil  ; Will be handled by the wrapper
    
    :websocket-post
    nil  ; Will be handled by the wrapper
    
    ;; Default
    (response/response {:error "Unknown handler type"})))

;; Middleware composition
(defn create-pure-app
  "Creates a Ring application using pure functions"
  [websocket registry-atom]
  (fn [request]
    (let [registry-state @registry-atom
          handler-result (match-route request websocket registry-state)]
      
      ;; Process logs
      (logging/process-log-events (:logs handler-result))
      
      ;; Handle special cases (WebSocket routes)
      (case (:type handler-result)
        :websocket-handshake
        ((:handler-fn handler-result) request)
        
        :websocket-post
        ((:handler-fn handler-result) request)
        
        ;; Standard responses
        (build-response handler-result)))))

;; Pure web server component
(defrecord PureWebServer [config websocket registry server]
  component/Lifecycle
  
  (start [this]
    (let [logs [(logging/log-info "Starting web server on port" (:port config))]
          registry-atom (atom (registry/empty-registry))
          app (-> (create-pure-app websocket registry-atom)
                  (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false)))
          server (http-kit/run-server app {:port (:port config)
                                           :thread 8})]
      ;; Process startup logs
      (logging/process-log-events logs)
      (assoc this 
             :server server
             :registry registry-atom)))
  
  (stop [this]
    (let [logs [(logging/log-info "Stopping web server")]]
      (when server
        (server :timeout 100))
      (logging/process-log-events logs)
      (dissoc this :server :registry))))

;; Pure configuration functions
(defn validate-server-config
  "Pure function to validate server configuration"
  [config]
  (let [required-keys [:port]
        missing-keys (remove #(contains? config %) required-keys)]
    (if (empty? missing-keys)
      {:valid true :config config}
      {:valid false 
       :errors missing-keys
       :message (str "Missing required server config: " missing-keys)})))

(defn normalize-server-config
  "Pure function to normalize server configuration"
  [config]
  (merge {:port 3000
          :thread 8
          :timeout 100}
         config))

(defn create-server-config
  "Pure function to create normalized and validated server config"
  [raw-config]
  (let [normalized (normalize-server-config raw-config)
        validation (validate-server-config normalized)]
    (if (:valid validation)
      {:success true :config (:config validation)}
      {:success false :error (:message validation)})))

;; Request analysis functions (pure)
(defn analyze-request
  "Pure function to analyze incoming request"
  [request]
  {:method (:request-method request)
   :uri (:uri request)
   :headers (select-keys (:headers request) ["user-agent" "accept"])
   :timestamp (System/currentTimeMillis)})

(defn is-api-request?
  "Pure function to check if request is an API request"
  [request]
  (.startsWith (:uri request) "/api/"))

(defn is-websocket-request?
  "Pure function to check if request is WebSocket related"
  [request]
  (= (:uri request) "/chsk"))

(defn categorize-request
  "Pure function to categorize request type"
  [request]
  (cond
    (is-websocket-request? request) :websocket
    (is-api-request? request) :api
    (= (:uri request) "/") :root
    (= (:uri request) "/health") :health
    (.startsWith (:uri request) "/") :static
    :else :unknown))

(defn new-pure-web-server
  "Creates a new pure web server component"
  [config]
  (map->PureWebServer {:config config}))