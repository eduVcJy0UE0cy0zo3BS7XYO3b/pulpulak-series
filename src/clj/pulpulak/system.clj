(ns pulpulak.system
  (:require [com.stuartsierra.component :as component]
            [pulpulak.components.web-server-pure :as web-server]
            [pulpulak.components.websocket-pure :as websocket]
            [pulpulak.components.game-engine :as game-engine]
            [pulpulak.components.database-pure :as database]
            [pulpulak.config-pure :as config]
            [pulpulak.utils.logging-pure :as logging]))

(defn new-system
  "Creates a new system with all components"
  [config-data]
  (component/system-map
   :config (config/new-config-component "development")
   
   :database (database/new-pure-database (:database config-data))
   
   :game-engine (component/using
                 (game-engine/new-game-engine)
                 [:database :config])
   
   :websocket (component/using
               (websocket/new-pure-websocket-server (:websocket config-data))
               [:game-engine])
   
   :web-server (component/using
                (web-server/new-pure-web-server (:server config-data))
                [:websocket :config])))

(defn start-system!
  "Starts the system with the given configuration"
  [config]
  (let [system (new-system config)]
    (logging/process-log-events [(logging/log-info "Starting Pulpulak system...")])
    (component/start system)))

(defn stop-system!
  "Stops the given system"
  [system]
  (logging/process-log-events [(logging/log-info "Stopping Pulpulak system...")])
  (component/stop system))