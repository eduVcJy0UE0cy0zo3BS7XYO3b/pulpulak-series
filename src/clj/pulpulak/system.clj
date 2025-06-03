(ns pulpulak.system
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [pulpulak.components.web-server :as web-server]
            [pulpulak.components.websocket :as websocket]
            [pulpulak.components.game-engine :as game-engine]
            [pulpulak.components.database :as database]
            [pulpulak.config :as config]))

(defn new-system
  "Creates a new system with all components"
  [config]
  (component/system-map
   :config config
   
   :database (database/new-database (:database config))
   
   :game-engine (component/using
                 (game-engine/new-game-engine)
                 [:database :config])
   
   :websocket (component/using
               (websocket/new-websocket-server (:websocket config))
               [:game-engine])
   
   :web-server (component/using
                (web-server/new-web-server (:server config))
                [:websocket :config])))

(defn start-system!
  "Starts the system with the given configuration"
  [config]
  (let [system (new-system config)]
    (log/info "Starting Pulpulak system...")
    (component/start system)))

(defn stop-system!
  "Stops the given system"
  [system]
  (log/info "Stopping Pulpulak system...")
  (component/stop system))