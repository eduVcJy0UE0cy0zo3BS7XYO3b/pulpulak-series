(ns pulpulak.server
  (:require [taoensso.timbre :as log]
            [pulpulak.system :as system]
            [pulpulak.config :as config])
  (:gen-class))

(defonce ^:private system-instance (atom nil))

(defn -main [& args]
  (log/info "Starting Pulpulak server...")
  
  ;; Load game configurations
  (require 'pulpulak.games.pulpulak-config)
  
  ;; Start the system
  (let [full-config (config/load-configuration)
        sys (system/start-system! full-config)]
    (reset! system-instance sys)
    (log/info "Pulpulak server started successfully")))

(defn stop []
  (when-let [sys @system-instance]
    (system/stop-system! sys)
    (reset! system-instance nil)))