(ns pulpulak.server
  (:require [pulpulak.system :as system]
            [pulpulak.config-pure :as config]
            [pulpulak.utils.logging-pure :as logging])
  (:gen-class))

(defonce ^:private system-instance (atom nil))

(defn -main [& args]
  (logging/process-log-events [(logging/log-info "Starting Pulpulak server...")])
  
  ;; Load game configurations
  (require 'pulpulak.games.pulpulak-config)
  
  ;; Start the system
  (let [config-data {:server {:port 3000}
                     :database {}
                     :websocket {}}
        sys (system/start-system! config-data)]
    (reset! system-instance sys)
    (logging/process-log-events [(logging/log-info "Pulpulak server started successfully")])))

(defn stop []
  (when-let [sys @system-instance]
    (system/stop-system! sys)
    (reset! system-instance nil)))