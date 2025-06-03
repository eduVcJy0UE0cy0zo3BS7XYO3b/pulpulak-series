(ns pulpulak.game.registry
  (:require [taoensso.timbre :as log]))

(defonce games (atom {}))

(defprotocol GameConfig
  "Protocol for game configurations"
  (get-game-id [this])
  (get-game-name [this])
  (get-game-description [this])
  (get-story-data [this])
  (get-location-data [this])
  (get-npc-data [this])
  (get-quest-data [this])
  (get-game-constants [this])
  (initialize-game [this]))

(defn register-game! [game-config]
  (let [game-id (get-game-id game-config)]
    (log/info "Registering game:" game-id)
    (swap! games assoc game-id game-config)
    game-id))

(defn unregister-game! [game-id]
  (log/info "Unregistering game:" game-id)
  (swap! games dissoc game-id))

(defn get-game [game-id]
  (get @games game-id))

(defn list-games []
  (map (fn [[id config]]
         {:id id
          :name (get-game-name config)
          :description (get-game-description config)})
       @games))

(defn game-exists? [game-id]
  (contains? @games game-id))