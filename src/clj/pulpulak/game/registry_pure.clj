(ns pulpulak.game.registry-pure
  "Pure functional game registry - no side effects"
  (:require [pulpulak.utils.logging-pure :as logging]))

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

;; Pure functions for registry operations
(defn empty-registry
  "Creates an empty game registry"
  []
  {})

(defn register-game
  "Pure function to add a game to registry. Returns new registry."
  [registry game-config]
  (let [game-id (get-game-id game-config)]
    (assoc registry game-id game-config)))

(defn unregister-game
  "Pure function to remove a game from registry. Returns new registry."
  [registry game-id]
  (dissoc registry game-id))

(defn get-game
  "Pure function to get a game from registry"
  [registry game-id]
  (get registry game-id))

(defn list-games
  "Pure function to list all games in registry"
  [registry]
  (vec (map (fn [[id config]]
              {:id id
               :name (get-game-name config)
               :description (get-game-description config)})
            registry)))

(defn game-exists?
  "Pure function to check if game exists in registry"
  [registry game-id]
  (contains? registry game-id))

(defn get-games-by-criteria
  "Pure function to filter games by criteria"
  [registry criteria-fn]
  (filter criteria-fn (vals registry)))

;; Registry state container (only used at component boundaries)
(defn create-registry-state
  "Creates an initial registry state container"
  []
  {:registry (empty-registry)})

(defn update-registry-state
  "Updates registry state using a pure function"
  [state update-fn & args]
  (update state :registry #(apply update-fn % args)))