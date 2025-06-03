(ns pulpulak.components.database-pure
  "Pure functional database operations without side effects"
  (:require [com.stuartsierra.component :as component]
            [clojure.spec.alpha :as s]
            [pulpulak.utils.logging-pure :as logging]))

;; Database state operations (pure functions)
(defn empty-db-state
  "Creates an empty database state"
  []
  {:games {}
   :rooms {}
   :player-rooms {}})

(defn save-game-pure
  "Pure function to save game state. Returns [new-db-state result]"
  [db-state room-id game-state]
  (if (s/valid? :pulpulak.game.pure/game-state game-state)
    [(assoc-in db-state [:games room-id] game-state)
     {:success true}]
    [db-state
     {:success false
      :error (s/explain-str :pulpulak.game.pure/game-state game-state)}]))

(defn get-game-pure
  "Pure function to get game state"
  [db-state room-id]
  (get-in db-state [:games room-id]))

(defn delete-game-pure
  "Pure function to delete game. Returns new db-state"
  [db-state room-id]
  (-> db-state
      (update :games dissoc room-id)
      (update :rooms dissoc room-id)))

(defn list-games-pure
  "Pure function to list all games"
  [db-state]
  (vals (:games db-state)))

(defn set-player-room-pure
  "Pure function to associate player with room. Returns new db-state"
  [db-state player-id room-id]
  (assoc-in db-state [:player-rooms player-id] room-id))

(defn get-player-room-pure
  "Pure function to get player's room"
  [db-state player-id]
  (get-in db-state [:player-rooms player-id]))

(defn remove-player-room-pure
  "Pure function to remove player-room association. Returns new db-state"
  [db-state player-id]
  (update db-state :player-rooms dissoc player-id))

;; Database operations with transactions
(defn transaction
  "Applies multiple pure operations atomically. Returns [new-state results]"
  [db-state operations]
  (reduce (fn [[state results] operation]
            (let [[new-state result] (operation state)]
              [new-state (conj results result)]))
          [db-state []]
          operations))

;; Database component that uses pure functions
(defprotocol PureDatabase
  (get-state [this])
  (update-state! [this update-fn])
  (query [this query-fn]))

(defrecord PureDatabaseComponent [state-atom]
  component/Lifecycle
  
  (start [this]
    (logging/process-log-events [(logging/log-info "Starting pure database component")])
    (assoc this :state-atom (atom (empty-db-state))))
  
  (stop [this]
    (logging/process-log-events [(logging/log-info "Stopping pure database component")])
    (dissoc this :state-atom))
  
  PureDatabase
  
  (get-state [this]
    @state-atom)
  
  (update-state! [this update-fn]
    (swap! state-atom update-fn))
  
  (query [this query-fn]
    (query-fn @state-atom)))

;; Helper functions for common operations
(defn create-game-operation
  "Creates a pure operation to save a game"
  [room-id game-state]
  (fn [db-state]
    (save-game-pure db-state room-id game-state)))

(defn delete-game-operation
  "Creates a pure operation to delete a game"
  [room-id]
  (fn [db-state]
    [(delete-game-pure db-state room-id) {:success true}]))

(defn associate-player-operation
  "Creates a pure operation to associate player with room"
  [player-id room-id]
  (fn [db-state]
    [(set-player-room-pure db-state player-id room-id) {:success true}]))

(defn new-pure-database
  "Creates a new pure database component"
  [config]
  (map->PureDatabaseComponent config))