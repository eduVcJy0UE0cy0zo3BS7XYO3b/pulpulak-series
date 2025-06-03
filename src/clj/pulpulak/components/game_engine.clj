(ns pulpulak.components.game-engine
  (:require [com.stuartsierra.component :as component]
            [pulpulak.game.pure :as pure]
            [pulpulak.game.commands :as commands]
            [pulpulak.game.events :as events]
            [pulpulak.game.event-store-pure :as event-store]
            [pulpulak.components.database-pure :as db-pure]
            [pulpulak.utils.logging-pure :as logging]))

(defprotocol GameEngine
  "Protocol for game engine operations"
  (create-game [this room-id game-config players]
    "Creates a new game instance")
  (process-command [this room-id player-id command data]
    "Processes a game command and returns events")
  (get-game-view [this room-id player-id]
    "Gets the game state from a player's perspective"))

(defrecord GameEngineComponent [database config event-store]
  component/Lifecycle
  
  (start [this]
    (logging/process-log-events [(logging/log-info "Starting game engine")])
    (assoc this :event-store (event-store/new-event-store)))
  
  (stop [this]
    (logging/process-log-events [(logging/log-info "Stopping game engine")])
    (dissoc this :event-store))
  
  GameEngine
  
  (create-game [this room-id game-config players]
    (let [initial-state (pure/create-initial-state room-id game-config players)
          create-event (events/->GameCreated room-id players)
          ;; Use pure operations
          save-op (db-pure/create-game-operation room-id initial-state)
          event-op (fn [store] (event-store/append-event store create-event))]
      ;; Apply operations
      (db-pure/update-state! database save-op)
      (event-store/update-event-store! event-store event-op)
      {:success true}))
  
  (process-command [this room-id player-id command data]
    (let [current-state (db-pure/query database #(db-pure/get-game-pure % room-id))]
      (if current-state
        (let [command-fn (commands/get-command-handler command)
              result (command-fn current-state player-id data)]
          (if (:error result)
            {:success false :error (:error result)}
            (let [{:keys [new-state events]} result
                  save-op (db-pure/create-game-operation room-id new-state)
                  event-op (fn [store] (event-store/append-events store events))]
              ;; Apply operations atomically
              (db-pure/update-state! database save-op)
              (event-store/update-event-store! event-store event-op)
              {:success true :events events})))
        {:success false :error "Game not found"})))
  
  (get-game-view [this room-id player-id]
    (when-let [game-state (db-pure/query database #(db-pure/get-game-pure % room-id))]
      (pure/create-player-view game-state player-id))))

(defn new-game-engine
  "Creates a new game engine component"
  []
  (map->GameEngineComponent {}))