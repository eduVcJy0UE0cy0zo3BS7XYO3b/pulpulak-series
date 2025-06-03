(ns pulpulak.components.game-engine
  (:require [com.stuartsierra.component :as component]
            [taoensso.timbre :as log]
            [pulpulak.game.pure :as pure]
            [pulpulak.game.commands :as commands]
            [pulpulak.game.events :as events]
            [pulpulak.game.registry :as registry]))

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
    (log/info "Starting game engine")
    (assoc this :event-store (atom [])))
  
  (stop [this]
    (log/info "Stopping game engine")
    (dissoc this :event-store))
  
  GameEngine
  
  (create-game [this room-id game-config players]
    (let [initial-state (pure/create-initial-state room-id game-config players)
          events [(events/->GameCreated room-id players)]
          result ((:save-game! database) room-id initial-state)]
      (when (:success result)
        (swap! event-store conj events))
      result))
  
  (process-command [this room-id player-id command data]
    (if-let [current-state ((:get-game database) room-id)]
      (let [command-fn (commands/get-command-handler command)
            {:keys [new-state events error]} (command-fn current-state player-id data)]
        (if error
          {:success false :error error}
          (let [save-result ((:save-game! database) room-id new-state)]
            (if (:success save-result)
              (do
                (swap! event-store into events)
                {:success true :events events})
              save-result))))
      {:success false :error "Game not found"}))
  
  (get-game-view [this room-id player-id]
    (when-let [game-state ((:get-game database) room-id)]
      (pure/create-player-view game-state player-id))))

(defn new-game-engine
  "Creates a new game engine component"
  []
  (map->GameEngineComponent {}))