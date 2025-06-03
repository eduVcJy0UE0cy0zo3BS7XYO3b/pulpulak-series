(ns pulpulak.components.database
  (:require [com.stuartsierra.component :as component]
            [clojure.spec.alpha :as s]
            [taoensso.timbre :as log]))

;; Specs for data validation
(s/def ::room-id string?)
(s/def ::player-id string?)
(s/def ::username string?)
(s/def ::role #{:princess :helper})
(s/def ::outfit #{:princess :servant})
(s/def ::location keyword?)

(s/def ::player
  (s/keys :req-un [::player-id ::username ::role ::outfit ::location]
          :opt-un [::ready ::inventory]))

(s/def ::players (s/map-of ::player-id ::player))

(s/def ::game-state
  (s/keys :req-un [::room-id ::players ::phase]
          :opt-un [::scene-id ::shared-state ::princess-state ::helper-state]))

(defprotocol GameDatabase
  "Protocol for game state storage"
  (get-game [this room-id]
    "Retrieves a game by room ID")
  (save-game! [this room-id game-state]
    "Saves or updates a game state")
  (delete-game! [this room-id]
    "Deletes a game")
  (list-games [this]
    "Lists all active games")
  (get-player-room [this player-id]
    "Gets the room ID for a player")
  (set-player-room! [this player-id room-id]
    "Associates a player with a room")
  (remove-player-room! [this player-id]
    "Removes player-room association"))

(defrecord InMemoryDatabase [games rooms player-rooms]
  component/Lifecycle
  
  (start [this]
    (log/info "Starting in-memory database")
    (assoc this
           :games (atom {})
           :rooms (atom {})
           :player-rooms (atom {})))
  
  (stop [this]
    (log/info "Stopping in-memory database")
    (dissoc this :games :rooms :player-rooms))
  
  GameDatabase
  
  (get-game [this room-id]
    (get @(:games this) room-id))
  
  (save-game! [this room-id game-state]
    (if (s/valid? ::game-state game-state)
      (do
        (swap! (:games this) assoc room-id game-state)
        {:success true})
      {:success false
       :error (s/explain-str ::game-state game-state)}))
  
  (delete-game! [this room-id]
    (swap! (:games this) dissoc room-id)
    (swap! (:rooms this) dissoc room-id))
  
  (list-games [this]
    (vals @(:games this)))
  
  (get-player-room [this player-id]
    (get @(:player-rooms this) player-id))
  
  (set-player-room! [this player-id room-id]
    (swap! (:player-rooms this) assoc player-id room-id))
  
  (remove-player-room! [this player-id]
    (swap! (:player-rooms this) dissoc player-id)))

(defn new-database
  "Creates a new database component"
  [config]
  (map->InMemoryDatabase config))