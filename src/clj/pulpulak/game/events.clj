(ns pulpulak.game.events
  "Event definitions for game state changes")

(defprotocol GameEvent
  "Protocol for game events"
  (event-type [this])
  (apply-event [this game-state]))

(defrecord GameCreated [room-id players]
  GameEvent
  (event-type [_] :game-created)
  (apply-event [_ game-state] game-state))

(defrecord PlayerMoved [player-id location-id]
  GameEvent
  (event-type [_] :player-moved)
  (apply-event [_ game-state]
    (assoc-in game-state [:players player-id :location] location-id)))

(defrecord OutfitSwapped [player-id new-outfit]
  GameEvent
  (event-type [_] :outfit-swapped)
  (apply-event [_ game-state]
    (assoc-in game-state [:players player-id :outfit] new-outfit)))

(defrecord NPCInteraction [player-id npc-id choice-id]
  GameEvent
  (event-type [_] :npc-interaction)
  (apply-event [_ game-state]
    ;; This would be handled by the pure function
    game-state))

(defrecord ItemUsed [player-id item-id target]
  GameEvent
  (event-type [_] :item-used)
  (apply-event [_ game-state]
    (update-in game-state [:player-states player-id :inventory] disj item-id)))

(defrecord QuestStarted [player-id quest-id]
  GameEvent
  (event-type [_] :quest-started)
  (apply-event [_ game-state]
    (assoc-in game-state [:quests player-id quest-id :status] :active)))

(defrecord QuestCompleted [player-id quest-id rewards]
  GameEvent
  (event-type [_] :quest-completed)
  (apply-event [_ game-state]
    (-> game-state
        (assoc-in [:quests player-id quest-id :status] :completed)
        (update-in [:player-states player-id :completed-quests] conj quest-id))))

(defn replay-events
  "Replays a sequence of events to reconstruct game state"
  [initial-state events]
  (reduce (fn [state event]
            (apply-event event state))
          initial-state
          events))