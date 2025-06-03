(ns pulpulak.game.commands
  "Command handlers for game actions"
  (:require [pulpulak.game.pure :as pure]
            [pulpulak.game.events :as events]))

(defmulti handle-command
  "Multimethod for handling game commands"
  (fn [command-type _ _ _] command-type))

(defmethod handle-command :move
  [_ game-state player-id {:keys [location-id]}]
  (if (pure/can-perform-action? game-state player-id :move {:location location-id})
    {:new-state (pure/apply-move game-state player-id location-id)
     :events [(events/->PlayerMoved player-id location-id)]}
    {:error "Cannot move to that location"}))

(defmethod handle-command :swap-outfit
  [_ game-state player-id _]
  (if (pure/can-perform-action? game-state player-id :swap-outfit nil)
    (let [new-state (pure/apply-outfit-swap game-state player-id)
          new-outfit (get-in new-state [:players player-id :outfit])]
      {:new-state new-state
       :events [(events/->OutfitSwapped player-id new-outfit)]})
    {:error "Cannot change outfit here"}))

(defmethod handle-command :talk-to-npc
  [_ game-state player-id {:keys [npc-id choice-id]}]
  (if (pure/can-perform-action? game-state player-id :talk-to-npc {:npc-id npc-id})
    {:new-state (pure/apply-npc-interaction game-state player-id npc-id choice-id)
     :events [(events/->NPCInteraction player-id npc-id choice-id)]}
    {:error "Cannot talk to that NPC"}))

(defmethod handle-command :use-item
  [_ game-state player-id {:keys [item-id target]}]
  (if (pure/can-perform-action? game-state player-id :use-item {:item-id item-id})
    {:new-state (update-in game-state [:player-states player-id :inventory] disj item-id)
     :events [(events/->ItemUsed player-id item-id target)]}
    {:error "You don't have that item"}))

(defmethod handle-command :default
  [command-type _ _ _]
  {:error (str "Unknown command: " command-type)})

(defn get-command-handler
  "Returns a command handler function"
  [command-type]
  (fn [game-state player-id data]
    (handle-command command-type game-state player-id data)))