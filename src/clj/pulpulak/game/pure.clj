(ns pulpulak.game.pure
  "Pure functions for game logic without side effects"
  (:require [pulpulak.game.data.locations :as locations]
            [pulpulak.game.data.npcs :as npcs]
            [pulpulak.game.data.quests :as quests]))

(defn create-initial-state
  "Creates the initial game state"
  [room-id game-config players]
  {:room-id room-id
   :game-id (:id game-config)
   :phase :waiting
   :players players
   :shared-state {:current-location "castle"
                  :time-of-day "morning"
                  :discovered-locations #{}
                  :inventory #{}}
   :princess-state {:loyalty {}
                    :relationships {}
                    :secrets #{}}
   :helper-state {:trust-level 50
                  :discovered-info #{}}
   :quests {}
   :npc-states {}})

(defn can-move?
  "Pure function to check if movement is valid"
  [game-state player target-location]
  (let [current-location (:location player)
        connections (locations/get-connections current-location)
        discovered? (or (not (locations/is-hidden? target-location))
                        (contains? (get-in game-state [:shared-state :discovered-locations])
                                   target-location))]
    (and (some #{target-location} connections)
         discovered?)))

(defn can-swap-outfit?
  "Pure function to check if outfit swap is allowed"
  [game-state player]
  (let [location (locations/get-location (:location player))
        other-players (remove #(= (:player-id %) (:player-id player))
                              (vals (:players game-state)))]
    (and (= :princess (:role player))
         (:can-change-outfit location)
         (not-any? #(= (:location %) (:location player)) other-players))))

(defn can-talk-to-npc?
  "Checks if player can talk to specific NPC"
  [game-state player npc-id]
  (let [npcs-at-location (npcs/get-npcs-at-location (:location player))]
    (contains? (set (keys npcs-at-location)) npc-id)))

(defn has-item?
  "Checks if player has an item"
  [game-state player item-id]
  (contains? (get-in game-state [:player-states (:player-id player) :inventory] #{})
             item-id))

(defn can-perform-action?
  "Checks if a player can perform an action"
  [game-state player-id action-type action-data]
  (let [player (get-in game-state [:players player-id])]
    (case action-type
      :move (can-move? game-state player (:location action-data))
      :swap-outfit (can-swap-outfit? game-state player)
      :talk-to-npc (can-talk-to-npc? game-state player (:npc-id action-data))
      :use-item (has-item? game-state player (:item-id action-data))
      false)))

(defn apply-move
  "Applies movement to game state"
  [game-state player-id target-location]
  (assoc-in game-state [:players player-id :location] target-location))

(defn apply-outfit-swap
  "Applies outfit change to game state"
  [game-state player-id]
  (let [current-outfit (get-in game-state [:players player-id :outfit])
        new-outfit (if (= current-outfit :princess) :servant :princess)]
    (assoc-in game-state [:players player-id :outfit] new-outfit)))

(defn apply-npc-interaction
  "Applies the results of NPC interaction"
  [game-state player-id npc-id choice-id]
  (let [player (get-in game-state [:players player-id])
        outfit-type (if (= (:outfit player) :princess) :noble :commoner)
        dialogue (npcs/get-dialogue npc-id outfit-type :initial)
        choice (first (filter #(= (:id %) choice-id) (:choices dialogue)))]
    (cond-> game-state
      (:unlocks choice)
      (update-in [:player-states player-id :flags] (fnil conj #{}) (:unlocks choice))
      
      (and (= :princess (:role player)) (npcs/likes-noble? npc-id))
      (update-in [:princess-state :loyalty npc-id] (fnil + 0) 5))))

(defn get-available-actions
  "Gets all actions available to a player in current state"
  [game-state player]
  (cond-> []
    (can-swap-outfit? game-state player)
    (conj {:type :swap-outfit :label "Change outfit"})

    (seq (npcs/get-npcs-at-location (:location player)))
    (conj {:type :talk-to-npc :label "Talk to someone"})

    true
    (conj {:type :move :label "Go somewhere else"})))

(defn get-player-quests
  "Gets quest information for a player"
  [game-state player-id]
  (let [player-quests (get-in game-state [:quests player-id] {})]
    {:active (filter #(= :active (:status (val %))) player-quests)
     :completed (filter #(= :completed (:status (val %))) player-quests)}))

(defn create-player-view
  "Creates a view of the game state for a specific player"
  [game-state player-id]
  (let [player (get-in game-state [:players player-id])
        location (locations/get-location (:location player))
        other-player (first (remove #(= player-id (key %)) (:players game-state)))]
    {:room-id (:room-id game-state)
     :phase (:phase game-state)
     :player (select-keys player [:username :role :outfit :location])
     :other-player (when other-player
                     (select-keys (val other-player) [:username :role :location]))
     :location {:name (:name location)
                :description (:description location)
                :connections (filter #(can-move? game-state player %) 
                                     (:connections location))}
     :available-actions (get-available-actions game-state player)
     :quests (get-player-quests game-state player-id)}))
