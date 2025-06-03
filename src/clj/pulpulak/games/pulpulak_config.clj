(ns pulpulak.games.pulpulak-config
  (:require [pulpulak.game.registry :as registry]
            [pulpulak.game.data.locations :as locations]
            [pulpulak.game.data.npcs :as npcs]
            [pulpulak.game.data.quests :as quests]
            [pulpulak.game.story :as story]))

(deftype PulpulakGameConfig []
  registry/GameConfig
  
  (get-game-id [_]
    :pulpulak)
  
  (get-game-name [_]
    "Pulpulak Series")
  
  (get-game-description [_]
    "A cooperative adventure where a princess and her helper navigate palace intrigue and magical mysteries")
  
  (get-story-data [_]
    story/scenes)
  
  (get-location-data [_]
    locations/locations)
  
  (get-npc-data [_]
    npcs/npcs)
  
  (get-quest-data [_]
    quests/quests)
  
  (get-game-constants [_]
    {:max-players 2
     :roles [:princess :helper]
     :starting-location {:princess :princess-chamber
                         :helper :servants-quarters}
     :starting-outfit {:princess :princess
                       :helper :servant}
     :max-loyalty 100
     :initial-trust 50})
  
  (initialize-game [this]
    ;; Any async initialization if needed
    this))

(defn create-config []
  (PulpulakGameConfig.))

;; Auto-register on namespace load
(def config (create-config))
(registry/register-game! config)