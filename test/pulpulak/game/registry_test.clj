(ns pulpulak.game.registry-test
  (:require [clojure.test :refer :all]
            [pulpulak.game.registry :as registry]))

(deftype TestGameConfig []
  registry/GameConfig
  (get-game-id [_] :test-game)
  (get-game-name [_] "Test Game")
  (get-game-description [_] "A test game")
  (get-story-data [_] {})
  (get-location-data [_] {})
  (get-npc-data [_] {})
  (get-quest-data [_] {})
  (get-game-constants [_] {})
  (initialize-game [this] this))

(deftest test-game-registry
  (testing "Game registration and retrieval"
    ;; Clear registry first
    (reset! registry/games {})
    
    (let [test-config (TestGameConfig.)]
      ;; Register game
      (is (= :test-game (registry/register-game! test-config)))
      (is (true? (registry/game-exists? :test-game)))
      
      ;; Retrieve game
      (let [retrieved (registry/get-game :test-game)]
        (is (instance? TestGameConfig retrieved))
        (is (= :test-game (registry/get-game-id retrieved))))
      
      ;; List games
      (let [games (registry/list-games)]
        (is (= 1 (count games)))
        (is (= {:id :test-game
                :name "Test Game"
                :description "A test game"}
               (first games))))
      
      ;; Unregister game
      (registry/unregister-game! :test-game)
      (is (false? (registry/game-exists? :test-game))))))