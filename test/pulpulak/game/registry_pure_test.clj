(ns pulpulak.game.registry-pure-test
  (:require [clojure.test :refer :all]
            [pulpulak.game.registry-pure :as registry]))

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

(deftest test-pure-game-registry
  (testing "Pure functional game registration and retrieval"
    (let [test-config (TestGameConfig.)
          empty-registry (registry/empty-registry)]
      
      ;; Register game (pure function)
      (let [new-registry (registry/register-game empty-registry test-config)]
        (is (registry/game-exists? new-registry :test-game))
        
        ;; Retrieve game
        (let [retrieved (registry/get-game new-registry :test-game)]
          (is (instance? TestGameConfig retrieved))
          (is (= :test-game (registry/get-game-id retrieved))))
        
        ;; List games
        (let [games (registry/list-games new-registry)]
          (is (= 1 (count games)))
          (is (= {:id :test-game
                  :name "Test Game"
                  :description "A test game"}
                 (first games))))
        
        ;; Unregister game (pure function)
        (let [final-registry (registry/unregister-game new-registry :test-game)]
          (is (false? (registry/game-exists? final-registry :test-game)))
          ;; Original registry unchanged
          (is (registry/game-exists? new-registry :test-game))))))
  
  (testing "Registry immutability"
    (let [test-config (TestGameConfig.)
          registry1 (registry/empty-registry)
          registry2 (registry/register-game registry1 test-config)]
      
      ;; Original registry unchanged
      (is (false? (registry/game-exists? registry1 :test-game)))
      (is (true? (registry/game-exists? registry2 :test-game))))))