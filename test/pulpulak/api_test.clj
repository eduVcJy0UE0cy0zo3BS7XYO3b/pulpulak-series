(ns pulpulak.api-test
  (:require [clojure.test :refer :all]
            [pulpulak.game.registry-pure :as registry]
            [pulpulak.games.pulpulak-config :as pulpulak-config]))

(deftest test-pure-registry-functionality
  (testing "Pure game registry operations"
    (testing "Empty registry"
      (let [registry-state (registry/empty-registry)
            games (registry/list-games registry-state)]
        (is (vector? games))
        (is (empty? games))))
    
    (testing "Register and retrieve game"
      (let [test-config (pulpulak-config/create-pulpulak-config)
            registry-state (registry/register-game (registry/empty-registry) test-config)
            game (registry/get-game registry-state :pulpulak)]
        (is (some? game))
        (is (= "Pulpulak Series" (registry/get-game-name game)))
        (is (= 2 (get-in (registry/get-game-constants game) [:max-players])))))
    
    (testing "Multiple games"
      (let [config1 (pulpulak-config/create-pulpulak-config)
            registry1 (registry/register-game (registry/empty-registry) config1)
            games (registry/list-games registry1)]
        (is (= 1 (count games)))
        (is (= :pulpulak (:id (first games))))))
    
    (testing "Game exists check"
      (let [test-config (pulpulak-config/create-pulpulak-config)
            registry-state (registry/register-game (registry/empty-registry) test-config)]
        (is (registry/game-exists? registry-state :pulpulak))
        (is (not (registry/game-exists? registry-state :nonexistent)))))
    
    (testing "Unregister game"
      (let [test-config (pulpulak-config/create-pulpulak-config)
            registry-with-game (registry/register-game (registry/empty-registry) test-config)
            registry-without-game (registry/unregister-game registry-with-game :pulpulak)]
        (is (registry/game-exists? registry-with-game :pulpulak))
        (is (not (registry/game-exists? registry-without-game :pulpulak)))))))

(deftest test-game-config-protocol
  (testing "Game config protocol implementation"
    (let [config (pulpulak-config/create-pulpulak-config)]
      (is (= :pulpulak (registry/get-game-id config)))
      (is (= "Pulpulak Series" (registry/get-game-name config)))
      (is (string? (registry/get-game-description config)))
      (is (map? (registry/get-story-data config)))
      (is (map? (registry/get-location-data config)))
      (is (map? (registry/get-npc-data config)))
      (is (map? (registry/get-quest-data config)))
      (is (map? (registry/get-game-constants config))))))

(deftest test-registry-immutability
  (testing "Registry operations maintain immutability"
    (let [original-registry (registry/empty-registry)
          test-config (pulpulak-config/create-pulpulak-config)
          new-registry (registry/register-game original-registry test-config)]
      
      ;; Original registry should be unchanged
      (is (empty? (registry/list-games original-registry)))
      (is (not (registry/game-exists? original-registry :pulpulak)))
      
      ;; New registry should have the game
      (is (= 1 (count (registry/list-games new-registry))))
      (is (registry/game-exists? new-registry :pulpulak)))))