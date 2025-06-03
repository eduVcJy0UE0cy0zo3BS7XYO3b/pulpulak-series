(ns pulpulak.frontend-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [re-frame.core :as rf]
            [pulpulak.events :as events]
            [pulpulak.subs :as subs]))

(defn reset-db []
  (rf/dispatch-sync [::events/initialize]))

(deftest test-initialization
  (testing "Initial app state"
    (reset-db)
    
    (is (= :main-menu @(rf/subscribe [::subs/screen])))
    (is (false? @(rf/subscribe [::subs/loading])))
    (is (nil? @(rf/subscribe [::subs/error])))
    (is (nil? @(rf/subscribe [::subs/room-id])))
    (is (nil? @(rf/subscribe [::subs/username])))))

(deftest test-screen-navigation
  (testing "Screen navigation events"
    (reset-db)
    
    (rf/dispatch-sync [::events/set-screen :lobby])
    (is (= :lobby @(rf/subscribe [::subs/screen])))
    
    (rf/dispatch-sync [::events/set-screen :game])
    (is (= :game @(rf/subscribe [::subs/screen])))))

(deftest test-room-creation-flow
  (testing "Room creation updates state correctly"
    (reset-db)
    
    ;; Simulate room creation response
    (rf/dispatch-sync [::events/room-created {:room-id "ABC123" :role :princess}])
    
    (is (= "ABC123" @(rf/subscribe [::subs/room-id])))
    (is (= :princess @(rf/subscribe [::subs/role])))
    (is (= :lobby @(rf/subscribe [::subs/screen])))
    (is (false? @(rf/subscribe [::subs/loading])))))

(deftest test-room-joining-flow
  (testing "Room joining updates state correctly"
    (reset-db)
    
    ;; Simulate room join response
    (rf/dispatch-sync [::events/room-joined 
                       {:room-id "XYZ789" 
                        :role :helper
                        :game-state {:player {:username "Bob"}}}])
    
    (is (= "XYZ789" @(rf/subscribe [::subs/room-id])))
    (is (= :helper @(rf/subscribe [::subs/role])))
    (is (= :lobby @(rf/subscribe [::subs/screen])))
    (is (some? @(rf/subscribe [::subs/game-state])))))

(deftest test-game-state-updates
  (testing "Game state updates are handled correctly"
    (reset-db)
    
    (let [test-game-state {:room-id "TEST123"
                           :phase :playing
                           :location {:id :throne-room
                                      :name "Throne Room"
                                      :description "A grand throne room"}
                           :player {:username "Alice"
                                    :role :princess
                                    :outfit :princess
                                    :location :throne-room}
                           :quests {:active []
                                    :objectives []}}]
      
      (rf/dispatch-sync [::events/game-state-update {:game-state test-game-state}])
      
      (let [game-state @(rf/subscribe [::subs/game-state])]
        (is (= "TEST123" (:room-id game-state)))
        (is (= :playing (:phase game-state)))
        (is (= :throne-room (get-in game-state [:location :id])))))))

(deftest test-error-handling
  (testing "Errors are displayed and can be cleared"
    (reset-db)
    
    (rf/dispatch-sync [::events/error "Connection failed"])
    (is (= "Connection failed" @(rf/subscribe [::subs/error])))
    
    (rf/dispatch-sync [::events/clear-error])
    (is (nil? @(rf/subscribe [::subs/error])))))

(deftest test-player-ready-state
  (testing "Player ready state is tracked"
    (reset-db)
    
    ;; Set up initial game state
    (rf/dispatch-sync [::events/room-created {:room-id "ABC123" :role :princess}])
    (rf/dispatch-sync [::events/game-state-update 
                       {:game-state {:player {:ready false}}}])
    
    ;; Mark player as ready
    (rf/dispatch-sync [::events/player-ready])
    
    (let [game-state @(rf/subscribe [::subs/game-state])]
      (is (true? (get-in game-state [:player :ready]))))))

(deftest test-outfit-swap-permissions
  (testing "Can swap outfit calculation"
    (reset-db)
    
    ;; Princess alone in a valid location
    (rf/dispatch-sync [::events/initialize])
    (rf/dispatch-sync [::events/room-created {:room-id "ABC123" :role :princess}])
    (rf/dispatch-sync [::events/game-state-update
                       {:game-state {:player {:role :princess
                                              :location :princess-chamber}
                                     :other-player nil}}])
    
    (is (true? @(rf/subscribe [::subs/can-swap-outfit?])))
    
    ;; Princess with helper in same location
    (rf/dispatch-sync [::events/game-state-update
                       {:game-state {:player {:role :princess
                                              :location :princess-chamber}
                                     :other-player {:location :princess-chamber}}}])
    
    (is (false? @(rf/subscribe [::subs/can-swap-outfit?])))
    
    ;; Helper cannot swap
    (rf/dispatch-sync [::events/room-joined {:room-id "ABC123" :role :helper}])
    (rf/dispatch-sync [::events/game-state-update
                       {:game-state {:player {:role :helper}}}])
    
    (is (false? @(rf/subscribe [::subs/can-swap-outfit?])))))

(deftest test-player-disconnect-handling
  (testing "Player disconnect returns to main menu"
    (reset-db)
    
    ;; Set up game state
    (rf/dispatch-sync [::events/game-started {:game-state {}}])
    (is (= :game @(rf/subscribe [::subs/screen])))
    
    ;; Handle disconnect
    (rf/dispatch-sync [::events/player-disconnected {:player-id "other-player"}])
    
    (is (= :main-menu @(rf/subscribe [::subs/screen])))
    (is (= "Other player disconnected" @(rf/subscribe [::subs/error])))))