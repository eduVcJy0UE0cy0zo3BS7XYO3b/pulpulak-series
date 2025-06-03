(ns pulpulak.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 ::screen
 (fn [db]
   (:screen db)))

(rf/reg-sub
 ::loading
 (fn [db]
   (:loading db)))

(rf/reg-sub
 ::error
 (fn [db]
   (:error db)))

(rf/reg-sub
 ::room-id
 (fn [db]
   (:room-id db)))

(rf/reg-sub
 ::username
 (fn [db]
   (:username db)))

(rf/reg-sub
 ::role
 (fn [db]
   (:role db)))

(rf/reg-sub
 ::game-state
 (fn [db]
   (:game-state db)))

(rf/reg-sub
 ::current-scene
 (fn [db]
   (get-in db [:game-state :current-scene])))

(rf/reg-sub
 ::player
 (fn [db]
   (get-in db [:game-state :player])))

(rf/reg-sub
 ::other-player
 (fn [db]
   (get-in db [:game-state :other-player])))

(rf/reg-sub
 ::is-princess?
 (fn [db]
   (= :princess (:role db))))

(rf/reg-sub
 ::can-swap-outfit?
 (fn [db]
   (let [player (get-in db [:game-state :player])
         other-player (get-in db [:game-state :other-player])]
     (and (= :princess (:role player))
          (or (nil? other-player)
              (not= (:location player) (:location other-player)))))))