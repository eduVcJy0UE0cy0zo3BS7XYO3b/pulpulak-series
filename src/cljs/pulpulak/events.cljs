(ns pulpulak.events
  (:require [re-frame.core :as rf]
            [pulpulak.websocket :as ws]))

;; Initialize DB
(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:screen :main-menu
    :loading false
    :error nil
    :room-id nil
    :username nil
    :role nil
    :game-state nil}))

;; Screen navigation
(rf/reg-event-db
 ::set-screen
 (fn [db [_ screen]]
   (assoc db :screen screen)))

;; WebSocket events
(rf/reg-event-db
 ::websocket-connected
 (fn [db _]
   (assoc db :connected true)))

;; Room creation
(rf/reg-event-fx
 ::create-room
 (fn [{:keys [db]} [_ username]]
   {:db (assoc db :loading true :username username)
    :dispatch [::send-create-room username]}))

(rf/reg-event-fx
 ::send-create-room
 (fn [{:keys [db]} [_ username]]
   (ws/send-with-callback!
    :game/create-room
    {:username username}
    (fn [response]
      (if (:success response)
        (rf/dispatch [::room-created response])
        (rf/dispatch [::error (:error response)]))))
   {}))

(rf/reg-event-db
 ::room-created
 (fn [db [_ {:keys [room-id role]}]]
   (-> db
       (assoc :loading false
              :room-id room-id
              :role role
              :screen :lobby))))

;; Room joining
(rf/reg-event-fx
 ::join-room
 (fn [{:keys [db]} [_ room-id username]]
   {:db (assoc db :loading true :username username)
    :dispatch [::send-join-room room-id username]}))

(rf/reg-event-fx
 ::send-join-room
 (fn [_ [_ room-id username]]
   (ws/send-with-callback!
    :game/join-room
    {:room-id room-id :username username}
    (fn [response]
      (if (:success response)
        (rf/dispatch [::room-joined response])
        (rf/dispatch [::error (:error response)]))))
   {}))

(rf/reg-event-db
 ::room-joined
 (fn [db [_ {:keys [room-id role game-state]}]]
   (-> db
       (assoc :loading false
              :room-id room-id
              :role role
              :game-state game-state
              :screen :lobby))))

;; Player ready
(rf/reg-event-fx
 ::player-ready
 (fn [{:keys [db]} _]
   (ws/send! :game/player-ready {:room-id (:room-id db)})
   {:db (assoc-in db [:game-state :player :ready] true)}))

;; Game events
(rf/reg-event-db
 ::player-joined
 (fn [db [_ player-data]]
   (assoc-in db [:game-state :other-player] player-data)))

(rf/reg-event-db
 ::game-started
 (fn [db [_ {:keys [game-state]}]]
   (-> db
       (assoc :game-state game-state
              :screen :game))))

(rf/reg-event-db
 ::game-state-update
 (fn [db [_ {:keys [game-state]}]]
   (assoc db :game-state game-state)))

;; Game actions
(rf/reg-event-fx
 ::make-choice
 (fn [{:keys [db]} [_ choice-id]]
   (ws/send! :game/make-choice
             {:room-id (:room-id db)
              :choice-id choice-id})
   {}))

(rf/reg-event-fx
 ::request-outfit-swap
 (fn [{:keys [db]} _]
   (ws/send! :game/request-outfit-swap
             {:room-id (:room-id db)})
   {}))

(rf/reg-event-fx
 ::move-to-location
 (fn [{:keys [db]} [_ location-id]]
   (ws/send! :game/move-to-location
             {:room-id (:room-id db)
              :location-id location-id})
   {}))

(rf/reg-event-fx
 ::talk-to-npc
 (fn [{:keys [db]} [_ npc-id choice-id]]
   (ws/send! :game/talk-to-npc
             {:room-id (:room-id db)
              :npc-id npc-id
              :choice-id choice-id})
   {}))

(rf/reg-event-fx
 ::use-item
 (fn [{:keys [db]} [_ item-id target]]
   (ws/send! :game/use-item
             {:room-id (:room-id db)
              :item-id item-id
              :target target})
   {}))

(rf/reg-event-db
 ::outfit-swapped
 (fn [db [_ {:keys [new-outfit game-state]}]]
   (assoc db :game-state game-state)))

;; Error handling
(rf/reg-event-db
 ::error
 (fn [db [_ error]]
   (assoc db :loading false :error error)))

(rf/reg-event-db
 ::clear-error
 (fn [db _]
   (assoc db :error nil)))

(rf/reg-event-db
 ::player-disconnected
 (fn [db [_ {:keys [player-id]}]]
   (-> db
       (assoc :error "Other player disconnected")
       (assoc :screen :main-menu))))