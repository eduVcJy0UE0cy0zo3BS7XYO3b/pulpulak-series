(ns pulpulak.websocket
  (:require [taoensso.sente :as sente]
            [re-frame.core :as rf]
            [pulpulak.events :as events]))

(defonce socket (atom nil))
(defonce router (atom nil))

(defmulti handle-message :id)

(defmethod handle-message :default
  [{:keys [id]}]
  (println "Unhandled message:" id))

(defmethod handle-message :chsk/state
  [{:keys [?data]}]
  (let [[old-state new-state] ?data]
    (when (:open? new-state)
      (rf/dispatch [::events/websocket-connected]))))

(defmethod handle-message :chsk/recv
  [{:keys [?data]}]
  (let [[event-id event-data] ?data]
    (case event-id
      :game/player-joined (rf/dispatch [::events/player-joined event-data])
      :game/started (rf/dispatch [::events/game-started event-data])
      :game/state-update (rf/dispatch [::events/game-state-update event-data])
      :game/outfit-swapped (rf/dispatch [::events/outfit-swapped event-data])
      :game/player-disconnected (rf/dispatch [::events/player-disconnected event-data])
      (println "Unknown event:" event-id))))

(defn init! []
  (let [{:keys [chsk ch-recv send-fn state] :as socket-client}
        (sente/make-channel-socket-client!
         "/chsk"
         {:type :auto
          :packer :edn
          :client-id (str (random-uuid))})]
    
    (reset! socket socket-client)
    
    ;; Start router
    (reset! router
            (sente/start-client-chsk-router! ch-recv handle-message))))

(defn send! [event-id data]
  (when-let [send-fn (:send-fn @socket)]
    (send-fn [event-id data])))

(defn send-with-callback! [event-id data callback-fn]
  (when-let [send-fn (:send-fn @socket)]
    (send-fn [event-id data] 5000 callback-fn)))