(ns pulpulak.components.websocket
  (:require [com.stuartsierra.component :as component]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [taoensso.timbre :as log]
            [pulpulak.game.commands :as commands]))

(defmulti handle-message :id)

(defmethod handle-message :default
  [{:keys [id]}]
  (log/debug "Unhandled message:" id))

(defmethod handle-message :chsk/ws-ping [_])

(defmethod handle-message :chsk/uidport-open
  [{:keys [uid]}]
  (log/info "Client connected:" uid))

(defmethod handle-message :chsk/uidport-close
  [{:keys [uid websocket]}]
  (log/info "Client disconnected:" uid)
  ;; Clean up player state
  (when-let [room-id ((:get-player-room (:database (:game-engine websocket))) uid)]
    ((:remove-player-room! (:database (:game-engine websocket))) uid)))

(defmethod handle-message :game/command
  [{:keys [uid ?data ?reply-fn websocket]}]
  (let [{:keys [room-id command data]} ?data
        game-engine (:game-engine websocket)
        result ((:process-command game-engine) room-id uid command data)]
    (when ?reply-fn
      (?reply-fn result))
    ;; Broadcast events to room
    (when (:success result)
      (doseq [event (:events result)]
        (broadcast-event websocket room-id event)))))

(defrecord WebSocketServer [config game-engine socket router]
  component/Lifecycle
  
  (start [this]
    (log/info "Starting WebSocket server")
    (let [socket (sente/make-channel-socket-server!
                  (get-sch-adapter)
                  {:user-id-fn (fn [ring-req]
                                 (get-in ring-req [:params :client-id]))})
          router (sente/start-server-chsk-router!
                  (:ch-recv socket)
                  (fn [msg] (handle-message (assoc msg :websocket this))))]
      (assoc this
             :socket socket
             :router router)))
  
  (stop [this]
    (log/info "Stopping WebSocket server")
    (when router (router))
    (dissoc this :socket :router)))

(defn broadcast-event
  "Broadcasts an event to all players in a room"
  [websocket room-id event]
  (let [database (:database (:game-engine websocket))
        game ((:get-game database) room-id)
        send-fn (get-in websocket [:socket :send-fn])]
    (doseq [[player-id _] (:players game)]
      (send-fn player-id [:game/event {:event-type (pulpulak.game.events/event-type event)
                                        :data event}]))))

(defn new-websocket-server
  "Creates a new WebSocket server component"
  [config]
  (map->WebSocketServer {:config config}))