(ns pulpulak.components.websocket-pure
  "Pure functional WebSocket handling"
  (:require [com.stuartsierra.component :as component]
            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]
            [pulpulak.utils.logging-pure :as logging]
            [pulpulak.game.commands :as commands]
            [pulpulak.game.events :as events]))

;; Pure message handling functions
(defn handle-ping
  "Pure function to handle ping messages"
  [message]
  {:type :ping-response
   :logs [(logging/log-debug "Received ping")]})

(defn handle-connection
  "Pure function to handle client connections"
  [uid]
  {:type :connection-established
   :uid uid
   :logs [(logging/log-info "Client connected" {:uid uid})]})

(defn handle-disconnection
  "Pure function to handle client disconnections"
  [uid player-room-mapping]
  (if-let [room-id (get player-room-mapping uid)]
    {:type :player-disconnected
     :uid uid
     :room-id room-id
     :cleanup-required true
     :logs [(logging/log-info "Player disconnected from room" 
                              {:uid uid :room-id room-id})]}
    {:type :client-disconnected
     :uid uid
     :cleanup-required false
     :logs [(logging/log-info "Client disconnected" {:uid uid})]}))

(defn handle-game-command
  "Pure function to handle game commands"
  [message game-state-fn process-command-fn]
  (let [{:keys [uid ?data]} message
        {:keys [room-id command data]} ?data]
    (if-let [game-state (game-state-fn room-id)]
      (let [result (process-command-fn room-id uid command data)]
        (if (:success result)
          {:type :command-processed
           :result result
           :room-id room-id
           :events (:events result)
           :logs [(logging/log-info "Command processed successfully" 
                                   {:uid uid :command command :room-id room-id})]}
          {:type :command-failed
           :result result
           :logs [(logging/log-warn "Command failed" 
                                   {:uid uid :command command :error (:error result)})]}))
      {:type :game-not-found
       :room-id room-id
       :logs [(logging/log-error "Game not found" {:room-id room-id :uid uid})]})))

;; Pure message routing
(defn route-message
  "Pure function to route messages to appropriate handlers"
  [message game-state-fn process-command-fn player-room-mapping]
  (case (:id message)
    :chsk/ws-ping 
    (handle-ping message)
    
    :chsk/uidport-open 
    (handle-connection (:uid message))
    
    :chsk/uidport-close 
    (handle-disconnection (:uid message) player-room-mapping)
    
    :game/command 
    (handle-game-command message game-state-fn process-command-fn)
    
    ;; Default case
    {:type :unhandled
     :message-id (:id message)
     :logs [(logging/log-debug "Unhandled message" {:id (:id message)})]}))

;; Pure broadcast functions
(defn create-broadcast-event
  "Pure function to create a broadcast event"
  [event room-id player-ids]
  {:type :broadcast
   :room-id room-id
   :player-ids player-ids
   :event-type (events/event-type event)
   :event-data event})

(defn calculate-room-recipients
  "Pure function to calculate who should receive a room broadcast"
  [room-id game-state]
  (->> (:players game-state)
       (map first)
       vec))

;; WebSocket server with pure core
(defrecord PureWebSocketServer [config game-engine socket router player-rooms]
  component/Lifecycle
  
  (start [this]
    (let [logs [(logging/log-info "Starting WebSocket server")]
          socket (sente/make-channel-socket-server!
                  (get-sch-adapter)
                  {:user-id-fn (fn [ring-req]
                                 (get-in ring-req [:params :client-id]))})
          router (sente/start-server-chsk-router!
                  (:ch-recv socket)
                  (fn [msg] (process-message-with-effects this msg)))]
      ;; Process logs at startup
      (logging/process-log-events logs)
      (assoc this
             :socket socket
             :router router
             :player-rooms (atom {}))))
  
  (stop [this]
    (let [logs [(logging/log-info "Stopping WebSocket server")]]
      (when router (router))
      (logging/process-log-events logs)
      (dissoc this :socket :router :player-rooms))))

(defn process-message-with-effects
  "Processes a message using pure functions and applies side effects"
  [websocket message]
  (let [player-room-mapping @(:player-rooms websocket)
        game-state-fn (fn [room-id] 
                        ((:get-game-view (:game-engine websocket)) room-id nil))
        process-command-fn (:process-command (:game-engine websocket))
        
        result (route-message message game-state-fn process-command-fn player-room-mapping)]
    
    ;; Process logs first
    (logging/process-log-events (:logs result))
    
    ;; Apply side effects based on result type
    (case (:type result)
      :player-disconnected
      (when (:cleanup-required result)
        (swap! (:player-rooms websocket) dissoc (:uid result)))
      
      :command-processed
      (let [send-fn (get-in websocket [:socket :send-fn])
            room-id (:room-id result)
            game-state (game-state-fn room-id)]
        ;; Send reply if requested
        (when (:?reply-fn message)
          ((:?reply-fn message) (:result result)))
        ;; Broadcast events to room
        (when-let [events (:events result)]
          (let [recipients (calculate-room-recipients room-id game-state)]
            (doseq [event events
                    player-id recipients]
              (send-fn player-id [:game/event {:event-type (events/event-type event)
                                               :data event}])))))
      
      :command-failed
      (when (:?reply-fn message)
        ((:?reply-fn message) (:result result)))
      
      ;; Other result types don't require additional side effects
      nil)))

(defn broadcast-event-pure
  "Pure function to calculate broadcast parameters"
  [room-id event game-state]
  (let [recipients (calculate-room-recipients room-id game-state)]
    (create-broadcast-event event room-id recipients)))

(defn execute-broadcast
  "Executes a broadcast using WebSocket send function"
  [websocket broadcast-spec]
  (let [send-fn (get-in websocket [:socket :send-fn])]
    (doseq [player-id (:player-ids broadcast-spec)]
      (send-fn player-id [:game/event {:event-type (:event-type broadcast-spec)
                                       :data (:event-data broadcast-spec)}]))))

(defn new-pure-websocket-server
  "Creates a new pure WebSocket server component"
  [config]
  (map->PureWebSocketServer {:config config}))