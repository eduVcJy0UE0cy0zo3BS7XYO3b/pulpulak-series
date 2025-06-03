(ns pulpulak.game.event-store-pure
  "Pure functional event store implementation"
  (:require [pulpulak.game.events :as events]
            [clojure.spec.alpha :as s]
            [com.stuartsierra.component :as component]))

;; Event store state
(defn empty-event-store
  "Creates an empty event store"
  []
  {:events []
   :snapshots {}
   :version 0})

;; Pure event operations
(defn append-event
  "Pure function to append an event. Returns new event-store"
  [event-store event]
  (-> event-store
      (update :events conj event)
      (update :version inc)))

(defn append-events
  "Pure function to append multiple events. Returns new event-store"
  [event-store events]
  (reduce append-event event-store events))

(defn get-events
  "Pure function to get events from a version"
  [event-store from-version]
  (->> (:events event-store)
       (drop from-version)))

(defn get-events-for-aggregate
  "Pure function to get events for a specific aggregate (room)"
  [event-store aggregate-id from-version]
  (->> (get-events event-store from-version)
       (filter #(= (:aggregate-id %) aggregate-id))))

(defn create-snapshot
  "Pure function to create a snapshot. Returns new event-store"
  [event-store aggregate-id version state]
  (assoc-in event-store [:snapshots aggregate-id] 
            {:version version
             :state state
             :timestamp (System/currentTimeMillis)}))

(defn get-snapshot
  "Pure function to get the latest snapshot for aggregate"
  [event-store aggregate-id]
  (get-in event-store [:snapshots aggregate-id]))

;; Event sourcing reconstruction
(defn rebuild-state
  "Pure function to rebuild state from events"
  [initial-state events]
  (reduce (fn [state event]
            (events/apply-event event state))
          initial-state
          events))

(defn rebuild-from-snapshot
  "Pure function to rebuild state from snapshot + subsequent events"
  [event-store aggregate-id initial-state-fn]
  (if-let [snapshot (get-snapshot event-store aggregate-id)]
    (let [events (get-events-for-aggregate event-store 
                                           aggregate-id 
                                           (:version snapshot))]
      (rebuild-state (:state snapshot) events))
    (let [events (get-events-for-aggregate event-store aggregate-id 0)]
      (rebuild-state (initial-state-fn) events))))

;; Query functions
(defn filter-events
  "Pure function to filter events by predicate"
  [event-store predicate]
  (filter predicate (:events event-store)))

(defn count-events
  "Pure function to count events"
  [event-store]
  (count (:events event-store)))

(defn get-version
  "Pure function to get current version"
  [event-store]
  (:version event-store))

;; Command processing with events
(defn process-command-with-events
  "Pure function that processes a command and returns [new-state events]"
  [current-state command-fn & args]
  (let [result (apply command-fn current-state args)]
    (if (:success result)
      [(:new-state result) (:events result)]
      [current-state []])))

;; Event store component
(defrecord EventStoreComponent [state]
  component/Lifecycle
  
  (start [this]
    (assoc this :state (atom (empty-event-store))))
  
  (stop [this]
    (dissoc this :state)))

(defn update-event-store!
  "Updates event store using a pure function"
  [component update-fn & args]
  (apply swap! (:state component) update-fn args))

(defn query-event-store
  "Queries event store using a pure function"
  [component query-fn & args]
  (apply query-fn @(:state component) args))

(defn new-event-store
  "Creates a new event store component"
  []
  (map->EventStoreComponent {}))