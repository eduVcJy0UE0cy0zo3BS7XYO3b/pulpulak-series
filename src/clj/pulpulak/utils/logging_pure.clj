(ns pulpulak.utils.logging-pure
  "Pure functional logging that returns log events instead of side effects"
  (:require [taoensso.timbre :as timbre]))

;; Log event data structure
(defrecord LogEvent [level message data timestamp])

(defn create-log-event
  "Pure function to create a log event"
  [level message data]
  (->LogEvent level message data (System/currentTimeMillis)))

;; Pure logging functions that return log events
(defn log-info
  "Pure function that returns an info log event"
  ([message] (create-log-event :info message nil))
  ([message data] (create-log-event :info message data)))

(defn log-debug
  "Pure function that returns a debug log event"
  ([message] (create-log-event :debug message nil))
  ([message data] (create-log-event :debug message data)))

(defn log-warn
  "Pure function that returns a warning log event"
  ([message] (create-log-event :warn message nil))
  ([message data] (create-log-event :warn message data)))

(defn log-error
  "Pure function that returns an error log event"
  ([message] (create-log-event :error message nil))
  ([message data] (create-log-event :error message data)))

;; Functional composition with logging
(defn with-logging
  "Higher-order function that adds logging to a pure function"
  [f log-fn]
  (fn [& args]
    (let [start-time (System/currentTimeMillis)
          result (apply f args)
          end-time (System/currentTimeMillis)
          duration (- end-time start-time)
          log-event (log-fn result duration)]
      {:result result
       :log-events [log-event]})))

(defn log-operation
  "Returns a function that logs an operation"
  [operation-name]
  (fn [result duration]
    (log-info (str "Completed " operation-name) 
              {:duration-ms duration :success (not (contains? result :error))})))

;; Log event processing
(defn process-log-events
  "Processes log events by actually logging them (side effect boundary)"
  [log-events]
  (doseq [event log-events]
    (case (:level event)
      :info (timbre/info (:message event) (:data event))
      :debug (timbre/debug (:message event) (:data event))
      :warn (timbre/warn (:message event) (:data event))
      :error (timbre/error (:message event) (:data event)))))

;; Functional logging monad-like structure
(defn log-return
  "Returns a value with empty log"
  [value]
  {:value value :logs []})

(defn log-bind
  "Monadic bind for logged computations"
  [logged-value f]
  (let [result (f (:value logged-value))]
    {:value (:value result)
     :logs (concat (:logs logged-value) (:logs result))}))

(defn log-lift
  "Lifts a pure function into logged computation"
  [f]
  (fn [& args]
    {:value (apply f args) :logs []}))

(defn log-and-lift
  "Lifts a function and adds logging"
  [f log-message]
  (fn [& args]
    (let [result (apply f args)]
      {:value result 
       :logs [(log-info log-message {:args args :result result})]})))

;; Macros for cleaner syntax
(defmacro with-pure-logging
  "Macro to wrap code with pure logging"
  [log-message & body]
  `(let [start# (System/currentTimeMillis)
         result# (do ~@body)
         end# (System/currentTimeMillis)]
     {:value result#
      :logs [(log-info ~log-message {:duration-ms (- end# start#)})]}))

(defmacro log-do
  "Monadic do notation for logged computations"
  [& bindings-and-body]
  (let [bindings (partition 2 (butlast bindings-and-body))
        body (last bindings-and-body)]
    (reduce (fn [acc [binding expr]]
              `(log-bind ~expr (fn [~binding] ~acc)))
            `(log-return ~body)
            (reverse bindings))))