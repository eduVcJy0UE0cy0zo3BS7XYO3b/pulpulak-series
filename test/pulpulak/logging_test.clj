(ns pulpulak.logging-test
  (:require [clojure.test :refer :all]
            [pulpulak.utils.logging-pure :as logging]))

(deftest test-log-event-creation
  (testing "Log event creation"
    (let [event (logging/log-info "test message" {:key "value"})]
      (is (= :info (:level event)))
      (is (= "test message" (:message event)))
      (is (= {:key "value"} (:data event)))
      (is (number? (:timestamp event))))))

(deftest test-different-log-levels
  (testing "Different log levels"
    (let [info-event (logging/log-info "info")
          debug-event (logging/log-debug "debug")
          warn-event (logging/log-warn "warn")
          error-event (logging/log-error "error")]
      (is (= :info (:level info-event)))
      (is (= :debug (:level debug-event)))
      (is (= :warn (:level warn-event)))
      (is (= :error (:level error-event))))))

(deftest test-pure-logging-monad
  (testing "Pure logging monad-like operations"
    (let [result (logging/log-return 42)]
      (is (= 42 (:value result)))
      (is (empty? (:logs result))))
    
    (let [logged-add (logging/log-and-lift + "Adding numbers")
          result (logged-add 2 3)]
      (is (= 5 (:value result)))
      (is (= 1 (count (:logs result)))))))

(deftest test-logging-composition
  (testing "Logging with function composition"
    (let [add-with-log (logging/with-logging + (logging/log-operation "addition"))
          result (add-with-log 2 3)]
      (is (= 5 (:result result)))
      (is (= 1 (count (:log-events result)))))))

(deftest test-process-log-events-structure
  (testing "Process log events doesn't throw errors"
    (let [events [(logging/log-info "test 1")
                  (logging/log-debug "test 2")
                  (logging/log-warn "test 3")
                  (logging/log-error "test 4")]]
      ;; This should not throw an exception
      (is (nil? (logging/process-log-events events))))))