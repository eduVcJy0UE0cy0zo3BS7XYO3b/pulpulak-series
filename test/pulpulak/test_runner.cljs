(ns pulpulak.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [pulpulak.frontend-test]))

(enable-console-print!)

(defn -main []
  (run-tests 'pulpulak.frontend-test))

(set! *main-cli-fn* -main)