(ns user
  (:require [pulpulak.server :as server]))

(defn start []
  (server/-main))

(defn stop []
  (server/stop))

(defn restart []
  (stop)
  (start))