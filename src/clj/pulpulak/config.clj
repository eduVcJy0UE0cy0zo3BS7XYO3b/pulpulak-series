(ns pulpulak.config
  (:require [cprop.core :refer [load-config]]
            [cprop.source :as source]
            [clojure.java.io :as io]))

(defn load-config-from-edn []
  (let [env-name (keyword (or (System/getenv "ENV") "development"))
        base-config (source/from-resource "config.edn")]
    (merge (:default base-config)
           (get base-config env-name))))

(defn load-configuration []
  (merge (load-config-from-edn)
         (load-config)))

(defn server-port []
  (let [config (load-configuration)]
    (get-in config [:server :port] 3000)))