(ns pulpulak.config-pure
  "Pure functional configuration management"
  (:require [cprop.core :refer [load-config]]
            [cprop.source :as source]
            [clojure.java.io :as io]
            [com.stuartsierra.component :as component]))

;; Pure config operations
(defn load-config-from-edn
  "Pure function to load configuration from EDN file"
  [env-name]
  (let [base-config (source/from-resource "config.edn")]
    (merge (:default base-config)
           (get base-config env-name))))

(defn load-env-config
  "Pure function to load environment configuration"
  []
  (load-config))

(defn merge-configs
  "Pure function to merge multiple configuration maps"
  [& configs]
  (apply merge configs))

(defn get-config-value
  "Pure function to get a configuration value by path"
  [config & path]
  (get-in config path))

(defn validate-config
  "Pure function to validate configuration"
  [config required-keys]
  (let [missing-keys (remove #(get-in config %) required-keys)]
    (if (empty? missing-keys)
      {:valid true :config config}
      {:valid false 
       :errors missing-keys
       :message (str "Missing required config keys: " missing-keys)})))

;; Configuration transformations
(defn transform-config
  "Pure function to transform configuration values"
  [config transformations]
  (reduce (fn [cfg [path transform-fn]]
            (update-in cfg path transform-fn))
          config
          transformations))

(defn normalize-config
  "Pure function to normalize configuration structure"
  [config]
  (transform-config config
    [[[:server :port] #(or % 3000)]
     [[:game :max-players-per-room] #(or % 2)]
     [[:logging :level] #(or % :info)]]))

;; Configuration component that caches loaded config
(defrecord ConfigComponent [config env-name]
  component/Lifecycle
  
  (start [this]
    (let [env (keyword (or (System/getenv "ENV") env-name "development"))
          edn-config (load-config-from-edn env)
          env-config (load-env-config)
          merged-config (merge-configs edn-config env-config)
          normalized-config (normalize-config merged-config)]
      (assoc this :config normalized-config)))
  
  (stop [this]
    (dissoc this :config)))

;; Config access functions
(defn get-server-port
  "Pure function to get server port from config"
  [config]
  (get-config-value config :server :port))

(defn get-database-config
  "Pure function to get database config"
  [config]
  (get-config-value config :database))

(defn get-logging-config
  "Pure function to get logging config"
  [config]
  (get-config-value config :logging))

(defn get-game-config
  "Pure function to get game config"
  [config]
  (get-config-value config :game))

;; Configuration builder pattern
(defn config-builder
  "Creates a configuration builder"
  [base-config]
  {:config base-config
   :transformations []})

(defn add-transformation
  "Adds a transformation to the builder"
  [builder path transform-fn]
  (update builder :transformations conj [path transform-fn]))

(defn build-config
  "Builds the final configuration from builder"
  [builder]
  (transform-config (:config builder) (:transformations builder)))

(defn new-config-component
  "Creates a new configuration component"
  [env-name]
  (map->ConfigComponent {:env-name env-name}))