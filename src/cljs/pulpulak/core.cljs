(ns pulpulak.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [pulpulak.events :as events]
            [pulpulak.views :as views]
            [pulpulak.websocket :as ws]))

(defn dev-setup []
  (when ^boolean goog.DEBUG
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (rf/clear-subscription-cache!)
  (rdom/render [views/main-panel]
               (.getElementById js/document "app")))

(defn ^:export init []
  (rf/dispatch-sync [::events/initialize])
  (dev-setup)
  (ws/init!)
  (mount-root))