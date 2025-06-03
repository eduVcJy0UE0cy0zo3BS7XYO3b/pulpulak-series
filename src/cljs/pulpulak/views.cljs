(ns pulpulak.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [pulpulak.subs :as subs]
            [pulpulak.events :as events]
            [pulpulak.views.main-menu :as main-menu]
            [pulpulak.views.lobby :as lobby]
            [pulpulak.views.game :as game]))

(defn error-notification []
  (when-let [error @(rf/subscribe [::subs/error])]
    [:div.error-notification
     [:span error]
     [:button {:on-click #(rf/dispatch [::events/clear-error])} "×"]]))

(defn loading-spinner []
  [:div.loading-spinner
   [:div.spinner]])

(defn main-panel []
  (let [screen @(rf/subscribe [::subs/screen])
        loading @(rf/subscribe [::subs/loading])]
    [:div.app-container
     [error-notification]
     (when loading [loading-spinner])
     (case screen
       :main-menu [main-menu/main-menu]
       :lobby [lobby/lobby-screen]
       :game [game/game-screen]
       [:div "Unknown screen"])]))