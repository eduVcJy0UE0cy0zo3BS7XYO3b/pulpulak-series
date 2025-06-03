(ns pulpulak.views.lobby
  (:require [re-frame.core :as rf]
            [pulpulak.subs :as subs]
            [pulpulak.events :as events]))

(defn player-card [player role]
  [:div.player-card {:class (name role)}
   [:div.role-icon
    (if (= role :princess) "👑" "🛡️")]
   [:h3 (:username player)]
   [:div.role-label (if (= role :princess) "Princess" "Helper")]
   (when (:ready player)
     [:div.ready-indicator "✓ Ready"])])

(defn lobby-screen []
  (let [room-id @(rf/subscribe [::subs/room-id])
        username @(rf/subscribe [::subs/username])
        role @(rf/subscribe [::subs/role])
        game-state @(rf/subscribe [::subs/game-state])
        player (:player game-state)
        other-player (:other-player game-state)]
    [:div.lobby-screen
     [:h1 "Game Lobby"]
     
     [:div.room-info
      [:h2 "Room Code: " [:span.room-code room-id]]
      [:p.instruction "Share this code with your partner to join"]]
     
     [:div.players-container
      [player-card {:username username :ready (:ready player)} role]
      
      (if other-player
        [player-card other-player (:role other-player)]
        [:div.player-card.waiting
         [:div.loading-dots "Waiting for player..."]])]
     
     (when (and other-player (not (:ready player)))
       [:button.ready-button
        {:on-click #(rf/dispatch [::events/player-ready])}
        "Ready to Start"])
     
     (when (:ready player)
       [:div.waiting-message "Waiting for other player..."])]))