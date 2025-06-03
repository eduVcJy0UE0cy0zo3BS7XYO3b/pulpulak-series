(ns pulpulak.views.game
  (:require [re-frame.core :as rf]
            [pulpulak.subs :as subs]
            [pulpulak.events :as events]
            [clojure.string :as str]))

(defn outfit-indicator [outfit]
  [:div.outfit-indicator
   [:span "Current Outfit: "]
   [:span.outfit-name (if (= outfit :princess) "Princess Dress" "Servant Clothes")]])

(defn choice-button [choice]
  [:button.choice-button
   {:on-click #(rf/dispatch [::events/make-choice (:id choice)])}
   (:text choice)])

(defn scene-display []
  (let [scene @(rf/subscribe [::subs/current-scene])]
    [:div.scene-display
     [:h2 (:title scene)]
     [:div.scene-description
      [:p (:description scene)]]
     [:div.choices-container
      (for [choice (:choices scene)]
        ^{:key (:id choice)}
        [choice-button choice])]]))

(defn player-status []
  (let [player @(rf/subscribe [::subs/player])
        other-player @(rf/subscribe [::subs/other-player])
        can-swap? @(rf/subscribe [::subs/can-swap-outfit?])]
    [:div.player-status
     [:div.status-row
      [:span.label "You: "]
      [:span (:username player) " (" (name (:role player)) ")"]]
     
     (when (= (:role player) :princess)
       [:div.status-row
        [outfit-indicator (:outfit player)]
        (when can-swap?
          [:button.swap-button
           {:on-click #(rf/dispatch [::events/request-outfit-swap])}
           "Swap Outfit"])])
     
     [:div.status-row
      [:span.label "Location: "]
      [:span (:location player)]]
     
     (when other-player
       [:div.status-row
        [:span.label "Partner: "]
        [:span (:username other-player) " at " (:location other-player)]])])) 

(defn location-display []
  (let [game-state @(rf/subscribe [::subs/game-state])
        location (:location game-state)]
    [:div.location-display
     [:div.location-header
      [:span.location-icon (:icon location)]
      [:h3 (:name location)]]
     [:p.location-description (:description location)]
     
     (when (seq (:npcs game-state))
       [:div.npcs-section
        [:h4 "People here:"]
        [:div.npc-list
         (for [npc (:npcs game-state)]
           ^{:key (:id npc)}
           [:div.npc-card
            [:span.npc-name (:name npc)]
            [:button.talk-button
             {:on-click #(rf/dispatch [::events/show-npc-dialogue (:id npc)])}
             "Talk"]])]])
     
     [:div.connections-section
      [:h4 "Go to:"]
      [:div.location-buttons
       (for [loc-id (:connections location)]
         ^{:key loc-id}
         [:button.location-button
          {:on-click #(rf/dispatch [::events/move-to-location loc-id])}
          (name loc-id)])]]]))

(defn quest-panel []
  (let [game-state @(rf/subscribe [::subs/game-state])
        quests (:quests game-state)]
    [:div.quest-panel
     [:h3 "Quests"]
     (if (seq (:active quests))
       [:div.active-quests
        (for [quest (:active quests)]
          ^{:key (:id quest)}
          [:div.quest-item
           [:h4 (:title quest)]
           [:div.quest-progress
            [:span (str (:current-step quest) "/" (:total-steps quest) " steps")]]])]
       [:p "No active quests"])
     
     (when (seq (:objectives quests))
       [:div.objectives
        [:h4 "Current Objectives:"]
        (for [obj (:objectives quests)]
          ^{:key (str (:quest-id obj) "-" (:objective obj))}
          [:div.objective-item
           [:p (:objective obj)]
           [:span.objective-location (str "Location: " (name (:location obj)))]])])]))

(defn inventory-panel []
  (let [player @(rf/subscribe [::subs/player])]
    [:div.inventory-panel
     [:h3 "Inventory"]
     (if (seq (:inventory player))
       [:div.inventory-items
        (for [item (:inventory player)]
          ^{:key item}
          [:div.inventory-item (name item)])]
       [:p "Empty"])]))

(defn game-screen []
  [:div.game-screen
   [:div.game-header
    [:h1 "Pulpulak Series"]
    [player-status]]
   
   [:div.game-main
    [:div.game-left
     [location-display]
     [scene-display]]
    
    [:div.game-right
     [quest-panel]
     [inventory-panel]]]])