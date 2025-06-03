(ns pulpulak.views.main-menu
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [pulpulak.events :as events]))

(defn main-menu []
  (let [username (r/atom "")
        room-code (r/atom "")]
    (fn []
      [:div.main-menu
       [:h1 "Pulpulak Series"]
       [:div.menu-card
        [:h2 "Enter Your Name"]
        [:input {:type "text"
                 :placeholder "Your name"
                 :value @username
                 :on-change #(reset! username (-> % .-target .-value))}]
        
        [:div.button-group
         [:button.primary-button
          {:on-click #(when (not-empty @username)
                        (rf/dispatch [::events/create-room @username]))}
          "Create Room"]
         
         [:div.divider "OR"]
         
         [:div.join-section
          [:input {:type "text"
                   :placeholder "Room Code"
                   :value @room-code
                   :on-change #(reset! room-code (-> % .-target .-value .-toUpperCase))}]
          [:button.secondary-button
           {:on-click #(when (and (not-empty @username)
                                  (not-empty @room-code))
                         (rf/dispatch [::events/join-room @room-code @username]))}
           "Join Room"]]]]])))