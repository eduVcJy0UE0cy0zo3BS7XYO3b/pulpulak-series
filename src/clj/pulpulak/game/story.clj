(ns pulpulak.game.story)

(def scenes
  {:intro {:id :intro
           :title "The Beginning"
           :descriptions {:princess "You are Princess Pulpulak, standing in your castle chambers. Your loyal helper awaits your commands."
                          :helper "You serve Princess Pulpulak faithfully. You stand ready in the castle, awaiting her orders."}
           :choices [{:id "explore-castle"
                      :text "Explore the castle"
                      :type :regular
                      :effects [{:type :next-scene :scene-id :castle-hall}]}
                     {:id "go-to-village"
                      :text "Visit the village"
                      :type :synchronous
                      :required-role :princess
                      :effects [{:type :next-scene :scene-id :village-entrance}
                                {:type :update-location :location "village"}]}
                     {:id "check-supplies"
                      :text "Check the supply room"
                      :type :regular
                      :required-role :helper
                      :effects [{:type :next-scene :scene-id :supply-room}]}]}
   
   :castle-hall {:id :castle-hall
                 :title "Castle Hall"
                 :descriptions {:princess "The grand hall of your castle stretches before you, adorned with tapestries depicting your family's history."
                                :helper "You enter the castle's grand hall, its magnificence reminding you of your duty to the princess."}
                 :choices [{:id "return-chambers"
                            :text "Return to chambers"
                            :type :regular
                            :effects [{:type :next-scene :scene-id :intro}]}
                           {:id "visit-throne"
                            :text "Approach the throne"
                            :type :regular
                            :required-role :princess
                            :effects [{:type :next-scene :scene-id :throne-room}]}]}
   
   :village-entrance {:id :village-entrance
                      :title "Village Entrance"
                      :descriptions {:princess "You arrive at the village entrance. The guards bow respectfully as they recognize you."
                                     :helper "You accompany the princess to the village. The atmosphere changes as people notice the royal presence."}
                      :choices [{:id "talk-to-villagers"
                                 :text "Speak with the villagers"
                                 :type :regular
                                 :effects [{:type :next-scene :scene-id :village-square}
                                           {:type :add-loyalty :target "villagers" :amount 10}]}
                                {:id "visit-market"
                                 :text "Browse the market"
                                 :type :regular
                                 :effects [{:type :next-scene :scene-id :market}]}
                                {:id "return-castle"
                                 :text "Return to the castle"
                                 :type :synchronous
                                 :effects [{:type :next-scene :scene-id :intro}
                                           {:type :update-location :location "castle"}]}]}})

(defn get-scene [scene-id]
  (get scenes scene-id))