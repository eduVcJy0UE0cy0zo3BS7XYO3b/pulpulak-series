;; Quest for Princess: Lost Royal Relic
(quest princess_lost_relic
  (metadata
    (title "Lost Royal Relic")
    (description "Find the ancient amulet that disappeared from the treasury")
    (character princess))

  ;; Определения квестовых действий
  (quest-actions
    (action start_noble_quest
      (description "Начать квест принцессы")
      (effects
        (start-quest princess_lost_relic)
        (set-global-memory princess_lost_relic true))))

  (variables
    (quest_stage 0)
    (has_library_info false)
    (knows_amulet_location false))

  (triggers
    ;; Quest starts when talking to advisor in noble outfit
    (on-dialogue royal_advisor
      (when (and
        (outfit-is "princess_dress")
        (at-location throne_room)
        (not (quest-started "princess_lost_relic"))))))

  (steps
    ;; Step 1: Get quest from advisor
    (step talk_to_advisor
      (description "Talk to the royal advisor about the missing relic")
      (require
        (and
          (at-location throne_room)
          (talking-to royal_advisor)
          (= $quest_stage 0)))
      (actions
        (set-quest-var "quest_stage" 1)
        (show-message "Advisor: Your Highness, the ancient royal amulet has disappeared!")
        (set-memory "knows_about_amulet" true)
        (add-dialogue librarian
          (option ask_about_amulet
            "Ask about the ancient amulet"
            "I need information about an ancient royal amulet..."))))

    ;; Step 2: Research in library
    (step research_library
      (description "Find information about the amulet in the library")
      (require
        (and
          (at-location library)
          (talking-to librarian)
          (= $quest_stage 1)
          (not $has_library_info)))
      (actions
        (set-quest-var "has_library_info" true)
        (show-message "Librarian: Ah yes, the Amulet of Kings... According to the texts, it resonates with royal gardens.")
        (set-memory "amulet_garden_connection" true)))

    ;; Step 3: Search the gardens
    (step search_gardens
      (description "Search for the amulet in the royal gardens")
      (require
        (and
          (at-location garden)
          (= $quest_stage 1)
          $has_library_info
          (not $knows_amulet_location)))
      (actions
        (show-message "You notice something glinting near the fountain...")
        (set-quest-var "knows_amulet_location" true)
        (spawn-item garden "royal_amulet")))

    ;; Step 4: Find the amulet
    (step find_amulet
      (description "Retrieve the royal amulet")
      (require
        (and
          (at-location garden)
          $knows_amulet_location
          (not (has-item royal_amulet))))
      (actions
        (give-items royal_amulet)
        (set-quest-var "quest_stage" 2)
        (show-message "You found the Royal Amulet!")
        (set-memory "has_royal_amulet" true)))

    ;; Step 5: Return to advisor
    (step return_to_advisor
      (description "Return the amulet to the royal advisor")
      (require
        (and
          (at-location throne_room)
          (talking-to royal_advisor)
          (= $quest_stage 2)
          (has-item royal_amulet)))
      (actions
        (take-items royal_amulet)
        (complete-quest))))

  (on-complete
    ;; Removed loyalty system
    (show-message "Advisor: Excellent work, Your Highness! The kingdom is in your debt.")
    (set-memory "completed_relic_quest" true)))