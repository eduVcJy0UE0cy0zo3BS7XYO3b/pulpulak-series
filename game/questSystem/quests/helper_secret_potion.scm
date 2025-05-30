;; Квест помощницы: Секретное зелье исцеления
(quest helper_secret_potion
  (metadata
    (title "Секретное зелье исцеления")
    (description "Найти ингредиенты для мощного зелья исцеления")
    (character helper))

  ;; Определения квестовых действий
  (quest-actions
    (action start_common_quest
      (description "Начать квест помощницы")
      (effects
        (start-quest helper_secret_potion)
        (set-global-memory helper_secret_potion true)))
    
    (action advance_potion_quest
      (description "Продвинуть квест зелья")
      (effects
        (update-quest-state 
          (current-npc cook)
          (sync-location true)
          (sync-outfit true))
        (process-quest-step true)))
    
    (action find_gardener_quest
      (description "Найти садовника")
      (effects
        (update-quest-state
          (current-npc gardener)
          (sync-location true)
          (sync-outfit true))
        (process-quest-step true)))
    
    (action go_to_greenhouse
      (description "Перейти в теплицу")
      (effects
        (move-to-location greenhouse)
        (update-npcs true))))

  (variables
    (ingredients_found 0)
    (herbs_list ("moonflower" "dragon_root" "phoenix_feather"))
    (recipe_known false))

  (triggers
    ;; Квест начинается при разговоре с поваром в простой одежде
    (on-dialogue cook
      (when (and
        (outfit-is "common")
        (at-location kitchen)
        (not (quest-started "helper_secret_potion"))))))

  (steps
    ;; Шаг 1: Получить детали квеста от повара
    (step learn_quest_details
      (description "Узнать подробности о лечебном зелье")
      (require
        (at-location kitchen)
        (talking-to cook)
        (has-memory "helped_cooking"))
      (actions
        (set-memory "potion_quest_started" true)
        (set-memory "needs_healing_herbs" true)
        (show-message "Повар рассказал о болезни в деревне и необходимости создать лечебное зелье")
        ;; Добавляем диалог садовнику  
        (add-dialogue gardener
          (option ask_about_herbs
            "Спросить о лечебных травах"
            "Ах, лечебные травы? Да, у меня есть кое-что особенное... Пойдёмте в теплицу, покажу редкие растения!"
            (quest_action "find_gardener_quest")))
        ;; Обновляем диалог повара
        (add-dialogue cook
          (option check_ingredients
            "Показать найденные ингредиенты"
            "Отлично! Но нам нужны все три редких компонента"))))

    ;; Шаг 2: Найти садовника
    (step find_gardener
      (description "Найти садовника в секретном саду")
      (require
        (at-location secret_garden)
        (has-memory "potion_quest_started"))
      (actions
        (set-memory "gardener_found" true)
        (show-message "Вы нашли садовника, ухаживающего за редкими растениями")
        ;; Добавляем опцию узнать о теплице
        (add-dialogue gardener
          (option ask_about_greenhouse
            "Узнать о теплице с редкими растениями"
            "В моей теплице растут самые редкие травы королевства. Пойдёмте, покажу"))))

    ;; Шаг 3: Получить ингредиенты в теплице
    (step talk_to_gardener
      (description "Поговорить с садовником о редких растениях")
      (require
        (at-location greenhouse)
        (talking-to gardener)
        (has-memory "gardener_found"))
      (actions
        ;; Даём первый ингредиент
        (give-items moonflower)
        (set-memory "has_moonflower" true)
        (show-message "Садовник дал вам лунный цветок - первый ингредиент")
        
        ;; Рассказываем о других ингредиентах
        (add-dialogue gardener
          (option learn_about_others
            "Узнать о других ингредиентах"
            "Вам также понадобятся корень дракона и перо феникса. Поищите их в лесу и у алхимика"))
        
        ;; Открываем новые локации
        (reveal-location deep_forest)
        (reveal-location alchemist_shop)
        
        ;; Спавним ингредиенты
        (spawn-item deep_forest dragon_root)
        (spawn-item alchemist_shop phoenix_feather)))

    ;; Шаг 4: Собрать корень дракона
    (step collect_dragon_root
      (description "Найти корень дракона в глубоком лесу")
      (optional true)  ;; Опциональный шаг
      (require
        (at-location deep_forest)
        (has-memory "has_moonflower"))
      (actions
        (if (has-item dragon_root)
          (progn
            (set-memory "has_dragon_root" true)
            (show-message "Вы нашли редкий корень дракона!"))
          (show-message "Нужно поискать более тщательно..."))))

    ;; Шаг 5: Получить перо феникса
    (step collect_phoenix_feather
      (description "Получить перо феникса у алхимика")
      (optional true)  ;; Опциональный шаг
      (require
        (at-location alchemist_shop)
        (has-memory "has_moonflower"))
      (actions
        (if (has-item phoenix_feather)
          (progn
            (set-memory "has_phoenix_feather" true)
            (show-message "Алхимик продал вам настоящее перо феникса!"))
          (show-message "Алхимик ждёт достойную оплату за перо..."))))

    ;; Шаг 6: Вернуться к повару со всеми ингредиентами
    (step return_to_cook
      (description "Вернуться к повару с ингредиентами")
      (require
        (at-location kitchen)
        (talking-to cook)
        (all (moonflower dragon_root phoenix_feather) 
             (has-item $item)))
      (actions
        (show-message "Повар восхищён! Все ингредиенты собраны!")
        ;; Забираем ингредиенты и даём зелье
        (take-items moonflower dragon_root phoenix_feather)
        (give-items healing_potion)
        (set-memory "potion_created" true)
        (complete-quest)))
  )

  ;; Ветвление для альтернативного пути
  (branch
    (case (and (at-location village) 
               (has-memory "potion_created"))
      (show-message "Вы можете использовать зелье для лечения больных в деревне")
      (add-dialogue village_elder
        (option heal_villagers
          "Предложить зелье для лечения"
          "Вы спасли нашу деревню! Мы в вечном долгу перед вами!"))
      (if (talking-to village_elder)
        (progn
          (take-items healing_potion)
          ;; Removed loyalty system
          (set-memory "village_saved" true))))
    
    (default
      ;; Ничего не делаем, если условия не выполнены
      ))

  (on-complete
    ;; Removed loyalty system
    (set-memory "master_gardener" true)
    (show-message "Вы создали легендарное зелье исцеления! Ваши навыки травничества теперь не знают равных")
    ;; Добавляем постквестовые диалоги
    (add-dialogue cook
      (option cooking_lessons
        "Узнать новые рецепты"
        "После такого успеха, я научу вас особым королевским рецептам!"))
    (add-dialogue gardener
      (option advanced_herbs
        "Изучить продвинутое травничество"
        "Вы доказали свое мастерство. Позвольте показать вам секретные знания..."))))