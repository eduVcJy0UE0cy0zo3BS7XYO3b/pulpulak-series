;; =================================================
;; QUEST NARRATIVE: Секретное зелье исцеления
;; =================================================
;; КОНТЕКСТ: В деревне началась странная болезнь. Люди слабеют,
;; а обычные лекарства не помогают. Местный повар знает древний
;; рецепт целебного зелья, но ингредиенты рассеяны и редки.
;;
;; ПРОБЛЕМА: Нужно собрать три магических компонента:
;; лунный цветок, корень дракона и перо феникса. Каждый
;; ингредиент находится в разных локациях у разных NPC.
;;
;; МОТИВАЦИЯ: Спасение жизней больных людей - благородная
;; цель. Без зелья эпидемия может распространиться дальше.
;;
;; ОЖИДАЕМЫЙ РЕЗУЛЬТАТ: Создание легендарного зелья исцеления,
;; спасение деревни и получение репутации мастера травника.
;;
;; КЛЮЧЕВЫЕ ПЕРСОНАЖИ:
;; - Повар (знает рецепт)
;; - Садовник (владеет лунным цветком)
;; - Лесной житель (хранит корень дракона)
;; - Алхимик (торгует пером феникса)
;;
;; ВАЖНЫЕ ЛОКАЦИИ:
;; - Кухня (начало квеста)
;; - Секретный сад (поиск садовника)
;; - Теплица (получение лунного цветка)
;; - Глубокий лес (сбор корня дракона)
;; - Лавка алхимика (покупка пера феникса)
;; =================================================

(quest helper_secret_potion
  (metadata
    (title "Секретное зелье исцеления")
    (description "Найти ингредиенты для мощного зелья исцеления")
    (character helper))

  ;; Начальное состояние
  (initial-state
    (player-location kitchen)
    (player-outfit common)
    (inventory '())
    (memory '())
    (quest-stage 0))
  
  ;; Конечное состояние
  (goal-state
    (conditions
      (player-has-item healing_potion)
      (memory-contains potion_created true)
      (quest-completed)))
  
  ;; Функциональный путь решения
  (solution-path
    (move kitchen)
    (interact cook ask_about_illness)
    (move secret_garden)
    (interact gardener ask_about_herbs)
    (move greenhouse)
    (interact gardener get_moonflower)
    (move deep_forest)
    (collect-item dragon_root)
    (move alchemist_shop)
    (interact alchemist buy_phoenix_feather)
    (move kitchen)
    (interact cook create_potion))
  
  ;; Альтернативные пути
  (alternative-paths
    (path-herb-first
      (move greenhouse)
      (interact gardener get_moonflower)
      (move deep_forest)
      (collect-item dragon_root)
      (move alchemist_shop)
      (interact alchemist buy_phoenix_feather)
      (move kitchen)
      (interact cook create_potion))
    (path-forest-first
      (move deep_forest)
      (collect-item dragon_root)
      (move greenhouse)
      (interact gardener get_moonflower)
      (move alchemist_shop)
      (interact alchemist buy_phoenix_feather)
      (move kitchen)
      (interact cook create_potion)))

  (variables
    (quest_stage 0)
    (ingredients_collected '())
    (required_ingredients '(moonflower dragon_root phoenix_feather))
    (recipe_known false)
    (has_moonflower false)
    (has_dragon_root false)
    (has_phoenix_feather false))

  (triggers
    ;; Квест начинается при разговоре с поваром в простой одежде
    (on-dialogue cook
      (when (and
        (outfit-is common)
        (at-location kitchen)
        (not (quest-started helper_secret_potion))))))
  
  ;; Тестовые функции для проверки состояний
  (test-functions
    (test-initial-state
      (lambda ()
        (and
          (= (player-location) kitchen)
          (= (player-outfit) common)
          (empty? (player-inventory))
          (= (quest-stage) 0))))
    
    (test-can-start-quest
      (lambda ()
        (and
          (at-location kitchen)
          (outfit-is common)
          (not (quest-started helper_secret_potion)))))
    
    (test-first-ingredient-collected
      (lambda ()
        (and
          (player-has-item moonflower)
          (= (get-memory has_moonflower) true)
          (> (quest-stage) 0))))
    
    (test-all-ingredients-collected
      (lambda ()
        (and
          (player-has-item moonflower)
          (player-has-item dragon_root)
          (player-has-item phoenix_feather)
          (>= (quest-stage) 3))))
    
    (test-quest-completed
      (lambda ()
        (and
          (player-has-item healing_potion)
          (get-memory potion_created)
          (quest-completed helper_secret_potion))))

  (steps
    ;; Шаг 1: Начать квест - поговорить с поваром
    (step start_potion_quest
      (description "Начать квест секретного зелья")
      (require
        (and
          (at-location kitchen)
          (outfit-is common)
          (= $quest_stage 0)))
      (actions
        (set-quest-var quest_stage 1)
        (set-memory potion_quest_started true)
        (set-memory recipe_known true)
        (show-message "Повар: 'Помогите! В деревне странная болезнь. Мне нужны: лунный цветок, корень дракона и перо феникса!'")
        (reveal-location secret_garden)
        (reveal-location greenhouse)
        (reveal-location deep_forest)
        (reveal-location alchemist_shop)))

    ;; Шаг 2: Получить лунный цветок в теплице
    (step get_moonflower
      (description "Получить лунный цветок от садовника")
      (require
        (and
          (at-location greenhouse)
          (>= $quest_stage 1)
          (not $has_moonflower)))
      (actions
        (give-items moonflower)
        (set-quest-var has_moonflower true)
        (add-item-to-list ingredients_collected moonflower)
        (show-message "Садовник: 'Лунный цветок растет только в полнолуние. Корень дракона ищите в лесу, а перо феникса - у алхимика.'")
        (when (= (length $ingredients_collected) 1)
          (show-message "Первый ингредиент найден!"))))

    ;; Шаг 3: Найти корень дракона в лесу
    (step get_dragon_root
      (description "Найти корень дракона в глубоком лесу")
      (require
        (and
          (at-location deep_forest)
          (>= $quest_stage 1)
          (not $has_dragon_root)))
      (actions
        (collect-item dragon_root)
        (set-quest-var has_dragon_root true)
        (add-item-to-list ingredients_collected dragon_root)
        (show-message "Вы нашли редкий корень дракона среди древних дубов!")
        (when (= (length $ingredients_collected) 2)
          (show-message "Два из трёх ингредиентов найдено!"))))

    ;; Шаг 4: Купить перо феникса у алхимика
    (step get_phoenix_feather
      (description "Купить перо феникса у алхимика")
      (require
        (and
          (at-location alchemist_shop)
          (>= $quest_stage 1)
          (not $has_phoenix_feather)))
      (actions
        (interact alchemist buy_phoenix_feather)
        (take-items phoenix_feather)
        (set-quest-var has_phoenix_feather true)
        (add-item-to-list ingredients_collected phoenix_feather)
        (show-message "Алхимик: 'Перо феникса - очень редкая вещь. Используйте осторожно!'")
        (when (= (length $ingredients_collected) 3)
          (show-message "Все ингредиенты собраны! Возвращайтесь к повару."))))

    ;; Шаг 5: Создать зелье с поваром
    (step create_healing_potion
      (description "Создать секретное зелье с поваром")
      (require
        (and
          (at-location kitchen)
          (>= $quest_stage 1)
          $has_moonflower
          $has_dragon_root
          $has_phoenix_feather
          (= (length $ingredients_collected) 3)))
      (actions
        (interact cook create_potion)
        (take-items moonflower dragon_root phoenix_feather)
        (give-items healing_potion)
        (set-memory potion_created true)
        (set-quest-var quest_stage 2)
        (show-message "Повар: 'Невероятно! Вы создали легендарное зелье исцеления!'")
        (complete-quest)))

    ;; Опциональный шаг: Использовать зелье для лечения деревни
    (step heal_village
      (description "Использовать зелье для лечения больных")
      (optional true)
      (require
        (and
          (at-location village)
          (player-has-item healing_potion)
          (get-memory potion_created)))
      (actions
        (interact village_elder heal_villagers)
        (take-items healing_potion)
        (set-memory village_saved true)
        (show-message "Деревенский старейшина: 'Вы спасли нашу деревню! Мы в вечном долгу перед вами!'")
        (give-items rare_herb_seeds village_blessing)))
  )\n\n;; Отдельные тестовые случаи\n(test-case negative-test-invalid-location\n  (description "Проверка невозможности начать квест в неправильной локации")\n  (move throne_room)\n  (assert-error (interact cook ask_about_illness))\n  (assert (not (can-start-quest helper_secret_potion))))\n\n(test-case negative-test-wrong-outfit\n  (description "Проверка невозможности начать квест в неправильной одежде")\n  (move kitchen)\n  (change-outfit noble)\n  (assert (not (can-start-quest helper_secret_potion)))\n  (assert-error (interact cook ask_about_illness)))\n\n(test-case boundary-test-partial-ingredients\n  (description "Проверка невозможности создать зелье с неполным набором")\n  (set-state \n    (location kitchen)\n    (outfit common)\n    (quest-stage 1)\n    (inventory '(moonflower dragon_root)))\n  (assert (not (can-complete-step create_healing_potion)))\n  (assert-error (interact cook create_potion)))

  ;; Тестовая последовательность для полного выполнения
  (test-sequence complete-quest-test
    (description "Полный тест прохождения квеста")
    (assert-initial-state)
    (move kitchen)
    (assert (can-start-quest helper_secret_potion))
    (interact cook ask_about_illness)
    (assert (quest-started helper_secret_potion))
    (assert (= (quest-stage) 1))
    
    ;; Получение лунного цветка
    (move greenhouse)
    (interact gardener get_moonflower)
    (assert (player-has-item moonflower))
    (assert $has_moonflower)
    
    ;; Получение корня дракона
    (move deep_forest)
    (collect-item dragon_root)
    (assert (player-has-item dragon_root))
    (assert $has_dragon_root)
    
    ;; Получение пера феникса
    (move alchemist_shop)
    (interact alchemist buy_phoenix_feather)
    (assert (player-has-item phoenix_feather))
    (assert $has_phoenix_feather)
    (assert (= (length $ingredients_collected) 3))
    
    ;; Создание зелья
    (move kitchen)
    (interact cook create_potion)
    (assert (player-has-item healing_potion))
    (assert (get-memory potion_created))
    (assert (quest-completed helper_secret_potion))
    
    ;; Опциональное лечение деревни
    (move village)
    (interact village_elder heal_villagers)
    (assert (get-memory village_saved))
    (assert (player-has-item village_blessing)))

  (on-complete
    (set-memory master_gardener true)
    (set-memory quest_completed true)
    (show-message "Вы создали легендарное зелье исцеления! Ваши навыки травничества теперь не знают равных"))))