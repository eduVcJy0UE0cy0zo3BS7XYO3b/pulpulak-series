;; ===============================================
;; QUEST NARRATIVE: Потерянная королевская реликвия
;; ===============================================
;; КОНТЕКСТ: Из королевской сокровищницы пропал древний амулет королей -
;; семейная реликвия, передающаяся из поколения в поколение. Без амулета
;; коронация принцессы не может состояться по древним законам.
;;
;; ПРОБЛЕМА: Амулет исчез при загадочных обстоятельствах. Нет следов
;; взлома, охрана ничего не видела. Нужно найти реликвию до церемонии
;; коронации, иначе легитимность власти будет поставлена под сомнение.
;;
;; МОТИВАЦИЯ: Восстановление королевской традиции и подготовка к коронации.
;; Амулет не просто драгоценность - он символ власти и преемственности.
;; Без него принцесса не сможет стать полноправной королевой.
;;
;; ОЖИДАЕМЫЙ РЕЗУЛЬТАТ: Возвращение амулета на место, восстановление
;; порядка и возможность провести торжественную коронацию.
;;
;; КЛЮЧЕВЫЕ ПЕРСОНАЖИ:
;; - Королевский советник (знает о пропаже, дает задание)
;; - Библиотекарь (хранитель древних знаний об амулете)
;; - Садовник (может знать о тайных местах в саду)
;;
;; ВАЖНЫЕ ЛОКАЦИИ:
;; - Тронный зал (начало квеста)
;; - Библиотека (исследование истории амулета)
;; - Королевский сад (поиск спрятанной реликвии)
;; ===============================================

(quest princess_lost_relic
  (metadata
    (title "Потерянная королевская реликвия")
    (description "Найти древний амулет, исчезнувший из сокровищницы")
    (character princess))

  ;; Начальное состояние
  (initial-state
    (player-location throne_room)
    (player-outfit princess_dress)
    (inventory '())
    (memory '())
    (quest-stage 0))
  
  ;; Конечное состояние
  (goal-state
    (conditions
      (quest-completed)
      (memory-contains amulet_returned true)
      (npc-satisfied royal_advisor)))
  
  ;; Функциональный путь решения
  (solution-path
    (move throne_room)
    (interact royal_advisor report_missing_amulet)
    (move library)
    (interact librarian research_amulet_history)
    (move garden)
    (search-location amulet)
    (collect-item royal_amulet)
    (move throne_room)
    (interact royal_advisor return_amulet))
  
  ;; Альтернативные пути
  (alternative-paths
    (path-direct-search
      (move garden)
      (search-location thoroughly)
      (collect-item royal_amulet)
      (move throne_room)
      (interact royal_advisor return_amulet))
    (path-library-first
      (move library)
      (interact librarian research_amulet_history)
      (move garden)
      (search-location amulet)
      (collect-item royal_amulet)
      (move throne_room)
      (interact royal_advisor return_amulet)))

  (variables
    (quest_stage 0)
    (amulet_research_done false)
    (garden_searched false)
    (amulet_found false)
    (library_visited false))

  (triggers
    ;; Quest starts when talking to advisor in princess dress
    (on-dialogue royal_advisor
      (when (and
        (outfit-is princess_dress)
        (at-location throne_room)
        (not (quest-started princess_lost_relic))))))
  
  ;; Тестовые функции для проверки состояний
  (test-functions
    (test-initial-state
      (lambda ()
        (and
          (= (player-location) throne_room)
          (= (player-outfit) princess_dress)
          (empty? (player-inventory))
          (= (quest-stage) 0))))
    
    (test-can-start-quest
      (lambda ()
        (and
          (at-location throne_room)
          (outfit-is princess_dress)
          (not (quest-started princess_lost_relic)))))
    
    (test-research-completed
      (lambda ()
        (and
          $amulet_research_done
          $library_visited
          (get-memory amulet_garden_connection))))
    
    (test-amulet-found
      (lambda ()
        (and
          $amulet_found
          (player-has-item royal_amulet)
          $garden_searched)))
    
    (test-quest-completed
      (lambda ()
        (and
          (quest-completed princess_lost_relic)
          (get-memory amulet_returned)
          (npc-satisfied royal_advisor))))

  (steps
    ;; Шаг 1: Получить задание от советника
    (step get_quest_from_advisor
      (description "Получить квест от королевского советника")
      (require
        (and
          (at-location throne_room)
          (outfit-is princess_dress)
          (= $quest_stage 0)))
      (actions
        (set-quest-var quest_stage 1)
        (set-memory amulet_missing true)
        (set-memory quest_urgency high)
        (show-message "Советник: 'Ваше Высочество! Древний королевский амулет исчез! Коронация под угрозой!'")
        (reveal-location library)
        (reveal-location garden)))

    ;; Шаг 2: Исследовать историю амулета в библиотеке
    (step research_amulet_history
      (description "Исследовать историю амулета")
      (require
        (and
          (at-location library)
          (>= $quest_stage 1)
          (not $amulet_research_done)))
      (actions
        (interact librarian research_ancient_texts)
        (set-quest-var amulet_research_done true)
        (set-quest-var library_visited true)
        (set-memory amulet_garden_connection true)
        (set-memory ancient_knowledge_gained true)
        (show-message "Библиотекарь: 'Амулет Королей... По древним текстам, он резонирует с королевскими садами. Поищите у фонтана.'")
        (reveal-location fountain_area)))

    ;; Шаг 3: Обыскать королевские сады
    (step search_royal_gardens
      (description "Обыскать королевские сады")
      (require
        (and
          (at-location garden)
          (>= $quest_stage 1)
          (not $garden_searched)))
      (actions
        (search-location royal_gardens)
        (set-quest-var garden_searched true)
        (when $amulet_research_done
          (show-message "Воспользовавшись знаниями из библиотеки, вы находите отблеск у фонтана!")
          (spawn-item garden royal_amulet))
        (when (not $amulet_research_done)
          (show-message "Сады огромны. Нужно больше информации о местонахождении амулета.")
          (set-memory need_more_info true))))

    ;; Шаг 4: Найти амулет
    (step find_royal_amulet
      (description "Найти и поднять королевский амулет")
      (require
        (and
          (at-location garden)
          $garden_searched
          (item-spawned-at garden royal_amulet)
          (not $amulet_found)))
      (actions
        (collect-item royal_amulet)
        (set-quest-var amulet_found true)
        (set-quest-var quest_stage 2)
        (set-memory has_royal_amulet true)
        (show-message "Успех! Вы нашли королевский амулет! Он тепло пульсирует в ваших руках.")))

    ;; Шаг 5: Вернуть амулет советнику
    (step return_amulet_to_advisor
      (description "Вернуть амулет королевскому советнику")
      (require
        (and
          (at-location throne_room)
          (= $quest_stage 2)
          $amulet_found
          (player-has-item royal_amulet)))
      (actions
        (interact royal_advisor return_royal_amulet)
        (take-items royal_amulet)
        (set-memory amulet_returned true)
        (set-memory coronation_ready true)
        (show-message "Советник: 'Отличная работа, Ваше Высочество! Королевство в вашем долгу. Коронация может состояться!'")
        (complete-quest))))

  ;; Тестовая последовательность для полного выполнения
  (test-sequence complete-quest-test
    (description "Полный тест прохождения квеста")
    (assert-initial-state)
    (move throne_room)
    (assert (can-start-quest princess_lost_relic))
    (interact royal_advisor report_missing_amulet)
    (assert (quest-started princess_lost_relic))
    (assert (= (quest-stage) 1))
    
    ;; Исследование в библиотеке
    (move library)
    (interact librarian research_ancient_texts)
    (assert $amulet_research_done)
    (assert (get-memory amulet_garden_connection))
    
    ;; Поиск в саду
    (move garden)
    (search-location royal_gardens)
    (assert $garden_searched)
    (collect-item royal_amulet)
    (assert (player-has-item royal_amulet))
    (assert $amulet_found)
    
    ;; Возвращение амулета
    (move throne_room)
    (interact royal_advisor return_royal_amulet)
    (assert (get-memory amulet_returned))
    (assert (quest-completed princess_lost_relic)))

  (on-complete
    (set-memory completed_relic_quest true)
    (set-memory royal_legitimacy_restored true)
    (show-message "Королевская реликвия возвращена! Коронация может состояться по всем древним традициям."))))

;; Отдельные тестовые случаи
(test-case negative-test-wrong-outfit
  (description "Проверка невозможности начать квест в неправильной одежде")
  (move throne_room)
  (change-outfit common)
  (assert (not (can-start-quest princess_lost_relic)))
  (assert-error (interact royal_advisor report_missing_amulet)))

(test-case negative-test-wrong-location
  (description "Проверка невозможности начать квест в неправильной локации")
  (move kitchen)
  (change-outfit princess_dress)
  (assert (not (can-start-quest princess_lost_relic)))
  (assert-error (interact royal_advisor report_missing_amulet)))

(test-case boundary-test-search-without-info
  (description "Проверка поиска без предварительного исследования")
  (set-state 
    (location garden)
    (outfit princess_dress)
    (quest-stage 1)
    (amulet_research_done false))
  (search-location royal_gardens)
  (assert $garden_searched)
  (assert (get-memory need_more_info))
  (assert (not (item-spawned-at garden royal_amulet))))

(test-case alternative-test-direct-search
  (description "Альтернативный путь: прямой поиск в саду")
  (set-state 
    (location garden)
    (outfit princess_dress)
    (quest-stage 1)
    (amulet_research_done true))
  (search-location royal_gardens)
  (assert (item-spawned-at garden royal_amulet))
  (collect-item royal_amulet)
  (assert (player-has-item royal_amulet)))