;; Функциональная версия квеста "Потерянная королевская реликвия"
;; game/functional/quests/princess_lost_relic_functional.scm

(define-library (game functional quests princess-lost-relic)
  (export princess-lost-relic-quest
          validate-princess-lost-relic
          test-princess-lost-relic)
  (import (scheme base)
          (game functional core state)
          (game functional core effects)
          (game functional core transformers)
          (game functional domains character)
          (game functional domains quest))
  
  (begin
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
    ;; ===============================================

    ;; ======================================
    ;; QUEST DEFINITION (FUNCTIONAL STYLE)
    ;; ======================================
    
    (define princess-lost-relic-quest
      (list
        'quest-id 'princess-lost-relic
        'title "Потерянная королевская реликвия"
        'description "Найти древний амулет, исчезнувший из сокровищницы"
        'character 'princess
        'initial-requirements (list 'at-location 'throne_room
                                   'outfit-is 'princess_dress
                                   'not-quest-active 'princess-lost-relic)
        'steps (list
          (quest-step-get-assignment)
          (quest-step-research-amulet)
          (quest-step-search-gardens)
          (quest-step-find-amulet)
          (quest-step-return-amulet))
        'completion-rewards (list 
          'memory-set 'completed_relic_quest #t
          'memory-set 'royal_legitimacy_restored #t
          'achievement 'royal-relic-finder)))

    ;; ======================================
    ;; QUEST STEPS (PURE FUNCTIONAL DEFINITIONS)
    ;; ======================================
    
    ;; Шаг 1: Получить задание от советника
    (define (quest-step-get-assignment)
      (list
        'step-id 'get_quest_from_advisor
        'description "Получить квест от королевского советника"
        'requirements (quest-step-requirements-1)
        'validation (quest-step-validation-1)
        'effects (quest-step-effects-1)))
    
    (define (quest-step-requirements-1)
      (lambda (state character-id)
        (and (character-at-location? state character-id 'throne_room)
             (character-wearing-outfit? state character-id 'princess_dress)
             (not (quest-active? state character-id 'princess-lost-relic)))))
    
    (define (quest-step-validation-1)
      (lambda (state character-id)
        (and (character-can-interact? state character-id 'royal_advisor)
             (character-wearing-outfit? state character-id 'princess_dress))))
    
    (define (quest-step-effects-1)
      (lambda (character-id)
        (do-effects
          ;; Начать квест
          (_ <- (start-quest-effect character-id 'princess-lost-relic))
          
          ;; Установить память квеста
          (_ <- (update-state-effect (set-quest-memory 'amulet_missing #t)))
          (_ <- (update-state-effect (set-quest-memory 'quest_urgency 'high)))
          
          ;; Добавить диалог с советником
          (_ <- (add-quest-dialogue-effect character-id 'royal_advisor
                  (list 'text "Ваше Высочество! Древний королевский амулет исчез! Коронация под угрозой!"
                        'quest_action 'start_relic_quest)))
          
          ;; Разблокировать локации
          (_ <- (reveal-location-effect 'library))
          (_ <- (reveal-location-effect 'garden))
          
          ;; Логирование
          (_ <- (log-effect "Quest 'princess-lost-relic' started - advisor briefing completed"))
          
          (return 'quest-started))))
    
    ;; Шаг 2: Исследовать историю амулета в библиотеке
    (define (quest-step-research-amulet)
      (list
        'step-id 'research_amulet_history
        'description "Исследовать историю амулета"
        'requirements (quest-step-requirements-2)
        'validation (quest-step-validation-2)
        'effects (quest-step-effects-2)))
    
    (define (quest-step-requirements-2)
      (lambda (state character-id)
        (and (character-at-location? state character-id 'library)
             (quest-active? state character-id 'princess-lost-relic)
             (not (quest-memory-get state 'amulet_research_done)))))
    
    (define (quest-step-validation-2)
      (lambda (state character-id)
        (character-can-interact? state character-id 'librarian)))
    
    (define (quest-step-effects-2)
      (lambda (character-id)
        (do-effects
          ;; Взаимодействие с библиотекарем
          (_ <- (interact-with-npc-effect character-id 'librarian))
          
          ;; Установить прогресс исследования
          (_ <- (update-state-effect (set-quest-memory 'amulet_research_done #t)))
          (_ <- (update-state-effect (set-quest-memory 'library_visited #t)))
          (_ <- (update-state-effect (set-quest-memory 'amulet_garden_connection #t)))
          (_ <- (update-state-effect (set-quest-memory 'ancient_knowledge_gained #t)))
          
          ;; Добавить специальный диалог
          (_ <- (add-quest-dialogue-effect character-id 'librarian
                  (list 'text "Амулет Королей... По древним текстам, он резонирует с королевскими садами. Поищите у фонтана."
                        'quest_action 'research_complete)))
          
          ;; Разблокировать специальную область
          (_ <- (reveal-location-effect 'fountain_area))
          
          ;; Продвинуть квест
          (_ <- (advance-quest-effect character-id 'princess-lost-relic))
          
          (return 'research-completed))))
    
    ;; Шаг 3: Обыскать королевские сады
    (define (quest-step-search-gardens)
      (list
        'step-id 'search_royal_gardens
        'description "Обыскать королевские сады"
        'requirements (quest-step-requirements-3)
        'validation (quest-step-validation-3)
        'effects (quest-step-effects-3)))
    
    (define (quest-step-requirements-3)
      (lambda (state character-id)
        (and (character-at-location? state character-id 'garden)
             (quest-active? state character-id 'princess-lost-relic)
             (not (quest-memory-get state 'garden_searched)))))
    
    (define (quest-step-validation-3)
      (lambda (state character-id)
        #t)) ; Поиск всегда возможен в саду
    
    (define (quest-step-effects-3)
      (lambda (character-id)
        (do-effects
          ;; Отметить поиск в саду
          (_ <- (update-state-effect (set-quest-memory 'garden_searched #t)))
          
          ;; Проверить, было ли проведено исследование
          (state <- (get-state-effect))
          (research-done <- (return-effect (quest-memory-get state 'amulet_research_done)))
          
          ;; Условное выполнение в зависимости от исследования
          (_ <- (if research-done
                  ;; Если исследование проведено - появляется амулет
                  (do-effects
                    (_ <- (spawn-item-effect 'garden 'royal_amulet))
                    (_ <- (log-effect "Using library knowledge, princess finds the amulet by the fountain!"))
                    (_ <- (trigger-event-effect (list 'amulet-discovered 'garden)))
                    (return 'amulet-spawned))
                  ;; Если нет - только подсказка
                  (do-effects
                    (_ <- (update-state-effect (set-quest-memory 'need_more_info #t)))
                    (_ <- (log-effect "Gardens are vast. Need more information about amulet location."))
                    (return 'need-more-info))))
          
          (return 'garden-searched))))
    
    ;; Шаг 4: Найти амулет
    (define (quest-step-find-amulet)
      (list
        'step-id 'find_royal_amulet
        'description "Найти и поднять королевский амулет"
        'requirements (quest-step-requirements-4)
        'validation (quest-step-validation-4)
        'effects (quest-step-effects-4)))
    
    (define (quest-step-requirements-4)
      (lambda (state character-id)
        (and (character-at-location? state character-id 'garden)
             (quest-memory-get state 'garden_searched)
             (item-available-at? state 'garden 'royal_amulet)
             (not (quest-memory-get state 'amulet_found)))))
    
    (define (quest-step-validation-4)
      (lambda (state character-id)
        (item-available-at? state 'garden 'royal_amulet)))
    
    (define (quest-step-effects-4)
      (lambda (character-id)
        (do-effects
          ;; Собрать амулет
          (_ <- (add-item-effect character-id 'royal_amulet))
          
          ;; Отметить находку
          (_ <- (update-state-effect (set-quest-memory 'amulet_found #t)))
          (_ <- (update-state-effect (set-quest-memory 'has_royal_amulet #t)))
          
          ;; Продвинуть квест к финальной стадии
          (_ <- (advance-quest-effect character-id 'princess-lost-relic))
          
          ;; Логирование
          (_ <- (log-effect "SUCCESS! Princess found the royal amulet! It pulses warmly in her hands."))
          (_ <- (trigger-event-effect (list 'amulet-collected character-id)))
          
          (return 'amulet-found))))
    
    ;; Шаг 5: Вернуть амулет советнику
    (define (quest-step-return-amulet)
      (list
        'step-id 'return_amulet_to_advisor
        'description "Вернуть амулет королевскому советнику"
        'requirements (quest-step-requirements-5)
        'validation (quest-step-validation-5)
        'effects (quest-step-effects-5)))
    
    (define (quest-step-requirements-5)
      (lambda (state character-id)
        (and (character-at-location? state character-id 'throne_room)
             (quest-memory-get state 'amulet_found)
             (character-has-item? state character-id 'royal_amulet))))
    
    (define (quest-step-validation-5)
      (lambda (state character-id)
        (and (character-can-interact? state character-id 'royal_advisor)
             (character-has-item? state character-id 'royal_amulet))))
    
    (define (quest-step-effects-5)
      (lambda (character-id)
        (do-effects
          ;; Взаимодействие с советником
          (_ <- (interact-with-npc-effect character-id 'royal_advisor))
          
          ;; Отдать амулет
          (_ <- (remove-item-effect character-id 'royal_amulet))
          
          ;; Установить финальную память
          (_ <- (update-state-effect (set-quest-memory 'amulet_returned #t)))
          (_ <- (update-state-effect (set-quest-memory 'coronation_ready #t)))
          
          ;; Добавить финальный диалог
          (_ <- (add-quest-dialogue-effect character-id 'royal_advisor
                  (list 'text "Отличная работа, Ваше Высочество! Королевство в вашем долгу. Коронация может состояться!"
                        'quest_action 'quest_complete)))
          
          ;; Завершить квест
          (_ <- (complete-quest-effect character-id 'princess-lost-relic))
          
          ;; Финальное событие
          (_ <- (trigger-event-effect (list 'quest-completed 'princess-lost-relic 'coronation-possible)))
          
          (return 'quest-completed))))

    ;; ======================================
    ;; QUEST VALIDATION (FUNCTIONAL TESTING)
    ;; ======================================
    
    ;; Валидация квеста - проверка целостности определения
    (define (validate-princess-lost-relic quest-definition)
      (and (list? quest-definition)
           (equal? (cadr (assq 'quest-id quest-definition)) 'princess-lost-relic)
           (string? (cadr (assq 'title quest-definition)))
           (equal? (cadr (assq 'character quest-definition)) 'princess)
           (list? (cadr (assq 'steps quest-definition)))
           (= (length (cadr (assq 'steps quest-definition))) 5)))

    ;; ======================================
    ;; FUNCTIONAL QUEST TESTS
    ;; ======================================
    
    ;; Тест полного прохождения квеста
    (define (test-princess-lost-relic)
      (test-complete-quest-sequence))
    
    ;; Тестовая последовательность
    (define (test-complete-quest-sequence)
      (lambda (initial-state)
        (do-effects
          ;; Проверка начального состояния
          (_ <- (validate-effect 
                  (lambda (state) 
                    (and (character-at-location? state 'princess 'throne_room)
                         (character-wearing-outfit? state 'princess 'princess_dress)))
                  "Invalid initial state for quest"))
          
          ;; Начало квеста
          (_ <- (log-effect "TEST: Starting princess lost relic quest"))
          (quest-started <- ((quest-step-effects-1) 'princess))
          (_ <- (validate-effect
                  (lambda (state) (quest-active? state 'princess 'princess-lost-relic))
                  "Quest should be active after starting"))
          
          ;; Исследование в библиотеке
          (_ <- (move-character-effect 'princess 'library))
          (research-result <- ((quest-step-effects-2) 'princess))
          (_ <- (validate-effect
                  (lambda (state) (quest-memory-get state 'amulet_research_done))
                  "Research should be completed"))
          
          ;; Поиск в саду
          (_ <- (move-character-effect 'princess 'garden))
          (search-result <- ((quest-step-effects-3) 'princess))
          (_ <- (validate-effect
                  (lambda (state) (quest-memory-get state 'garden_searched))
                  "Garden should be searched"))
          
          ;; Сбор амулета
          (collect-result <- ((quest-step-effects-4) 'princess))
          (_ <- (validate-effect
                  (lambda (state) 
                    (and (character-has-item? state 'princess 'royal_amulet)
                         (quest-memory-get state 'amulet_found)))
                  "Amulet should be collected"))
          
          ;; Возврат амулета
          (_ <- (move-character-effect 'princess 'throne_room))
          (completion-result <- ((quest-step-effects-5) 'princess))
          (_ <- (validate-effect
                  (lambda (state) 
                    (and (quest-completed? state 'princess 'princess-lost-relic)
                         (quest-memory-get state 'amulet_returned)))
                  "Quest should be completed"))
          
          (_ <- (log-effect "TEST: Princess lost relic quest completed successfully"))
          (return 'test-passed))))

    ;; ======================================
    ;; HELPER FUNCTIONS
    ;; ======================================
    
    ;; Получить память квеста (заглушка)
    (define (quest-memory-get state key)
      ;; В реальной реализации получаем из состояния квеста
      #f)
    
    ;; Проверить доступность предмета в локации (заглушка)
    (define (item-available-at? state location item)
      ;; В реальной реализации проверяем мировое состояние
      #t)
    
    ;; Эффект появления предмета (заглушка)
    (define (spawn-item-effect location item)
      (do-effects
        (_ <- (log-effect (format "Item ~a spawned at ~a" item location)))
        (_ <- (trigger-event-effect (list 'item-spawned location item)))
        (return #t)))
    
    ;; Добавление квестового диалога (заглушка)
    (define (add-quest-dialogue-effect character-id npc-id dialogue-data)
      (do-effects
        (_ <- (log-effect (format "Quest dialogue added for ~a with ~a" character-id npc-id)))
        (return #t)))
    
    ;; Разблокировка локации (заглушка)
    (define (reveal-location-effect location)
      (do-effects
        (_ <- (log-effect (format "Location ~a revealed" location)))
        (_ <- (trigger-event-effect (list 'location-revealed location)))
        (return #t)))
    
    ;; Форматирование (упрощенная версия)
    (define (format template . args)
      (define (replace-first str old new)
        (let ((pos (string-search old str)))
          (if pos
            (string-append 
              (substring str 0 pos)
              new
              (substring str (+ pos (string-length old))))
            str)))
      
      (fold-left (lambda (str arg)
                   (replace-first str "~a" (object->string arg)))
                 template
                 args))
    
    ;; Поиск подстроки (заглушка)
    (define (string-search needle haystack)
      #f) ; Упрощенная реализация
    
    ;; Конвертация объекта в строку (заглушка)
    (define (object->string obj)
      (cond
        ((string? obj) obj)
        ((symbol? obj) (symbol->string obj))
        ((number? obj) (number->string obj))
        ((boolean? obj) (if obj "#t" "#f"))
        (else "object")))))