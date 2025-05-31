;; Функциональная логика персонажей
;; game/functional/domains/character.scm

(define-library (game functional domains character)
  (export character-can-move?
          character-can-interact? 
          character-has-item?
          character-at-location?
          character-wearing-outfit?
          move-character-effect
          add-item-effect
          remove-item-effect
          swap-outfits-effect
          interact-with-npc-effect
          change-outfit-effect
          update-character-stat-effect
          get-character-choices-effect)
  (import (scheme base)
          (game functional core state)
          (game functional core effects)
          (game functional core transformers))
  
  (begin
    ;; ======================================
    ;; CHARACTER PREDICATES (PURE FUNCTIONS)
    ;; ======================================
    
    ;; Может ли персонаж переместиться в локацию
    (define (character-can-move? state character-id location)
      (and (character-exists? state character-id)
           (location-accessible? state location character-id)
           (not (character-in-dialogue? state character-id))
           (not (location-equals? state character-id location))))
    
    ;; Может ли персонаж взаимодействовать с NPC
    (define (character-can-interact? state character-id npc-id)
      (and (character-exists? state character-id)
           (npc-exists? state npc-id)
           (npc-at-same-location? state character-id npc-id)
           (npc-available? state npc-id)
           (not (character-in-dialogue? state character-id))))
    
    ;; Есть ли у персонажа предмет
    (define (character-has-item? state character-id item)
      (let ((character (get-character-data state character-id)))
        (and character
             (member item (character-inventory character)))))
    
    ;; Находится ли персонаж в локации
    (define (character-at-location? state character-id location)
      (let ((character (get-character-data state character-id)))
        (and character
             (equal? (character-location character) location))))
    
    ;; Носит ли персонаж определенный наряд
    (define (character-wearing-outfit? state character-id outfit)
      (let ((character (get-character-data state character-id)))
        (and character
             (equal? (character-outfit character) outfit))))
    
    ;; ======================================
    ;; CHARACTER EFFECTS (MONADIC ACTIONS)
    ;; ======================================
    
    ;; Эффект перемещения персонажа
    (define (move-character-effect character-id location)
      (do-effects
        ;; Валидация перемещения
        (state <- (get-state-effect))
        (_ <- (if (character-can-move? state character-id location)
                (return-effect #t)
                (error-effect (format "Cannot move ~a to ~a" character-id location))))
        
        ;; Логирование
        (_ <- (log-effect (format "~a moves to ~a" character-id location)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (move-character character-id location)))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'character-moved character-id location)))
        
        ;; Проверка на автоматические взаимодействия
        (_ <- (check-location-triggers-effect character-id location))
        
        (return #t)))
    
    ;; Эффект добавления предмета
    (define (add-item-effect character-id item)
      (do-effects
        ;; Проверка вместимости инвентаря
        (state <- (get-state-effect))
        (character <- (return-effect (get-character-data state character-id)))
        (inventory-size <- (return-effect (length (character-inventory character))))
        (_ <- (if (< inventory-size 10) ; максимум 10 предметов
                (return-effect #t)
                (error-effect "Inventory is full")))
        
        ;; Логирование
        (_ <- (log-effect (format "~a receives ~a" character-id item)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (add-item-to-character character-id item)))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'item-added character-id item)))
        
        ;; Проверка квестовых триггеров
        (_ <- (check-item-quest-triggers-effect character-id item))
        
        (return #t)))
    
    ;; Эффект удаления предмета
    (define (remove-item-effect character-id item)
      (do-effects
        ;; Проверка наличия предмета
        (state <- (get-state-effect))
        (_ <- (if (character-has-item? state character-id item)
                (return-effect #t)
                (error-effect (format "~a does not have ~a" character-id item))))
        
        ;; Логирование
        (_ <- (log-effect (format "~a loses ~a" character-id item)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (remove-item-from-character character-id item)))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'item-removed character-id item)))
        
        (return #t)))
    
    ;; Эффект обмена нарядами
    (define (swap-outfits-effect char1 char2)
      (do-effects
        ;; Получение текущих нарядов
        (state <- (get-state-effect))
        (character1 <- (return-effect (get-character-data state char1)))
        (character2 <- (return-effect (get-character-data state char2)))
        (outfit1 <- (return-effect (character-outfit character1)))
        (outfit2 <- (return-effect (character-outfit character2)))
        
        ;; Проверка возможности обмена
        (_ <- (if (and (character-at-location? state char1 (character-location character1))
                      (character-at-location? state char2 (character-location character2))
                      (equal? (character-location character1) (character-location character2)))
                (return-effect #t)
                (error-effect "Characters must be in the same location to swap outfits")))
        
        ;; Логирование
        (_ <- (log-effect (format "~a and ~a swap outfits" char1 char2)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect 
                (compose-transformers
                  (set-character-outfit char1 outfit2)
                  (set-character-outfit char2 outfit1))))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'outfits-swapped char1 char2 outfit1 outfit2)))
        
        (return (list outfit1 outfit2))))
    
    ;; Эффект взаимодействия с NPC
    (define (interact-with-npc-effect character-id npc-id)
      (do-effects
        ;; Валидация взаимодействия
        (state <- (get-state-effect))
        (_ <- (if (character-can-interact? state character-id npc-id)
                (return-effect #t)
                (error-effect (format "Cannot interact: ~a with ~a" character-id npc-id))))
        
        ;; Логирование
        (_ <- (log-effect (format "~a interacts with ~a" character-id npc-id)))
        
        ;; Получение доступных диалогов
        (dialogues <- (get-available-dialogues-effect character-id npc-id))
        
        ;; Обновление состояния диалога персонажа
        (_ <- (update-state-effect 
                (update-character-dialogue-state character-id
                  (list 'talking-to npc-id dialogues))))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'dialogue-started character-id npc-id)))
        
        (return dialogues)))
    
    ;; Эффект смены наряда
    (define (change-outfit-effect character-id new-outfit)
      (do-effects
        ;; Проверка доступности наряда
        (state <- (get-state-effect))
        (_ <- (if (outfit-available? state character-id new-outfit)
                (return-effect #t)
                (error-effect (format "Outfit ~a not available for ~a" new-outfit character-id))))
        
        ;; Логирование
        (_ <- (log-effect (format "~a changes to ~a" character-id new-outfit)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (set-character-outfit character-id new-outfit)))
        
        ;; Триггер события  
        (_ <- (trigger-event-effect (list 'outfit-changed character-id new-outfit)))
        
        (return #t)))
    
    ;; Эффект обновления статистики персонажа
    (define (update-character-stat-effect character-id stat-name delta)
      (do-effects
        ;; Получение текущего значения
        (state <- (get-state-effect))
        (character <- (return-effect (get-character-data state character-id)))
        (current-value <- (return-effect (get-character-stat character stat-name)))
        (new-value <- (return-effect (+ current-value delta)))
        
        ;; Применение ограничений
        (clamped-value <- (return-effect (clamp new-value 0 100)))
        
        ;; Логирование
        (_ <- (log-effect (format "~a ~a: ~a -> ~a" character-id stat-name current-value clamped-value)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (set-character-stat character-id stat-name clamped-value)))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'stat-changed character-id stat-name clamped-value)))
        
        (return clamped-value)))
    
    ;; Эффект получения доступных выборов для персонажа
    (define (get-character-choices-effect character-id)
      (do-effects
        (state <- (get-state-effect))
        (character <- (return-effect (get-character-data state character-id)))
        (location <- (return-effect (character-location character)))
        
        ;; Получение выборов перемещения
        (movement-choices <- (get-movement-choices-effect character-id location))
        
        ;; Получение выборов взаимодействия
        (interaction-choices <- (get-interaction-choices-effect character-id location))
        
        ;; Получение квестовых выборов
        (quest-choices <- (get-quest-choices-effect character-id))
        
        ;; Получение системных выборов (смена наряда и т.д.)
        (system-choices <- (get-system-choices-effect character-id))
        
        (return (append movement-choices 
                       interaction-choices 
                       quest-choices 
                       system-choices))))
    
    ;; ======================================
    ;; HELPER FUNCTIONS
    ;; ======================================
    
    ;; Существует ли персонаж
    (define (character-exists? state character-id)
      (not (null? (get-character-data state character-id))))
    
    ;; Доступна ли локация для персонажа
    (define (location-accessible? state location character-id)
      (let ((world (get-world-state state))
            (character (get-character-data state character-id)))
        (and character
             (let ((locations (world-locations world)))
               (let ((location-data (cdr (assq location locations))))
                 (cdr (assq 'accessible location-data)))))))
    
    ;; Находится ли персонаж в диалоге
    (define (character-in-dialogue? state character-id)
      (let ((character (get-character-data state character-id)))
        (and character
             (let ((dialogue-state (character-dialogue-state character)))
               (and (not (null? dialogue-state))
                    (equal? (car dialogue-state) 'talking-to))))))
    
    ;; Равна ли локация персонажа указанной
    (define (location-equals? state character-id location)
      (let ((character (get-character-data state character-id)))
        (and character
             (equal? (character-location character) location))))
    
    ;; Существует ли NPC
    (define (npc-exists? state npc-id)
      (let ((world (get-world-state state)))
        (not (null? (assq npc-id (world-npcs world))))))
    
    ;; Находится ли NPC в той же локации что и персонаж
    (define (npc-at-same-location? state character-id npc-id)
      (let ((character (get-character-data state character-id))
            (world (get-world-state state)))
        (and character
             (let ((npc-data (cdr (assq npc-id (world-npcs world)))))
               (and npc-data
                    (equal? (character-location character)
                           (cdr (assq 'location npc-data))))))))
    
    ;; Доступен ли NPC для взаимодействия
    (define (npc-available? state npc-id)
      (let ((world (get-world-state state)))
        (let ((npc-data (cdr (assq npc-id (world-npcs world)))))
          (and npc-data
               (equal? (cdr (assq 'dialogue_state npc-data)) 'available)))))
    
    ;; Получить статистику персонажа
    (define (get-character-stat character stat-name)
      (let ((stats (character-stats character)))
        (cdr (assq stat-name stats))))
    
    ;; Ограничить значение диапазоном
    (define (clamp value min-val max-val)
      (cond
        ((< value min-val) min-val)
        ((> value max-val) max-val)
        (else value)))
    
    ;; Форматирование строк (упрощенная версия)
    (define (format template . args)
      (define (replace-placeholders str replacements)
        (if (null? replacements)
          str
          (let ((placeholder "~a")
                (replacement (car replacements)))
            (replace-placeholders 
              (string-replace str placeholder (format-value replacement))
              (cdr replacements)))))
      (replace-placeholders template args))
    
    ;; Форматирование значения в строку
    (define (format-value value)
      (cond
        ((string? value) value)
        ((number? value) (number->string value))
        ((symbol? value) (symbol->string value))
        ((boolean? value) (if value "#t" "#f"))
        (else (object->string value))))
    
    ;; Заглушки для функций, которые будут реализованы в других модулях
    (define (check-location-triggers-effect character-id location)
      (return-effect #t))
    
    (define (check-item-quest-triggers-effect character-id item)  
      (return-effect #t))
    
    (define (get-available-dialogues-effect character-id npc-id)
      (return-effect '()))
    
    (define (outfit-available? state character-id outfit)
      #t)
    
    (define (get-movement-choices-effect character-id location)
      (return-effect '()))
    
    (define (get-interaction-choices-effect character-id location)
      (return-effect '()))
    
    (define (get-quest-choices-effect character-id)
      (return-effect '()))
    
    (define (get-system-choices-effect character-id)
      (return-effect '()))
    
    ;; Заглушка для string-replace
    (define (string-replace str old new)
      str)))