;; Функциональная система квестов
;; game/functional/domains/quest.scm

(define-library (game functional domains quest)
  (export quest-available?
          quest-requirements-met?
          quest-active?
          quest-completed?
          start-quest-effect
          advance-quest-effect
          complete-quest-effect
          check-quest-triggers-effect
          execute-quest-action-effect
          get-quest-dialogues-effect
          validate-quest-step-effect)
  (import (scheme base)
          (game functional core state)
          (game functional core effects)
          (game functional core transformers))
  
  (begin
    ;; ======================================
    ;; QUEST PREDICATES (PURE FUNCTIONS)
    ;; ======================================
    
    ;; Доступен ли квест для персонажа
    (define (quest-available? state character-id quest-id)
      (and (not (quest-active? state character-id quest-id))
           (not (quest-completed? state character-id quest-id))
           (quest-triggers-met? state character-id quest-id)))
    
    ;; Выполнены ли требования для шага квеста
    (define (quest-requirements-met? state character-id quest-id step-id)
      (let ((quest (get-active-quest state character-id quest-id)))
        (and quest
             (equal? (quest-current-step quest) step-id)
             (all-step-requirements-satisfied? state character-id quest-id step-id))))
    
    ;; Активен ли квест для персонажа
    (define (quest-active? state character-id quest-id)
      (not (null? (get-active-quest state character-id quest-id))))
    
    ;; Завершен ли квест для персонажа
    (define (quest-completed? state character-id quest-id)
      (let ((quest-system (get-quest-state state)))
        (let ((completed-quests (quest-system-completed quest-system)))
          (not (null? (find-completed-quest completed-quests character-id quest-id))))))
    
    ;; ======================================
    ;; QUEST EFFECTS (MONADIC ACTIONS)
    ;; ======================================
    
    ;; Эффект начала квеста
    (define (start-quest-effect character-id quest-id)
      (do-effects
        ;; Валидация возможности старта
        (state <- (get-state-effect))
        (_ <- (if (quest-available? state character-id quest-id)
                (return-effect #t)
                (error-effect (format "Quest ~a not available for ~a" quest-id character-id))))
        
        ;; Загрузка данных квеста
        (quest-data <- (load-quest-data-effect quest-id))
        
        ;; Логирование
        (_ <- (log-effect (format "Starting quest ~a for ~a" quest-id character-id)))
        
        ;; Создание активного квеста
        (active-quest <- (return-effect (make-active-quest quest-id quest-data)))
        
        ;; Обновление состояния
        (_ <- (update-state-effect (start-quest character-id active-quest)))
        
        ;; Выполнение начальных действий квеста
        (_ <- (execute-quest-start-actions-effect character-id quest-data))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'quest-started character-id quest-id)))
        
        ;; Установка глобальной памяти квеста
        (_ <- (update-state-effect (set-quest-memory quest-id #t)))
        
        (return active-quest)))
    
    ;; Эффект продвижения квеста
    (define (advance-quest-effect character-id quest-id)
      (do-effects
        ;; Получение текущего шага
        (current-step <- (get-current-quest-step-effect character-id quest-id))
        
        ;; Валидация возможности продвижения
        (state <- (get-state-effect))
        (_ <- (if (quest-requirements-met? state character-id quest-id current-step)
                (return-effect #t)
                (error-effect (format "Requirements not met for quest ~a step ~a" quest-id current-step))))
        
        ;; Вычисление следующего шага
        (next-step <- (calculate-next-step-effect quest-id current-step))
        
        ;; Логирование
        (_ <- (log-effect (format "Advancing quest ~a from step ~a to ~a" quest-id current-step next-step)))
        
        ;; Выполнение действий текущего шага
        (_ <- (execute-step-completion-actions-effect character-id quest-id current-step))
        
        ;; Обновление шага квеста
        (_ <- (update-state-effect (advance-quest-step character-id quest-id next-step)))
        
        ;; Выполнение действий нового шага
        (_ <- (execute-step-start-actions-effect character-id quest-id next-step))
        
        ;; Проверка на завершение квеста
        (quest-complete? <- (check-quest-completion-effect character-id quest-id next-step))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'quest-advanced character-id quest-id current-step next-step)))
        
        ;; Автоматическое завершение если это финальный шаг
        (_ <- (if quest-complete?
                (complete-quest-effect character-id quest-id)
                (return-effect #t)))
        
        (return next-step)))
    
    ;; Эффект завершения квеста
    (define (complete-quest-effect character-id quest-id)
      (do-effects
        ;; Получение наград квеста
        (rewards <- (get-quest-rewards-effect quest-id))
        
        ;; Логирование
        (_ <- (log-effect (format "Completing quest ~a for ~a" quest-id character-id)))
        
        ;; Применение наград
        (_ <- (apply-quest-rewards-effect character-id rewards))
        
        ;; Выполнение финальных действий квеста
        (_ <- (execute-quest-completion-actions-effect character-id quest-id))
        
        ;; Перемещение квеста в завершенные
        (_ <- (update-state-effect (complete-quest character-id quest-id)))
        
        ;; Триггер события
        (_ <- (trigger-event-effect (list 'quest-completed character-id quest-id rewards)))
        
        ;; Проверка разблокировки новых квестов
        (_ <- (check-unlocked-quests-effect character-id quest-id))
        
        (return rewards)))
    
    ;; Эффект проверки триггеров квестов
    (define (check-quest-triggers-effect character-id trigger-type trigger-data)
      (do-effects
        (state <- (get-state-effect))
        
        ;; Получение всех потенциальных квестов для проверки
        (potential-quests <- (get-potential-quests-effect character-id))
        
        ;; Проверка каждого квеста на соответствие триггеру
        (triggered-quests <- (filter-triggered-quests-effect potential-quests trigger-type trigger-data))
        
        ;; Автоматический старт квестов с подходящими триггерами
        (_ <- (sequence-effects 
                (map (lambda (quest-id) (start-quest-effect character-id quest-id))
                     triggered-quests)))
        
        ;; Проверка активных квестов на прогресс
        (active-quests <- (get-active-quests-effect character-id))
        (_ <- (check-active-quests-progress-effect character-id active-quests trigger-type trigger-data))
        
        (return triggered-quests)))
    
    ;; Эффект выполнения квестового действия
    (define (execute-quest-action-effect character-id action-type action-data)
      (case action-type
        ((set-memory)
         (do-effects
           (key <- (return-effect (car action-data)))
           (value <- (return-effect (cadr action-data)))
           (_ <- (update-state-effect (add-npc-memory character-id key value)))
           (return #t)))
        
        ((add-dialogue)
         (do-effects
           (npc-id <- (return-effect (car action-data)))
           (dialogue-data <- (return-effect (cdr action-data)))
           (_ <- (add-quest-dialogue-effect character-id npc-id dialogue-data))
           (return #t)))
        
        ((give-item)
         (do-effects
           (item <- (return-effect (car action-data)))
           (_ <- (add-item-effect character-id item))
           (return #t)))
        
        ((reveal-location)
         (do-effects
           (location <- (return-effect (car action-data)))
           (_ <- (reveal-location-effect location))
           (return #t)))
        
        ((trigger-scene)
         (do-effects
           (scene-id <- (return-effect (car action-data)))
           (_ <- (trigger-scene-effect scene-id))
           (return #t)))
        
        (else
         (error-effect (format "Unknown quest action: ~a" action-type)))))
    
    ;; Эффект получения квестовых диалогов
    (define (get-quest-dialogues-effect character-id npc-id)
      (do-effects
        (state <- (get-state-effect))
        (active-quests <- (get-active-quests-effect character-id))
        
        ;; Собрать диалоги от всех активных квестов
        (quest-dialogues <- (sequence-effects
                             (map (lambda (quest)
                                    (get-quest-npc-dialogues-effect quest npc-id))
                                  active-quests)))
        
        ;; Фильтрация доступных диалогов
        (available-dialogues <- (filter-available-dialogues-effect character-id quest-dialogues))
        
        (return (apply append available-dialogues))))
    
    ;; Эффект валидации шага квеста
    (define (validate-quest-step-effect character-id quest-id step-id)
      (do-effects
        (state <- (get-state-effect))
        (quest <- (return-effect (get-active-quest state character-id quest-id)))
        
        (_ <- (if quest
                (return-effect #t)
                (error-effect (format "Quest ~a not active for ~a" quest-id character-id))))
        
        ;; Проверка корректности шага
        (valid-step? <- (check-step-validity-effect quest step-id))
        (_ <- (if valid-step?
                (return-effect #t)
                (error-effect (format "Invalid step ~a for quest ~a" step-id quest-id))))
        
        ;; Проверка требований шага
        (requirements-met? <- (check-step-requirements-effect character-id quest-id step-id))
        (_ <- (if requirements-met?
                (return-effect #t)
                (error-effect (format "Requirements not met for step ~a" step-id))))
        
        (return #t)))
    
    ;; ======================================
    ;; HELPER FUNCTIONS
    ;; ======================================
    
    ;; Проверка выполнения триггеров квеста
    (define (quest-triggers-met? state character-id quest-id)
      ;; Упрощенная проверка - в реальности нужно загружать квест и проверять триггеры
      #t)
    
    ;; Получение активного квеста
    (define (get-active-quest state character-id quest-id)
      (let ((quest-system (get-quest-state state)))
        (let ((active-quests (quest-system-active quest-system)))
          (let ((character-quests (cdr (assq character-id active-quests))))
            (and character-quests
                 (assq quest-id character-quests))))))
    
    ;; Поиск завершенного квеста
    (define (find-completed-quest completed-quests character-id quest-id)
      (let ((character-completed (filter (lambda (entry)
                                          (equal? (car entry) character-id))
                                        completed-quests)))
        (and (not (null? character-completed))
             (assq quest-id (cdr (car character-completed))))))
    
    ;; Проверка выполнения всех требований шага
    (define (all-step-requirements-satisfied? state character-id quest-id step-id)
      ;; Упрощенная проверка - нужно загружать данные квеста и проверять требования
      #t)
    
    ;; Создание активного квеста
    (define (make-active-quest quest-id quest-data)
      (list quest-id
            'title (quest-title quest-data)
            'current-step (quest-initial-step quest-data)
            'started-at (current-time)
            'data quest-data))
    
    ;; Получение заголовка квеста
    (define (quest-title quest-data)
      (cdr (assq 'title quest-data)))
    
    ;; Получение начального шага квеста
    (define (quest-initial-step quest-data)
      (let ((steps (cdr (assq 'steps quest-data))))
        (and (not (null? steps))
             (car (car steps)))))
    
    ;; Текущее время (заглушка)
    (define (current-time)
      0)
    
    ;; Заглушки для функций, которые будут реализованы
    (define (load-quest-data-effect quest-id)
      (return-effect (list (cons 'title "Test Quest")
                          (cons 'steps '((step1 "First step"))))))
    
    (define (execute-quest-start-actions-effect character-id quest-data)
      (return-effect #t))
    
    (define (get-current-quest-step-effect character-id quest-id)
      (return-effect 'step1))
    
    (define (calculate-next-step-effect quest-id current-step)
      (return-effect 'step2))
    
    (define (execute-step-completion-actions-effect character-id quest-id step)
      (return-effect #t))
    
    (define (execute-step-start-actions-effect character-id quest-id step)
      (return-effect #t))
    
    (define (check-quest-completion-effect character-id quest-id step)
      (return-effect #f))
    
    (define (get-quest-rewards-effect quest-id)
      (return-effect '()))
    
    (define (apply-quest-rewards-effect character-id rewards)
      (return-effect #t))
    
    (define (execute-quest-completion-actions-effect character-id quest-id)
      (return-effect #t))
    
    (define (check-unlocked-quests-effect character-id completed-quest-id)
      (return-effect #t))
    
    (define (get-potential-quests-effect character-id)
      (return-effect '()))
    
    (define (filter-triggered-quests-effect quests trigger-type trigger-data)
      (return-effect '()))
    
    (define (get-active-quests-effect character-id)
      (return-effect '()))
    
    (define (check-active-quests-progress-effect character-id quests trigger-type trigger-data)
      (return-effect #t))
    
    (define (add-quest-dialogue-effect character-id npc-id dialogue-data)
      (return-effect #t))
    
    (define (add-item-effect character-id item)
      (return-effect #t))
    
    (define (reveal-location-effect location)
      (return-effect #t))
    
    (define (trigger-scene-effect scene-id)
      (return-effect #t))
    
    (define (get-quest-npc-dialogues-effect quest npc-id)
      (return-effect '()))
    
    (define (filter-available-dialogues-effect character-id dialogues)
      (return-effect dialogues))
    
    (define (check-step-validity-effect quest step-id)
      (return-effect #t))
    
    (define (check-step-requirements-effect character-id quest-id step-id)
      (return-effect #t))))