;; Функциональное ядро: Трансформеры состояния
;; game/functional/core/state-transformers.scm

(define-library (game functional core transformers)
  (export state-transformer?
          compose-transformers
          identity-transformer
          character-transformer
          world-transformer
          quest-transformer
          move-character
          add-item-to-character
          remove-item-from-character
          set-character-outfit
          set-character-stat
          update-character-dialogue-state
          add-npc-memory
          set-npc-dialogue-state
          add-world-event
          set-world-time
          start-quest
          advance-quest-step
          complete-quest
          set-quest-memory)
  (import (scheme base)
          (game functional core state))
  
  (begin
    ;; ======================================
    ;; TRANSFORMER INFRASTRUCTURE
    ;; ======================================
    
    ;; Предикат для проверки трансформера состояния
    (define (state-transformer? f)
      (and (procedure? f)
           (>= (procedure-arity f) 1)))
    
    ;; Композиция трансформеров (применяются слева направо)
    (define (compose-transformers . transformers)
      (lambda (state)
        (fold-left (lambda (current-state transformer)
                     (transformer current-state))
                   state
                   transformers)))
    
    ;; Тождественный трансформер (не изменяет состояние)
    (define (identity-transformer state)
      state)
    
    ;; ======================================
    ;; GENERIC TRANSFORMERS
    ;; ======================================
    
    ;; Трансформер для персонажа
    (define (character-transformer character-id updater)
      (lambda (state)
        (let ((characters (game-state-characters state)))
          (make-game-state
            (game-state-scene state)
            (assoc-update characters character-id updater)
            (game-state-world state)
            (game-state-quests state)))))
    
    ;; Трансформер для мира
    (define (world-transformer updater)
      (lambda (state)
        (make-game-state
          (game-state-scene state)
          (game-state-characters state)
          (updater (game-state-world state))
          (game-state-quests state))))
    
    ;; Трансформер для системы квестов
    (define (quest-transformer updater)
      (lambda (state)
        (make-game-state
          (game-state-scene state)
          (game-state-characters state)
          (game-state-world state)
          (updater (game-state-quests state)))))
    
    ;; ======================================
    ;; CHARACTER TRANSFORMERS
    ;; ======================================
    
    ;; Переместить персонажа в новую локацию
    (define (move-character character-id new-location)
      (character-transformer character-id
        (lambda (character)
          (make-character
            (character-id character)
            new-location
            (character-outfit character)
            (character-inventory character)
            (character-stats character)
            (character-dialogue-state character)))))
    
    ;; Добавить предмет в инвентарь персонажа
    (define (add-item-to-character character-id item)
      (character-transformer character-id
        (lambda (character)
          (make-character
            (character-id character)
            (character-location character)
            (character-outfit character)
            (cons item (character-inventory character))
            (character-stats character)
            (character-dialogue-state character)))))
    
    ;; Удалить предмет из инвентаря персонажа
    (define (remove-item-from-character character-id item)
      (character-transformer character-id
        (lambda (character)
          (make-character
            (character-id character)
            (character-location character)
            (character-outfit character)
            (filter (lambda (inv-item) (not (equal? inv-item item)))
                   (character-inventory character))
            (character-stats character)
            (character-dialogue-state character)))))
    
    ;; Изменить наряд персонажа
    (define (set-character-outfit character-id new-outfit)
      (character-transformer character-id
        (lambda (character)
          (make-character
            (character-id character)
            (character-location character)
            new-outfit
            (character-inventory character)
            (character-stats character)
            (character-dialogue-state character)))))
    
    ;; Установить статистику персонажа
    (define (set-character-stat character-id stat-name value)
      (character-transformer character-id
        (lambda (character)
          (let ((updated-stats (assoc-set (character-stats character) stat-name value)))
            (make-character
              (character-id character)
              (character-location character)
              (character-outfit character)
              (character-inventory character)
              updated-stats
              (character-dialogue-state character))))))
    
    ;; Обновить состояние диалога персонажа
    (define (update-character-dialogue-state character-id new-dialogue-state)
      (character-transformer character-id
        (lambda (character)
          (make-character
            (character-id character)
            (character-location character)
            (character-outfit character)
            (character-inventory character)
            (character-stats character)
            new-dialogue-state))))
    
    ;; ======================================
    ;; WORLD TRANSFORMERS
    ;; ======================================
    
    ;; Добавить память NPC
    (define (add-npc-memory npc-id key value)
      (world-transformer
        (lambda (world)
          (let* ((npcs (world-npcs world))
                 (npc-data (cdr (assq npc-id npcs)))
                 (memory (cdr (assq 'memory npc-data)))
                 (updated-memory (assoc-set memory key value))
                 (updated-npc-data (assoc-set npc-data 'memory updated-memory))
                 (updated-npcs (assoc-set npcs npc-id updated-npc-data)))
            (make-world
              (world-locations world)
              updated-npcs
              (world-time world)
              (world-events world))))))
    
    ;; Установить состояние диалога NPC
    (define (set-npc-dialogue-state npc-id state)
      (world-transformer
        (lambda (world)
          (let* ((npcs (world-npcs world))
                 (npc-data (cdr (assq npc-id npcs)))
                 (updated-npc-data (assoc-set npc-data 'dialogue_state state))
                 (updated-npcs (assoc-set npcs npc-id updated-npc-data)))
            (make-world
              (world-locations world)
              updated-npcs
              (world-time world)
              (world-events world))))))
    
    ;; Добавить событие в мир
    (define (add-world-event event)
      (world-transformer
        (lambda (world)
          (make-world
            (world-locations world)
            (world-npcs world)
            (world-time world)
            (cons event (world-events world))))))
    
    ;; Установить время в мире
    (define (set-world-time new-time)
      (world-transformer
        (lambda (world)
          (make-world
            (world-locations world)
            (world-npcs world)
            new-time
            (world-events world)))))
    
    ;; ======================================
    ;; QUEST TRANSFORMERS
    ;; ======================================
    
    ;; Начать квест для персонажа
    (define (start-quest character-id quest-data)
      (quest-transformer
        (lambda (quest-system)
          (let ((active-quests (quest-system-active quest-system))
                (quest-entry (cons character-id quest-data)))
            (make-quest-system
              (cons quest-entry active-quests)
              (quest-system-completed quest-system)
              (quest-system-global-memory quest-system))))))
    
    ;; Продвинуть шаг квеста
    (define (advance-quest-step character-id quest-id new-step)
      (quest-transformer
        (lambda (quest-system)
          (let* ((active-quests (quest-system-active quest-system))
                 (character-quests (cdr (assq character-id active-quests)))
                 (quest (assq quest-id character-quests))
                 (updated-quest (if quest
                                  (assoc-set quest 'current-step new-step)
                                  #f)))
            (if updated-quest
              (let* ((updated-character-quests 
                       (assoc-set character-quests quest-id updated-quest))
                     (updated-active-quests 
                       (assoc-set active-quests character-id updated-character-quests)))
                (make-quest-system
                  updated-active-quests
                  (quest-system-completed quest-system)
                  (quest-system-global-memory quest-system)))
              quest-system)))))
    
    ;; Завершить квест
    (define (complete-quest character-id quest-id)
      (quest-transformer
        (lambda (quest-system)
          (let* ((active-quests (quest-system-active quest-system))
                 (completed-quests (quest-system-completed quest-system))
                 (character-quests (cdr (assq character-id active-quests)))
                 (quest (assq quest-id character-quests)))
            (if quest
              (let* ((remaining-character-quests 
                       (filter (lambda (q) (not (equal? (car q) quest-id)))
                              character-quests))
                     (updated-active-quests 
                       (assoc-set active-quests character-id remaining-character-quests))
                     (completed-entry (cons character-id quest))
                     (updated-completed-quests 
                       (cons completed-entry completed-quests)))
                (make-quest-system
                  updated-active-quests
                  updated-completed-quests
                  (quest-system-global-memory quest-system)))
              quest-system)))))
    
    ;; Установить глобальную память квестов
    (define (set-quest-memory key value)
      (quest-transformer
        (lambda (quest-system)
          (let ((updated-memory 
                  (assoc-set (quest-system-global-memory quest-system) key value)))
            (make-quest-system
              (quest-system-active quest-system)
              (quest-system-completed quest-system)
              updated-memory)))))
    
    ;; ======================================
    ;; UTILITY FUNCTIONS
    ;; ======================================
    
    ;; Обновить значение в ассоциативном списке
    (define (assoc-set alist key value)
      (let ((existing (assq key alist)))
        (if existing
          (map (lambda (pair)
                 (if (equal? (car pair) key)
                   (cons key value)
                   pair))
               alist)
          (cons (cons key value) alist))))
    
    ;; Обновить значение в ассоциативном списке с помощью функции
    (define (assoc-update alist key updater)
      (let ((existing (assq key alist)))
        (if existing
          (map (lambda (pair)
                 (if (equal? (car pair) key)
                   (cons key (updater (cdr pair)))
                   pair))
               alist)
          alist)))))