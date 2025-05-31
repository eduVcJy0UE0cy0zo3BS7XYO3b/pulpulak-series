;; Функциональное ядро: Иммутабельные структуры игрового состояния
;; game/functional/core/game-state.scm

(define-library (game functional core state)
  (export make-initial-state
          game-state?
          game-state-scene
          game-state-characters  
          game-state-world
          game-state-quests
          get-character-data
          get-world-state
          get-quest-state
          character?
          character-id
          character-location
          character-outfit
          character-inventory
          character-stats
          character-dialogue-state
          world?
          world-locations
          world-npcs
          world-time
          world-events
          quest-system?
          quest-system-active
          quest-system-completed
          quest-system-global-memory)
  (import (scheme base)
          (scheme record))
  
  (begin
    ;; ======================================
    ;; RECORD TYPES (IMMUTABLE DATA STRUCTURES)
    ;; ======================================
    
    ;; Основное игровое состояние
    (define-record-type game-state
      (make-game-state scene characters world quests)
      game-state?
      (scene game-state-scene)
      (characters game-state-characters)
      (world game-state-world)
      (quests game-state-quests))
    
    ;; Структура персонажа
    (define-record-type character
      (make-character id location outfit inventory stats dialogue-state)
      character?
      (id character-id)
      (location character-location)
      (outfit character-outfit)
      (inventory character-inventory)
      (stats character-stats)
      (dialogue-state character-dialogue-state))
    
    ;; Структура мира
    (define-record-type world
      (make-world locations npcs time events)
      world?
      (locations world-locations)
      (npcs world-npcs)
      (time world-time)
      (events world-events))
    
    ;; Система квестов
    (define-record-type quest-system
      (make-quest-system active-quests completed-quests global-memory)
      quest-system?
      (active-quests quest-system-active)
      (completed-quests quest-system-completed)
      (global-memory quest-system-global-memory))
    
    ;; Структура квеста
    (define-record-type quest
      (make-quest id title character current-step steps metadata)
      quest?
      (id quest-id)
      (title quest-title)
      (character quest-character)
      (current-step quest-current-step)
      (steps quest-steps)
      (metadata quest-metadata))
    
    ;; ======================================
    ;; INITIAL STATE CONSTRUCTORS
    ;; ======================================
    
    ;; Создание начального состояния игры
    (define (make-initial-state room-id players)
      (make-game-state
        'coop_awakening
        (make-initial-characters players)
        (make-initial-world)
        (make-initial-quest-system)))
    
    ;; Создание начальных персонажей
    (define (make-initial-characters players)
      (list 
        (cons 'princess
              (make-character 
                'princess
                'princess_chamber
                'princess_dress
                '()
                (make-initial-character-stats 'princess)
                '()))
        (cons 'helper
              (make-character
                'helper
                'princess_chamber  
                'common_dress
                '(translation_earrings voice_medallion)
                (make-initial-character-stats 'helper)
                '()))))
    
    ;; Создание начальной статистики персонажа
    (define (make-initial-character-stats character-type)
      (case character-type
        ((princess) 
         '((loyalty . 50)
           (knowledge . 30)
           (charm . 70)
           (secrets_revealed . 0)))
        ((helper)
         '((loyalty . 50)
           (knowledge . 60)
           (charm . 40)
           (secrets_revealed . 0)))
        (else '())))
    
    ;; Создание начального мира
    (define (make-initial-world)
      (make-world
        (make-initial-locations)
        (make-initial-npcs)
        'early_morning
        '()))
    
    ;; Начальные локации
    (define (make-initial-locations)
      '((princess_chamber . ((npcs . ())
                            (items . ())
                            (accessible . #t)))
        (throne_room . ((npcs . (royal_advisor))
                       (items . ())
                       (accessible . #t)))
        (kitchen . ((npcs . (cook))
                   (items . ())
                   (accessible . #t)))
        (garden . ((npcs . ())
                  (items . ())
                  (accessible . #t)))
        (village . ((npcs . (old_woman merchant))
                   (items . ())
                   (accessible . #t)))
        (forest . ((npcs . ())
                  (items . ())
                  (accessible . #t)))))
    
    ;; Начальные NPC
    (define (make-initial-npcs)
      '((royal_advisor . ((location . throne_room)
                         (dialogue_state . available)
                         (memory . ())))
        (cook . ((location . kitchen)
                (dialogue_state . available)
                (memory . ())))
        (old_woman . ((location . village)
                     (dialogue_state . available)
                     (memory . ())))
        (merchant . ((location . village)
                    (dialogue_state . available)
                    (memory . ())))))
    
    ;; Создание начальной системы квестов
    (define (make-initial-quest-system)
      (make-quest-system
        '() ; active quests
        '() ; completed quests  
        '((princess_lost_relic . #f)
          (helper_secret_potion . #f))))
    
    ;; ======================================
    ;; ACCESSOR FUNCTIONS
    ;; ======================================
    
    ;; Получить данные персонажа
    (define (get-character-data state character-id)
      (let ((characters (game-state-characters state)))
        (cdr (assq character-id characters))))
    
    ;; Получить состояние мира
    (define (get-world-state state)
      (game-state-world state))
    
    ;; Получить состояние квестов
    (define (get-quest-state state)
      (game-state-quests state))))