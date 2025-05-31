;; Полная игровая система Pulpulak на чистом Scheme
;; game/pulpulak-game.scm

;; ======================================
;; ОСНОВНЫЕ СТРУКТУРЫ ДАННЫХ
;; ======================================

;; Игровое состояние
(define (make-game-state room-id)
  (list 'game-state
        (list 'room-id room-id)
        (list 'scene 'coop_awakening)
        (list 'current-turn 'princess)
        (list 'chapter 1)
        (list 'players (make-hash-table))
        (list 'characters (make-initial-characters))
        (list 'world (make-initial-world))
        (list 'quests (make-initial-quests))
        (list 'events '())
        (list 'npc-dialogues (make-hash-table))))

;; Персонажи
(define (make-initial-characters)
  (list 
    (list 'princess
          (list 'id 'princess)
          (list 'location 'princess_chamber)
          (list 'outfit 'princess_dress)
          (list 'inventory '())
          (list 'stats (list (cons 'loyalty 50) (cons 'knowledge 30) (cons 'charm 70)))
          (list 'dialogue-state '()))
    (list 'helper
          (list 'id 'helper)
          (list 'location 'princess_chamber)
          (list 'outfit 'common_dress)
          (list 'inventory '(translation_earrings voice_medallion))
          (list 'stats (list (cons 'loyalty 50) (cons 'knowledge 60) (cons 'charm 40)))
          (list 'dialogue-state '()))))

;; Мир
(define (make-initial-world)
  (list 
    (list 'time 'early_morning)
    (list 'locations (list
      (list 'princess_chamber (list 'accessible #t) (list 'npcs '()) (list 'description "Elegant royal bedroom"))
      (list 'throne_room (list 'accessible #t) (list 'npcs '(royal_advisor)) (list 'description "Grand throne room"))
      (list 'kitchen (list 'accessible #t) (list 'npcs '(cook)) (list 'description "Busy castle kitchen"))
      (list 'garden (list 'accessible #t) (list 'npcs '()) (list 'description "Beautiful royal garden"))
      (list 'library (list 'accessible #t) (list 'npcs '(librarian)) (list 'description "Ancient library"))
      (list 'village (list 'accessible #t) (list 'npcs '(old_woman merchant)) (list 'description "Bustling village"))))
    (list 'npcs (list
      (list 'royal_advisor 
            (list 'location 'throne_room) 
            (list 'available #t)
            (list 'dialogue-tree 'advisor_tree)
            (list 'memory '()))
      (list 'cook 
            (list 'location 'kitchen) 
            (list 'available #t)
            (list 'dialogue-tree 'cook_tree)
            (list 'memory '()))
      (list 'librarian 
            (list 'location 'library) 
            (list 'available #t)
            (list 'dialogue-tree 'librarian_tree)
            (list 'memory '()))
      (list 'old_woman 
            (list 'location 'village) 
            (list 'available #t)
            (list 'dialogue-tree 'old_woman_tree)
            (list 'memory '()))
      (list 'merchant 
            (list 'location 'village) 
            (list 'available #t)
            (list 'dialogue-tree 'merchant_tree)
            (list 'memory '()))))
    (list 'events '())))

;; ======================================
;; ИГРОВАЯ ЛОГИКА
;; ======================================

;; Создание новой игры
(define (create-cooperative-game room-id player1-id player2-id)
  (let ((initial-state (make-game-state room-id)))
    (set-players initial-state player1-id player2-id)))

(define (set-players state player1-id player2-id)
  (let ((players (make-hash-table)))
    (hash-table-set! players 'princess player1-id)
    (hash-table-set! players 'helper player2-id)
    (update-game-state-property state 'players players)))

;; Присоединение игрока
(define (join-game state player-id preferred-character)
  (let ((players (get-game-state-property state 'players)))
    (cond
      ((and (eq? preferred-character 'princess) 
            (not (hash-table-ref/default players 'princess #f)))
       (hash-table-set! players 'princess player-id)
       (list 'success 'princess state))
      ((and (eq? preferred-character 'helper)
            (not (hash-table-ref/default players 'helper #f)))
       (hash-table-set! players 'helper player-id)  
       (list 'success 'helper state))
      ((not (hash-table-ref/default players 'princess #f))
       (hash-table-set! players 'princess player-id)
       (list 'success 'princess state))
      ((not (hash-table-ref/default players 'helper #f))
       (hash-table-set! players 'helper player-id)
       (list 'success 'helper state))
      (else
       (list 'error "Game is full" state)))))

;; Обработка выборов
(define (make-choice state player-id choice-id character)
  (cond
    ((not (player-can-act? state player-id character))
     (list 'error "Not your turn" state))
    ((not (choice-available? state character choice-id))
     (list 'error "Choice not available" state))
    (else
     (process-choice state character choice-id))))

(define (process-choice state character choice-id)
  (cond
    ((string-prefix? "move_to_" choice-id)
     (let ((location (string->symbol (substring choice-id 8))))
       (move-character state character location)))
    ((string-prefix? "dialogue_" choice-id)
     (let ((npc (string->symbol (substring choice-id 9))))
       (start-dialogue state character npc)))
    ((string-prefix? "quest_" choice-id)
     (let ((quest (string->symbol (substring choice-id 6))))
       (process-quest-action state character quest)))
    ((eq? choice-id 'explore)
     (explore-location state character))
    ((eq? choice-id 'rest)
     (rest-character state character))
    ((eq? choice-id 'swap_outfits)
     (initiate-outfit-swap state character))
    (else
     (list 'error "Unknown choice" state))))

;; ======================================
;; ДЕЙСТВИЯ ПЕРСОНАЖЕЙ
;; ======================================

;; Перемещение
(define (move-character state character location)
  (cond
    ((not (location-accessible? state location))
     (list 'error "Location not accessible" state))
    ((character-at-location? state character location)
     (list 'error "Already at location" state))
    (else
     (let ((new-state (update-character-location state character location)))
       (js-log (string-append (symbol->string character) " moves to " (symbol->string location)))
       (switch-turn new-state)
       (list 'success "Moved successfully" new-state)))))

;; Диалог с NPC
(define (start-dialogue state character npc)
  (cond
    ((not (npc-at-same-location? state character npc))
     (list 'error "NPC not at same location" state))
    ((not (npc-available? state npc))
     (list 'error "NPC not available" state))
    (else
     (let ((dialogue-tree (get-npc-dialogue-tree state npc)))
       (set-character-dialogue-state state character npc dialogue-tree)
       (js-log (string-append (symbol->string character) " starts dialogue with " (symbol->string npc)))
       (list 'success "Dialogue started" state)))))

;; Исследование локации
(define (explore-location state character)
  (let* ((location (get-character-location state character))
         (exploration-result (generate-exploration-result location)))
    (js-log (string-append (symbol->string character) " explores " (symbol->string location)))
    (switch-turn state)
    (list 'success exploration-result state)))

;; Отдых
(define (rest-character state character)
  (let ((new-state (restore-character-stats state character)))
    (js-log (string-append (symbol->string character) " rests and recovers"))
    (switch-turn new-state)
    (list 'success "Rested successfully" new-state)))

;; Обмен одеждой
(define (initiate-outfit-swap state character)
  (let* ((other-character (if (eq? character 'princess) 'helper 'princess))
         (both-alone? (and (characters-alone? state 'princess)
                          (characters-alone? state 'helper)))
         (same-location? (eq? (get-character-location state 'princess)
                             (get-character-location state 'helper))))
    (cond
      ((not same-location?)
       (list 'error "Characters must be in same location" state))
      ((not both-alone?)
       (list 'error "Characters must be alone" state))
      (else
       (swap-character-outfits state 'princess 'helper)
       (js-log "Princess and helper swap outfits")
       (list 'success "Outfits swapped" state)))))

;; ======================================
;; ПОЛУЧЕНИЕ ДОСТУПНЫХ ДЕЙСТВИЙ
;; ======================================

(define (get-available-actions state character)
  (let* ((location (get-character-location state character))
         (move-actions (get-movement-actions state character location))
         (npc-actions (get-npc-interaction-actions state character location))
         (special-actions (get-special-actions state character))
         (outfit-actions (get-outfit-actions state character)))
    (append move-actions npc-actions special-actions outfit-actions)))

(define (get-movement-actions state character current-location)
  (let ((locations '(princess_chamber throne_room kitchen garden library village)))
    (filter-map
      (lambda (location)
        (if (and (not (eq? location current-location))
                (location-accessible? state location))
          (list (string-append "move_to_" (symbol->string location))
                (string-append "Move to " (symbol->string location)))
          #f))
      locations)))

(define (get-npc-interaction-actions state character location)
  (let* ((npcs (get-npcs-at-location state location))
         (available-npcs (filter (lambda (npc) (npc-available? state npc)) npcs)))
    (map (lambda (npc)
           (list (string-append "dialogue_" (symbol->string npc))
                 (string-append "Talk to " (symbol->string npc))))
         available-npcs)))

(define (get-special-actions state character)
  (list 
    (list "explore" "Explore current location")
    (list "rest" "Rest and recover")))

(define (get-outfit-actions state character)
  (let* ((other-character (if (eq? character 'princess) 'helper 'princess))
         (same-location? (eq? (get-character-location state character)
                             (get-character-location state other-character)))
         (both-alone? (and (characters-alone? state 'princess)
                          (characters-alone? state 'helper))))
    (if (and same-location? both-alone?)
      (list (list "swap_outfits" "Swap outfits with partner"))
      '())))

;; ======================================
;; СИСТЕМА ДИАЛОГОВ
;; ======================================

(define (process-dialogue-choice state character npc choice-id)
  (let* ((dialogue-state (get-character-dialogue-state state character))
         (current-node (get-current-dialogue-node dialogue-state))
         (choice (find-dialogue-choice current-node choice-id)))
    (cond
      ((not choice)
       (list 'error "Invalid dialogue choice" state))
      (else
       (let* ((next-node (get-choice-next-node choice))
              (effects (get-choice-effects choice))
              (new-state (apply-dialogue-effects state character npc effects)))
         (if (dialogue-complete? next-node)
           (end-dialogue new-state character npc)
           (set-dialogue-node new-state character next-node)))))))

(define (end-dialogue state character npc)
  (let ((new-state (clear-character-dialogue-state state character)))
    (js-log (string-append (symbol->string character) " ends dialogue with " (symbol->string npc)))
    (list 'success "Dialogue ended" new-state)))

;; ======================================
;; СИСТЕМА КВЕСТОВ
;; ======================================

(define (process-quest-action state character quest-action)
  (let* ((active-quests (get-active-quests state character))
         (quest-id (get-quest-action-id quest-action))
         (quest (find-quest quest-id)))
    (cond
      ((not quest)
       (list 'error "Quest not found" state))
      ((quest-completed? state quest-id)
       (list 'error "Quest already completed" state))
      (else
       (execute-quest-step state character quest quest-action)))))

(define (execute-quest-step state character quest step-action)
  (let* ((current-step (get-current-quest-step state character quest))
         (requirements (get-step-requirements current-step))
         (actions (get-step-actions current-step)))
    (cond
      ((not (requirements-met? state character requirements))
       (list 'error "Quest requirements not met" state))
      (else
       (let ((new-state (apply-quest-actions state character actions)))
         (advance-quest-step new-state character quest)
         (js-log (string-append (symbol->string character) " advances quest " (symbol->string quest)))
         (list 'success "Quest step completed" new-state))))))

;; ======================================
;; УТИЛИТЫ И ГЕТТЕРЫ
;; ======================================

;; Получение свойств состояния
(define (get-game-state-property state property)
  (let ((pair (assq property (cdr state))))
    (if pair (cadr pair) #f)))

(define (update-game-state-property state property new-value)
  (cons (car state)
        (update-alist (cdr state) property new-value)))

;; Работа с персонажами
(define (get-character-data state character-id)
  (let ((characters (get-game-state-property state 'characters)))
    (assq character-id characters)))

(define (get-character-property character property)
  (let ((pair (assq property (cdr character))))
    (if pair (cadr pair) #f)))

(define (update-character-property state character-id property new-value)
  (let* ((characters (get-game-state-property state 'characters))
         (updated-characters (update-character-in-list characters character-id property new-value)))
    (update-game-state-property state 'characters updated-characters)))

(define (get-character-location state character)
  (let ((character-data (get-character-data state character)))
    (get-character-property character-data 'location)))

(define (update-character-location state character location)
  (update-character-property state character 'location location))

;; Проверки персонажей
(define (character-at-location? state character location)
  (eq? (get-character-location state character) location))

(define (characters-alone? state character)
  (let* ((location (get-character-location state character))
         (npcs-here (get-npcs-at-location state location)))
    (null? npcs-here)))

(define (npc-at-same-location? state character npc)
  (let* ((char-location (get-character-location state character))
         (npc-location (get-npc-location state npc)))
    (eq? char-location npc-location)))

;; Работа с мираом
(define (location-accessible? state location)
  (let* ((world (get-game-state-property state 'world))
         (locations (get-game-state-property world 'locations))
         (location-data (assq location locations)))
    (if location-data
      (get-game-state-property location-data 'accessible)
      #f)))

(define (get-npcs-at-location state location)
  (let* ((world (get-game-state-property state 'world))
         (npcs (get-game-state-property world 'npcs)))
    (filter-map
      (lambda (npc-data)
        (if (eq? (get-npc-location-from-data npc-data) location)
          (car npc-data)
          #f))
      npcs)))

(define (get-npc-location state npc)
  (let* ((world (get-game-state-property state 'world))
         (npcs (get-game-state-property world 'npcs))
         (npc-data (assq npc npcs)))
    (if npc-data
      (get-game-state-property npc-data 'location)
      #f)))

(define (npc-available? state npc)
  (let* ((world (get-game-state-property state 'world))
         (npcs (get-game-state-property world 'npcs))
         (npc-data (assq npc npcs)))
    (if npc-data
      (get-game-state-property npc-data 'available)
      #f)))

;; Управление ходами
(define (switch-turn state)
  (let ((current-turn (get-game-state-property state 'current-turn)))
    (update-game-state-property state 'current-turn
      (if (eq? current-turn 'princess) 'helper 'princess))))

(define (player-can-act? state player-id character)
  (let* ((players (get-game-state-property state 'players))
         (current-turn (get-game-state-property state 'current-turn))
         (character-player (hash-table-ref/default players character #f)))
    (and (eq? character-player player-id)
         (eq? current-turn character))))

;; Вспомогательные функции
(define (update-alist alist key new-value)
  (cond
    ((null? alist) (list (list key new-value)))
    ((eq? (caar alist) key) 
     (cons (list key new-value) (cdr alist)))
    (else 
     (cons (car alist) (update-alist (cdr alist) key new-value)))))

(define (update-character-in-list characters character-id property new-value)
  (map (lambda (character)
         (if (eq? (car character) character-id)
           (cons (car character)
                 (update-alist (cdr character) property new-value))
           character))
       characters))

(define (filter-map proc lst)
  (cond
    ((null? lst) '())
    (else
      (let ((result (proc (car lst))))
        (if result
          (cons result (filter-map proc (cdr lst)))
          (filter-map proc (cdr lst)))))))

(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

;; JavaScript интерфейс
(define (symbol->string sym)
  (js-symbol->string sym))

(define (string-append . strings)
  (apply js-string-append strings))

;; ======================================
;; ЭКСПОРТИРОВАННЫЕ ФУНКЦИИ ДЛЯ JAVASCRIPT
;; ======================================

(define (pulpulak-create-game room-id)
  "Создать новую кооперативную игру"
  (make-game-state room-id))

(define (pulpulak-join-game state player-id character)
  "Присоединиться к игре"
  (join-game state player-id character))

(define (pulpulak-make-choice state player-id choice-id character)
  "Сделать выбор в игре"
  (make-choice state player-id choice-id character))

(define (pulpulak-get-actions state character)
  "Получить доступные действия"
  (get-available-actions state character))

(define (pulpulak-process-dialogue state character npc choice-id)
  "Обработать выбор в диалоге"
  (process-dialogue-choice state character npc choice-id))

(define (pulpulak-get-game-data state room-id)
  "Получить данные игры для клиента"
  (list 'game-data
        (list 'room-id room-id)
        (list 'scene (get-game-state-property state 'scene))
        (list 'current-turn (get-game-state-property state 'current-turn))
        (list 'chapter (get-game-state-property state 'chapter))
        (list 'characters (get-game-state-property state 'characters))
        (list 'locations (extract-location-data state))
        (list 'quests (extract-quest-data state))))

(define (extract-location-data state)
  "Извлечь данные локаций для клиента"
  (let* ((world (get-game-state-property state 'world))
         (locations (get-game-state-property world 'locations)))
    (map (lambda (location-data)
           (list (car location-data)
                 (get-game-state-property location-data 'description)))
         locations)))

(define (extract-quest-data state)
  "Извлечь данные квестов для клиента"
  (let ((quests (get-game-state-property state 'quests)))
    (list 'active (get-game-state-property quests 'active)
          'completed (get-game-state-property quests 'completed))))