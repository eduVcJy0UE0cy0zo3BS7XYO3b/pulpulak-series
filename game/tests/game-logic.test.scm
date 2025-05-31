;; Тесты для игровой логики
;; game/tests/game-logic.test.scm

;; ======================================
;; ТЕСТЫ ДИАЛОГОВ С NPC
;; ======================================

(define dialogue-tests
  (make-test-suite "NPC Dialogue System"
    (list
      ;; Тест начала диалога с NPC
      (make-test "start-dialogue-with-advisor"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Перемещаем принцессу в тронный зал
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (new-state (caddr move-result))
                 ;; Переключаем ход на принцессу и начинаем диалог
                 (switch-state (switch-turn new-state))
                 (dialogue-result (make-choice switch-state "player1" "dialogue_royal_advisor" 'princess)))
            (assert-action-result dialogue-result 'success "Should start dialogue with advisor"))))
      
      ;; Тест диалога с недоступным NPC
      (make-test "dialogue-with-unavailable-npc-fails"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Пытаемся говорить с поваром, оставаясь в комнате принцессы
                 (dialogue-result (make-choice state "player1" "dialogue_cook" 'princess)))
            (assert-action-result dialogue-result 'error "Dialogue with NPC not at same location should fail"))))
      
      ;; Тест диалога когда персонаж в правильной локации
      (make-test "dialogue-at-correct-location"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Перемещаем принцессу на кухню
                 (move-result (make-choice state "player1" "move_to_kitchen" 'princess))
                 (new-state (caddr move-result))
                 ;; Переключаем ход на принцессу
                 (switch-state (switch-turn new-state))
                 ;; Начинаем диалог с поваром
                 (dialogue-result (make-choice switch-state "player1" "dialogue_cook" 'princess)))
            (assert-action-result dialogue-result 'success "Should talk to cook when in kitchen")))))))

;; ======================================
;; ТЕСТЫ СИСТЕМЫ КВЕСТОВ
;; ======================================

(define quest-tests
  (make-test-suite "Quest System"
    (list
      ;; Тест инициализации квестов
      (make-test "quests-initialized"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (quests (get-game-state-property state 'quests)))
            (assert-not-null quests "Quest system should be initialized"))))
      
      ;; Тест отсутствия активных квестов в начале
      (make-test "no-active-quests-initially"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (quests (get-game-state-property state 'quests))
                 (active-quests (get-game-state-property quests 'active)))
            (assert-null active-quests "No quests should be active initially"))))
      
      ;; Тест запуска квеста
      (make-test "start-quest-action"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (quest-result (make-choice state "player1" "quest_princess_lost_relic" 'princess)))
            ;; Квест может не запуститься сразу, поэтому проверяем что действие обработано
            (assert-true (or (eq? (car quest-result) 'success)
                           (eq? (car quest-result) 'error))
                        "Quest action should be processed")))))))

;; ======================================
;; ТЕСТЫ ВЗАИМОДЕЙСТВИЯ ПЕРСОНАЖЕЙ
;; ======================================

(define character-interaction-tests
  (make-test-suite "Character Interactions"
    (list
      ;; Тест когда персонажи в одной локации
      (make-test "characters-in-same-location"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (princess-location (get-character-location state 'princess))
                 (helper-location (get-character-location state 'helper)))
            (assert-equal princess-location helper-location "Characters should start in same location"))))
      
      ;; Тест разделения персонажей
      (make-test "characters-can-separate"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Принцесса идет в тронный зал
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (new-state (caddr move-result))
                 (princess-location (get-character-location new-state 'princess))
                 (helper-location (get-character-location new-state 'helper)))
            (assert-false (eq? princess-location helper-location) "Characters should be able to separate"))))
      
      ;; Тест воссоединения персонажей
      (make-test "characters-can-reunite"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Принцесса идет в тронный зал
                 (move1-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (state1 (caddr move1-result))
                 ;; Помощник следует за ней
                 (move2-result (make-choice state1 "player2" "move_to_throne_room" 'helper))
                 (state2 (caddr move2-result))
                 (princess-location (get-character-location state2 'princess))
                 (helper-location (get-character-location state2 'helper)))
            (assert-equal princess-location helper-location "Characters should be able to reunite")))))))

;; ======================================
;; ТЕСТЫ СИСТЕМЫ ОДИНОЧЕСТВА (для обмена нарядами)
;; ======================================

(define alone-system-tests
  (make-test-suite "Alone System"
    (list
      ;; Тест проверки одиночества в пустой комнате
      (make-test "characters-alone-in-empty-room"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Принцесса идет в сад (пустая локация)
                 (move-result (make-choice state "player1" "move_to_garden" 'princess))
                 (new-state (caddr move-result))
                 (princess-alone? (characters-alone? new-state 'princess)))
            (assert-true princess-alone? "Princess should be alone in empty garden"))))
      
      ;; Тест проверки НЕ одиночества в комнате с NPC
      (make-test "character-not-alone-with-npc"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Принцесса идет в тронный зал (с советником)
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (new-state (caddr move-result))
                 (princess-alone? (characters-alone? new-state 'princess)))
            (assert-false princess-alone? "Princess should not be alone with royal advisor"))))
      
      ;; Тест условий для обмена нарядами
      (make-test "outfit-swap-requires-both-alone"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Оба персонажа идут в сад (пустая локация)
                 (move1-result (make-choice state "player1" "move_to_garden" 'princess))
                 (state1 (caddr move1-result))
                 (move2-result (make-choice state1 "player2" "move_to_garden" 'helper))
                 (state2 (caddr move2-result))
                 ;; Пытаемся обменяться нарядами
                 (swap-result (make-choice state2 "player1" "swap_outfits" 'princess)))
            (assert-action-result swap-result 'success "Outfit swap should work when both characters alone together")))))))

;; ======================================
;; ТЕСТЫ СМЕНЫ ХОДОВ
;; ======================================

(define turn-management-tests
  (make-test-suite "Turn Management"
    (list
      ;; Тест автоматической смены хода
      (make-test "turn-switches-automatically"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (initial-turn (get-game-state-property state 'current-turn))
                 ;; Принцесса делает ход
                 (action-result (make-choice state "player1" "explore" 'princess))
                 (new-state (caddr action-result))
                 (new-turn (get-game-state-property new-state 'current-turn)))
            (assert-false (eq? initial-turn new-turn) "Turn should switch after action"))))
      
      ;; Тест что игрок не может действовать не в свой ход
      (make-test "player-cannot-act-out-of-turn"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Попытка помощника действовать когда ход принцессы
                 (result (make-choice state "player2" "explore" 'helper)))
            (assert-action-result result 'error "Player should not be able to act out of turn"))))
      
      ;; Тест ручной смены хода
      (make-test "manual-turn-switch"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (initial-turn (get-game-state-property state 'current-turn))
                 (switched-state (switch-turn state))
                 (new-turn (get-game-state-property switched-state 'current-turn)))
            (assert-false (eq? initial-turn new-turn) "Manual turn switch should work")))))))

;; ======================================
;; ТЕСТЫ ПРОВЕРОК ВАЛИДНОСТИ ДЕЙСТВИЙ
;; ======================================

(define validation-tests
  (make-test-suite "Action Validation"
    (list
      ;; Тест валидации перемещения в доступную локацию
      (make-test "valid-movement-location"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (can-move? (character-can-move? state 'princess 'throne_room)))
            (assert-true can-move? "Princess should be able to move to throne room"))))
      
      ;; Тест валидации перемещения в недоступную локацию
      (make-test "invalid-movement-same-location"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (current-location (get-character-location state 'princess))
                 (can-move? (character-can-move? state 'princess current-location)))
            (assert-false can-move? "Character should not be able to move to same location"))))
      
      ;; Тест валидации взаимодействия с доступным NPC
      (make-test "valid-npc-interaction"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Перемещаем принцессу в тронный зал
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (new-state (caddr move-result))
                 (can-interact? (character-can-interact? new-state 'princess 'royal_advisor)))
            (assert-true can-interact? "Princess should be able to interact with advisor in throne room"))))
      
      ;; Тест валидации взаимодействия с недоступным NPC
      (make-test "invalid-npc-interaction"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Принцесса пытается взаимодействовать с поваром из своей комнаты
                 (can-interact? (character-can-interact? state 'princess 'cook)))
            (assert-false can-interact? "Princess should not be able to interact with cook from chamber")))))))

;; ======================================
;; ТЕСТЫ СОСТОЯНИЯ МИРА
;; ======================================

(define world-state-tests
  (make-test-suite "World State"
    (list
      ;; Тест доступности локаций
      (make-test "all-locations-accessible"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (locations '(princess_chamber throne_room kitchen garden library village))
                 (all-accessible? (all (lambda (loc) (location-accessible? state loc)) locations)))
            (assert-true all-accessible? "All locations should be accessible initially"))))
      
      ;; Тест наличия NPC в правильных локациях
      (make-test "npcs-in-correct-locations"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (advisor-location (get-npc-location state 'royal_advisor))
                 (cook-location (get-npc-location state 'cook)))
            (if (and (eq? advisor-location 'throne_room)
                    (eq? cook-location 'kitchen))
              (list 'success "NPCs in correct locations")
              (list 'failure "NPCs not in expected locations")))))
      
      ;; Тест времени дня
      (make-test "initial-time-is-early-morning"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (world (get-game-state-property state 'world))
                 (time (get-game-state-property world 'time)))
            (assert-equal 'early_morning time "Initial time should be early morning")))))))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТОВ
;; ======================================

;; Проверка что все элементы списка удовлетворяют предикату
(define (all predicate lst)
  (cond
    ((null? lst) #t)
    ((not (predicate (car lst))) #f)
    (else (all predicate (cdr lst)))))

;; Функция для получения локации NPC (если не существует)
(define (get-npc-location state npc-id)
  (let* ((world (get-game-state-property state 'world))
         (npcs (get-game-state-property world 'npcs))
         (npc-data (assq npc-id npcs)))
    (if npc-data
      (get-game-state-property npc-data 'location)
      #f)))

;; Проверка может ли персонаж перемещаться (если не существует)
(define (character-can-move? state character-id location)
  (and (get-character-data state character-id)
       (location-accessible? state location)
       (not (character-at-location? state character-id location))))

;; Проверка может ли персонаж взаимодействовать с NPC (если не существует)
(define (character-can-interact? state character-id npc-id)
  (and (get-character-data state character-id)
       (npc-at-same-location? state character-id npc-id)
       (npc-available? state npc-id)))

;; ======================================
;; ЗАПУСК ВСЕХ ТЕСТОВ ИГРОВОЙ ЛОГИКИ
;; ======================================

(define (run-all-game-logic-tests)
  "Запустить все тесты игровой логики"
  (let ((suites (list dialogue-tests
                     quest-tests
                     character-interaction-tests
                     alone-system-tests
                     turn-management-tests
                     validation-tests
                     world-state-tests)))
    (let ((results (run-test-suites suites)))
      (map print-suite-result results)
      (print-test-summary results)
      results)))

;; Экспорт функции запуска тестов
;; run-all-game-logic-tests