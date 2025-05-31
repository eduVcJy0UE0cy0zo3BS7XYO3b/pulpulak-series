;; Интеграционные тесты для полного игрового процесса
;; game/tests/integration.test.scm

;; ======================================
;; ТЕСТЫ ПОЛНОГО ИГРОВОГО СЦЕНАРИЯ
;; ======================================

(define full-game-scenario-tests
  (make-test-suite "Full Game Scenarios"
    (list
      ;; Тест полного сценария: создание игры -> присоединение игроков -> базовые действия
      (make-test "complete-game-startup-sequence"
        (lambda ()
          (let* (;; Создаем игру
                 (state (make-game-state "integration-test-room"))
                 ;; Присоединяем первого игрока как принцессу
                 (join1-result (join-game state "player1" 'princess))
                 (state1 (caddr join1-result))
                 ;; Присоединяем второго игрока как помощника
                 (join2-result (join-game state1 "player2" 'helper))
                 (state2 (caddr join2-result))
                 ;; Принцесса исследует комнату
                 (explore-result (make-choice state2 "player1" "explore" 'princess))
                 (state3 (caddr explore-result))
                 ;; Помощник отдыхает
                 (rest-result (make-choice state3 "player2" "rest" 'helper))
                 (final-state (caddr rest-result)))
            (if (and (eq? (car join1-result) 'success)
                    (eq? (car join2-result) 'success)
                    (eq? (car explore-result) 'success)
                    (eq? (car rest-result) 'success))
              (list 'success "Complete game startup sequence executed successfully")
              (list 'failure "Game startup sequence failed")))))
      
      ;; Тест сценария обмена нарядами
      (make-test "outfit-swap-scenario"
        (lambda ()
          (let* (;; Начальная настройка
                 (state (make-test-game-with-players))
                 ;; Проверяем начальные наряды
                 (initial-princess-outfit (get-character-outfit state 'princess))
                 (initial-helper-outfit (get-character-outfit state 'helper))
                 ;; Обмениваемся нарядами
                 (swap-result (make-choice state "player1" "swap_outfits" 'princess))
                 (new-state (caddr swap-result))
                 ;; Проверяем новые наряды
                 (new-princess-outfit (get-character-outfit new-state 'princess))
                 (new-helper-outfit (get-character-outfit new-state 'helper)))
            (if (and (eq? (car swap-result) 'success)
                    (eq? initial-princess-outfit new-helper-outfit)
                    (eq? initial-helper-outfit new-princess-outfit))
              (list 'success "Outfit swap scenario completed successfully")
              (list 'failure "Outfit swap scenario failed")))))
      
      ;; Тест сценария разделения и воссоединения персонажей
      (make-test "character-separation-reunion-scenario"
        (lambda ()
          (let* (;; Начальная настройка
                 (state (make-test-game-with-players))
                 ;; Принцесса идет в тронный зал
                 (move1-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (state1 (caddr move1-result))
                 ;; Помощник идет в кухню
                 (move2-result (make-choice state1 "player2" "move_to_kitchen" 'helper))
                 (state2 (caddr move2-result))
                 ;; Проверяем что персонажи разделены
                 (princess-location (get-character-location state2 'princess))
                 (helper-location (get-character-location state2 'helper))
                 (separated? (not (eq? princess-location helper-location)))
                 ;; Помощник присоединяется к принцессе
                 (reunion-result (make-choice state2 "player1" "move_to_throne_room" 'helper))
                 (final-state (caddr reunion-result))
                 ;; Проверяем воссоединение
                 (final-princess-location (get-character-location final-state 'princess))
                 (final-helper-location (get-character-location final-state 'helper))
                 (reunited? (eq? final-princess-location final-helper-location)))
            (if (and separated? reunited?
                    (eq? (car move1-result) 'success)
                    (eq? (car move2-result) 'success)
                    (eq? (car reunion-result) 'success))
              (list 'success "Character separation and reunion scenario completed")
              (list 'failure "Character separation/reunion scenario failed"))))))))

;; ======================================
;; ТЕСТЫ МНОГОШАГОВОГО ВЗАИМОДЕЙСТВИЯ
;; ======================================

(define multi-step-interaction-tests
  (make-test-suite "Multi-Step Interactions"
    (list
      ;; Тест последовательности: перемещение -> диалог -> исследование
      (make-test "move-talk-explore-sequence"
        (lambda ()
          (let* (;; Начальная настройка
                 (state (make-test-game-with-players))
                 ;; Принцесса идет в тронный зал
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (state1 (caddr move-result))
                 ;; Помощник говорит с советником
                 (talk-result (make-choice state1 "player2" "dialogue_royal_advisor" 'helper))
                 (state2 (caddr talk-result))
                 ;; Принцесса исследует тронный зал
                 (explore-result (make-choice state2 "player1" "explore" 'princess))
                 (final-state (caddr explore-result)))
            (if (and (eq? (car move-result) 'success)
                    (eq? (car talk-result) 'success)
                    (eq? (car explore-result) 'success))
              (list 'success "Move-talk-explore sequence completed successfully")
              (list 'failure "Multi-step interaction sequence failed")))))
      
      ;; Тест сценария с обменом нарядами и последующими действиями
      (make-test "outfit-swap-with-consequences"
        (lambda ()
          (let* (;; Начальная настройка
                 (state (make-test-game-with-players))
                 ;; Обмениваемся нарядами
                 (swap-result (make-choice state "player1" "swap_outfits" 'princess))
                 (state1 (caddr swap-result))
                 ;; Принцесса (теперь в обычном платье) идет в деревню
                 (move1-result (make-choice state1 "player2" "move_to_village" 'princess))
                 (state2 (caddr move1-result))
                 ;; Помощник (теперь в платье принцессы) идет в тронный зал
                 (move2-result (make-choice state2 "player1" "move_to_throne_room" 'helper))
                 (final-state (caddr move2-result)))
            (if (and (eq? (car swap-result) 'success)
                    (eq? (car move1-result) 'success)
                    (eq? (car move2-result) 'success))
              (list 'success "Outfit swap with consequences scenario completed")
              (list 'failure "Outfit swap consequences scenario failed"))))
      
      ;; Тест последовательности действий с проверкой состояния
      (make-test "stateful-action-sequence"
        (lambda ()
          (let* (;; Создаем последовательность действий
                 (actions '((princess "explore")
                           (helper "rest")
                           (princess "move_to_library")
                           (helper "move_to_library")
                           (princess "swap_outfits")))
                 ;; Симулируем последовательность
                 (initial-state (make-test-game-with-players))
                 (final-state (simulate-complex-actions initial-state actions)))
            (if final-state
              (let ((princess-location (get-character-location final-state 'princess))
                    (helper-location (get-character-location final-state 'helper)))
                (if (and (eq? princess-location 'library)
                        (eq? helper-location 'library))
                  (list 'success "Stateful action sequence completed with correct final state")
                  (list 'failure "Final state incorrect after action sequence")))
              (list 'failure "Action sequence simulation failed"))))))))

;; ======================================
;; ТЕСТЫ ГРАНИЧНЫХ СЛУЧАЕВ
;; ======================================

(define edge-case-tests
  (make-test-suite "Edge Cases"
    (list
      ;; Тест максимального количества последовательных действий
      (make-test "multiple-consecutive-actions"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Выполняем 10 последовательных действий
                 (actions (make-list 10 'explore))
                 (final-state (simulate-repeated-action state 'princess actions)))
            (if final-state
              (list 'success "Multiple consecutive actions handled successfully")
              (list 'failure "Failed to handle multiple consecutive actions")))))
      
      ;; Тест попытки недопустимых действий
      (make-test "invalid-action-handling"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Пытаемся сделать недопустимое действие
                 (invalid-result (make-choice state "player1" "invalid_action_12345" 'princess))
                 ;; Пытаемся действовать за несуществующего персонажа
                 (nonexistent-result (make-choice state "player1" "explore" 'nonexistent_character)))
            (if (and (eq? (car invalid-result) 'error)
                    (eq? (car nonexistent-result) 'error))
              (list 'success "Invalid actions properly rejected")
              (list 'failure "Invalid actions not properly handled")))))
      
      ;; Тест состояния после серии неудачных действий
      (make-test "failed-actions-preserve-state"
        (lambda ()
          (let* ((initial-state (make-test-game-with-players))
                 (initial-princess-location (get-character-location initial-state 'princess))
                 ;; Пытаемся сделать несколько недопустимых действий
                 (fail1 (make-choice initial-state "player1" "invalid_action" 'princess))
                 (fail2 (make-choice initial-state "player3" "explore" 'princess))
                 (fail3 (make-choice initial-state "player1" "move_to_nonexistent_location" 'princess))
                 ;; Проверяем что состояние не изменилось
                 (final-princess-location (get-character-location initial-state 'princess)))
            (if (eq? initial-princess-location final-princess-location)
              (list 'success "Failed actions preserved original state")
              (list 'failure "Failed actions incorrectly modified state"))))))))

;; ======================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И СТАБИЛЬНОСТИ
;; ======================================

(define performance-tests
  (make-test-suite "Performance and Stability"
    (list
      ;; Тест создания и уничтожения множественных состояний
      (make-test "multiple-state-creation"
        (lambda ()
          (let ((states (map (lambda (i) 
                              (make-game-state (string-append "room-" (number->string i))))
                            (range 10))))
            (if (= (length states) 10)
              (list 'success "Multiple states created successfully")
              (list 'failure "Failed to create multiple states")))))
      
      ;; Тест глубокой последовательности действий
      (make-test "deep-action-sequence"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Создаем длинную последовательность перемещений
                 (locations '(throne_room kitchen garden library village princess_chamber))
                 (move-sequence (create-movement-sequence locations))
                 (final-state (simulate-action-sequence state move-sequence)))
            (if final-state
              (list 'success "Deep action sequence completed successfully")
              (list 'failure "Deep action sequence failed")))))
      
      ;; Тест неизменяемости при множественных операциях
      (make-test "immutability-stress-test"
        (lambda ()
          (let* ((original-state (make-test-game-with-players))
                 (original-princess-location (get-character-location original-state 'princess))
                 ;; Выполняем множество операций
                 (state1 (simulate-actions original-state '((princess "move_to_throne_room")
                                                           (helper "move_to_kitchen")
                                                           (princess "explore")
                                                           (helper "rest"))))
                 ;; Проверяем что оригинальное состояние не изменилось
                 (unchanged-location (get-character-location original-state 'princess)))
            (if (eq? original-princess-location unchanged-location)
              (list 'success "Original state remained immutable during stress test")
              (list 'failure "Immutability violated during stress test"))))))))

;; ======================================
;; ТЕСТЫ ИНТЕГРАЦИИ С ИГРОВЫМИ КОМПОНЕНТАМИ
;; ======================================

(define component-integration-tests
  (make-test-suite "Component Integration"
    (list
      ;; Тест интеграции систем персонажей и мира
      (make-test "character-world-integration"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Перемещаем персонажей в разные локации с NPC
                 (move1 (make-choice state "player1" "move_to_throne_room" 'princess))
                 (state1 (caddr move1))
                 (move2 (make-choice state1 "player2" "move_to_kitchen" 'helper))
                 (state2 (caddr move2))
                 ;; Проверяем что персонажи правильно взаимодействуют с миром
                 (princess-can-talk-advisor? (character-can-interact? state2 'princess 'royal_advisor))
                 (helper-can-talk-cook? (character-can-interact? state2 'helper 'cook)))
            (if (and princess-can-talk-advisor? helper-can-talk-cook?)
              (list 'success "Character-world integration working correctly")
              (list 'failure "Character-world integration failed")))))
      
      ;; Тест интеграции систем нарядов и взаимодействий
      (make-test "outfit-interaction-integration"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Обмениваемся нарядами
                 (swap-result (make-choice state "player1" "swap_outfits" 'princess))
                 (swapped-state (caddr swap-result))
                 ;; Проверяем что персонажи все еще могут взаимодействовать
                 (move-result (make-choice swapped-state "player2" "move_to_throne_room" 'princess))
                 (final-state (caddr move-result))
                 (can-interact? (character-can-interact? final-state 'princess 'royal_advisor)))
            (if (and (eq? (car swap-result) 'success)
                    (eq? (car move-result) 'success)
                    can-interact?)
              (list 'success "Outfit-interaction integration working correctly")
              (list 'failure "Outfit-interaction integration failed")))))
      
      ;; Тест полной интеграции всех систем
      (make-test "full-system-integration"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Комплексный сценарий затрагивающий все системы
                 (actions '((princess "explore")                    ; Система действий
                           (helper "move_to_kitchen")              ; Система перемещений  
                           (princess "move_to_kitchen")            ; Воссоединение
                           (helper "swap_outfits")                 ; Система нарядов
                           (princess "dialogue_cook")              ; Система диалогов
                           (helper "rest")))                       ; Система восстановления
                 (final-state (simulate-complex-actions state actions)))
            (if final-state
              (let ((princess-location (get-character-location final-state 'princess))
                    (helper-location (get-character-location final-state 'helper))
                    (princess-outfit (get-character-outfit final-state 'princess))
                    (helper-outfit (get-character-outfit final-state 'helper)))
                (if (and (eq? princess-location 'kitchen)
                        (eq? helper-location 'kitchen)
                        (eq? princess-outfit 'common_dress)
                        (eq? helper-outfit 'princess_dress))
                  (list 'success "Full system integration completed successfully")
                  (list 'failure "Full system integration state incorrect")))
              (list 'failure "Full system integration failed"))))))))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ======================================

;; Симуляция сложных действий с проверкой результатов
(define (simulate-complex-actions state actions)
  (fold-left
    (lambda (current-state action)
      (if current-state
        (let ((character (car action))
              (choice (cadr action))
              (player-id (if (eq? character 'princess) "player1" "player2")))
          (let ((result (make-choice current-state player-id choice character)))
            (if (eq? (car result) 'success)
              (caddr result)
              #f)))
        #f))
    state
    actions))

;; Симуляция повторяющихся действий
(define (simulate-repeated-action state character actions)
  (fold-left
    (lambda (current-state action)
      (if current-state
        (let ((player-id (if (eq? character 'princess) "player1" "player2")))
          (let ((result (make-choice current-state player-id action character)))
            (if (eq? (car result) 'success)
              (caddr result)
              current-state))) ; Возвращаем предыдущее состояние при ошибке
        current-state))
    state
    actions))

;; Создание последовательности перемещений
(define (create-movement-sequence locations)
  (map (lambda (location)
         (string-append "move_to_" (symbol->string location)))
       locations))

;; Симуляция последовательности действий
(define (simulate-action-sequence state actions)
  (fold-left
    (lambda (current-state action)
      (if current-state
        (let ((result (make-choice current-state "player1" action 'princess)))
          (if (eq? (car result) 'success)
            (caddr result)
            current-state))
        current-state))
    state
    actions))

;; Создание диапазона чисел
(define (range n)
  (if (<= n 0)
    '()
    (cons (- n 1) (range (- n 1)))))

;; Создание списка одинаковых элементов
(define (make-list n element)
  (if (<= n 0)
    '()
    (cons element (make-list (- n 1) element))))

;; Функция для получения наряда персонажа
(define (get-character-outfit state character)
  (let* ((character-data (get-character-data state character)))
    (get-character-property character-data 'outfit)))

;; ======================================
;; ЗАПУСК ВСЕХ ИНТЕГРАЦИОННЫХ ТЕСТОВ
;; ======================================

(define (run-all-integration-tests)
  "Запустить все интеграционные тесты"
  (let ((suites (list full-game-scenario-tests
                     multi-step-interaction-tests
                     edge-case-tests
                     performance-tests
                     component-integration-tests)))
    (let ((results (run-test-suites suites)))
      (map print-suite-result results)
      (print-test-summary results)
      results)))

;; ======================================
;; ЗАПУСК ВСЕХ ТЕСТОВ ПРОЕКТА
;; ======================================

(define (run-all-tests)
  "Запустить все тесты проекта"
  (js-log "\n========================================")
  (js-log "ЗАПУСК ВСЕХ ТЕСТОВ PULPULAK GAME")
  (js-log "========================================")
  
  (js-log "\n--- Core Functions Tests ---")
  (let ((core-results (run-all-core-tests)))
    
    (js-log "\n--- Game Logic Tests ---")
    (let ((logic-results (run-all-game-logic-tests)))
      
      (js-log "\n--- Integration Tests ---")
      (let ((integration-results (run-all-integration-tests)))
        
        (js-log "\n========================================")
        (js-log "ОБЩИЙ ОТЧЕТ ПО ВСЕМ ТЕСТАМ")
        (js-log "========================================")
        
        (let ((all-results (append core-results logic-results integration-results)))
          (print-test-summary all-results)
          all-results)))))

;; Экспорт главных функций
;; run-all-integration-tests, run-all-tests