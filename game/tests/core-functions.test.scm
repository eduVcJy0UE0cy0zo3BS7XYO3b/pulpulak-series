;; Тесты для основных функций игры
;; game/tests/core-functions.test.scm

;; Загружаем тестовый фреймворк и игровую логику
;; (load "game/test-framework.scm")
;; (load "game/pulpulak-game.scm")

;; ======================================
;; ТЕСТЫ СОЗДАНИЯ ИГРОВОГО СОСТОЯНИЯ
;; ======================================

(define state-creation-tests
  (make-test-suite "Game State Creation"
    (list
      ;; Тест создания базового состояния
      (make-test "create-basic-game-state"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (assert-true state "Game state should be created successfully"))))
      
      ;; Тест проверки начальной сцены
      (make-test "initial-scene-is-coop-awakening"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (assert-game-state state 'scene 'coop_awakening "Initial scene should be coop_awakening"))))
      
      ;; Тест проверки начального хода
      (make-test "initial-turn-is-princess"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (assert-game-state state 'current-turn 'princess "Initial turn should be princess"))))
      
      ;; Тест создания персонажей
      (make-test "characters-are-created"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (characters (get-game-state-property state 'characters)))
            (assert-not-null characters "Characters should be created"))))
      
      ;; Тест начальных локаций персонажей
      (make-test "characters-start-in-princess-chamber"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (let ((princess-loc-result (assert-character-location state 'princess 'princess_chamber "Princess starts in chamber"))
                  (helper-loc-result (assert-character-location state 'helper 'princess_chamber "Helper starts in chamber")))
              (if (and (eq? (car princess-loc-result) 'success)
                      (eq? (car helper-loc-result) 'success))
                (list 'success "Both characters start in princess chamber")
                (list 'failure "Characters not in correct initial location"))))))
      
      ;; Тест начальных нарядов
      (make-test "characters-have-initial-outfits"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (let ((princess-outfit-result (assert-character-outfit state 'princess 'princess_dress "Princess has princess dress"))
                  (helper-outfit-result (assert-character-outfit state 'helper 'common_dress "Helper has common dress")))
              (if (and (eq? (car princess-outfit-result) 'success)
                      (eq? (car helper-outfit-result) 'success))
                (list 'success "Both characters have correct initial outfits")
                (list 'failure "Characters don't have correct initial outfits"))))))
      
      ;; Тест инвентаря помощника
      (make-test "helper-has-initial-inventory"
        (lambda ()
          (let ((state (make-game-state "test-room")))
            (let ((earrings-result (assert-character-has-item state 'helper 'translation_earrings "Helper has earrings"))
                  (medallion-result (assert-character-has-item state 'helper 'voice_medallion "Helper has medallion")))
              (if (and (eq? (car earrings-result) 'success)
                      (eq? (car medallion-result) 'success))
                (list 'success "Helper has correct initial inventory")
                (list 'failure "Helper missing initial inventory items"))))))
      
      ;; Тест мира
      (make-test "world-is-initialized"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (world (get-game-state-property state 'world)))
            (assert-not-null world "World should be initialized")))))))

;; ======================================
;; ТЕСТЫ ДОБАВЛЕНИЯ ИГРОКОВ
;; ======================================

(define player-management-tests
  (make-test-suite "Player Management"
    (list
      ;; Тест присоединения игрока как принцессы
      (make-test "join-as-princess"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (result (join-game state "player1" 'princess)))
            (if (eq? (car result) 'success)
              (assert-equal 'princess (cadr result) "Player should join as princess")
              (list 'failure "Failed to join as princess")))))
      
      ;; Тест присоединения игрока как помощника
      (make-test "join-as-helper"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (result (join-game state "player1" 'helper)))
            (if (eq? (car result) 'success)
              (assert-equal 'helper (cadr result) "Player should join as helper")
              (list 'failure "Failed to join as helper")))))
      
      ;; Тест присоединения двух игроков
      (make-test "join-two-players"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (result1 (join-game state "player1" 'princess))
                 (new-state (if (eq? (car result1) 'success) (caddr result1) state))
                 (result2 (join-game new-state "player2" 'helper)))
            (if (and (eq? (car result1) 'success)
                    (eq? (car result2) 'success))
              (list 'success "Two players joined successfully")
              (list 'failure "Failed to join two players")))))
      
      ;; Тест переполнения игры
      (make-test "game-full-prevents-third-player"
        (lambda ()
          (let* ((state (make-game-state "test-room"))
                 (result1 (join-game state "player1" 'princess))
                 (state1 (caddr result1))
                 (result2 (join-game state1 "player2" 'helper))
                 (state2 (caddr result2))
                 (result3 (join-game state2 "player3" 'princess)))
            (if (eq? (car result3) 'error)
              (list 'success "Third player correctly rejected")
              (list 'failure "Third player should be rejected"))))))))

;; ======================================
;; ТЕСТЫ ИГРОВЫХ ДЕЙСТВИЙ
;; ======================================

(define action-tests
  (make-test-suite "Game Actions"
    (list
      ;; Тест перемещения персонажа
      (make-test "character-movement"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "move_to_throne_room" 'princess)))
            (if (eq? (car result) 'success)
              (let ((new-state (caddr result)))
                (assert-character-location new-state 'princess 'throne_room "Princess should move to throne room"))
              (list 'failure (string-append "Movement failed: " (cadr result)))))))
      
      ;; Тест смены хода после действия
      (make-test "turn-switches-after-action"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (initial-turn (get-game-state-property state 'current-turn))
                 (result (make-choice state "player1" "move_to_throne_room" 'princess)))
            (if (eq? (car result) 'success)
              (let* ((new-state (caddr result))
                     (new-turn (get-game-state-property new-state 'current-turn)))
                (assert-false (eq? initial-turn new-turn) "Turn should switch after action"))
              (list 'failure "Action failed, cannot test turn switching")))))
      
      ;; Тест неправильного хода
      (make-test "wrong-turn-rejected"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player2" "move_to_throne_room" 'helper)))
            (assert-action-result result 'error "Wrong turn should be rejected"))))
      
      ;; Тест недоступного выбора
      (make-test "invalid-choice-rejected"  
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "invalid_choice" 'princess)))
            (assert-action-result result 'error "Invalid choice should be rejected"))))
      
      ;; Тест исследования локации
      (make-test "explore-location"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "explore" 'princess)))
            (assert-action-result result 'success "Exploration should succeed"))))
      
      ;; Тест отдыха персонажа
      (make-test "character-rest"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "rest" 'princess)))
            (assert-action-result result 'success "Rest should succeed")))))))

;; ======================================
;; ТЕСТЫ СИСТЕМЫ НАРЯДОВ
;; ======================================

(define outfit-tests
  (make-test-suite "Outfit System"
    (list
      ;; Тест обмена нарядами когда персонажи вместе
      (make-test "outfit-swap-when-together"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "swap_outfits" 'princess)))
            (if (eq? (car result) 'success)
              (let ((new-state (caddr result)))
                (let ((princess-outfit-result (assert-character-outfit new-state 'princess 'common_dress "Princess should have common dress"))
                      (helper-outfit-result (assert-character-outfit new-state 'helper 'princess_dress "Helper should have princess dress")))
                  (if (and (eq? (car princess-outfit-result) 'success)
                          (eq? (car helper-outfit-result) 'success))
                    (list 'success "Outfits swapped successfully")
                    (list 'failure "Outfits not swapped correctly"))))
              (list 'failure (string-append "Outfit swap failed: " (cadr result)))))))
      
      ;; Тест обмена нарядами когда персонажи в разных локациях
      (make-test "outfit-swap-different-locations-fails"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 ;; Перемещаем принцессу в другую локацию
                 (move-result (make-choice state "player1" "move_to_throne_room" 'princess))
                 (new-state (if (eq? (car move-result) 'success) (caddr move-result) state))
                 ;; Переключаем ход на помощника и пытаемся обменяться нарядами
                 (swap-result (make-choice new-state "player2" "swap_outfits" 'helper)))
            (assert-action-result swap-result 'error "Outfit swap should fail when characters are apart")))))))

;; ======================================
;; ТЕСТЫ НЕИЗМЕНЯЕМОСТИ СОСТОЯНИЯ
;; ======================================

(define immutability-tests
  (make-test-suite "State Immutability"
    (list
      ;; Тест неизменяемости после действия
      (make-test "state-immutable-after-action"
        (lambda ()
          (let* ((original-state (make-test-game-with-players))
                 (result (make-choice original-state "player1" "move_to_throne_room" 'princess)))
            (if (eq? (car result) 'success)
              (let ((new-state (caddr result)))
                (assert-state-immutable original-state new-state "State should be immutable"))
              (list 'failure "Action failed, cannot test immutability")))))
      
      ;; Тест неизменяемости исходного состояния персонажей
      (make-test "character-state-immutable"
        (lambda ()
          (let* ((original-state (make-test-game-with-players))
                 (original-princess-location (get-character-location original-state 'princess))
                 (result (make-choice original-state "player1" "move_to_throne_room" 'princess)))
            (if (eq? (car result) 'success)
              (let ((princess-location-after (get-character-location original-state 'princess)))
                (assert-equal original-princess-location princess-location-after 
                            "Original state character location should not change"))
              (list 'failure "Action failed, cannot test character immutability"))))))))

;; ======================================
;; ТЕСТЫ ПОЛУЧЕНИЯ ДОСТУПНЫХ ДЕЙСТВИЙ
;; ======================================

(define available-actions-tests
  (make-test-suite "Available Actions"
    (list
      ;; Тест получения действий перемещения
      (make-test "movement-actions-available"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (actions (get-available-actions state 'princess))
                 (move-actions (filter (lambda (action) 
                                       (string-prefix? "move_to_" (car action))) 
                                     actions)))
            (assert-true (> (length move-actions) 0) "Movement actions should be available"))))
      
      ;; Тест получения специальных действий
      (make-test "special-actions-available"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (actions (get-available-actions state 'princess))
                 (has-explore? (any (lambda (action) (equal? (car action) "explore")) actions))
                 (has-rest? (any (lambda (action) (equal? (car action) "rest")) actions)))
            (if (and has-explore? has-rest?)
              (list 'success "Special actions (explore, rest) are available")
              (list 'failure "Missing special actions")))))
      
      ;; Тест действий обмена нарядами
      (make-test "outfit-swap-action-when-together"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (actions (get-available-actions state 'princess))
                 (has-swap? (any (lambda (action) (equal? (car action) "swap_outfits")) actions)))
            (assert-true has-swap? "Outfit swap should be available when characters are together")))))))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТОВ
;; ======================================

;; Проверка наличия элемента в списке (any)
(define (any predicate lst)
  (cond
    ((null? lst) #f)
    ((predicate (car lst)) #t)
    (else (any predicate (cdr lst)))))

;; Проверка префикса строки
(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? prefix (substring str 0 (string-length prefix)))))

;; ======================================
;; ЗАПУСК ВСЕХ ТЕСТОВ
;; ======================================

(define (run-all-core-tests)
  "Запустить все тесты основных функций"
  (let ((suites (list state-creation-tests
                     player-management-tests
                     action-tests
                     outfit-tests
                     immutability-tests
                     available-actions-tests)))
    (let ((results (run-test-suites suites)))
      (map print-suite-result results)
      (print-test-summary results)
      results)))

;; Экспорт функции запуска тестов
;; run-all-core-tests