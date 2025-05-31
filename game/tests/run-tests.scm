;; Главный файл для запуска всех тестов
;; game/tests/run-tests.scm

;; ======================================
;; ЗАГРУЗКА ВСЕХ НЕОБХОДИМЫХ ФАЙЛОВ
;; ======================================

;; Загружаем основную игровую логику
(load "game/pulpulak-game.scm")

;; Загружаем тестовый фреймворк
(load "game/test-framework.scm")

;; Загружаем все тестовые наборы
(load "game/tests/core-functions.test.scm")
(load "game/tests/game-logic.test.scm")
(load "game/tests/integration.test.scm")

;; ======================================
;; КОНФИГУРАЦИЯ ТЕСТОВ
;; ======================================

;; Настройки для запуска тестов
(define *test-config*
  '((verbose . #t)
    (stop-on-failure . #f)
    (timeout . 30000)
    (log-level . info)))

;; Получить значение конфигурации
(define (get-test-config key)
  (let ((pair (assq key *test-config*)))
    (if pair (cdr pair) #f)))

;; ======================================
;; УТИЛИТЫ ДЛЯ ЗАПУСКА ТЕСТОВ
;; ======================================

;; Запуск одного набора тестов с отчетом
(define (run-suite-with-report suite-name run-fn)
  (js-log (string-append "\n=== Запуск тестов: " suite-name " ==="))
  (let* ((start-time (current-time))
         (results (run-fn))
         (end-time (current-time))
         (duration (- end-time start-time)))
    (js-log (string-append "Время выполнения: " (number->string duration) "ms"))
    results))

;; Текущее время (заглушка)
(define (current-time)
  0) ; Упрощенная реализация

;; ======================================
;; ОТДЕЛЬНЫЕ ЗАПУСКИ НАБОРОВ ТЕСТОВ
;; ======================================

;; Запуск только основных тестов
(define (run-core-tests-only)
  "Запустить только тесты основных функций"
  (run-suite-with-report "Core Functions" run-all-core-tests))

;; Запуск только тестов игровой логики
(define (run-logic-tests-only)
  "Запустить только тесты игровой логики"
  (run-suite-with-report "Game Logic" run-all-game-logic-tests))

;; Запуск только интеграционных тестов
(define (run-integration-tests-only)
  "Запустить только интеграционные тесты"
  (run-suite-with-report "Integration" run-all-integration-tests))

;; ======================================
;; БЫСТРЫЕ ТЕСТЫ (подмножество для CI)
;; ======================================

(define quick-core-tests
  (make-test-suite "Quick Core Tests"
    (list
      (make-test "quick-state-creation"
        (lambda ()
          (let ((state (make-game-state "quick-test")))
            (assert-true state "Game state created"))))
      
      (make-test "quick-player-join"
        (lambda ()
          (let* ((state (make-game-state "quick-test"))
                 (result (join-game state "player1" 'princess)))
            (assert-action-result result 'success "Player joined"))))
      
      (make-test "quick-movement"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "move_to_throne_room" 'princess)))
            (assert-action-result result 'success "Movement works")))))))

(define quick-logic-tests
  (make-test-suite "Quick Logic Tests"
    (list
      (make-test "quick-turn-switch"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (initial-turn (get-game-state-property state 'current-turn))
                 (new-state (switch-turn state))
                 (new-turn (get-game-state-property new-state 'current-turn)))
            (assert-false (eq? initial-turn new-turn) "Turn switches"))))
      
      (make-test "quick-outfit-swap"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "swap_outfits" 'princess)))
            (assert-action-result result 'success "Outfit swap works")))))))

(define quick-integration-tests
  (make-test-suite "Quick Integration Tests"
    (list
      (make-test "quick-game-flow"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (actions '((princess "explore") (helper "rest")))
                 (final-state (simulate-complex-actions state actions)))
            (assert-not-null final-state "Basic game flow works")))))))

;; Запуск быстрых тестов
(define (run-quick-tests)
  "Запустить быстрые тесты (для CI/CD)"
  (js-log "\n=== БЫСТРЫЕ ТЕСТЫ ===")
  (let ((suites (list quick-core-tests quick-logic-tests quick-integration-tests)))
    (let ((results (run-test-suites suites)))
      (map print-suite-result results)
      (print-test-summary results)
      results)))

;; ======================================
;; ТЕСТЫ ДЛЯ ОТЛАДКИ
;; ======================================

;; Отладочные тесты для конкретных проблем
(define debug-tests
  (make-test-suite "Debug Tests"
    (list
      (make-test "debug-state-structure"
        (lambda ()
          (let ((state (make-game-state "debug-room")))
            (js-log (string-append "State structure: " (value->string state)))
            (assert-true state "Debug: state created"))))
      
      (make-test "debug-character-data"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (princess (get-character-data state 'princess)))
            (js-log (string-append "Princess data: " (value->string princess)))
            (assert-not-null princess "Debug: princess data exists"))))
      
      (make-test "debug-action-flow"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (result (make-choice state "player1" "explore" 'princess)))
            (js-log (string-append "Action result: " (value->string result)))
            (assert-true (list? result) "Debug: action returns result")))))))

;; Запуск отладочных тестов
(define (run-debug-tests)
  "Запустить отладочные тесты с подробным выводом"
  (js-log "\n=== ОТЛАДОЧНЫЕ ТЕСТЫ ===")
  (let ((results (run-test-suite debug-tests)))
    (print-suite-result results)
    results))

;; ======================================
;; СТРЕСС-ТЕСТЫ
;; ======================================

;; Стресс-тесты для проверки производительности
(define stress-tests
  (make-test-suite "Stress Tests"
    (list
      (make-test "stress-multiple-states"
        (lambda ()
          (let ((states (map (lambda (i) (make-game-state (string-append "stress-" (number->string i))))
                            (range 100))))
            (assert-equal 100 (length states) "Created 100 game states"))))
      
      (make-test "stress-long-action-sequence"
        (lambda ()
          (let* ((state (make-test-game-with-players))
                 (long-actions (make-list 50 'explore))
                 (final-state (simulate-repeated-action state 'princess long-actions)))
            (assert-not-null final-state "Handled 50 consecutive actions"))))
      
      (make-test "stress-rapid-turn-switching"
        (lambda ()
          (let ((final-state (fold-left (lambda (s _) (switch-turn s))
                                       (make-test-game-with-players)
                                       (range 100))))
            (assert-not-null final-state "Handled 100 turn switches")))))))

;; Запуск стресс-тестов
(define (run-stress-tests)
  "Запустить стресс-тесты"
  (js-log "\n=== СТРЕСС-ТЕСТЫ ===")
  (let ((results (run-test-suite stress-tests)))
    (print-suite-result results)
    results))

;; ======================================
;; ГЛАВНЫЕ ФУНКЦИИ ЗАПУСКА
;; ======================================

;; Запуск всех тестов с подробным отчетом
(define (run-complete-test-suite)
  "Запустить полный набор тестов"
  (js-log "\n########################################")
  (js-log "# ПОЛНЫЙ НАБОР ТЕСТОВ PULPULAK GAME")
  (js-log "########################################")
  
  (let* ((core-results (run-core-tests-only))
         (logic-results (run-logic-tests-only))
         (integration-results (run-integration-tests-only))
         (quick-results (run-quick-tests))
         (stress-results (run-stress-tests)))
    
    (js-log "\n########################################")
    (js-log "# ФИНАЛЬНЫЙ ОТЧЕТ")
    (js-log "########################################")
    
    (let ((all-results (append core-results logic-results integration-results)))
      (print-test-summary all-results)
      all-results)))

;; Запуск тестов для Continuous Integration
(define (run-ci-tests)
  "Запустить тесты для CI/CD (быстрые + основные)"
  (js-log "\n=== CI/CD ТЕСТЫ ===")
  (let* ((quick-results (run-quick-tests))
         (core-results (run-core-tests-only)))
    (let ((combined-results (append quick-results core-results)))
      (print-test-summary combined-results)
      ;; Возвращаем код выхода
      (let* ((total-passed (fold-left + 0 (map caddr combined-results)))
             (total-failed (fold-left + 0 (map cadddr combined-results))))
        (if (= total-failed 0) 0 1)))))

;; Интерактивный запуск тестов
(define (run-interactive-tests)
  "Интерактивный режим выбора тестов"
  (js-log "\n=== ИНТЕРАКТИВНЫЙ РЕЖИМ ТЕСТОВ ===")
  (js-log "Доступные опции:")
  (js-log "1. Основные тесты (core)")
  (js-log "2. Тесты логики (logic)")
  (js-log "3. Интеграционные тесты (integration)")
  (js-log "4. Быстрые тесты (quick)")
  (js-log "5. Все тесты (all)")
  (js-log "6. Отладочные тесты (debug)")
  (js-log "7. Стресс-тесты (stress)")
  
  ;; Для демонстрации запускаем все тесты
  (js-log "\nЗапуск всех тестов...")
  (run-all-tests))

;; ======================================
;; ЭКСПОРТ И ИНИЦИАЛИЗАЦИЯ
;; ======================================

;; Главная функция по умолчанию
(define (main)
  "Главная функция - запускает все тесты"
  (run-all-tests))

;; Функция для запуска из командной строки
(define (run-tests-from-cli args)
  "Запуск тестов с аргументами командной строки"
  (cond
    ((null? args) (run-all-tests))
    ((equal? (car args) "quick") (run-quick-tests))
    ((equal? (car args) "core") (run-core-tests-only))
    ((equal? (car args) "logic") (run-logic-tests-only))
    ((equal? (car args) "integration") (run-integration-tests-only))
    ((equal? (car args) "debug") (run-debug-tests))
    ((equal? (car args) "stress") (run-stress-tests))
    ((equal? (car args) "ci") (run-ci-tests))
    ((equal? (car args) "complete") (run-complete-test-suite))
    ((equal? (car args) "interactive") (run-interactive-tests))
    (else 
      (js-log "Неизвестная команда. Доступные: quick, core, logic, integration, debug, stress, ci, complete, interactive")
      (run-all-tests))))

;; Вывод справки
(define (print-test-help)
  "Вывести справку по использованию тестов"
  (js-log "\n=== СПРАВКА ПО ТЕСТАМ PULPULAK GAME ===")
  (js-log "")
  (js-log "Использование:")
  (js-log "  (run-tests-from-cli '())         - все тесты")
  (js-log "  (run-tests-from-cli '(\"quick\"))   - быстрые тесты")
  (js-log "  (run-tests-from-cli '(\"core\"))    - основные функции")
  (js-log "  (run-tests-from-cli '(\"logic\"))   - игровая логика")
  (js-log "  (run-tests-from-cli '(\"integration\")) - интеграционные")
  (js-log "  (run-tests-from-cli '(\"debug\"))   - отладочные")
  (js-log "  (run-tests-from-cli '(\"stress\"))  - стресс-тесты")
  (js-log "  (run-tests-from-cli '(\"ci\"))      - для CI/CD")
  (js-log "  (run-tests-from-cli '(\"complete\")) - полный набор")
  (js-log "")
  (js-log "Функции для прямого вызова:")
  (js-log "  (run-all-tests)          - все тесты")
  (js-log "  (run-quick-tests)        - быстрые тесты")
  (js-log "  (run-debug-tests)        - отладочные тесты")
  (js-log "  (run-stress-tests)       - стресс-тесты")
  (js-log ""))

;; Инициализация при загрузке файла
(js-log "\n=== ТЕСТОВАЯ СИСТЕМА PULPULAK GAME ЗАГРУЖЕНА ===")
(js-log "Используйте (print-test-help) для справки")
(js-log "Используйте (run-all-tests) для запуска всех тестов")

;; Экспорт основных функций
;; main, run-all-tests, run-quick-tests, run-debug-tests, run-stress-tests
;; run-tests-from-cli, print-test-help, run-complete-test-suite, run-ci-tests