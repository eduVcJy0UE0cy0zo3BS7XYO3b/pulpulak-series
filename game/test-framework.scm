;; Функциональный тестовый фреймворк для Scheme
;; game/test-framework.scm

;; ======================================
;; ТЕСТОВЫЙ ФРЕЙМВОРК
;; ======================================

;; Структура теста
(define (make-test name test-fn)
  (list 'test name test-fn))

(define (test-name test)
  (cadr test))

(define (test-fn test)
  (caddr test))

;; Результат теста
(define (make-test-result name passed? message)
  (list 'test-result name passed? message))

(define (test-result-name result)
  (cadr result))

(define (test-result-passed? result)
  (caddr result))

(define (test-result-message result)
  (cadddr result))

;; Тестовый набор
(define (make-test-suite name tests)
  (list 'test-suite name tests))

(define (suite-name suite)
  (cadr suite))

(define (suite-tests suite)
  (caddr suite))

;; ======================================
;; УТВЕРЖДЕНИЯ (ASSERTIONS)
;; ======================================

;; Базовое утверждение
(define (assert condition message)
  (if condition
    (list 'success message)
    (list 'failure message)))

;; Проверка равенства
(define (assert-equal expected actual message)
  (if (equal? expected actual)
    (list 'success (string-append message " - PASSED"))
    (list 'failure 
          (string-append message " - FAILED: expected " 
                        (value->string expected) 
                        " but got " 
                        (value->string actual)))))

;; Проверка на истинность
(define (assert-true value message)
  (if value
    (list 'success (string-append message " - PASSED"))
    (list 'failure (string-append message " - FAILED: expected true"))))

;; Проверка на ложность
(define (assert-false value message)
  (if (not value)
    (list 'success (string-append message " - PASSED"))
    (list 'failure (string-append message " - FAILED: expected false"))))

;; Проверка предиката
(define (assert-predicate predicate value message)
  (if (predicate value)
    (list 'success (string-append message " - PASSED"))
    (list 'failure (string-append message " - FAILED: predicate test failed"))))

;; Проверка на nil/пустоту
(define (assert-null value message)
  (if (null? value)
    (list 'success (string-append message " - PASSED"))
    (list 'failure (string-append message " - FAILED: expected null/empty"))))

;; Проверка на не-nil
(define (assert-not-null value message)
  (if (not (null? value))
    (list 'success (string-append message " - PASSED"))
    (list 'failure (string-append message " - FAILED: expected non-null"))))

;; ======================================
;; СПЕЦИАЛЬНЫЕ УТВЕРЖДЕНИЯ ДЛЯ ИГРЫ
;; ======================================

;; Проверка состояния игры
(define (assert-game-state state property expected message)
  (let ((actual (get-game-state-property state property)))
    (assert-equal expected actual 
                 (string-append "Game state " (symbol->string property) " " message))))

;; Проверка локации персонажа
(define (assert-character-location state character expected-location message)
  (let ((actual-location (get-character-location state character)))
    (assert-equal expected-location actual-location
                 (string-append (symbol->string character) " location " message))))

;; Проверка наряда персонажа
(define (assert-character-outfit state character expected-outfit message)
  (let* ((character-data (get-character-data state character))
         (actual-outfit (get-character-property character-data 'outfit)))
    (assert-equal expected-outfit actual-outfit
                 (string-append (symbol->string character) " outfit " message))))

;; Проверка инвентаря
(define (assert-character-has-item state character item message)
  (let* ((character-data (get-character-data state character))
         (inventory (get-character-property character-data 'inventory))
         (has-item? (member item inventory)))
    (assert-true has-item?
                (string-append (symbol->string character) " has " 
                             (symbol->string item) " " message))))

;; Проверка результата действия
(define (assert-action-result result expected-status message)
  (let ((status (car result)))
    (assert-equal expected-status status
                 (string-append "Action result " message))))

;; ======================================
;; ВЫПОЛНЕНИЕ ТЕСТОВ
;; ======================================

;; Выполнить один тест
(define (run-test test)
  (let* ((name (test-name test))
         (test-function (test-fn test))
         (result (test-function)))
    (if (eq? (car result) 'success)
      (make-test-result name #t (cadr result))
      (make-test-result name #f (cadr result)))))

;; Выполнить набор тестов
(define (run-test-suite suite)
  (let* ((name (suite-name suite))
         (tests (suite-tests suite))
         (results (map run-test tests))
         (passed (filter test-result-passed? results))
         (failed (filter (lambda (r) (not (test-result-passed? r))) results)))
    (list 'suite-result 
          name 
          (length passed) 
          (length failed) 
          results)))

;; Выполнить несколько наборов тестов
(define (run-test-suites suites)
  (map run-test-suite suites))

;; ======================================
;; ОТЧЕТНОСТЬ
;; ======================================

;; Вывести результат теста
(define (print-test-result result)
  (let ((name (test-result-name result))
        (passed? (test-result-passed? result))
        (message (test-result-message result)))
    (js-log (string-append "[" 
                          (if passed? "PASS" "FAIL") 
                          "] " 
                          name 
                          ": " 
                          message))))

;; Вывести результаты набора тестов
(define (print-suite-result suite-result)
  (let ((name (cadr suite-result))
        (passed-count (caddr suite-result))
        (failed-count (cadddr suite-result))
        (results (car (cddddr suite-result))))
    (js-log (string-append "\n=== Test Suite: " name " ==="))
    (map print-test-result results)
    (js-log (string-append "Passed: " (number->string passed-count) 
                          ", Failed: " (number->string failed-count)
                          ", Total: " (number->string (+ passed-count failed-count))))))

;; Вывести общую статистику
(define (print-test-summary suite-results)
  (let* ((total-passed (fold-left + 0 (map caddr suite-results)))
         (total-failed (fold-left + 0 (map cadddr suite-results)))
         (total-tests (+ total-passed total-failed)))
    (js-log "\n=== TEST SUMMARY ===")
    (js-log (string-append "Total Tests: " (number->string total-tests)))
    (js-log (string-append "Passed: " (number->string total-passed)))
    (js-log (string-append "Failed: " (number->string total-failed)))
    (js-log (string-append "Success Rate: " 
                          (number->string (if (> total-tests 0)
                                            (* 100 (/ total-passed total-tests))
                                            100))
                          "%"))))

;; ======================================
;; ТЕСТОВЫЕ УТИЛИТЫ
;; ======================================

;; Создать тестовое состояние игры
(define (make-test-game-state)
  (make-game-state "test-room"))

;; Создать состояние с двумя игроками
(define (make-test-game-with-players)
  (let ((state (make-test-game-state)))
    (set-players state "player1" "player2")))

;; Симуляция последовательности действий
(define (simulate-actions state actions)
  (fold-left
    (lambda (current-state action)
      (let ((character (car action))
            (choice (cadr action)))
        (let ((result (make-choice current-state "player1" choice character)))
          (if (eq? (car result) 'success)
            (caddr result)
            current-state))))
    state
    actions))

;; Проверка неизменяемости состояния
(define (assert-state-immutable original-state modified-state message)
  (assert-false (eq? original-state modified-state)
               (string-append "State immutability " message)))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ======================================

;; Конвертация значения в строку для отображения
(define (value->string value)
  (cond
    ((string? value) value)
    ((symbol? value) (symbol->string value))
    ((number? value) (number->string value))
    ((boolean? value) (if value "true" "false"))
    ((null? value) "null")
    ((list? value) 
     (string-append "(" 
                   (fold-left (lambda (acc item)
                               (string-append acc " " (value->string item)))
                             ""
                             value)
                   ")"))
    (else "unknown")))

;; Фильтрация списка
(define (filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))

;; Fold-left если нет встроенной
(define (fold-left proc init lst)
  (if (null? lst)
    init
    (fold-left proc (proc init (car lst)) (cdr lst))))

;; Конвертация числа в строку (базовая)
(define (number->string num)
  (if (number? num) 
    (js-number->string num)
    "NaN"))

;; JavaScript интерфейс для логирования
(define (js-log message)
  (display message)
  (newline))

;; ======================================
;; ЭКСПОРТ ФУНКЦИЙ
;; ======================================

;; Основные функции создания тестов
;; make-test, make-test-suite, run-test, run-test-suite, run-test-suites

;; Утверждения
;; assert, assert-equal, assert-true, assert-false, assert-predicate
;; assert-null, assert-not-null

;; Игровые утверждения  
;; assert-game-state, assert-character-location, assert-character-outfit
;; assert-character-has-item, assert-action-result

;; Выполнение и отчетность
;; print-test-result, print-suite-result, print-test-summary

;; Утилиты
;; make-test-game-state, make-test-game-with-players, simulate-actions
;; assert-state-immutable