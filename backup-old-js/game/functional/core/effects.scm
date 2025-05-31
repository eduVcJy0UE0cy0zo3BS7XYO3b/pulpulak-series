;; Функциональное ядро: Система эффектов
;; game/functional/core/effects.scm

(define-library (game functional core effects)
  (export game-effect?
          return-effect
          bind-effect
          map-effect
          sequence-effects
          run-effect
          log-effect
          update-state-effect
          trigger-event-effect
          get-state-effect
          validate-effect
          conditional-effect
          error-effect
          catch-effect
          with-transaction
          parallel-effects)
  (import (scheme base))
  
  (begin
    ;; ======================================
    ;; EFFECT MONAD INFRASTRUCTURE
    ;; ======================================
    
    ;; Effect type: State -> (Value, State, [SideEffect])
    (define-record-type game-effect
      (make-game-effect computation)
      game-effect?
      (computation effect-computation))
    
    ;; Чистое значение без эффектов (return/pure)
    (define (return-effect value)
      (make-game-effect
        (lambda (state)
          (list value state '()))))
    
    ;; Последовательная композиция эффектов (bind/>>=)
    (define (bind-effect effect f)
      (make-game-effect
        (lambda (state)
          (let* ((result ((effect-computation effect) state))
                 (value (first result))
                 (new-state (second result))
                 (side-effects (third result))
                 (next-result ((effect-computation (f value)) new-state))
                 (final-value (first next-result))
                 (final-state (second next-result))
                 (next-side-effects (third next-result)))
            (list final-value 
                  final-state 
                  (append side-effects next-side-effects))))))
    
    ;; Map функция над эффектами (fmap)
    (define (map-effect f effect)
      (bind-effect effect
        (lambda (value)
          (return-effect (f value)))))
    
    ;; Выполнение последовательности эффектов
    (define (sequence-effects effects)
      (define (sequence-helper remaining-effects acc-values)
        (if (null? remaining-effects)
          (return-effect (reverse acc-values))
          (bind-effect (car remaining-effects)
            (lambda (value)
              (sequence-helper (cdr remaining-effects)
                             (cons value acc-values))))))
      (sequence-helper effects '()))
    
    ;; Выполнение эффекта с начальным состоянием
    (define (run-effect effect initial-state)
      ((effect-computation effect) initial-state))
    
    ;; ======================================
    ;; BASIC EFFECTS
    ;; ======================================
    
    ;; Эффект логирования
    (define (log-effect message)
      (make-game-effect
        (lambda (state)
          (list #t state (list (list 'log message))))))
    
    ;; Эффект обновления состояния
    (define (update-state-effect transformer)
      (make-game-effect
        (lambda (state)
          (let ((new-state (transformer state)))
            (list #t new-state '())))))
    
    ;; Эффект триггера игрового события
    (define (trigger-event-effect event-data)
      (make-game-effect
        (lambda (state)
          (list #t state (list (list 'event event-data))))))
    
    ;; Эффект получения текущего состояния
    (define (get-state-effect)
      (make-game-effect
        (lambda (state)
          (list state state '()))))
    
    ;; Эффект валидации
    (define (validate-effect predicate error-message)
      (make-game-effect
        (lambda (state)
          (if (predicate state)
            (list #t state '())
            (list #f state (list (list 'error error-message)))))))
    
    ;; Условный эффект
    (define (conditional-effect predicate then-effect else-effect)
      (make-game-effect
        (lambda (state)
          (if predicate
            ((effect-computation then-effect) state)
            ((effect-computation else-effect) state)))))
    
    ;; Эффект ошибки
    (define (error-effect message)
      (make-game-effect
        (lambda (state)
          (list #f state (list (list 'error message))))))
    
    ;; ======================================
    ;; ADVANCED EFFECTS
    ;; ======================================
    
    ;; Обработка ошибок
    (define (catch-effect effect error-handler)
      (make-game-effect
        (lambda (state)
          (let ((result ((effect-computation effect) state)))
            (let ((value (first result))
                  (new-state (second result))
                  (side-effects (third result)))
              (let ((errors (filter (lambda (se) (equal? (car se) 'error)) side-effects)))
                (if (null? errors)
                  result
                  ((effect-computation (error-handler errors)) state))))))))
    
    ;; Транзакционное выполнение (откат состояния при ошибке)
    (define (with-transaction effect)
      (make-game-effect
        (lambda (initial-state)
          (let ((result ((effect-computation effect) initial-state)))
            (let ((value (first result))
                  (final-state (second result))
                  (side-effects (third result)))
              (let ((errors (filter (lambda (se) (equal? (car se) 'error)) side-effects)))
                (if (null? errors)
                  result
                  (list #f initial-state side-effects))))))))
    
    ;; Параллельное выполнение эффектов (все или ничего)
    (define (parallel-effects effects)
      (make-game-effect
        (lambda (state)
          (let ((results (map (lambda (effect) 
                               ((effect-computation effect) state)) 
                             effects)))
            (let ((values (map first results))
                  (all-side-effects (apply append (map third results))))
              (let ((errors (filter (lambda (se) (equal? (car se) 'error)) all-side-effects)))
                (if (null? errors)
                  (list values state all-side-effects)
                  (list #f state all-side-effects))))))))
    
    ;; ======================================
    ;; UTILITY MACROS (for do-notation style)
    ;; ======================================
    
    ;; Макрос для удобного написания последовательностей эффектов
    ;; Использование: (do-effects (x <- effect1) (y <- effect2) (return (+ x y)))
    (define-syntax do-effects
      (syntax-rules (<- return)
        ;; Базовый случай: (return expr)
        ((do-effects (return expr))
         (return-effect expr))
        
        ;; Простое выражение без привязки
        ((do-effects expr)
         expr)
        
        ;; Привязка переменной: (var <- effect) body...
        ((do-effects (var <- effect) body ...)
         (bind-effect effect 
           (lambda (var) 
             (do-effects body ...))))
        
        ;; Выражение без привязки: effect body...
        ((do-effects effect body ...)
         (bind-effect effect 
           (lambda (_) 
             (do-effects body ...))))))
    
    ;; Условный эффект с синтаксисом if
    (define-syntax if-effect
      (syntax-rules (then else)
        ((if-effect condition then then-effect else else-effect)
         (bind-effect (get-state-effect)
           (lambda (state)
             (if condition
               then-effect
               else-effect))))))
    
    ;; When эффект
    (define-syntax when-effect
      (syntax-rules ()
        ((when-effect condition effect)
         (if-effect condition 
           then effect 
           else (return-effect #t)))))
    
    ;; Unless эффект
    (define-syntax unless-effect
      (syntax-rules ()
        ((unless-effect condition effect)
         (if-effect condition 
           then (return-effect #t)
           else effect))))
    
    ;; ======================================
    ;; DEBUGGING AND INTROSPECTION
    ;; ======================================
    
    ;; Трассировка выполнения эффекта
    (define (trace-effect label effect)
      (make-game-effect
        (lambda (state)
          (let ((start-time (current-second)))
            (let ((result ((effect-computation effect) state)))
              (let ((end-time (current-second))
                    (value (first result))
                    (final-state (second result))
                    (side-effects (third result)))
                (let ((trace-info (list 'trace label 
                                      (- end-time start-time) 
                                      value)))
                  (list value 
                        final-state 
                        (cons trace-info side-effects)))))))))
    
    ;; Проверка инвариантов состояния
    (define (assert-invariant predicate message effect)
      (make-game-effect
        (lambda (state)
          (if (predicate state)
            (let ((result ((effect-computation effect) state)))
              (let ((final-state (second result)))
                (if (predicate final-state)
                  result
                  (list #f state (list (list 'invariant-violation message))))))
            (list #f state (list (list 'precondition-violation message)))))))
    
    ;; ======================================
    ;; HELPER FUNCTIONS
    ;; ======================================
    
    ;; Получение первого элемента списка
    (define (first lst)
      (car lst))
    
    ;; Получение второго элемента списка
    (define (second lst)
      (cadr lst))
    
    ;; Получение третьего элемента списка
    (define (third lst)
      (caddr lst))
    
    ;; Текущее время (заглушка для BiwaScheme)
    (define (current-second)
      0)))