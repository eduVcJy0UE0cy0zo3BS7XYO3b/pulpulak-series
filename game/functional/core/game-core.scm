;; Основная функциональная библиотека игры
;; game/functional/core/game-core.scm

;; ======================================
;; СТРУКТУРЫ ДАННЫХ ИГРЫ
;; ======================================

;; Создание начального состояния игры
(define (make-initial-state room-id players)
  (list 'game-state
        'room-id room-id
        'scene 'coop_awakening
        'current-turn 'princess
        'chapter 1
        'characters (make-initial-characters players)
        'world (make-initial-world)
        'quests (make-initial-quests)
        'events '()))

;; Создание начальных персонажей
(define (make-initial-characters players)
  (list 
    (list 'princess
          'id 'princess
          'location 'princess_chamber
          'outfit 'princess_dress
          'inventory '()
          'stats (list (cons 'loyalty 50) (cons 'knowledge 30) (cons 'charm 70))
          'dialogue-state '())
    (list 'helper
          'id 'helper
          'location 'princess_chamber  
          'outfit 'common_dress
          'inventory '(translation_earrings voice_medallion)
          'stats (list (cons 'loyalty 50) (cons 'knowledge 60) (cons 'charm 40))
          'dialogue-state '())))

;; Создание начального мира
(define (make-initial-world)
  (list 'time 'early_morning
        'locations (list 
          (list 'princess_chamber 'accessible #t 'npcs '())
          (list 'throne_room 'accessible #t 'npcs '(royal_advisor))
          (list 'kitchen 'accessible #t 'npcs '(cook))
          (list 'garden 'accessible #t 'npcs '())
          (list 'library 'accessible #t 'npcs '(librarian))
          (list 'village 'accessible #t 'npcs '(old_woman merchant)))
        'npcs (list
          (list 'royal_advisor 'location 'throne_room 'available #t 'memory '())
          (list 'cook 'location 'kitchen 'available #t 'memory '())
          (list 'librarian 'location 'library 'available #t 'memory '())
          (list 'old_woman 'location 'village 'available #t 'memory '())
          (list 'merchant 'location 'village 'available #t 'memory '()))
        'events '()))

;; Создание начального состояния квестов
(define (make-initial-quests)
  (list 'active '()
        'completed '()
        'global-memory '()))

;; ======================================
;; ГЕТТЕРЫ СОСТОЯНИЯ
;; ======================================

;; Получить данные персонажа
(define (get-character-data state character-id)
  (let ((characters (get-value state 'characters)))
    (assoc character-id characters)))

;; Получить значение из ассоциативного списка состояния
(define (get-value state key)
  (let ((pair (assoc key (cdr state))))
    (if pair (cadr pair) #f)))

;; Получить значение из данных персонажа
(define (get-character-value character-data key)
  (let ((pair (assoc key (cdr character-data))))
    (if pair (cadr pair) #f)))

;; Получить состояние мира
(define (get-world-state state)
  (get-value state 'world))

;; Получить состояние квестов
(define (get-quest-state state)
  (get-value state 'quests))

;; ======================================
;; ПРЕДИКАТЫ СОСТОЯНИЯ
;; ======================================

;; Находится ли персонаж в локации
(define (character-at-location? state character-id location)
  (let ((character (get-character-data state character-id)))
    (and character
         (eq? (get-character-value character 'location) location))))

;; Есть ли у персонажа предмет
(define (character-has-item? state character-id item)
  (let ((character (get-character-data state character-id)))
    (and character
         (member item (get-character-value character 'inventory)))))

;; Носит ли персонаж определенный наряд
(define (character-wearing-outfit? state character-id outfit)
  (let ((character (get-character-data state character-id)))
    (and character
         (eq? (get-character-value character 'outfit) outfit))))

;; Может ли персонаж переместиться в локацию
(define (character-can-move? state character-id location)
  (and (get-character-data state character-id)
       (location-accessible? state location)
       (not (character-at-location? state character-id location))))

;; Доступна ли локация
(define (location-accessible? state location)
  (let* ((world (get-world-state state))
         (locations (get-value world 'locations))
         (location-data (assoc location locations)))
    (and location-data
         (get-value location-data 'accessible))))

;; Может ли персонаж взаимодействовать с NPC
(define (character-can-interact? state character-id npc-id)
  (and (get-character-data state character-id)
       (npc-at-same-location? state character-id npc-id)
       (npc-available? state npc-id)))

;; Находится ли NPC в той же локации что и персонаж
(define (npc-at-same-location? state character-id npc-id)
  (let* ((character (get-character-data state character-id))
         (char-location (get-character-value character 'location))
         (world (get-world-state state))
         (npcs (get-value world 'npcs))
         (npc-data (assoc npc-id npcs)))
    (and character npc-data
         (eq? char-location (get-value npc-data 'location)))))

;; Доступен ли NPC для взаимодействия
(define (npc-available? state npc-id)
  (let* ((world (get-world-state state))
         (npcs (get-value world 'npcs))
         (npc-data (assoc npc-id npcs)))
    (and npc-data
         (get-value npc-data 'available))))

;; ======================================
;; ТРАНСФОРМЕРЫ СОСТОЯНИЯ
;; ======================================

;; Переместить персонажа
(define (move-character state character-id new-location)
  (update-character-property state character-id 'location new-location))

;; Добавить предмет персонажу
(define (add-item-to-character state character-id item)
  (let* ((character (get-character-data state character-id))
         (current-inventory (get-character-value character 'inventory))
         (new-inventory (cons item current-inventory)))
    (update-character-property state character-id 'inventory new-inventory)))

;; Удалить предмет у персонажа
(define (remove-item-from-character state character-id item)
  (let* ((character (get-character-data state character-id))
         (current-inventory (get-character-value character 'inventory))
         (new-inventory (remove item current-inventory)))
    (update-character-property state character-id 'inventory new-inventory)))

;; Изменить наряд персонажа
(define (set-character-outfit state character-id new-outfit)
  (update-character-property state character-id 'outfit new-outfit))

;; Обновить свойство персонажа
(define (update-character-property state character-id property new-value)
  (let* ((characters (get-value state 'characters))
         (character (assoc character-id characters))
         (updated-character (update-assoc-property character property new-value))
         (updated-characters (replace-assoc characters character-id updated-character)))
    (update-state-property state 'characters updated-characters)))

;; Обновить свойство состояния
(define (update-state-property state property new-value)
  (let ((updated-pairs (replace-assoc (cdr state) property new-value)))
    (cons 'game-state updated-pairs)))

;; ======================================
;; ИГРОВАЯ ЛОГИКА
;; ======================================

;; Валидация действия
(define (validate-action state character action)
  (cond
    ((not (get-character-data state character)) #f)
    ((not (list? action)) #f)
    ((not (>= (length action) 2)) #f)
    (else
      (let ((action-type (car action)))
        (case action-type
          ((move)
           (let ((location (cadr action)))
             (character-can-move? state character location)))
          ((interact)
           (let ((npc-id (cadr action)))
             (character-can-interact? state character npc-id)))
          ((choice dialogue)
           #t) ; Базовая валидация для выборов и диалогов
          (else #f))))))

;; Обработка игрового действия
(define (process-game-action state character action)
  (if (validate-action state character action)
    (let ((action-type (car action)))
      (case action-type
        ((move)
         (let ((location (cadr action)))
           (js-log (string-append (symbol->string character) " moves to " (symbol->string location)))
           (emit-event! 'character-moved (list character location))
           (move-character state character location)))
        ((interact)
         (let ((npc-id (cadr action)))
           (js-log (string-append (symbol->string character) " interacts with " (symbol->string npc-id)))
           (emit-event! 'interaction (list character npc-id))
           state)) ; Базовая реализация - состояние не меняется
        ((choice)
         (let ((choice-id (cadr action)))
           (js-log (string-append (symbol->string character) " makes choice: " (symbol->string choice-id)))
           (emit-event! 'choice-made (list character choice-id))
           state)) ; Базовая реализация
        (else state)))
    (begin
      (js-log "Invalid action")
      state)))

;; Получение доступных действий
(define (get-available-actions state character)
  (let* ((character-data (get-character-data state character))
         (current-location (get-character-value character-data 'location))
         (world (get-world-state state))
         (locations (get-value world 'locations)))
    
    ;; Возвращаем базовые действия перемещения
    (append 
      (get-movement-actions state character current-location locations)
      (get-interaction-actions state character current-location)
      (get-basic-actions))))

;; Получить действия перемещения
(define (get-movement-actions state character current-location locations)
  (filter-map
    (lambda (location-data)
      (let ((location-id (car location-data)))
        (if (and (not (eq? location-id current-location))
                (get-value location-data 'accessible))
          (list 'move location-id
                (string-append "Move to " (symbol->string location-id)))
          #f)))
    locations))

;; Получить действия взаимодействия
(define (get-interaction-actions state character current-location)
  (let* ((world (get-world-state state))
         (npcs (get-value world 'npcs)))
    (filter-map
      (lambda (npc-data)
        (let ((npc-id (car npc-data))
              (npc-location (get-value npc-data 'location)))
          (if (and (eq? npc-location current-location)
                  (get-value npc-data 'available))
            (list 'interact npc-id
                  (string-append "Talk to " (symbol->string npc-id)))
            #f)))
      npcs)))

;; Получить базовые действия
(define (get-basic-actions)
  (list 
    (list 'choice 'explore "Explore current location")
    (list 'choice 'rest "Rest and recover")))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ======================================

;; Обновить значение в ассоциативном списке
(define (update-assoc-property alist property new-value)
  (cons (car alist) ; Сохраняем ключ
        (replace-assoc (cdr alist) property new-value)))

;; Заменить значение в ассоциативном списке
(define (replace-assoc alist key new-value)
  (cond
    ((null? alist) (list (list key new-value)))
    ((eq? (caar alist) key) 
     (cons (list key new-value) (cdr alist)))
    (else 
     (cons (car alist) (replace-assoc (cdr alist) key new-value)))))

;; Удалить элемент из списка
(define (remove item lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) item) (cdr lst))
    (else (cons (car lst) (remove item (cdr lst))))))

;; Filter-map функция
(define (filter-map proc lst)
  (cond
    ((null? lst) '())
    (else
      (let ((result (proc (car lst))))
        (if result
          (cons result (filter-map proc (cdr lst)))
          (filter-map proc (cdr lst)))))))

;; Конвертация символа в строку (если нет встроенной)
(define (symbol->string sym)
  (if (symbol? sym)
    (string-append "" sym) ; Упрощенная конвертация
    (if (string? sym) sym "unknown")))

;; Конкатенация строк
(define (string-append . strings)
  (fold-left string-concat "" strings))

;; Простая конкатенация строк (если нет встроенной)
(define (string-concat s1 s2)
  s1) ; Упрощенная реализация

;; Fold-left функция (если нет встроенной)
(define (fold-left proc init lst)
  (if (null? lst)
    init
    (fold-left proc (proc init (car lst)) (cdr lst))))

;; Проверка на символ (если нет встроенной)
(define (symbol? obj)
  #t) ; Упрощенная реализация

;; Проверка на строку (если нет встроенной)  
(define (string? obj)
  #t) ; Упрощенная реализация