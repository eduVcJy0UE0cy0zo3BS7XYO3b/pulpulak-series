;; Полная игровая логика на Scheme
;; game/functional/core/game-logic.scm

;; ======================================
;; СОЗДАНИЕ ИГРОВОГО СОСТОЯНИЯ
;; ======================================

(define (create-game-state room-id players)
  "Создает новое игровое состояние"
  (list 'game-state
        (list 'room-id room-id)
        (list 'scene 'coop_awakening)
        (list 'current-turn 'princess)
        (list 'chapter 1)
        (list 'characters (create-characters players))
        (list 'world (create-world))
        (list 'quests (create-quests))
        (list 'events '())))

(define (create-characters players)
  "Создает персонажей игры"
  (list 
    (list 'princess
          (list 'id 'princess)
          (list 'location 'princess_chamber)
          (list 'outfit 'princess_dress)
          (list 'inventory '())
          (list 'stats (list (cons 'loyalty 50) (cons 'knowledge 30) (cons 'charm 70))))
    (list 'helper
          (list 'id 'helper)
          (list 'location 'princess_chamber)
          (list 'outfit 'common_dress)
          (list 'inventory '(translation_earrings voice_medallion))
          (list 'stats (list (cons 'loyalty 50) (cons 'knowledge 60) (cons 'charm 40))))))

(define (create-world)
  "Создает игровой мир"
  (list 
    (list 'time 'early_morning)
    (list 'locations (list
      (list 'princess_chamber (list 'accessible #t) (list 'npcs '()))
      (list 'throne_room (list 'accessible #t) (list 'npcs '(royal_advisor)))
      (list 'kitchen (list 'accessible #t) (list 'npcs '(cook)))
      (list 'garden (list 'accessible #t) (list 'npcs '()))
      (list 'library (list 'accessible #t) (list 'npcs '(librarian)))
      (list 'village (list 'accessible #t) (list 'npcs '(old_woman merchant)))))
    (list 'npcs (list
      (list 'royal_advisor (list 'location 'throne_room) (list 'available #t))
      (list 'cook (list 'location 'kitchen) (list 'available #t))
      (list 'librarian (list 'location 'library) (list 'available #t))
      (list 'old_woman (list 'location 'village) (list 'available #t))
      (list 'merchant (list 'location 'village) (list 'available #t))))
    (list 'events '())))

(define (create-quests)
  "Создает систему квестов"
  (list 
    (list 'active '())
    (list 'completed '())
    (list 'memory '())))

;; ======================================
;; ПОЛУЧЕНИЕ ДАННЫХ ИЗ СОСТОЯНИЯ
;; ======================================

(define (get-from-state state key)
  "Получает значение из состояния игры"
  (let ((pair (assq key (cdr state))))
    (if pair (cadr pair) #f)))

(define (get-character state character-id)
  "Получает данные персонажа"
  (let ((characters (get-from-state state 'characters)))
    (assq character-id characters)))

(define (get-from-character character key)
  "Получает значение из данных персонажа"
  (let ((pair (assq key (cdr character))))
    (if pair (cadr pair) #f)))

(define (get-character-location state character-id)
  "Получает локацию персонажа"
  (let ((character (get-character state character-id)))
    (if character (get-from-character character 'location) #f)))

(define (get-character-inventory state character-id)
  "Получает инвентарь персонажа"
  (let ((character (get-character state character-id)))
    (if character (get-from-character character 'inventory) '())))

(define (get-world-locations state)
  "Получает список локаций"
  (let ((world (get-from-state state 'world)))
    (get-from-state world 'locations)))

;; ======================================
;; ПОЛУЧЕНИЕ ДОСТУПНЫХ ДЕЙСТВИЙ
;; ======================================

(define (get-available-actions-for-character state character-id)
  "Получает все доступные действия для персонажа"
  (let ((character-location (get-character-location state character-id))
        (locations (get-world-locations state)))
    (append 
      (get-movement-actions state character-id character-location locations)
      (get-interaction-actions state character-id character-location)
      (get-basic-actions))))

(define (get-movement-actions state character-id current-location locations)
  "Получает действия перемещения"
  (filter-actions
    (lambda (location-data)
      (let ((location-id (car location-data))
            (location-props (cdr location-data)))
        (if (and (not (eq? location-id current-location))
                (is-location-accessible? location-props))
          (create-move-action location-id)
          #f)))
    locations))

(define (get-interaction-actions state character-id current-location)
  "Получает действия взаимодействия с NPC"
  (let* ((world (get-from-state state 'world))
         (npcs (get-from-state world 'npcs)))
    (filter-actions
      (lambda (npc-data)
        (let ((npc-id (car npc-data))
              (npc-props (cdr npc-data)))
          (if (and (eq? (get-npc-location npc-props) current-location)
                  (is-npc-available? npc-props))
            (create-interact-action npc-id)
            #f)))
      npcs)))

(define (get-basic-actions)
  "Получает базовые игровые действия"
  (list 
    (create-choice-action 'explore "Explore current location")
    (create-choice-action 'rest "Rest and recover")
    (create-choice-action 'inventory "Check inventory")))

;; ======================================
;; СОЗДАНИЕ ДЕЙСТВИЙ
;; ======================================

(define (create-move-action location)
  "Создает действие перемещения"
  (list 'action
        (list 'type 'move)
        (list 'location location)
        (list 'text (string-append "Move to " (symbol->string location)))))

(define (create-interact-action npc-id)
  "Создает действие взаимодействия"
  (list 'action
        (list 'type 'interact)
        (list 'npc npc-id)
        (list 'text (string-append "Talk to " (symbol->string npc-id)))))

(define (create-choice-action choice-id text)
  "Создает действие выбора"
  (list 'action
        (list 'type 'choice)
        (list 'id choice-id)
        (list 'text text)))

;; ======================================
;; ВАЛИДАЦИЯ ДЕЙСТВИЙ
;; ======================================

(define (validate-game-action state character-id action)
  "Валидирует игровое действие"
  (let ((action-type (get-action-type action)))
    (cond
      ((not (get-character state character-id)) #f)
      ((eq? action-type 'move) (validate-move-action state character-id action))
      ((eq? action-type 'interact) (validate-interact-action state character-id action))
      ((eq? action-type 'choice) (validate-choice-action state character-id action))
      (else #f))))

(define (validate-move-action state character-id action)
  "Валидирует действие перемещения"
  (let ((target-location (get-action-location action))
        (current-location (get-character-location state character-id)))
    (and target-location
         (not (eq? target-location current-location))
         (is-location-accessible-in-state? state target-location))))

(define (validate-interact-action state character-id action)
  "Валидирует действие взаимодействия"
  (let ((npc-id (get-action-npc action))
        (character-location (get-character-location state character-id)))
    (and npc-id
         (is-npc-at-location? state npc-id character-location)
         (is-npc-available-in-state? state npc-id))))

(define (validate-choice-action state character-id action)
  "Валидирует действие выбора"
  (let ((choice-id (get-action-choice-id action)))
    (and choice-id
         (is-valid-choice? choice-id))))

;; ======================================
;; ОБРАБОТКА ДЕЙСТВИЙ
;; ======================================

(define (process-game-action state character-id action)
  "Обрабатывает игровое действие"
  (if (validate-game-action state character-id action)
    (let ((action-type (get-action-type action)))
      (cond
        ((eq? action-type 'move) (process-move-action state character-id action))
        ((eq? action-type 'interact) (process-interact-action state character-id action))
        ((eq? action-type 'choice) (process-choice-action state character-id action))
        (else state)))
    state))

(define (process-move-action state character-id action)
  "Обрабатывает перемещение персонажа"
  (let ((new-location (get-action-location action)))
    (js-log (string-append (symbol->string character-id) " moves to " (symbol->string new-location)))
    (update-character-location state character-id new-location)))

(define (process-interact-action state character-id action)
  "Обрабатывает взаимодействие с NPC"
  (let ((npc-id (get-action-npc action)))
    (js-log (string-append (symbol->string character-id) " interacts with " (symbol->string npc-id)))
    ;; Пока просто логируем, можно расширить логику диалогов
    state))

(define (process-choice-action state character-id action)
  "Обрабатывает выбор игрока"
  (let ((choice-id (get-action-choice-id action)))
    (js-log (string-append (symbol->string character-id) " makes choice: " (symbol->string choice-id)))
    ;; Расширим логику обработки выборов
    state))

;; ======================================
;; ОБНОВЛЕНИЕ СОСТОЯНИЯ
;; ======================================

(define (update-character-location state character-id new-location)
  "Обновляет локацию персонажа"
  (let* ((characters (get-from-state state 'characters))
         (updated-characters (update-character-property characters character-id 'location new-location)))
    (update-state-property state 'characters updated-characters)))

(define (update-character-property characters character-id property new-value)
  "Обновляет свойство персонажа"
  (map (lambda (character)
         (if (eq? (car character) character-id)
           (update-character-data character property new-value)
           character))
       characters))

(define (update-character-data character property new-value)
  "Обновляет данные персонажа"
  (cons (car character)
        (update-property-list (cdr character) property new-value)))

(define (update-property-list prop-list property new-value)
  "Обновляет список свойств"
  (cond
    ((null? prop-list) (list (list property new-value)))
    ((eq? (caar prop-list) property) 
     (cons (list property new-value) (cdr prop-list)))
    (else 
     (cons (car prop-list) (update-property-list (cdr prop-list) property new-value)))))

(define (update-state-property state property new-value)
  "Обновляет свойство состояния игры"
  (cons (car state)
        (update-property-list (cdr state) property new-value)))

;; ======================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ======================================

(define (get-action-type action)
  "Получает тип действия"
  (get-from-character action 'type))

(define (get-action-location action)
  "Получает локацию из действия"
  (get-from-character action 'location))

(define (get-action-npc action)
  "Получает NPC из действия"
  (get-from-character action 'npc))

(define (get-action-choice-id action)
  "Получает ID выбора из действия"
  (get-from-character action 'id))

(define (is-location-accessible? location-props)
  "Проверяет доступность локации"
  (let ((accessible-pair (assq 'accessible location-props)))
    (if accessible-pair (cadr accessible-pair) #f)))

(define (is-location-accessible-in-state? state location-id)
  "Проверяет доступность локации в состоянии"
  (let* ((locations (get-world-locations state))
         (location-data (assq location-id locations)))
    (if location-data
      (is-location-accessible? (cdr location-data))
      #f)))

(define (get-npc-location npc-props)
  "Получает локацию NPC"
  (let ((location-pair (assq 'location npc-props)))
    (if location-pair (cadr location-pair) #f)))

(define (is-npc-available? npc-props)
  "Проверяет доступность NPC"
  (let ((available-pair (assq 'available npc-props)))
    (if available-pair (cadr available-pair) #f)))

(define (is-npc-at-location? state npc-id location)
  "Проверяет находится ли NPC в локации"
  (let* ((world (get-from-state state 'world))
         (npcs (get-from-state world 'npcs))
         (npc-data (assq npc-id npcs)))
    (if npc-data
      (eq? (get-npc-location (cdr npc-data)) location)
      #f)))

(define (is-npc-available-in-state? state npc-id)
  "Проверяет доступность NPC в состоянии"
  (let* ((world (get-from-state state 'world))
         (npcs (get-from-state world 'npcs))
         (npc-data (assq npc-id npcs)))
    (if npc-data
      (is-npc-available? (cdr npc-data))
      #f)))

(define (is-valid-choice? choice-id)
  "Проверяет валидность выбора"
  (member choice-id '(explore rest inventory)))

(define (filter-actions proc lst)
  "Фильтрует действия, убирает #f"
  (cond
    ((null? lst) '())
    (else
      (let ((result (proc (car lst))))
        (if result
          (cons result (filter-actions proc (cdr lst)))
          (filter-actions proc (cdr lst)))))))

;; Упрощенная конвертация символа в строку
(define (symbol->string sym)
  "Конвертирует символ в строку"
  (cond
    ((symbol? sym) (js-symbol->string sym))
    ((string? sym) sym)
    (else "unknown")))

(define (string-append str1 str2)
  "Соединяет строки"
  (js-string-append str1 str2))

;; ======================================
;; ЭКСПОРТИРОВАННЫЕ ФУНКЦИИ ДЛЯ JAVASCRIPT
;; ======================================

(define (scheme-create-game room-id players)
  "Основная функция создания игры для JavaScript"
  (create-game-state room-id players))

(define (scheme-get-actions state character-id)
  "Основная функция получения действий для JavaScript"
  (get-available-actions-for-character state character-id))

(define (scheme-validate-action state character-id action)
  "Основная функция валидации для JavaScript"
  (validate-game-action state character-id action))

(define (scheme-process-action state character-id action)
  "Основная функция обработки действий для JavaScript"
  (process-game-action state character-id action))