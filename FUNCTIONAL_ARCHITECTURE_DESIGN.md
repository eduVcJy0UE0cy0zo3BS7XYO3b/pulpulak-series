# Функциональный дизайн архитектуры + BiwaScheme

## Анализ текущих императивных паттернов

### Проблемы текущей архитектуры

#### 1. Мутабельное состояние
```javascript
// gameState.js - Прямая мутация объектов
changeScene(newScene) {
    this.currentScene = newScene;  // МУТАЦИЯ
}

// gameStateManager.js - Мутации везде
updateCharacterStat(gameState, character, statName, value) {
    gameState.stats[character][statName] = value;  // МУТАЦИЯ
    return gameState;
}

addToInventory(gameState, character, item) {
    inventory.push(item);  // МУТАЦИЯ
    return gameState;
}
```

#### 2. Побочные эффекты в логике
```javascript
// coopGameLogic.js - Смешение логики и эффектов
makeChoice(roomId, playerId, choiceId, character) {
    const gameState = this.lobbyLogic.getGameState(roomId);  // ЭФФЕКТ: получение состояния
    const result = this.choiceHandler.makeChoice(gameState, choiceId, character);  // МУТАЦИЯ
    this.lobbyLogic.switchTurn(gameState);  // ПОБОЧНЫЙ ЭФФЕКТ
    this.outfitSystem.cancelOutfitRequest(roomId);  // ПОБОЧНЫЙ ЭФФЕКТ
}
```

#### 3. Императивные вычисления состояния
```javascript
// Множественные проверки и условные мутации
if (result.success) {
    if (choiceId.startsWith('move_to_')) {
        this.outfitSystem.cancelOutfitRequest(roomId);
    } else if (!isMovement && !isNPCInteraction) {
        this.lobbyLogic.switchTurn(gameState);
        this.outfitSystem.cancelOutfitRequest(roomId);
    }
}
```

#### 4. Разбросанная логика управления состоянием
- `GameState` - локальное состояние объекта
- `GameStateManager` - глобальные операции
- `CoopGameLogic` - игровая логика
- `OutfitSystem` - состояние системы одежды
- `QuestSystem` - состояние квестов

## Функциональная архитектура на BiwaScheme

### Принципы нового дизайна

#### 1. Иммутабельное состояние игры
```scheme
;; Все состояние игры как неизменяемая структура данных
(define-record-type game-state
  (make-game-state scene characters world quests)
  game-state?
  (scene game-state-scene)
  (characters game-state-characters)
  (world game-state-world)
  (quests game-state-quests))

;; Чистые функции для трансформации состояния
(define (update-character-location state character location)
  (let ((updated-chars (assoc-update (game-state-characters state)
                                   character
                                   (lambda (char-data)
                                     (assoc-set char-data 'location location)))))
    (make-game-state (game-state-scene state)
                     updated-chars
                     (game-state-world state)
                     (game-state-quests state))))
```

#### 2. Композиция чистых функций
```scheme
;; Функциональные трансформеры состояния
(define (game-action->state-transformer action)
  (match action
    (('move character location)
     (lambda (state) (update-character-location state character location)))
    (('add-item character item)
     (lambda (state) (add-item-to-character state character item)))
    (('start-quest character quest-id)
     (lambda (state) (start-character-quest state character quest-id)))
    (('complete-quest character quest-id)
     (lambda (state) (complete-character-quest state character quest-id)))))

;; Композиция множественных действий
(define (apply-actions state actions)
  (fold-left (lambda (current-state action)
               ((game-action->state-transformer action) current-state))
             state
             actions))
```

#### 3. Система эффектов как монады
```scheme
;; Game Effect Monad для чистого описания побочных эффектов
(define-record-type game-effect
  (make-game-effect computation)
  game-effect?
  (computation game-effect-computation))

(define (return-effect value)
  (make-game-effect (lambda (state) (list value state))))

(define (bind-effect effect f)
  (make-game-effect
    (lambda (state)
      (let* ((result ((game-effect-computation effect) state))
             (value (car result))
             (new-state (cadr result)))
        ((game-effect-computation (f value)) new-state)))))

;; Макрос для удобного написания
(define-syntax do-effects
  (syntax-rules (<- return)
    ((do-effects (var <- computation) body ...)
     (bind-effect computation (lambda (var) (do-effects body ...))))
    ((do-effects (return expr))
     (return-effect expr))
    ((do-effects expr)
     expr)))
```

#### 4. Функциональная игровая логика
```scheme
;; Чистая логика выбора действий
(define (validate-choice state character choice)
  (and (character-can-act? state character)
       (choice-available? state character choice)
       (meets-requirements? state character choice)))

(define (process-choice state character choice)
  (if (validate-choice state character choice)
    (do-effects
      (new-state <- (apply-choice-effects state character choice))
      (follow-up <- (check-follow-up-actions new-state character choice))
      (final-state <- (apply-follow-up-effects new-state follow-up))
      (return (success-result final-state)))
    (return (failure-result "Недопустимый выбор"))))
```

### Архитектура модулей

#### 1. Ядро функциональной системы
```
/game/functional/
├── core/
│   ├── game-state.scm         # Иммутабельные структуры данных
│   ├── state-transformers.scm # Чистые функции трансформации
│   ├── effects.scm            # Система эффектов
│   └── validators.scm         # Чистые функции валидации
├── domains/
│   ├── character.scm          # Логика персонажей
│   ├── location.scm           # Логика локаций
│   ├── inventory.scm          # Логика инвентаря
│   ├── dialogue.scm           # Логика диалогов
│   └── quest.scm              # Логика квестов
└── integration/
    ├── js-bridge.scm          # Мост с JavaScript
    ├── biwa-runtime.js        # BiwaScheme runtime
    └── functional-engine.js   # Функциональный движок
```

#### 2. Система состояния
```scheme
;; core/game-state.scm
(define-library (game functional core state)
  (export make-initial-state
          game-state?
          get-character-data
          get-world-state
          get-quest-state
          update-state)
  (import (scheme base)
          (scheme record)
          (functional core immutable))
  
  (begin
    ;; Иммутабельные записи для всех типов данных
    (define-record-type character
      (make-character id location outfit inventory stats dialogue-state)
      character?
      (id character-id)
      (location character-location)
      (outfit character-outfit)
      (inventory character-inventory)
      (stats character-stats)
      (dialogue-state character-dialogue-state))
    
    (define-record-type world
      (make-world locations npcs time events)
      world?
      (locations world-locations)
      (npcs world-npcs)
      (time world-time)
      (events world-events))
    
    (define-record-type quest-system
      (make-quest-system active-quests completed-quests global-memory)
      quest-system?
      (active-quests quest-system-active)
      (completed-quests quest-system-completed)
      (global-memory quest-system-global-memory))
    
    ;; Конструктор начального состояния
    (define (make-initial-state room-id players)
      (make-game-state
        'coop_awakening
        (make-initial-characters players)
        (make-initial-world)
        (make-initial-quest-system)))))
```

#### 3. Композиция трансформеров состояния
```scheme
;; core/state-transformers.scm
(define-library (game functional core transformers)
  (export state-transformer?
          compose-transformers
          character-transformer
          world-transformer
          quest-transformer)
  (import (scheme base))
  
  (begin
    ;; Базовый тип трансформера состояния
    (define (state-transformer? f)
      (and (procedure? f)
           (= (procedure-arity f) 1)))
    
    ;; Композиция трансформеров
    (define (compose-transformers . transformers)
      (lambda (state)
        (fold-left (lambda (current-state transformer)
                     (transformer current-state))
                   state
                   transformers)))
    
    ;; Специализированные трансформеры
    (define (character-transformer character-id updater)
      (lambda (state)
        (let ((characters (game-state-characters state)))
          (set-game-state-characters 
            state
            (assoc-update characters character-id updater)))))
    
    (define (move-character character-id new-location)
      (character-transformer character-id
        (lambda (character)
          (set-character-location character new-location))))
    
    (define (add-item-to-character character-id item)
      (character-transformer character-id
        (lambda (character)
          (set-character-inventory 
            character
            (cons item (character-inventory character))))))))
```

#### 4. Система эффектов
```scheme
;; core/effects.scm
(define-library (game functional core effects)
  (export game-effect
          return-effect
          bind-effect
          run-effect
          log-effect
          update-state-effect
          trigger-event-effect)
  (import (scheme base))
  
  (begin
    ;; Эффект как функция State -> (Value, State, [SideEffect])
    (define-record-type game-effect
      (make-game-effect computation)
      game-effect?
      (computation effect-computation))
    
    ;; Чистое значение без эффектов
    (define (return-effect value)
      (make-game-effect
        (lambda (state)
          (list value state '()))))
    
    ;; Последовательная композиция эффектов
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
    
    ;; Выполнение эффекта
    (define (run-effect effect initial-state)
      ((effect-computation effect) initial-state))
    
    ;; Предопределенные эффекты
    (define (log-effect message)
      (make-game-effect
        (lambda (state)
          (list #t state (list (list 'log message))))))
    
    (define (update-state-effect transformer)
      (make-game-effect
        (lambda (state)
          (list #t (transformer state) '()))))
    
    (define (trigger-event-effect event-data)
      (make-game-effect
        (lambda (state)
          (list #t state (list (list 'event event-data))))))))
```

#### 5. Функциональная логика персонажей
```scheme
;; domains/character.scm
(define-library (game functional domains character)
  (export character-can-move?
          character-can-interact?
          character-has-item?
          move-character-effect
          add-item-effect
          swap-outfits-effect)
  (import (scheme base)
          (game functional core state)
          (game functional core effects)
          (game functional core transformers))
  
  (begin
    ;; Чистые предикаты
    (define (character-can-move? state character-id location)
      (and (character-exists? state character-id)
           (location-accessible? state location)
           (not (character-in-dialogue? state character-id))))
    
    (define (character-can-interact? state character-id npc-id)
      (and (character-exists? state character-id)
           (npc-at-same-location? state character-id npc-id)
           (npc-available? state npc-id)))
    
    (define (character-has-item? state character-id item)
      (let ((character (get-character state character-id)))
        (member item (character-inventory character))))
    
    ;; Эффекты для действий персонажей
    (define (move-character-effect character-id location)
      (do-effects
        (_ <- (log-effect (format "~a moves to ~a" character-id location)))
        (_ <- (update-state-effect (move-character character-id location)))
        (_ <- (trigger-event-effect (list 'character-moved character-id location)))
        (return #t)))
    
    (define (add-item-effect character-id item)
      (do-effects
        (_ <- (log-effect (format "~a receives ~a" character-id item)))
        (_ <- (update-state-effect (add-item-to-character character-id item)))
        (_ <- (trigger-event-effect (list 'item-added character-id item)))
        (return #t)))
    
    (define (swap-outfits-effect char1 char2)
      (do-effects
        (outfit1 <- (get-character-outfit-effect char1))
        (outfit2 <- (get-character-outfit-effect char2))
        (_ <- (update-state-effect 
                (compose-transformers
                  (set-character-outfit char1 outfit2)
                  (set-character-outfit char2 outfit1))))
        (_ <- (log-effect (format "~a and ~a swap outfits" char1 char2)))
        (_ <- (trigger-event-effect (list 'outfits-swapped char1 char2)))
        (return #t)))))
```

#### 6. Функциональные квесты
```scheme
;; domains/quest.scm
(define-library (game functional domains quest)
  (export quest-available?
          quest-requirements-met?
          start-quest-effect
          advance-quest-effect
          complete-quest-effect)
  (import (scheme base)
          (game functional core state)
          (game functional core effects))
  
  (begin
    ;; Чистая логика квестов
    (define (quest-available? state character-id quest-id)
      (and (not (quest-active? state character-id quest-id))
           (not (quest-completed? state character-id quest-id))
           (quest-triggers-met? state character-id quest-id)))
    
    (define (quest-requirements-met? state character-id quest-id step-id)
      (let ((quest (get-active-quest state character-id quest-id))
            (character (get-character state character-id)))
        (and quest
             (equal? (quest-current-step quest) step-id)
             (all-requirements-satisfied? state character quest-id step-id))))
    
    ;; Эффекты для квестов
    (define (start-quest-effect character-id quest-id)
      (do-effects
        (quest-data <- (load-quest-data-effect quest-id))
        (_ <- (update-state-effect 
                (add-active-quest character-id quest-data)))
        (_ <- (log-effect (format "Quest ~a started for ~a" quest-id character-id)))
        (_ <- (trigger-event-effect (list 'quest-started character-id quest-id)))
        (return quest-data)))
    
    (define (advance-quest-effect character-id quest-id)
      (do-effects
        (current-step <- (get-current-quest-step-effect character-id quest-id))
        (next-step <- (calculate-next-step-effect quest-id current-step))
        (_ <- (update-state-effect 
                (set-quest-step character-id quest-id next-step)))
        (_ <- (execute-step-actions-effect character-id quest-id next-step))
        (_ <- (log-effect (format "Quest ~a advanced to step ~a" quest-id next-step)))
        (return next-step)))
    
    (define (complete-quest-effect character-id quest-id)
      (do-effects
        (rewards <- (get-quest-rewards-effect quest-id))
        (_ <- (apply-quest-rewards-effect character-id rewards))
        (_ <- (update-state-effect 
                (move-quest-to-completed character-id quest-id)))
        (_ <- (log-effect (format "Quest ~a completed by ~a" quest-id character-id)))
        (_ <- (trigger-event-effect (list 'quest-completed character-id quest-id)))
        (return rewards)))))
```

### JavaScript ↔ BiwaScheme интеграция

#### 1. Функциональный движок
```javascript
// integration/functional-engine.js
const BiwaScheme = require('biwascheme');

class FunctionalGameEngine {
    constructor() {
        this.interpreter = new BiwaScheme.Interpreter();
        this.setupFunctionalRuntime();
        this.gameState = null;
    }

    setupFunctionalRuntime() {
        // Загрузка функциональных библиотек
        this.interpreter.run(`
            (load "/game/functional/core/game-state.scm")
            (load "/game/functional/core/state-transformers.scm")
            (load "/game/functional/core/effects.scm")
            (load "/game/functional/domains/character.scm")
            (load "/game/functional/domains/quest.scm")
        `);

        // Мост для JavaScript функций
        this.interpreter.define_libfunc("js-log", 1, 1, (ar) => {
            console.log('[Scheme]', ar[0]);
        });

        this.interpreter.define_libfunc("js-emit-event", 2, 2, (ar) => {
            this.emitGameEvent(ar[0], ar[1]);
        });

        this.interpreter.define_libfunc("js-save-state", 1, 1, (ar) => {
            this.gameState = this.schemeToJS(ar[0]);
        });
    }

    // Чистая функция: выполнить игровое действие
    async processAction(action, character) {
        try {
            const schemeAction = this.jsToScheme(action);
            const result = await this.interpreter.run(`
                (let* ((action '${schemeAction})
                       (character '${character})
                       (current-state (get-current-state))
                       (effect (process-game-action current-state character action))
                       (result (run-effect effect current-state)))
                  (let ((value (first result))
                        (new-state (second result))
                        (side-effects (third result)))
                    (js-save-state new-state)
                    (for-each (lambda (effect)
                                (js-emit-event (first effect) (second effect)))
                              side-effects)
                    value))
            `);
            
            return { success: true, result: this.schemeToJS(result) };
        } catch (error) {
            return { success: false, error: error.message };
        }
    }

    // Чистая функция: получить доступные действия
    async getAvailableActions(character) {
        const result = await this.interpreter.run(`
            (let ((character '${character})
                  (current-state (get-current-state)))
              (get-available-actions current-state character))
        `);
        
        return this.schemeToJS(result);
    }

    // Чистая функция: валидировать действие
    async validateAction(action, character) {
        const result = await this.interpreter.run(`
            (let ((action '${this.jsToScheme(action)})
                  (character '${character})
                  (current-state (get-current-state)))
              (validate-action current-state character action))
        `);
        
        return result;
    }

    jsToScheme(obj) {
        // Конвертация JavaScript объектов в Scheme S-expressions
        if (typeof obj === 'string') return `"${obj}"`;
        if (typeof obj === 'number') return obj.toString();
        if (typeof obj === 'boolean') return obj ? '#t' : '#f';
        if (Array.isArray(obj)) {
            return `(${obj.map(item => this.jsToScheme(item)).join(' ')})`;
        }
        if (typeof obj === 'object') {
            const pairs = Object.entries(obj)
                .map(([k, v]) => `(${k} ${this.jsToScheme(v)})`)
                .join(' ');
            return `(${pairs})`;
        }
        return obj.toString();
    }

    schemeToJS(scheme) {
        // Конвертация Scheme значений в JavaScript объекты
        // Реализация зависит от формата данных BiwaScheme
        return scheme;
    }

    emitGameEvent(type, data) {
        // Отправка событий в JavaScript игровую систему
        this.emit('gameEvent', { type, data });
    }
}

module.exports = FunctionalGameEngine;
```

#### 2. Интеграция с существующей системой
```javascript
// integration/biwa-runtime.js
const FunctionalGameEngine = require('./functional-engine');
const GameStateManager = require('../gameStateManager');

class BiwaGameRuntime {
    constructor() {
        this.functionalEngine = new FunctionalGameEngine();
        this.stateManager = new GameStateManager();
        this.setupEventHandlers();
    }

    setupEventHandlers() {
        this.functionalEngine.on('gameEvent', (event) => {
            this.handleGameEvent(event);
        });
    }

    // Мигрируем постепенно от императивной к функциональной системе
    async makeChoice(roomId, playerId, choiceId, character) {
        try {
            // Используем функциональную логику для валидации
            const isValid = await this.functionalEngine.validateAction(
                { type: 'choice', id: choiceId }, 
                character
            );

            if (!isValid) {
                return { success: false, message: "Недопустимое действие" };
            }

            // Выполняем действие через функциональный движок
            const result = await this.functionalEngine.processAction(
                { type: 'choice', id: choiceId },
                character
            );

            if (result.success) {
                // Синхронизируем состояние с imperative системой
                this.syncWithImperativeState(roomId);
                
                return {
                    success: true,
                    gameData: await this.getGameData(roomId),
                    message: result.result.message
                };
            }

            return result;
        } catch (error) {
            console.error('Error in functional choice processing:', error);
            return { success: false, message: error.message };
        }
    }

    async getGameData(roomId) {
        // Получаем данные из функционального состояния
        const functionalData = await this.functionalEngine.getCurrentState();
        
        // Конвертируем в формат, ожидаемый клиентом
        return this.convertFunctionalToLegacyFormat(functionalData);
    }

    syncWithImperativeState(roomId) {
        // Переходный период: синхронизация между системами
        const functionalState = this.functionalEngine.gameState;
        const imperativeState = this.stateManager.getGameState(roomId);
        
        // TODO: Постепенно убирать по мере миграции компонентов
    }

    handleGameEvent(event) {
        // Обработка событий из функциональной системы
        switch (event.type) {
            case 'character-moved':
                this.onCharacterMoved(event.data);
                break;
            case 'quest-started':
                this.onQuestStarted(event.data);
                break;
            case 'quest-completed':
                this.onQuestCompleted(event.data);
                break;
        }
    }
}

module.exports = BiwaGameRuntime;
```

### Преимущества функционального дизайна

#### 1. Предсказуемость
- Все изменения состояния происходят через чистые функции
- Нет скрытых мутаций или побочных эффектов
- Легко тестировать и отлаживать

#### 2. Композиционность
- Действия комбинируются через композицию функций
- Переиспользуемые трансформеры состояния
- Модульная архитектура доменов

#### 3. Тестируемость
```scheme
;; Простое тестирование чистых функций
(define-test "character movement"
  (let* ((initial-state (make-test-state))
         (moved-state (move-character initial-state 'princess 'throne-room)))
    (assert-equal (get-character-location moved-state 'princess) 'throne-room)))

(define-test "quest progression"
  (let* ((initial-state (make-test-state-with-quest))
         (effect (advance-quest-effect 'princess 'lost-relic))
         (result (run-effect effect initial-state))
         (final-state (second result)))
    (assert-true (quest-step-completed? final-state 'princess 'lost-relic 1))))
```

#### 4. Отладка и инспекция
```scheme
;; Полная трассировка изменений состояния
(define (trace-state-changes initial-state actions)
  (let loop ((state initial-state)
             (remaining-actions actions)
             (trace '()))
    (if (null? remaining-actions)
        (reverse trace)
        (let* ((action (car remaining-actions))
               (transformer (action->transformer action))
               (new-state (transformer state))
               (change (diff-states state new-state)))
          (loop new-state
                (cdr remaining-actions)
                (cons (list action change) trace))))))
```

## План поэтапной миграции

### Этап 1: Функциональное ядро (3-4 недели)
1. Установка BiwaScheme
2. Создание иммутабельных структур данных
3. Базовая система эффектов
4. Мост JavaScript ↔ Scheme

### Этап 2: Миграция доменов (4-5 недель)
1. Функциональная логика персонажей
2. Функциональная система квестов
3. Функциональные диалоги и взаимодействия
4. Интеграция с существующей системой

### Этап 3: Функциональный движок (2-3 недели)
1. Полная замена императивной логики
2. Оптимизация производительности
3. Расширенная система отладки
4. Удаление legacy кода

### Этап 4: Продвинутые функциональные возможности (2-3 недели)
1. Временные путешествия (undo/redo)
2. Горячая перезагрузка состояния
3. Функциональное тестирование
4. Метапрограммирование игровой логики

Функциональный дизайн + BiwaScheme создаст более надежную, предсказуемую и расширяемую архитектуру игры.