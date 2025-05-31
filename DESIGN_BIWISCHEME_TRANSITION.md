# Проектный документ: Переход на BiwaScheme

## Обзор

Данный документ описывает архитектурный переход от текущей системы на основе `s-expression.js` к нативному интерпретатору Scheme - BiwaScheme. Этот переход позволит улучшить производительность, расширить возможности DSL для квестов и обеспечить более глубокую интеграцию с функциональной парадигмой программирования.

## Текущее состояние системы

### Архитектура quest системы
- **Quest Engine** (`game/questSystem/questEngine.js`) - основной движок на базе `s-expression.js`
- **Data Loader** (`game/data/dataLoader.js`) - загрузчик S-expression данных
- **Story Data** (`game/data/story.scm`) - данные сюжета в S-expression формате
- **Quest Definitions** - квесты в `.scm` файлах (`game/questSystem/quests/`)

### Существующие возможности
1. Парсинг и выполнение S-expressions через JavaScript
2. Декларативные квесты с условиями и действиями
3. Функциональная композиция операций
4. Расширяемая система макросов
5. Интеграция с игровым состоянием

### Ограничения текущего решения
1. **Производительность**: дополнительный слой парсинга в JavaScript
2. **Возможности языка**: ограниченная поддержка Scheme-конструкций
3. **Отладка**: сложности с трассировкой выполнения S-expressions
4. **Расширяемость**: необходимость реимплементации Scheme-фичей в JavaScript

## BiwaScheme: Анализ возможностей

### Ключевые преимущества
1. **Нативный Scheme интерпретатор** - полная поддержка R7RS стандарта
2. **Высокая производительность** - оптимизированная виртуальная машина
3. **JavaScript интеграция** - прямой доступ к JavaScript объектам и функциям
4. **Node.js поддержка** - работа как в браузере, так и на сервере
5. **REPL для разработки** - интерактивная среда разработки
6. **Макросистема** - полноценная поддержка Scheme макросов

### Возможности интеграции

#### В Node.js
```javascript
const BiwaScheme = require("biwascheme");

// Выполнение Scheme кода
BiwaScheme.run("(+ 1 2)");

// Загрузка файлов
BiwaScheme.run_file("quest.scm");

// Установка JavaScript функций
BiwaScheme.define_libfunc("js-log", 1, 1, function(ar) {
    console.log(ar[0]);
});
```

#### В браузере
```html
<script src="biwascheme.js"></script>
<script type="text/biwascheme">
  (display "Executing Scheme in browser")
</script>
```

## Архитектура перехода

### Фаза 1: Подготовка инфраструктуры

#### 1.1 Установка BiwaScheme
```bash
npm install biwascheme
```

#### 1.2 Создание BiwaScheme Quest Engine
```
/game/questSystem/
├── biwaQuestEngine.js     # Новый движок на BiwaScheme
├── schemeIntegration.js   # Интеграция JavaScript ↔ Scheme
├── questRuntime.js        # Среда выполнения квестов
└── questDebugger.js       # Инструменты отладки
```

#### 1.3 JavaScript ↔ Scheme мост
Создание интерфейса для взаимодействия между JavaScript игровой логикой и Scheme квестами:

```javascript
// schemeIntegration.js
class SchemeGameBridge {
    constructor(gameState) {
        this.gameState = gameState;
        this.biwa = new BiwaScheme.Interpreter();
        this.setupGameFunctions();
    }

    setupGameFunctions() {
        // Экспорт игровых функций в Scheme
        this.biwa.define_libfunc("get-location", 0, 0, () => {
            return this.gameState.currentLocation;
        });
        
        this.biwa.define_libfunc("has-item?", 1, 1, (ar) => {
            return this.gameState.inventory.includes(ar[0]);
        });
        
        this.biwa.define_libfunc("set-memory!", 2, 2, (ar) => {
            this.gameState.memory[ar[0]] = ar[1];
        });
        
        // ... другие игровые функции
    }
}
```

### Фаза 2: Расширенный DSL на Scheme

#### 2.1 Нативные Scheme конструкции
Полноценное использование Scheme возможностей:

```scheme
;; Продвинутые макросы для квестов
(define-syntax define-quest
  (syntax-rules (metadata steps triggers)
    ((define-quest name 
       (metadata meta ...)
       (triggers trigger ...)
       (steps step ...))
     (begin
       (define quest-metadata (list meta ...))
       (define quest-triggers (list trigger ...))
       (define quest-steps (list step ...))
       (register-quest 'name quest-metadata quest-triggers quest-steps)))))

;; Функциональные композиции
(define (compose-actions . actions)
  (lambda (game-state)
    (fold-left (lambda (state action) (action state)) 
               game-state 
               actions)))

;; Продвинутые условия с паттерн-матчингом
(define-syntax match-game-state
  (syntax-rules (at has outfit when)
    ((match-game-state state
       (at location-pattern) action)
     (when (equal? (get-location state) 'location-pattern)
       action))))
```

#### 2.2 Расширенная система квестов
```scheme
;; quest_advanced_example.scm
(define-quest princess-diplomatic-mission
  (metadata
    (title "Дипломатическая миссия")
    (character princess)
    (difficulty advanced)
    (estimated-time 30))
  
  (triggers
    (on-dialogue royal-advisor
      (when (and (outfit-is? 'noble)
                 (quest-completed? 'princess-lost-relic)
                 (> (game-time) 60)))))
  
  (variables
    (embassy-countries '(northern-kingdom eastern-empire))
    (diplomatic-gifts '())
    (negotiation-results '()))
  
  (steps
    ;; Сложная логика с локальными переменными
    (step prepare-delegation
      (description "Подготовить дипломатическую делегацию")
      (require
        (at-location 'throne-room)
        (has-item? 'royal-seal))
      (let ((required-gifts (get-cultural-preferences embassy-countries)))
        (for-each 
          (lambda (gift) 
            (when (available-in-treasury? gift)
              (add-to-delegation gift)))
          required-gifts)))
    
    ;; Условные ветвления с состоянием
    (step choose-diplomatic-approach
      (branch
        ((> (loyalty 'nobles) 70)
         (set-approach! 'formal-protocol)
         (require-escort 'royal-guard))
        ((> (loyalty 'villagers) 70)
         (set-approach! 'people-diplomacy)
         (require-escort 'village-representatives))
        (else
         (set-approach! 'solo-mission)
         (show-message "Вы идете одна - это рискованно!"))))
    
    ;; Процедурная генерация
    (step conduct-negotiations
      (let ((outcomes (map negotiate-with embassy-countries)))
        (set-quest-var! 'negotiation-results outcomes)
        (when (all successful? outcomes)
          (trigger-celebration)
          (unlock-achievement 'master-diplomat))))))

;; Система тестирования с подробными проверками
(define-test-suite princess-diplomatic-mission-tests
  (test-case "successful-formal-protocol"
    (given
      (initial-state
        (location 'throne-room)
        (outfit 'noble)
        (inventory '(royal-seal diplomatic-gift))
        (loyalty '((nobles 80) (villagers 40)))))
    (when
      (execute-quest 'princess-diplomatic-mission))
    (then
      (assert-equal (get-approach) 'formal-protocol)
      (assert-true (quest-completed? 'princess-diplomatic-mission))
      (assert-contains (get-achievements) 'master-diplomat)))
  
  (test-case "failed-insufficient-preparation"
    (given
      (initial-state
        (location 'throne-room)
        (outfit 'common)
        (inventory '())
        (loyalty '((nobles 30) (villagers 30)))))
    (when
      (attempt-quest 'princess-diplomatic-mission))
    (then
      (assert-false (quest-triggered? 'princess-diplomatic-mission))
      (assert-equal (get-failure-reason) 'insufficient-preparation))))
```

### Фаза 3: Улучшенная отладка и разработка

#### 3.1 REPL интеграция
```javascript
// questDebugger.js
class SchemeQuestDebugger {
    constructor(questEngine) {
        this.engine = questEngine;
        this.repl = new BiwaScheme.Interpreter();
        this.setupDebugEnvironment();
    }

    setupDebugEnvironment() {
        // Доступ к игровому состоянию из REPL
        this.repl.define_libfunc("debug-state", 0, 0, () => {
            return this.engine.gameState;
        });
        
        // Пошаговое выполнение квестов
        this.repl.define_libfunc("step-quest", 1, 1, (ar) => {
            return this.engine.stepThroughQuest(ar[0]);
        });
        
        // Визуализация состояния квеста
        this.repl.define_libfunc("visualize-quest", 1, 1, (ar) => {
            return this.generateQuestVisualization(ar[0]);
        });
    }

    startInteractiveSession() {
        console.log("BiwaScheme Quest Debugger");
        console.log("Available commands:");
        console.log("  (debug-state) - показать текущее состояние игры");
        console.log("  (step-quest 'quest-name) - пошагово выполнить квест");
        console.log("  (visualize-quest 'quest-name) - визуализировать квест");
        
        // Запуск REPL
        this.repl.run_repl();
    }
}
```

#### 3.2 Визуализация квестов
```scheme
;; quest_visualizer.scm
(define (generate-quest-graph quest-name)
  "Создает граф выполнения квеста для визуализации"
  (let ((quest (get-quest quest-name)))
    (map (lambda (step)
           (list (step-id step)
                 (step-requirements step)
                 (step-actions step)
                 (possible-next-steps step)))
         (quest-steps quest))))

(define (trace-quest-execution quest-name initial-state)
  "Трассирует выполнение квеста с подробным логированием"
  (let ((trace-log '()))
    (with-quest-tracer
      (lambda (event-type data)
        (set! trace-log (cons (list event-type data (current-time)) trace-log)))
      (execute-quest quest-name initial-state))
    (reverse trace-log)))
```

### Фаза 4: Продвинутые возможности

#### 4.1 Метапрограммирование квестов
```scheme
;; Автоматическая генерация квестов из шаблонов
(define-syntax quest-template
  (syntax-rules (fetch-and-return escort-mission gather-items)
    ;; Шаблон "принести предмет"
    ((quest-template fetch-and-return item npc location reward)
     (define-quest ,(gensym 'fetch-quest)
       (metadata 
         (title ,(string-append "Принести " (symbol->string item)))
         (type fetch))
       (steps
         (step find-item
           (require (at-location location))
           (actions (collect-item item)))
         (step return-item
           (require 
             (has-item? item)
             (talking-to npc))
           (actions 
             (take-items item)
             (give-reward reward)
             (complete-quest))))))
    
    ;; Шаблон "сопроводить персонажа"
    ((quest-template escort-mission npc from-location to-location danger-level)
     (define-quest ,(gensym 'escort-quest)
       (metadata
         (title ,(string-append "Сопроводить " (symbol->string npc)))
         (type escort)
         (danger danger-level))
       (steps
         (step meet-escort
           (require 
             (at-location from-location)
             (talking-to npc))
           (actions (set-memory! 'escorting npc)))
         (step travel-safely
           (when (< (random) danger-level)
             (trigger-random-encounter))
           (actions (move-to to-location)))
         (step complete-escort
           (require (at-location to-location))
           (actions 
             (remove-memory! 'escorting)
             (complete-quest))))))))

;; Генерация квестов из данных
(define (generate-fetch-quests items-database npcs-database)
  (for-each 
    (lambda (item-entry)
      (let ((item (car item-entry))
            (location (cadr item-entry))
            (suitable-npcs (filter (lambda (npc) 
                                   (npc-interested-in? npc item)) 
                                 npcs-database)))
        (for-each
          (lambda (npc)
            (quest-template fetch-and-return item npc location 
                          (calculate-reward item npc)))
          suitable-npcs)))
    items-database))
```

#### 4.2 Система модулей и библиотек
```scheme
;; quest_library.scm - библиотека переиспользуемых компонентов
(define-library (quest-system core)
  (export define-quest quest-step quest-action)
  (import (scheme base) (game-integration))
  
  (begin
    (define-syntax define-quest ...)
    (define (quest-step id requirements actions) ...)
    (define (quest-action type . params) ...)))

(define-library (quest-system conditions)
  (export at-location? has-item? outfit-is? npc-memory?)
  (import (scheme base) (game-integration))
  
  (begin
    (define (at-location? loc) ...)
    (define (has-item? item) ...)
    (define (outfit-is? outfit) ...)
    (define (npc-memory? npc key) ...)))

(define-library (quest-system actions)
  (export set-memory! give-item! trigger-scene!)
  (import (scheme base) (game-integration))
  
  (begin
    (define (set-memory! key value) ...)
    (define (give-item! item) ...)
    (define (trigger-scene! scene) ...)))

;; В файлах квестов
(import (quest-system core)
        (quest-system conditions)
        (quest-system actions))
```

## План миграции

### Этап 1: Параллельная реализация (2-3 недели)
1. Установить BiwaScheme в проект
2. Создать BiwaScheme Quest Engine параллельно с существующим
3. Реализовать JavaScript ↔ Scheme интеграцию
4. Портировать 1-2 простых квеста для тестирования

### Этап 2: Расширенный DSL (2-3 недели)
1. Разработать систему макросов для квестов
2. Создать библиотеки переиспользуемых компонентов
3. Портировать все существующие квесты
4. Добавить систему тестирования

### Этап 3: Инструменты разработки (1-2 недели)
1. Интегрировать REPL для отладки
2. Создать инструменты визуализации квестов
3. Добавить автоматическую валидацию квестов
4. Создать документацию по новому DSL

### Этап 4: Оптимизация и продвинутые фичи (2-3 недели)
1. Оптимизировать производительность
2. Добавить метапрограммирование и генерацию квестов
3. Создать систему модулей
4. Протестировать и отладить всю систему

### Этап 5: Переход и очистка (1 неделя)
1. Переключить продакшн на BiwaScheme Engine
2. Удалить старый код на s-expression.js
3. Обновить документацию
4. Обучить команду новому DSL

## Преимущества перехода

### Для разработчиков
1. **Нативный Scheme** - полная мощь функционального программирования
2. **REPL** - интерактивная разработка и отладка
3. **Макросистема** - создание предметно-ориентированных конструкций
4. **Модульность** - переиспользуемые библиотеки квестов

### Для дизайнеров квестов
1. **Высокоуровневый DSL** - описание квестов на естественном языке
2. **Шаблоны** - быстрое создание типовых квестов
3. **Визуализация** - графическое представление логики квестов
4. **Тестирование** - автоматическая проверка корректности

### Для системы в целом
1. **Производительность** - более быстрое выполнение квестов
2. **Расширяемость** - легкое добавление новых возможностей
3. **Надежность** - строгая типизация и проверки
4. **Поддерживаемость** - чистый, функциональный код

## Риски и митигация

### Технические риски
1. **Сложность интеграции** - тщательное планирование API между JavaScript и Scheme
2. **Производительность** - профилирование и оптимизация критичных участков
3. **Совместимость** - поддержка обратной совместимости в переходный период

### Командные риски
1. **Кривая обучения** - обучение команды Scheme и функциональному программированию
2. **Временные затраты** - поэтапная миграция для минимизации простоев
3. **Отладка** - создание качественных инструментов отладки с самого начала

## Заключение

Переход на BiwaScheme представляет значительное улучшение архитектуры квестовой системы. Нативная поддержка Scheme обеспечит более мощный и гибкий DSL, улучшит производительность и откроет возможности для продвинутых техник функционального программирования.

Поэтапный подход к миграции минимизирует риски и позволит команде постепенно освоить новые инструменты. В результате мы получим более мощную, расширяемую и поддерживаемую систему квестов, которая послужит основой для дальнейшего развития игры.