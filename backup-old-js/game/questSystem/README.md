# Система квестов на S-выражениях

## Обзор

Новая система квестов использует S-выражения (s-expression.js) для декларативного описания квестовой логики. Это позволяет создавать сложные квесты с ветвлениями, условиями и динамическим поведением без написания императивного кода.

## Установка

```bash
npm install s-expression.js
```

## Архитектура

### Компоненты системы

1. **QuestEngine** (`questEngine.js`) - Основной движок для парсинга и выполнения квестов
2. **QuestRunner** (`questRunner.js`) - Управляет выполнением квестов и состоянием
3. **QuestIntegration** (`questIntegration.js`) - Интегрирует систему с существующей игрой

### Структура квеста

```lisp
(quest quest_id
  (metadata
    (title "Название квеста")
    (description "Описание")
    (character princess|helper|any))
  
  (variables
    (var_name initial_value)
    ...)
  
  (triggers
    (on-dialogue npc_id (when условие))
    (on-location location_id (when условие))
    (on-item item_id (when условие)))
  
  (steps
    (step step_id
      (description "Описание шага")
      (require условия...)
      (actions действия...))
    ...)
  
  (on-complete
    действия_при_завершении...))
```

## Язык условий

### Базовые операторы

```lisp
;; Сравнение
(= a b)          ;; Равенство
(!= a b)         ;; Неравенство  
(> a b)          ;; Больше
(>= a b)         ;; Больше или равно
(< a b)          ;; Меньше
(<= a b)         ;; Меньше или равно

;; Логические
(and условие1 условие2 ...)     ;; И
(or условие1 условие2 ...)      ;; ИЛИ
(not условие)                   ;; НЕ
```

### Игровые условия

```lisp
;; Проверка местоположения
(at-location location_id)

;; Проверка инвентаря
(has-item item_id)

;; Проверка памяти
(has-memory "key")

;; Проверка NPC
(talking-to npc_id)
(npc-memory npc_id "key")

;; Проверка одежды
(outfit-is "noble"|"common")

;; Проверка лояльности
(loyalty group_name > value)

;; Проверка квестов
(quest-started "quest_id")
(quest-completed "quest_id")
(quest-step "quest_id" "step_id")
```

### Коллекции

```lisp
;; Проверка вхождения
(in item list)

;; Проверка всех элементов
(all list predicate)

;; Проверка хотя бы одного
(any list predicate)
```

## Язык действий

### Управление памятью

```lisp
(set-memory "key" value)
(remove-memory "key")
(set-npc-memory npc_id "key" value)
```

### Управление диалогами

```lisp
(add-dialogue npc_id
  (option option_id
    "Текст выбора"
    "Ответ NPC"))

(remove-dialogue npc_id option_id)
```

### Управление миром

```lisp
(reveal-location location_id)
(hide-location location_id)
(spawn-item location_id item_id)
```

### Управление инвентарём

```lisp
(give-items item1 item2 ...)
(take-items item1 item2 ...)
```

### Управление квестами

```lisp
(complete-quest)
(fail-quest "reason")
(start-quest "quest_id")
(goto-step step_id)
```

### Эффекты персонажа

```lisp
(change-loyalty group amount)
(set-outfit "noble"|"common")
(trigger-scene scene_id)
(show-message "text")
```

### Управление потоком

```lisp
;; Условное выполнение
(if условие
  (then действия...)
  (else действия...))

;; Множественные условия
(cond
  (условие1 действия1...)
  (условие2 действия2...)
  (else действия...))

;; Локальные переменные
(let ((var1 value1) (var2 value2))
  действия...)
```

## Продвинутые возможности

### Макросы

```lisp
(defmacro macro_name (param1 param2)
  тело_макроса...)

;; Использование
(macro_name arg1 arg2)
```

### Переменные

```lisp
;; Доступ к переменным
$variable_name

;; Арифметика
(+ a b)
(- a b)
(* a b)
(/ a b)
```

### Ветвление

```lisp
(branch
  (case условие1
    действия1...)
  (case условие2
    действия2...)
  (default
    действия_по_умолчанию...))
```

### Циклы

```lisp
(loop условие_продолжения
  тело_цикла...)
```

## Примеры

### Простой квест

```lisp
(quest simple_fetch
  (metadata
    (title "Принести воды")
    (character any))
  
  (triggers
    (on-dialogue cook
      (when (not (quest-started "simple_fetch")))))
  
  (steps
    (step get_water
      (description "Принести воду из колодца")
      (require
        (at-location well))
      (actions
        (give-items water_bucket)
        (show-message "Вы набрали воды")))
    
    (step deliver_water
      (description "Отнести воду повару")
      (require
        (at-location kitchen)
        (talking-to cook)
        (has-item water_bucket))
      (actions
        (take-items water_bucket)
        (complete-quest))))
  
  (on-complete
    (change-loyalty villagers 5)
    (show-message "Повар благодарит вас!")))
```

### Квест с ветвлением

```lisp
(quest branching_quest
  (metadata
    (title "Выбор пути"))
  
  (steps
    (branch
      (case (outfit-is "noble")
        (step noble_path
          (description "Путь знати")
          (actions
            (change-loyalty nobles 10)
            (complete-quest))))
      
      (case (outfit-is "common")
        (step common_path
          (description "Путь простолюдина")
          (actions
            (change-loyalty villagers 10)
            (complete-quest)))))))
```

### Квест с макросами

```lisp
(quest macro_example
  (metadata
    (title "Пример с макросами"))
  
  (defmacro reward-player (item loyalty-group amount)
    (progn
      (give-items $item)
      (change-loyalty $loyalty-group $amount)
      (show-message "Награда получена!")))
  
  (steps
    (step complete
      (require (at-location throne_room))
      (actions
        (reward-player crown nobles 50)))))
```

## Интеграция с игрой

```javascript
// Создание интеграции
const integration = new QuestIntegration(gameLogic);

// Проверка доступных квестов
const available = integration.checkQuestTriggers();

// Начать квест
integration.startQuest('quest_id');

// Обработать взаимодействие с NPC
const updates = integration.handleNPCInteraction('npc_id');

// Получить статус квеста
const status = integration.getCurrentQuestStatus();
```

## Миграция старых квестов

```javascript
// Конвертировать старый формат
const oldQuest = { /* старый формат */ };
const sexp = integration.convertQuestToSExp(oldQuest);

// Сохранить конвертированный квест
integration.saveConvertedQuest(oldQuest);
```

## Отладка

```javascript
// Выполнить выражение в контексте квеста
const result = runner.debugEvaluate('quest_id', '(has-item sword)', character);

// Получить статус всех активных квестов
const activeQuests = runner.getActiveQuests();
```

## Преимущества системы

1. **Декларативность** - Квесты описываются как данные, а не код
2. **Гибкость** - Поддержка сложной логики, ветвлений, циклов
3. **Расширяемость** - Легко добавлять новые условия и действия
4. **Переиспользование** - Макросы позволяют создавать шаблоны
5. **Отладка** - S-выражения легко логировать и анализировать
6. **Версионирование** - Квесты хранятся как текстовые файлы

## Лучшие практики

1. Используйте говорящие имена для шагов и переменных
2. Группируйте связанные действия в макросы
3. Добавляйте описания к каждому шагу
4. Используйте переменные для динамических значений
5. Тестируйте квесты изолированно перед интеграцией
6. Документируйте сложную логику комментариями (;;)

## Производительность

- Парсинг квестов происходит один раз при загрузке
- Выполнение условий оптимизировано для быстрой проверки
- Макросы раскрываются во время выполнения
- Рекомендуется не более 100 шагов на квест
- Избегайте глубокой вложенности (более 5 уровней)