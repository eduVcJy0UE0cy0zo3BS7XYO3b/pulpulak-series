# DSL для квестов на основе S-выражений

## Обзор

Этот документ представляет альтернативный дизайн DSL для квестов, использующий S-выражения (символьные выражения) в стиле Lisp. S-выражения предоставляют унифицированный синтаксис для данных и кода, что делает DSL более мощным и расширяемым.

## Преимущества S-выражений

1. **Единообразный синтаксис**: Всё является списком
2. **Гомоиконность**: Код является данными, что упрощает метапрограммирование
3. **Простой парсинг**: Минимальный синтаксис облегчает разбор
4. **Композиция**: Легко комбинировать и вкладывать выражения
5. **Расширяемость**: Новые формы добавляются без изменения парсера

## Базовый синтаксис квеста

```lisp
(quest princess_lost_relic
  (metadata
    (title "Потерянная королевская реликвия")
    (description "Найти древний амулет, который пропал из сокровищницы")
    (character princess))
  
  (triggers
    (on-dialogue royal_advisor
      (when (and (= outfit "noble")
                 (has-memory "kingdom_talked")
                 (not (quest-started "princess_lost_relic"))))))
  
  (steps
    (step get_quest
      (description "Поговорить с королевским советником о пропаже")
      (require
        (at-location throne_room)
        (talking-to royal_advisor))
      (actions
        (set-memory "quest_accepted")
        (add-dialogue royal_advisor
          (option ask_about_progress
            "Спросить о прогрессе поисков"
            "Пока никаких новостей, Ваше Высочество"))))
    
    (step investigate_forest
      (description "Поискать реликвию в лесу")
      (require
        (at-location forest)
        (npc-memory old_woman "amulet_location"))
      (actions
        (reveal-location hidden_grove)
        (spawn-item hidden_grove ancient_amulet)))
    
    (step retrieve_amulet
      (description "Забрать амулет")
      (require
        (at-location hidden_grove)
        (has-item ancient_amulet))
      (complete-quest)))
  
  (on-complete
    (give-items ancient_amulet)
    (change-loyalty villagers +10)
    (unlock-achievement "royal_hero")
    (show-message "Вы вернули потерянную реликвию!")))
```

## Язык условий

```lisp
;; Базовые предикаты
(= outfit "noble")
(at-location forest)
(has-item ancient_amulet)
(has-memory "quest_accepted")

;; Составные условия
(and (= outfit "noble") 
     (at-location throne_room))

(or (has-item sword)
    (has-item dagger))

(not (quest-completed "helper_secret_potion"))

;; Условия с параметрами
(>= (loyalty villagers) 50)
(< (quest-step princess_lost_relic) 3)

;; Пользовательские предикаты
(can-afford (price 100))
(time-passed (minutes 10))
```

## Язык действий

```lisp
;; Управление памятью
(set-memory key value)
(remove-memory key)
(set-npc-memory npc key value)

;; Управление диалогами
(add-dialogue npc
  (option id text response
    (when condition)
    (effects ...)))

(remove-dialogue npc option-id)

;; Изменение мира
(reveal-location location-id)
(hide-location location-id)
(spawn-item location item-id)
(remove-item location item-id)

;; Управление инвентарём
(give-items item1 item2 ...)
(take-items item1 item2 ...)

;; Управление квестами
(start-quest quest-id)
(complete-quest)
(fail-quest reason)
(jump-to-step step-id)

;; Эффекты на персонажей
(change-loyalty group amount)
(set-outfit outfit-type)
(trigger-scene scene-id)
```

## Продвинутые возможности

### 1. Макросы и функции

```lisp
;; Определение переиспользуемых паттернов
(defpattern fetch-quest (item npc location)
  (step find-item
    (description (format "Найти ~a для ~a" item npc))
    (require (at-location location))
    (actions (spawn-item location item)))
  
  (step deliver-item
    (description (format "Отнести ~a к ~a" item npc))
    (require
      (has-item item)
      (talking-to npc))
    (actions
      (take-items item)
      (complete-quest))))

;; Использование паттерна
(quest simple_fetch
  (metadata ...)
  (steps
    (use-pattern fetch-quest 
      :item healing_herb
      :npc village_healer
      :location forest)))
```

### 2. Условное выполнение

```lisp
(step moral_choice
  (description "Сделать выбор")
  (actions
    (if (= outfit "noble")
      (then
        (set-flag "chose_nobles")
        (change-loyalty nobles +20)
        (change-loyalty villagers -10))
      (else
        (set-flag "chose_villagers")
        (change-loyalty nobles -10)
        (change-loyalty villagers +20)))))
```

### 3. Циклы и итерации

```lisp
(step gather_ingredients
  (description "Собрать все ингредиенты")
  (require
    (for-all (mushroom herb crystal)
      (has-item item)))
  (actions
    (for-each (mushroom herb crystal)
      (consume-item item))))
```

### 4. Динамические квесты

```lisp
(quest dynamic_quest
  (metadata
    (title (if (> (loyalty nobles) 50)
             "Благородная миссия"
             "Народное дело")))
  
  (steps
    (step dynamic_goal
      (description 
        (cond
          ((quest-completed "princess_lost_relic") 
           "Продолжить поиски артефактов")
          ((> (loyalty villagers) 75)
           "Помочь старейшине деревни")
          (else
           "Исследовать окрестности")))
      (require (evaluate-condition))
      (actions (execute-dynamic-action)))))
```

### 5. Композиция квестов

```lisp
(quest-chain epic_storyline
  (quests
    (quest prologue_quest ...)
    (quest main_quest
      (depends-on prologue_quest))
    (quest epilogue_quest
      (depends-on main_quest)
      (optional true))))
```

## Система типов (опционально)

```lisp
;; Определение типов для валидации
(deftype location (enum throne_room forest village hidden_grove))
(deftype character (enum princess helper))
(deftype outfit (enum noble common))

(deftype quest-step
  (struct
    (id symbol)
    (description string)
    (require condition)
    (actions (list action))))

;; Типизированный квест
(typed-quest princess_quest
  :character princess
  :steps (list quest-step)
  ...)
```

## Примеры сложных квестов

### 1. Квест с ветвлением

```lisp
(quest branching_quest
  (metadata
    (title "Расследование заговора")
    (character any))
  
  (steps
    (step discover_plot
      (description "Узнать о заговоре")
      (actions
        (branch
          ((= outfit "noble")
           (goto noble_path))
          ((= outfit "common") 
           (goto commoner_path))
          (else
           (fail-quest "Нужна подходящая одежда")))))
    
    (path noble_path
      (step report_to_court ...)
      (step confront_conspirators ...))
    
    (path commoner_path
      (step warn_villagers ...)
      (step organize_resistance ...))))
```

### 2. Квест с таймером

```lisp
(quest timed_rescue
  (metadata
    (title "Спасение до рассвета"))
  
  (on-start
    (set-timer dawn (minutes 20)))
  
  (steps
    (step find_prisoner
      (description "Найти пленника до рассвета")
      (require
        (not (timer-expired dawn)))
      (on-timeout
        (fail-quest "Не успели до рассвета")))
    ...))
```

### 3. Рекурсивные квесты

```lisp
(quest recursive_collection
  (metadata
    (title "Собрать все фрагменты"))
  
  (steps
    (step collect_fragment
      (description "Найти фрагмент карты")
      (require (< (count-items map_fragment) 5))
      (actions
        (give-items map_fragment)
        (if (< (count-items map_fragment) 5)
          (repeat-step collect_fragment)
          (complete-quest))))))
```

## Интеграция с игровой логикой

```lisp
;; Регистрация квеста в системе
(register-quest princess_lost_relic)

;; Проверка доступности квеста
(defun can-start-quest? (quest-id character)
  (let ((quest (get-quest quest-id)))
    (evaluate (quest-triggers quest) 
              (character-state character))))

;; Обработка шага квеста
(defun process-quest-step (quest step character)
  (when (evaluate (step-requirements step) 
                  (game-state))
    (execute-actions (step-actions step))
    (advance-quest quest)))
```

## Инструменты разработки

### 1. REPL для тестирования

```lisp
> (evaluate-condition '(and (= outfit "noble") (at-location forest)))
false

> (trace-quest princess_lost_relic)
Quest: princess_lost_relic
Current step: investigate_forest
Requirements not met:
  - (npc-memory old_woman "amulet_location") => false
```

### 2. Валидация квестов

```lisp
(validate-quest quest-definition)
;; Проверяет:
;; - Корректность синтаксиса
;; - Существование упомянутых NPC, локаций, предметов
;; - Достижимость всех шагов
;; - Отсутствие циклических зависимостей
```

## Преимущества S-выражений для квестов

1. **Мощная композиция**: Легко создавать сложные условия и действия
2. **Метапрограммирование**: Квесты могут генерировать другие квесты
3. **Отладка**: S-выражения легко логировать и анализировать
4. **Расширяемость**: Новые операторы добавляются тривиально
5. **Интероперабельность**: Легко интегрируется с JavaScript через простой транслятор

## Пример транслятора в JavaScript

```javascript
function evaluateSExp(sexp, context) {
  if (Array.isArray(sexp)) {
    const [op, ...args] = sexp;
    switch(op) {
      case 'and':
        return args.every(arg => evaluateSExp(arg, context));
      case '=':
        return context[args[0]] === args[1];
      case 'has-item':
        return context.inventory.includes(args[0]);
      // ... другие операторы
    }
  }
  return sexp;
}
```

## Заключение

S-выражения предоставляют элегантный и мощный способ определения квестов. Их простота и выразительность делают DSL легким для изучения, но достаточно мощным для создания сложных интерактивных историй. Гомоиконная природа S-выражений также открывает возможности для создания квестов, которые модифицируют себя во время выполнения.