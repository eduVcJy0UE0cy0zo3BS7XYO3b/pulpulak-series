# JSON-Object Oriented Game Design

## Цель
Преобразовать игру в максимально JSON-ориентированную архитектуру, где все игровые данные, логика и состояния представлены как JSON объекты с минимальным количеством hardcoded JavaScript кода.

## Текущая Проблема
Сейчас игровая логика разбросана по JavaScript файлам с hardcoded объектами:
- NPC диалоги: 941 строка сложных вложенных объектов
- Квесты: логика в JavaScript функциях
- Сцены: статические классы с hardcoded сценами
- Состояние игры: фиксированная структура

## Предлагаемая Архитектура

### 1. JSON Schema-Based Game State
```json
{
  "gameStateSchema": {
    "type": "object",
    "properties": {
      "roomId": {"type": "string"},
      "players": {
        "type": "object",
        "properties": {
          "princess": {"$ref": "#/definitions/Player"},
          "helper": {"$ref": "#/definitions/Player"}
        }
      },
      "currentScene": {"type": "string"},
      "gameData": {"$ref": "#/definitions/GameData"},
      "world": {"$ref": "#/definitions/World"}
    }
  }
}
```

### 2. JSON-Driven Rule Engine
```json
{
  "rules": [
    {
      "id": "outfit_change_rule",
      "conditions": [
        {"property": "player.location", "operator": "equals", "value": "bedroom"},
        {"property": "player.alone", "operator": "equals", "value": true}
      ],
      "actions": [
        {"type": "enable_action", "action": "change_outfit"}
      ]
    }
  ]
}
```

### 3. JSON Scene Definitions
```json
{
  "scenes": {
    "bedroom_intro": {
      "id": "bedroom_intro",
      "title": "В спальне",
      "roleSpecific": {
        "princess": {
          "description": "Вы просыпаетесь в роскошной спальне...",
          "actions": ["look_around", "change_outfit", "open_door"]
        },
        "helper": {
          "description": "Вы стоите у двери спальни принцессы...",
          "actions": ["knock", "wait", "leave"]
        }
      },
      "transitions": [
        {
          "trigger": "open_door",
          "conditions": [{"property": "door.locked", "operator": "equals", "value": false}],
          "target": "hallway"
        }
      ]
    }
  }
}
```

### 4. JSON NPC System
```json
{
  "npcs": {
    "village_elder": {
      "id": "village_elder",
      "name": "Староста деревни",
      "dialogues": {
        "default": {
          "greeting": {
            "conditions": [
              {"property": "player.role", "operator": "equals", "value": "princess"},
              {"property": "player.outfit", "operator": "equals", "value": "royal"}
            ],
            "text": "Ваше высочество! Как я рад вас видеть!",
            "choices": [
              {"text": "Здравствуйте", "response": "formal_response"},
              {"text": "Что случилось в деревне?", "response": "village_problems"}
            ]
          }
        }
      },
      "memory": {
        "loyalty": 0,
        "trust": 0,
        "hasMetBefore": false
      }
    }
  }
}
```

### 5. JSON Quest System
```json
{
  "quests": {
    "find_missing_villagers": {
      "id": "find_missing_villagers",
      "title": "Найти пропавших жителей",
      "description": "Несколько жителей деревни исчезли...",
      "steps": [
        {
          "id": "talk_to_elder",
          "description": "Поговорить со старостой",
          "conditions": [
            {"property": "npcs.village_elder.talked", "operator": "equals", "value": true}
          ],
          "rewards": [
            {"type": "experience", "amount": 10},
            {"type": "unlock_step", "step": "investigate_forest"}
          ]
        }
      ],
      "prerequisites": [
        {"property": "player.location", "operator": "equals", "value": "village_center"}
      ]
    }
  }
}
```

## Компоненты Системы

### 1. JSON Data Manager
```javascript
class JsonDataManager {
  constructor() {
    this.gameData = {};
    this.schema = {};
  }
  
  async loadData(dataType, source) {
    // Загрузка JSON данных из файлов или API
  }
  
  validateData(data, schema) {
    // Валидация данных по JSON Schema
  }
  
  query(path, conditions) {
    // JSONPath запросы к данным
  }
}
```

### 2. Rule Engine
```javascript
class RuleEngine {
  evaluateConditions(conditions, gameState) {
    // Оценка условий в JSON формате
  }
  
  executeActions(actions, gameState) {
    // Выполнение действий из JSON описания
  }
}
```

### 3. Scene Processor
```javascript
class SceneProcessor {
  renderScene(sceneId, playerRole, gameState) {
    // Рендеринг сцены из JSON данных
  }
  
  processTransition(trigger, gameState) {
    // Обработка переходов между сценами
  }
}
```

## Миграционный План

### Фаза 1: Инфраструктура
1. Создать JsonDataManager
2. Реализовать RuleEngine  
3. Добавить JSON Schema валидацию
4. Создать утилиты для работы с JSON данными

### Фаза 2: Простые Данные
1. Конвертировать locationData.js в JSON
2. Перевести constants.js в JSON конфигурацию
3. Преобразовать простые части gameState

### Фаза 3: Сложная Логика
1. Рефакторинг NPC диалогов в JSON + Rule Engine
2. Конвертация квестовой системы
3. Перевод coopStoryData в JSON сцены

### Фаза 4: Интеграция DataScript
1. Использование DataScript для сложных запросов
2. Реактивные обновления состояния
3. Продвинутая система отношений между объектами

## Преимущества

### Для Разработки
- **Модульность**: Четкое разделение данных и логики
- **Тестируемость**: Легкое unit-тестирование JSON данных
- **Валидация**: Автоматическая проверка корректности данных
- **Документация**: JSON Schema как живая документация

### Для Контента
- **Редактируемость**: Возможность изменения игрового контента без перекомпиляции
- **Визуальные редакторы**: Возможность создания GUI для редактирования
- **Версионирование**: Простое отслеживание изменений в контенте
- **Локализация**: Легкое добавление новых языков

### Для Производительности
- **Кэширование**: Эффективное кэширование JSON данных
- **Частичная загрузка**: Загрузка только необходимых частей игры
- **Оптимизация**: Возможность оптимизации структур данных

## Технические Детали

### JSON Schema Types
```json
{
  "definitions": {
    "Player": {
      "type": "object",
      "properties": {
        "id": {"type": "string"},
        "role": {"enum": ["princess", "helper"]},
        "outfit": {"enum": ["royal", "servant", "disguise"]},
        "location": {"type": "string"},
        "stats": {"$ref": "#/definitions/PlayerStats"}
      }
    },
    "Condition": {
      "type": "object",
      "properties": {
        "property": {"type": "string"},
        "operator": {"enum": ["equals", "not_equals", "greater", "less", "contains"]},
        "value": {}
      }
    }
  }
}
```

### Rule Evaluation Algorithm
1. Parse условия из JSON
2. Resolve значения properties из game state
3. Apply операторы для сравнения
4. Return boolean результат
5. Execute actions если условия true

### Data Loading Strategy
- **Lazy Loading**: Загрузка данных по требованию
- **Caching**: Кэширование часто используемых данных
- **Hot Reload**: Перезагрузка данных без перезапуска сервера
- **Validation**: Проверка JSON Schema при загрузке

## Пример Использования

```javascript
// Загрузка сцены
const scene = await dataManager.loadData('scene', 'bedroom_intro');

// Оценка условий
const canChangeOutfit = ruleEngine.evaluateConditions([
  {property: 'player.location', operator: 'equals', value: 'bedroom'},
  {property: 'player.alone', operator: 'equals', value: true}
], gameState);

// Рендеринг для роли
const content = sceneProcessor.renderScene('bedroom_intro', 'princess', gameState);
```

## Заключение

Переход к JSON-ориентированной архитектуре сделает игру более гибкой, модульной и простой в разработке. Данные станут первичными, а код - вторичным инструментом для их обработки.