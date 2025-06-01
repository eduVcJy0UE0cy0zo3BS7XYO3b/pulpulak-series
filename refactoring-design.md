# Дизайн документ: Рефакторинг coopGameLogic.js

## Проблематика

- **GameEngine.js** (~557 строк) - мёртвый код, НЕ используется в проекте
- **coopGameLogic.js** (~849 строк) - **СЛИШКОМ БОЛЬШОЙ** монолитный файл
- Смешаны разные уровни ответственности в одном классе
- Сложно поддерживать и тестировать

### Проблемы текущей архитектуры:
- **Нарушение Single Responsibility Principle** - один класс делает всё
- **Сложность тестирования** - большой класс с множественными зависимостями  
- **Плохая читаемость** - 849 строк в одном файле
- **Тесная связанность** - всё завязано на один большой класс

## Цель рефакторинга

**Разбить монолитный `coopGameLogic.js` на специализированные, небольшие модули** с чёткими границами ответственности.

## План рефакторинга

### Фаза 1: Декомпозиция на специализированные сервисы

#### 1.1 Новая модульная архитектура

```
game/
├── coopGameLogic.js          (~80 строк) - Главный координатор
├── services/
│   ├── GameStateService.js   (~120 строк) - Управление состоянием игр
│   ├── ChoiceService.js      (~100 строк) - Обработка выборов
│   ├── MovementService.js    (~80 строк)  - Система перемещения
│   ├── NPCService.js         (~150 строк) - Взаимодействие с NPC
│   ├── QuestService.js       (~140 строк) - Система квестов
│   ├── RequestService.js     (~90 строк)  - Система запросов
│   └── ValidationService.js  (~60 строк)  - Валидация действий
└── utils/
    ├── GameDataBuilder.js    (~80 строк)  - Построение данных для клиента
    └── EffectsProcessor.js   (~40 строк)  - Применение эффектов
```

### Фаза 2: Создание специализированных сервисов

#### 2.1 GameStateService - Управление жизненным циклом игр
```javascript
// game/services/GameStateService.js
class GameStateService {
    constructor() {
        this.games = new Map();
        this.stateManager = new ImmerStateManager();
    }
    
    createGame(roomId, players) { }
    getGame(roomId) { }
    updateGame(roomId, updater) { }
    removeGame(roomId) { }
    hasGame(roomId) { }
}
```

#### 2.2 ChoiceService - Обработка выборов
```javascript
// game/services/ChoiceService.js
class ChoiceService {
    constructor(gameStateService, movementService, npcService, requestService) {
        this.gameState = gameStateService;
        this.movement = movementService;
        this.npc = npcService;
        this.request = requestService;
    }
    
    processChoice(roomId, playerId, choiceId, character) { }
    getChoicesForCharacter(gameState, character, sceneData) { }
    getSceneChoices(gameState, character, sceneData) { }
    getSpecialChoices(gameState, character) { }
}
```

#### 2.3 MovementService - Система перемещения  
```javascript
// game/services/MovementService.js
class MovementService {
    constructor(gameStateService) {
        this.gameState = gameStateService;
    }
    
    processMovement(gameState, targetLocation, character) { }
    getMovementChoices(gameState, character) { }
    validateMovement(currentLocation, targetLocation) { }
}
```

#### 2.4 NPCService - Взаимодействие с NPC
```javascript
// game/services/NPCService.js
class NPCService {
    constructor(gameStateService, questService) {
        this.gameState = gameStateService;
        this.quest = questService;
    }
    
    processNPCInteraction(gameState, npcId, character) { }
    processDialogueChoice(roomId, playerId, choiceId, character) { }
    closeDialogue(roomId, playerId) { }
    getNPCInteractionChoices(gameState, character) { }
}
```

#### 2.5 QuestService - Система квестов
```javascript
// game/services/QuestService.js
class QuestService {
    constructor(gameStateService) {
        this.gameState = gameStateService;
    }
    
    startQuest(gameState, character, questId) { }
    updateQuestProgress(gameState, character, stepId) { }
    completeQuest(gameState, character) { }
    processQuestAction(gameState, character, choiceId, dialogueResult) { }
    canStartQuest(gameState, character, questId) { }
}
```

#### 2.6 RequestService - Система запросов
```javascript
// game/services/RequestService.js  
class RequestService {
    constructor() {
        this.activeRequests = new Map();
    }
    
    createRequest(roomId, requestType, fromPlayerId, fromCharacter, data) { }
    respondToRequest(roomId, playerId, accepted, responseData) { }
    getActiveRequest(roomId) { }
    cancelRequest(roomId) { }
    hasActiveRequest(roomId) { }
}
```

### Фаза 3: Упрощённый главный класс

```javascript
// game/coopGameLogic.js (~80 строк)
class CoopGameLogic {
    constructor() {
        // Инициализация сервисов
        this.gameState = new GameStateService();
        this.request = new RequestService();
        this.movement = new MovementService(this.gameState);
        this.quest = new QuestService(this.gameState);
        this.npc = new NPCService(this.gameState, this.quest);
        this.choice = new ChoiceService(this.gameState, this.movement, this.npc, this.request);
        this.validation = new ValidationService();
        this.dataBuilder = new GameDataBuilder();
    }
    
    // Публичные методы - простые делегаты
    startGame(roomId, players) {
        return this.gameState.createGame(roomId, players);
    }
    
    makeChoice(roomId, playerId, choiceId, character) {
        return this.choice.processChoice(roomId, playerId, choiceId, character);
    }
    
    getGameData(roomId) {
        const gameState = this.gameState.getGame(roomId);
        return this.dataBuilder.buildClientData(gameState);
    }
    
    // ... остальные методы как простые делегаты
}
```

### Фаза 4: Утилиты и вспомогательные классы

#### 4.1 GameDataBuilder - Построение данных для клиента
```javascript
// game/utils/GameDataBuilder.js
class GameDataBuilder {
    buildClientData(gameState) { }
    buildCharacterData(gameState) { }
    buildSceneData(sceneData) { }
    buildChoicesData(gameState, sceneData) { }
}
```

#### 4.2 EffectsProcessor - Применение эффектов
```javascript  
// game/utils/EffectsProcessor.js
class EffectsProcessor {
    static applyEffects(gameState, effects, character) { }
    static applyOutfitChange(gameState, character, outfit) { }
    static applyLocationChange(gameState, character, location) { }
    static applyInventoryChange(gameState, character, item) { }
}
```

### Фаза 5: Удаление мёртвого кода

1. **Удалить GameEngine.js** (~557 строк)
2. **Удалить GameEngineFactory.js** (~81 строка)  
3. **Удалить существующую папку managers/** (если она дублирует функциональность)
4. **Очистить импорты** в других файлах

## Ожидаемые результаты

### Размер файлов после рефакторинга:
- **coopGameLogic.js**: 849 → 80 строк (**-769 строк**)
- **Новые сервисы**: +820 строк (но в 8 отдельных файлах)
- **Удалённый мёртвый код**: -638 строк
- **Чистый выигрыш**: **-587 строк** общего объёма кода

### Преимущества:

1. **Лучшая читаемость**
   - Файлы 60-150 строк вместо 849
   - Чёткое разделение ответственности

2. **Упрощение тестирования**  
   - Можно тестировать каждый сервис изолированно
   - Лёгкое мокирование зависимостей

3. **Ускорение разработки**
   - Можно работать над разными частями параллельно
   - Меньше конфликтов при слиянии кода

4. **Лучшая расширяемость**
   - Легко добавлять новые фичи в соответствующие сервисы
   - Простая замена/модификация отдельных компонентов

5. **Соблюдение SOLID принципов**
   - Single Responsibility Principle
   - Dependency Inversion Principle
   - Interface Segregation Principle

### Миграционная стратегия:

1. **Создать новые сервисы** постепенно
2. **Переместить логику** по одному сервису за раз
3. **Обновить тесты** для каждого сервиса
4. **Запускать все тесты** после каждого шага
5. **Удалить мёртвый код** в конце

### Риски и митигация:

**Риски:**
- Временное увеличение сложности во время миграции
- Возможные баги при переносе логики
- Необходимость переписать существующие тесты

**Митигация:**  
- Пошаговая миграция с сохранением работоспособности
- Автоматические тесты на каждом шаге
- Возможность отката к предыдущей версии

## Критерии успеха

- ✅ Все существующие тесты проходят
- ✅ Функциональность игры сохранена полностью  
- ✅ coopGameLogic.js стал меньше 100 строк
- ✅ Каждый сервис меньше 200 строк
- ✅ Общий объём кода уменьшился на 500+ строк
- ✅ Новые фичи добавляются быстрее