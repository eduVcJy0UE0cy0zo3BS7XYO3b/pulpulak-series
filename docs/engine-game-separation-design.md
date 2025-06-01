# Дизайн документ: Миграция логики игры от движка

## 📋 Обзор

Данный документ описывает архитектурную миграцию от монолитной системы, где логика конкретной игры "Пулпулак" вшита в движок, к чистой архитектуре с разделением на универсальный движок и независимые игровые модули.

## 🎯 Цели миграции

### Основные цели:
- **Разделение ответственности**: Движок отвечает за общую логику, игра - за специфичную
- **Переиспользование**: Движок может работать с любыми кооперативными текстовыми играми
- **Масштабируемость**: Простое добавление новых игр без изменения движка
- **Тестируемость**: Изолированное тестирование движка и игровой логики

### Технические цели:
- Убрать все прямые импорты Pulpulak-данных из движка
- Создать универсальные интерфейсы для игровых конфигураций
- Реализовать dependency injection для всех игровых данных
- Создать систему плагинов для игр

## 🏗️ Текущая архитектура (проблемы)

```
┌─────────────────────────────────────────┐
│             Game Engine                 │
│  ┌─────────────────────────────────────┐│
│  │         CoopGameLogic.js            ││
│  │                                     ││
│  │  require('../games/pulpulak/...')   ││  ❌ Жесткая связь
│  │  const PulpulakGameConfig = ...     ││
│  │  if (questId === 'princess_lost..') ││  ❌ Хардкод логики
│  └─────────────────────────────────────┘│
│                                         │
│  ┌─────────────────────────────────────┐│
│  │       Data Managers                 ││
│  │                                     ││
│  │  GameDataManager.js                 ││  ❌ Импорты Pulpulak
│  │  QuestDataManager.js                ││  ❌ Специфичная логика
│  │  PlayerDataManager.js               ││
│  └─────────────────────────────────────┘│
└─────────────────────────────────────────┘
```

## 🎨 Целевая архитектура

```
┌─────────────────────────────────────────┐
│         Universal Game Engine           │
│  ┌─────────────────────────────────────┐│
│  │      CoopGameLogic.js               ││
│  │                                     ││
│  │  constructor(gameConfig: IGameCfg)  ││  ✅ Dependency Injection
│  │  this.gameConfig = gameConfig       ││
│  │  this.storyData = gameConfig.get... ││  ✅ Через интерфейсы
│  └─────────────────────────────────────┘│
│                                         │
│  ┌─────────────────────────────────────┐│
│  │    Universal Data Managers          ││
│  │                                     ││
│  │  GameDataManager(gameConfig)        ││  ✅ Конфигурируемые
│  │  QuestDataManager(gameConfig)       ││  ✅ Универсальные
│  │  PlayerDataManager(gameConfig)      ││
│  └─────────────────────────────────────┘│
└─────────────────────────────────────────┘
                    ▲
                    │ implements IGameConfig
                    │
┌─────────────────────────────────────────┐
│            Game Modules                 │
│  ┌─────────────────────────────────────┐│
│  │      PulpulakGameConfig.js          ││  ✅ Изолированная логика
│  │                                     ││
│  │  getStoryData() -> PulpulakStory    ││  ✅ Специфичные данные
│  │  getQuestData() -> PulpulakQuests   ││
│  │  getCharacters() -> ['princess'..   ││
│  └─────────────────────────────────────┘│
│                                         │
│  ┌─────────────────────────────────────┐│
│  │     DetectiveGameConfig.js          ││  ✅ Другие игры
│  │     FantasyGameConfig.js            ││
│  └─────────────────────────────────────┘│
└─────────────────────────────────────────┘
```

## 📋 План миграции

### Фаза 1: Создание интерфейсов и абстракций

#### 1.1 Создать универсальный игровой интерфейс
```javascript
// engine/interfaces/IGameConfig.js
class IGameConfig {
    // Данные игры
    getStoryData() { throw new Error('Not implemented'); }
    getLocationData() { throw new Error('Not implemented'); }
    getNPCData() { throw new Error('Not implemented'); }
    getQuestData() { throw new Error('Not implemented'); }
    
    // Конфигурация персонажей
    getCharacters() { throw new Error('Not implemented'); }
    getCharacterNames() { throw new Error('Not implemented'); }
    getAvailableOutfits(character) { throw new Error('Not implemented'); }
    
    // Игровая логика
    canSwitchOutfits(gameState, character) { throw new Error('Not implemented'); }
    getDynamicChoices(gameState, character) { throw new Error('Not implemented'); }
    createOutfitSwapChoice(character) { throw new Error('Not implemented'); }
    
    // Обработчики запросов
    getRequestHandlers() { throw new Error('Not implemented'); }
    
    // Валидация и правила
    validateGameRules(gameState) { throw new Error('Not implemented'); }
    getGameConstants() { throw new Error('Not implemented'); }
}
```

#### 1.2 Создать универсальные интерфейсы данных
```javascript
// engine/interfaces/IStoryData.js
class IStoryData {
    getScene(sceneId) { throw new Error('Not implemented'); }
    getAllScenes() { throw new Error('Not implemented'); }
    getInitialScene() { throw new Error('Not implemented'); }
}

// engine/interfaces/ILocationData.js
class ILocationData {
    getLocation(locationId) { throw new Error('Not implemented'); }
    getLocationInfo(locationId) { throw new Error('Not implemented'); }
    getConnections(locationId) { throw new Error('Not implemented'); }
    getAllLocations() { throw new Error('Not implemented'); }
}

// engine/interfaces/INPCData.js
class INPCData {
    getNPC(npcId) { throw new Error('Not implemented'); }
    getNPCsForLocation(location, gameState, character) { throw new Error('Not implemented'); }
    getNPCDialogue(npcId, outfit, memory, location, questState, globalMemory) { throw new Error('Not implemented'); }
    processDialogueChoice(npcId, choiceId, outfit, memory, isFollowUp, currentChoices, location) { throw new Error('Not implemented'); }
    getNPCAttitude(npcId, outfit) { throw new Error('Not implemented'); }
}

// engine/interfaces/IQuestData.js
class IQuestData {
    getQuest(questId) { throw new Error('Not implemented'); }
    createQuestInstance(questId) { throw new Error('Not implemented'); }
    getAllQuests() { throw new Error('Not implemented'); }
    getQuestsForCharacter(character) { throw new Error('Not implemented'); }
}
```

### Фаза 2: Рефакторинг движка

#### 2.1 Модифицировать CoopGameLogic для dependency injection
```javascript
// game/coopGameLogic.js
class CoopGameLogic {
    constructor(gameConfig) {
        if (!gameConfig || typeof gameConfig.getStoryData !== 'function') {
            throw new Error('GameConfig must implement IGameConfig interface');
        }
        
        this.gameConfig = gameConfig;
        
        // Получаем все данные через gameConfig
        this.storyData = gameConfig.getStoryData();
        this.locationData = gameConfig.getLocationData();
        this.npcData = gameConfig.getNPCData();
        this.questData = gameConfig.getQuestData();
        this.constants = gameConfig.getGameConstants();
        
        // Менеджеры данных тоже получают gameConfig
        const managers = dataManagerFactory.getManagers(gameConfig);
        this.gameData = managers.gameData;
        this.playerData = managers.playerData;
        this.questDataManager = managers.questData;
        this.requestData = managers.requestData;
        
        this.stateManager = new GameStateManager();
        this.immerStateManager = new ImmerStateManager();
    }
    
    // Убираем все прямые импорты и заменяем на this.gameConfig вызовы
    getSpecialChoices(gameState, character) {
        const getDynamicChoices = (gameState, character) => {
            return this.gameConfig.getDynamicChoices(gameState, character);
        };
        
        return ChoiceBuilder.getSpecialChoices(
            gameState, 
            character, 
            this.getMovementChoices.bind(this),
            this.getNPCInteractionChoices.bind(this),
            getDynamicChoices
        );
    }
    
    createOutfitSwapChoice(character) {
        return this.gameConfig.createOutfitSwapChoice(character);
    }
    
    canSwitchOutfits(gameState, character) {
        return this.gameConfig.canSwitchOutfits(gameState, character);
    }
}
```

#### 2.2 Рефакторинг Data Managers
```javascript
// game/managers/GameDataManager.js
class GameDataManager {
    constructor(gameConfig) {
        this.gameConfig = gameConfig;
        this.storyData = gameConfig.getStoryData();
        this.locationData = gameConfig.getLocationData();
        this.games = new Map();
    }
    
    createGame(roomId, players) {
        const characters = this.gameConfig.getCharacters();
        const initialScene = this.storyData.getInitialScene();
        
        // Универсальная логика создания игры
        const gameState = {
            roomId,
            currentScene: initialScene,
            players: {},
            stats: {},
            // ...
        };
        
        // Инициализация персонажей на основе gameConfig
        characters.forEach(character => {
            gameState.players[character] = players[character];
            gameState.stats[character] = {
                location: this.gameConfig.getInitialLocation(character),
                outfit: this.gameConfig.getInitialOutfit(character),
                inventory: [],
                awareness: 0,
                npcsPresent: []
            };
        });
        
        this.games.set(roomId, gameState);
        return gameState;
    }
}

// game/managers/DataManagerFactory.js
class DataManagerFactory {
    static getManagers(gameConfig) {
        const gameData = new GameDataManager(gameConfig);
        const playerData = new PlayerDataManager(gameConfig);
        const questData = new QuestDataManager(gameConfig);
        const requestData = new RequestManager();
        
        // Регистрируем обработчики запросов из gameConfig
        const requestHandlers = gameConfig.getRequestHandlers();
        if (requestHandlers) {
            requestHandlers.registerHandlers(requestData);
        }
        
        return { gameData, playerData, questData, requestData };
    }
}
```

#### 2.3 Рефакторинг утилит
```javascript
// game/utils/choiceBuilder.js
class ChoiceBuilder {
    static getCharacterName(character, gameConfig) {
        const characterNames = gameConfig.getCharacterNames();
        return characterNames[character] || character;
    }
    
    static createOutfitSwapChoice(character, gameConfig) {
        // Делегируем создание выбора в gameConfig
        return gameConfig.createOutfitSwapChoice(character);
    }
    
    static getMovementChoices(gameState, character, gameConfig) {
        const locationData = gameConfig.getLocationData();
        const currentLocation = gameState.stats[character].location;
        const locationInfo = locationData.getLocationInfo(currentLocation);
        
        if (!locationInfo) return [];
        
        const choices = [];
        locationInfo.connections.forEach(connection => {
            choices.push({
                id: `move_to_${connection.id}`,
                text: `${connection.icon} Перейти: ${connection.name}`,
                description: `Отправиться в ${connection.name}`,
                isMovement: true,
                targetLocation: connection.id
            });
        });
        
        return choices;
    }
}
```

### Фаза 3: Создание игровых модулей

#### 3.1 Рефакторинг PulpulakGameConfig
```javascript
// games/pulpulak/PulpulakGameConfig.js
const IGameConfig = require('../../engine/interfaces/IGameConfig');
const CoopStoryData = require('./data/coopStoryData');
const LocationData = require('./data/locationData');
const NPCData = require('./data/npcData');
const QuestData = require('./data/questData');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('./data/constants');
const RequestHandlers = require('./requestHandlers');

class PulpulakGameConfig extends IGameConfig {
    constructor() {
        super();
        this.gameId = 'pulpulak';
        this.gameName = 'Принцесса Пулпулак';
        this.gameVersion = '1.0.0';
    }
    
    // Данные игры
    getStoryData() { return CoopStoryData; }
    getLocationData() { return LocationData; }
    getNPCData() { return NPCData; }
    getQuestData() { return QuestData; }
    
    // Конфигурация персонажей
    getCharacters() { return ['princess', 'helper']; }
    getCharacterNames() { return CHARACTER_NAMES; }
    getCharacterRoles() { return CHARACTER_ROLES; }
    
    getInitialLocation(character) {
        return character === 'princess' ? 'princess_chamber' : 'servant_quarters';
    }
    
    getInitialOutfit(character) {
        return character === 'princess' ? 'nightgown' : 'common_dress';
    }
    
    getAvailableOutfits(character) {
        if (character === 'princess') {
            return ['nightgown', 'princess_dress', 'court_dress'];
        } else {
            return ['common_dress', 'servant_outfit'];
        }
    }
    
    // Игровая логика
    canSwitchOutfits(gameState, character) {
        const characterLocation = gameState.stats[character].location;
        const otherCharacter = character === 'princess' ? 'helper' : 'princess';
        const otherLocation = gameState.stats[otherCharacter].location;
        
        // Можно переодеваться только наедине
        if (characterLocation !== otherLocation) {
            return false;
        }
        
        // Нельзя переодеваться при посторонних NPC
        const npcsPresent = gameState.stats[character].npcsPresent || [];
        return npcsPresent.length === 0;
    }
    
    getDynamicChoices(gameState, character) {
        const choices = [];
        
        // Добавляем выбор обмена одеждой если возможно
        if (this.canSwitchOutfits(gameState, character)) {
            choices.push(this.createOutfitSwapChoice(character));
        }
        
        return choices;
    }
    
    createOutfitSwapChoice(character) {
        const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
        return {
            id: 'request_outfit_swap',
            text: '👗 Предложить поменяться одеждой',
            description: `Предложить ${otherCharacter} поменяться нарядами`,
            isOutfitRequest: true
        };
    }
    
    // Обработчики запросов
    getRequestHandlers() {
        return RequestHandlers;
    }
    
    // Константы и настройки
    getGameConstants() {
        return {
            OUTFIT_NAMES,
            CHARACTER_NAMES,
            CHARACTER_ROLES
        };
    }
    
    // Валидация игровых правил
    validateGameRules(gameState) {
        // Специфичные для Пулпулак правила валидации
        return { valid: true };
    }
    
    // Метаданные игры
    getGameMetadata() {
        return {
            id: this.gameId,
            name: this.gameName,
            version: this.gameVersion,
            description: 'Кооперативная текстовая приключенческая игра о принцессе и её помощнице',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedPlayTime: '30-60 minutes',
            tags: ['cooperative', 'text-adventure', 'roleplay', 'medieval']
        };
    }
}

module.exports = PulpulakGameConfig;
```

#### 3.2 Создание абстрактных адаптеров для данных
```javascript
// games/pulpulak/adapters/StoryDataAdapter.js
const IStoryData = require('../../../engine/interfaces/IStoryData');
const CoopStoryData = require('../data/coopStoryData');

class PulpulakStoryDataAdapter extends IStoryData {
    getScene(sceneId) {
        return CoopStoryData.getScene(sceneId);
    }
    
    getAllScenes() {
        return CoopStoryData.getAllScenes();
    }
    
    getInitialScene() {
        return 'intro_scene';
    }
}

module.exports = PulpulakStoryDataAdapter;
```

### Фаза 4: Обновление точки входа

#### 4.1 Модифицировать server.js и socketHandler.js
```javascript
// server.js
const express = require('express');
const { createServer } = require('http');
const { Server } = require('socket.io');
const PulpulakGameConfig = require('./games/pulpulak/PulpulakGameConfig');
const socketHandler = require('./network/socketHandler');

const app = express();
const server = createServer(app);
const io = new Server(server);

// Создаем конфигурацию игры
const gameConfig = new PulpulakGameConfig();

// Передаем конфигурацию в socketHandler
socketHandler(io, gameConfig);

// network/socketHandler.js
const CoopGameLogic = require('../game/coopGameLogic');

function socketHandler(io, gameConfig) {
    // Создаем экземпляр игровой логики с конфигурацией
    const gameLogic = new CoopGameLogic(gameConfig);
    
    io.on('connection', (socket) => {
        // Вся остальная логика остается той же
        socket.on('startGame', (data) => {
            const result = gameLogic.startGame(data.roomId, data.players);
            // ...
        });
        
        // ...
    });
}

module.exports = socketHandler;
```

### Фаза 5: Обновление тестов

#### 5.1 Создать моки для тестирования
```javascript
// game/__tests__/mocks/MockGameConfig.js
const IGameConfig = require('../../engine/interfaces/IGameConfig');

class MockGameConfig extends IGameConfig {
    constructor(options = {}) {
        super();
        this.options = options;
    }
    
    getStoryData() {
        return this.options.storyData || {
            getScene: () => ({ title: 'Test Scene', text: 'Test text', choices: {} })
        };
    }
    
    getLocationData() {
        return this.options.locationData || {
            getLocationInfo: () => ({ name: 'Test Location', connections: [] })
        };
    }
    
    // ... остальные методы с моками
    
    getCharacters() {
        return this.options.characters || ['player1', 'player2'];
    }
    
    canSwitchOutfits() {
        return this.options.canSwitchOutfits !== undefined ? this.options.canSwitchOutfits : true;
    }
}

module.exports = MockGameConfig;
```

#### 5.2 Обновить существующие тесты
```javascript
// game/__tests__/coopGameLogic.test.js
const CoopGameLogic = require('../coopGameLogic');
const MockGameConfig = require('./mocks/MockGameConfig');
const PulpulakGameConfig = require('../../games/pulpulak/PulpulakGameConfig');

describe('CoopGameLogic', () => {
    let gameLogic;
    
    describe('с Pulpulak конфигурацией', () => {
        beforeEach(() => {
            const gameConfig = new PulpulakGameConfig();
            gameLogic = new CoopGameLogic(gameConfig);
        });
        
        // Существующие тесты с Pulpulak
    });
    
    describe('с универсальной конфигурацией', () => {
        beforeEach(() => {
            const mockConfig = new MockGameConfig({
                characters: ['alice', 'bob'],
                canSwitchOutfits: false
            });
            gameLogic = new CoopGameLogic(mockConfig);
        });
        
        // Новые тесты для универсальной логики
        test('должен создавать игру с произвольными персонажами', () => {
            // ...
        });
    });
});
```

## 📦 Структура файлов после миграции

```
pulpulak-series/
├── engine/                           # ✅ Универсальный движок
│   ├── interfaces/
│   │   ├── IGameConfig.js
│   │   ├── IStoryData.js
│   │   ├── ILocationData.js
│   │   ├── INPCData.js
│   │   └── IQuestData.js
│   └── README.md
├── game/                            # ✅ Универсальная игровая логика
│   ├── coopGameLogic.js            # ✅ Без импортов Pulpulak
│   ├── managers/
│   │   ├── GameDataManager.js      # ✅ Принимает gameConfig
│   │   ├── PlayerDataManager.js    # ✅ Универсальный
│   │   ├── QuestDataManager.js     # ✅ Без хардкода
│   │   └── DataManagerFactory.js   # ✅ С dependency injection
│   └── utils/
│       ├── choiceProcessor.js      # ✅ Универсальный
│       ├── questProcessor.js       # ✅ Универсальный
│       └── ...
├── games/                          # ✅ Игровые модули
│   ├── pulpulak/
│   │   ├── PulpulakGameConfig.js   # ✅ Реализует IGameConfig
│   │   ├── data/                   # ✅ Изолированные данные
│   │   │   ├── coopStoryData.js
│   │   │   ├── locationData.js
│   │   │   ├── npcData.js
│   │   │   └── questData.js
│   │   └── requestHandlers.js
│   ├── detective/
│   │   ├── DetectiveGameConfig.js  # ✅ Новая игра
│   │   └── data/
│   └── _template/                  # ✅ Шаблон для новых игр
│       ├── TemplateGameConfig.js
│       └── data/
└── network/
    └── socketHandler.js            # ✅ Принимает gameConfig
```

## 🚀 Этапы внедрения

### Этап 1 (1-2 недели): Подготовка
- [ ] Создать интерфейсы `IGameConfig`, `IStoryData`, etc.
- [ ] Создать базовый `PulpulakGameConfig` с методами-заглушками
- [ ] Создать моки для тестирования

### Этап 2 (2-3 недели): Рефакторинг движка
- [ ] Модифицировать `CoopGameLogic` для dependency injection
- [ ] Обновить все Data Managers
- [ ] Рефакторить utility классы
- [ ] Обновить точки входа (`server.js`, `socketHandler.js`)

### Этап 3 (1-2 недели): Создание игрового модуля
- [ ] Полностью реализовать `PulpulakGameConfig`
- [ ] Создать адаптеры для существующих данных
- [ ] Перенести всю Pulpulak-специфичную логику в игровой модуль

### Этап 4 (1 неделя): Тестирование и документация
- [ ] Обновить все тесты
- [ ] Создать тесты для универсальной логики
- [ ] Написать документацию для создания новых игр
- [ ] Создать шаблон для новых игр

### Этап 5 (1 неделя): Валидация
- [ ] Создать простую тестовую игру для проверки универсальности
- [ ] Провести интеграционные тесты
- [ ] Оптимизация производительности

## 🎯 Критерии успеха

### Технические критерии:
- ✅ Движок не содержит импортов из `games/pulpulak/`
- ✅ Можно создать новую игру, реализовав только `IGameConfig`
- ✅ Все существующие тесты проходят
- ✅ Pulpulak игра работает идентично текущей версии

### Архитектурные критерии:
- ✅ Четкое разделение ответственности
- ✅ Высокая когезия внутри модулей
- ✅ Слабая связанность между движком и играми
- ✅ Возможность unit-тестирования в изоляции

## 🔄 Обратная совместимость

Во время миграции будет поддерживаться обратная совместимость:

1. **Промежуточный адаптер**: Создать адаптер, который оборачивает текущую логику в `IGameConfig`
2. **Постепенная миграция**: Переводить по одному модулю за раз
3. **Тестирование на каждом этапе**: Обеспечить работоспособность после каждого изменения

## 📚 Документация для разработчиков

После миграции будет создана документация:

1. **Game Development Guide**: Как создать новую игру
2. **Engine API Reference**: Документация всех интерфейсов
3. **Migration Guide**: Инструкции по переводу существующих игр
4. **Best Practices**: Рекомендации по архитектуре игр

## 🔮 Будущие возможности

После успешной миграции станут возможными:

- **Динамическая загрузка игр**: Загрузка игровых модулей во время выполнения
- **Игровой магазин**: Система распространения игр
- **Мультитенантность**: Запуск нескольких игр одновременно
- **A/B тестирование**: Тестирование разных версий игровой логики
- **Модульные обновления**: Обновление движка независимо от игр

Эта миграция заложит фундамент для масштабируемой платформы кооперативных текстовых игр.
