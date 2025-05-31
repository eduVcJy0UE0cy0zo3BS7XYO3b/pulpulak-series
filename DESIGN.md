# DESIGN: Переход на Immer для управления состоянием

## Обзор

Данный документ описывает план перехода кооперативной текстовой RPG "Пулпулак" на библиотеку **Immer** для иммутабельного управления состоянием игры. Переход направлен на улучшение надёжности, производительности и сопровождаемости кода.

## Текущая архитектура состояния

### Анализ существующих проблем

1. **Прямые мутации объектов**: `/game/coopGameLogic.js:135-137`
   ```javascript
   const { princess, helper } = gameState.stats;
   const tempOutfit = princess.outfit;
   princess.outfit = helper.outfit;
   helper.outfit = tempOutfit;
   ```

2. **Глубокое клонирование через JSON**: `/game/gameStateManager.js:102-103`
   ```javascript
   cloneGameState(gameState) {
       return JSON.parse(JSON.stringify(gameState));
   }
   ```

3. **Неконтролируемые мутации массивов**: `/game/coopGameLogic.js:602-604`
   ```javascript
   if (result.effects.item) {
       gameState.stats[character].inventory.push(result.effects.item);
   }
   ```

### Структура состояния

```javascript
GameState = {
    roomId: string,
    players: { princess: Player, helper: Player },
    currentScene: string,
    turnOrder: 'princess' | 'helper',
    chapter: number,
    stats: {
        princess: CharacterStats,
        helper: CharacterStats
    },
    npcMemory: { princess: Object, helper: Object },
    quests: { princess: QuestState, helper: QuestState },
    globalQuestMemory: Object,
    npcDialogues: { princess: Dialogue?, helper: Dialogue? }
}
```

## Цели миграции

1. **Иммутабельность**: Гарантировать, что состояние не изменяется напрямую
2. **Производительность**: Уменьшить overhead от глубокого клонирования
3. **Отладка**: Улучшить трекинг изменений состояния
4. **Безопасность**: Предотвратить случайные мутации между компонентами

## План миграции

### Фаза 1: Установка и базовая интеграция

#### 1.1 Установка зависимости
```bash
npm install immer
```

#### 1.2 Создание обёртки для Immer
Создать `/game/stateManager.js`:
```javascript
const { produce } = require('immer');

class ImmerStateManager {
    constructor() {
        this.enablePatches = true; // Для debugging
    }
    
    // Безопасное обновление состояния
    updateState(currentState, updater) {
        return produce(currentState, updater, (patches, inversePatches) => {
            if (this.enablePatches) {
                console.log('State patches:', patches);
            }
        });
    }
    
    // Создание иммутабельной копии
    createDraft(state) {
        return produce(state, draft => draft);
    }
}
```

### Фаза 2: Рефакторинг GameStateManager

#### 2.1 Обновление методов состояния
Заменить прямые мутации в `/game/gameStateManager.js`:

**До:**
```javascript
updateCharacterStat(gameState, character, statName, value) {
    gameState.stats[character][statName] = value;
    return gameState;
}
```

**После:**
```javascript
updateCharacterStat(gameState, character, statName, value) {
    return this.stateManager.updateState(gameState, draft => {
        draft.stats[character][statName] = value;
    });
}
```

#### 2.2 Инвентарь и коллекции
**До:**
```javascript
addToInventory(gameState, character, item) {
    const inventory = gameState.stats[character].inventory;
    inventory.push(item);
    return gameState;
}
```

**После:**
```javascript
addToInventory(gameState, character, item) {
    return this.stateManager.updateState(gameState, draft => {
        if (draft.stats[character].inventory.length >= MAX_INVENTORY_SIZE) {
            throw new Error('Inventory is full');
        }
        draft.stats[character].inventory.push(item);
    });
}
```

### Фаза 3: Рефакторинг CoopGameLogic

#### 3.1 Обмен одеждой
**До:** `/game/coopGameLogic.js:135-137`
```javascript
const { princess, helper } = gameState.stats;
const tempOutfit = princess.outfit;
princess.outfit = helper.outfit;
helper.outfit = tempOutfit;
```

**После:**
```javascript
return this.stateManager.updateState(gameState, draft => {
    const tempOutfit = draft.stats.princess.outfit;
    draft.stats.princess.outfit = draft.stats.helper.outfit;
    draft.stats.helper.outfit = tempOutfit;
});
```

#### 3.2 Применение эффектов
**До:** `/game/coopGameLogic.js:320-331`
```javascript
applyEffects(gameState, effects, character) {
    if (effects.outfit) {
        gameState.stats[character].outfit = effects.outfit;
    }
    if (effects.location) {
        gameState.stats[character].location = effects.location;
        gameState.stats[character].npcsPresent = this.getNPCsForLocation(effects.location, gameState, character);
    }
    if (effects.awareness) {
        gameState.stats[character].awareness += effects.awareness;
    }
}
```

**После:**
```javascript
applyEffects(gameState, effects, character) {
    return this.stateManager.updateState(gameState, draft => {
        if (effects.outfit) {
            draft.stats[character].outfit = effects.outfit;
        }
        if (effects.location) {
            draft.stats[character].location = effects.location;
            draft.stats[character].npcsPresent = this.getNPCsForLocation(effects.location, gameState, character);
        }
        if (effects.awareness) {
            draft.stats[character].awareness += effects.awareness;
        }
    });
}
```

#### 3.3 Обновление NPC памяти
**До:** `/game/coopGameLogic.js:598`
```javascript
gameState.npcMemory[character][npcId] = result.updatedMemory;
```

**После:**
```javascript
gameState = this.stateManager.updateState(gameState, draft => {
    draft.npcMemory[character][npcId] = result.updatedMemory;
});
```

### Фаза 4: Интеграция с Socket.IO

#### 4.1 Middleware для состояния
Создать middleware в `/network/stateMiddleware.js`:
```javascript
class StateMiddleware {
    constructor(stateManager) {
        this.stateManager = stateManager;
    }
    
    // Автоматическое логирование изменений состояния
    withStateLogging(operation, context) {
        return (gameState, ...args) => {
            const result = operation(gameState, ...args);
            console.log(`[${context}] State updated for room: ${gameState.roomId}`);
            return result;
        };
    }
    
    // Валидация состояния после изменений
    withValidation(operation, validator) {
        return (gameState, ...args) => {
            const result = operation(gameState, ...args);
            if (!validator(result)) {
                throw new Error('Invalid state after operation');
            }
            return result;
        };
    }
}
```

#### 4.2 Обновление SocketHandler
В `/network/socketHandler.js`:
```javascript
// Заменить прямые обновления состояния
const result = this.gameLogic.makeChoice(roomId, socket.id, data.choiceId, data.character);
if (result.success && result.gameData) {
    // gameData уже иммутабельно из Immer
    this.io.to(roomId).emit('game-update', result.gameData);
}
```

### Фаза 5: Оптимизация и безопасность

#### 5.1 Заморозка объектов в production
```javascript
const { produce, enableMapSet, setAutoFreeze } = require('immer');

// В production
if (process.env.NODE_ENV === 'production') {
    setAutoFreeze(true); // Замораживать результаты
}

// Поддержка Map/Set если нужно
enableMapSet();
```

#### 5.2 Селекторы состояния
Создать `/game/selectors.js`:
```javascript
const { createSelector } = require('reselect'); // Опционально

// Базовые селекторы
const getCharacterStats = (gameState, character) => gameState.stats[character];
const getCharacterLocation = (gameState, character) => gameState.stats[character].location;
const getCharacterInventory = (gameState, character) => gameState.stats[character].inventory;

// Мемоизированные селекторы для производительности
const getCharacterWithLocation = createSelector(
    [getCharacterStats, getCharacterLocation],
    (stats, location) => ({ ...stats, location })
);
```

## Тестирование

### Unit тесты для Immer интеграции
```javascript
// __tests__/immerIntegration.test.js
const { produce } = require('immer');
const GameStateManager = require('../game/gameStateManager');

describe('Immer State Management', () => {
    test('должен создавать иммутабельные обновления', () => {
        const manager = new GameStateManager();
        const initialState = manager.createInitialState('test', mockPlayers);
        
        const updatedState = manager.updateCharacterStat(initialState, 'princess', 'awareness', 50);
        
        // Исходное состояние не должно измениться
        expect(initialState.stats.princess.awareness).not.toBe(50);
        // Новое состояние должно иметь обновлённое значение
        expect(updatedState.stats.princess.awareness).toBe(50);
        // Объекты должны быть разными
        expect(updatedState).not.toBe(initialState);
    });
});
```

## Преимущества после миграции

1. **Предотвращение багов**: Никто не сможет случайно изменить состояние
2. **Лучшая отладка**: Патчи Immer покажут, что именно изменилось
3. **Производительность**: Структурное разделение вместо глубокого клонирования
4. **Time-travel debugging**: Возможность отката изменений
5. **Безопасность потоков**: Состояние не может быть изменено во время чтения

## Потенциальные риски

1. **Размер бандла**: Immer добавляет ~12KB
2. **Кривая обучения**: Команда должна понимать концепцию draft'ов
3. **Производительность**: Небольшой overhead при создании proxy объектов
4. **Совместимость**: Нужно обновить все места, работающие с состоянием

## Планы развития

1. **Redux интеграция**: В будущем можно добавить Redux для централизованного управления
2. **DevTools**: Интеграция с Redux DevTools для отладки
3. **Middleware**: Система middleware для логирования, валидации, persistence
4. **Undo/Redo**: Система отмены действий для игровых механик

## Заключение

Переход на Immer значительно улучшит архитектуру управления состоянием в игре "Пулпулак", сделав код более надёжным и поддерживаемым. Миграция должна проводиться поэтапно с тщательным тестированием каждого компонента.