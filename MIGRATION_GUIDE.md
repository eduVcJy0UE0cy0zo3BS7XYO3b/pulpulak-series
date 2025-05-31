# Миграция на новый игровой движок

Этот гайд показывает, как перейти от существующей системы к новому универсальному движку.

## Что было сделано

### 1. Создан универсальный движок
- **engine/GameEngine.js** - Основной движок
- **engine/interfaces/GameConfig.js** - Интерфейс конфигурации игры
- **engine/GameEngineFactory.js** - Фабрика для создания разных игр

### 2. Вынесены игровые данные в конфигурации
- **games/pulpulak/PulpulakGameConfig.js** - Конфигурация игры "Пулпулак"
- **games/detective/DetectiveGameConfig.js** - Пример другой игры

### 3. Создан пример интеграции
- **example_integration.js** - Показывает как использовать новый движок

## Преимущества нового подхода

1. **Переиспользование кода** - один движок для множества игр
2. **Простота добавления новых игр** - только конфигурация, без изменения движка
3. **Лучшая поддерживаемость** - исправления багов применяются ко всем играм
4. **Быстрая разработка** - новая игра = новый конфиг-файл

## Как мигрировать существующий код

### Шаг 1: Замена в server.js

Вместо:
```javascript
const CoopGameLogic = require('./game/coopGameLogic');
const gameLogic = new CoopGameLogic();
```

Используйте:
```javascript
const GameEngineFactory = require('./engine/GameEngineFactory');
const gameEngine = GameEngineFactory.createEngine('pulpulak');
```

### Шаг 2: Замена в socketHandler.js

Вместо:
```javascript
const gameData = gameLogic.startGame(roomId, players);
```

Используйте:
```javascript
const gameData = gameEngine.startGame(roomId, players);
```

### Шаг 3: Обновление обработчиков

Все методы остаются теми же:
- `startGame(roomId, players)`
- `makeChoice(roomId, playerId, choiceId, character)`
- `getGameData(roomId)`
- `removeGame(roomId)`

### Шаг 4: Добавление выбора игры

Добавьте возможность выбирать тип игры:

```javascript
// В главном меню
socket.on('select_game', ({ gameId }) => {
    const availableGames = GameEngineFactory.getAvailableGames();
    socket.emit('games_list', availableGames);
});

socket.on('start_game', ({ gameId, roomId, players }) => {
    const engine = GameEngineFactory.createEngine(gameId);
    // ... остальная логика
});
```

## Как создать новую игру

### 1. Создайте конфигурацию

```javascript
const GameConfigInterface = require('../engine/interfaces/GameConfig');

class MyGameConfig extends GameConfigInterface {
    constructor() {
        super();
        
        this.gameId = 'my_game';
        this.gameName = 'Моя игра';
        
        // Определите персонажей
        this.characters = {
            player1: { name: 'Игрок 1' },
            player2: { name: 'Игрок 2' }
        };
        
        // Определите локации
        this.locations = {
            start: {
                name: 'Начальная локация',
                connections: ['forest'],
                canChangeOutfit: true,
                icon: '🏠',
                npcs: []
            }
        };
        
        // Определите сюжет
        this.scenes = {
            opening: {
                title: 'Начало',
                text: 'Игра начинается...',
                choices: {
                    player1: [{ id: 'start', text: 'Начать' }],
                    player2: [{ id: 'start', text: 'Начать' }]
                }
            }
        };
        
        this.startingScene = 'opening';
        this.initialState = {
            startingLocation: 'start',
            startingOutfits: { player1: 'default', player2: 'default' }
        };
    }
}
```

### 2. Зарегистрируйте игру

```javascript
const GameEngineFactory = require('./engine/GameEngineFactory');
const MyGameConfig = require('./games/mygame/MyGameConfig');

GameEngineFactory.registerGame('my_game', MyGameConfig);
```

### 3. Используйте как обычно

```javascript
const engine = GameEngineFactory.createEngine('my_game');
const gameData = engine.startGame(roomId, players);
```

## Тестирование

Запустите пример:
```bash
node example_integration.js
```

Это покажет:
- Список доступных игр
- Валидацию конфигураций
- Запуск разных игр
- Выполнение действий

## Постепенная миграция

Можно мигрировать постепенно:

1. **Сначала** - добавить новый движок параллельно со старым
2. **Потом** - создать роут для выбора "новая/старая" система
3. **Затем** - протестировать новую систему
4. **Наконец** - полностью перейти на новую систему

## Обратная совместимость

Новый движок спроектирован так, чтобы API оставался тем же. Большая часть существующего кода будет работать без изменений.

## Что дальше?

1. Протестируйте новый движок
2. Создайте простую новую игру для проверки
3. Мигрируйте постепенно
4. Добавьте новые функции в движок по необходимости

Новая архитектура позволит легко создавать множество разных игр на одной основе!