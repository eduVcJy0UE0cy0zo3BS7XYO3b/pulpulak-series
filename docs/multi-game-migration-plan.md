# Подробный план миграции к мультиигровой системе

## Обзор проекта

### Текущее состояние
Система представляет собой кооперативную многопользовательскую текстовую игру с отличной архитектурой engine-game separation. В настоящее время сервер жестко привязан к игре "Пулпулак", но архитектура уже подготовлена для поддержки множественных игр через интерфейс `IGameConfig`.

### Цель миграции
Реализовать возможность выбора игры в главном меню, позволяя пользователям выбирать между доступными играми (Пулпулак, Детектив) без перезапуска сервера.

---

## Анализ текущей архитектуры

### Серверная часть

#### `server.js` (Главный сервер)
- **Текущий подход**: Прямой импорт `PulpulakGameConfig` на строке 6
- **Проблема**: Жесткая привязка к одной игре
- **Изменения потребуются**: Динамическая загрузка игр

```javascript
// Текущий код:
const PulpulakGameConfig = require('./games/pulpulak/PulpulakGameConfig');
const gameConfig = new PulpulakGameConfig();
```

#### `network/socketHandler.js` (Обработка сокетов)
- **Текущий подход**: Принимает одну `gameConfiguration` в конструкторе
- **Проблема**: Нет поддержки переключения между играми
- **Изменения потребуются**: Адаптация под мультиигровой режим

#### `game/coopGameLogic.js` (Игровая логика)
- **Текущий подход**: Работает с одной игровой конфигурацией
- **Состояние**: Хорошо абстрагирован, изменения минимальны

### Клиентская часть

#### `public/js/screens/mainMenu.mjs` (Главное меню)
- **Текущий подход**: Сразу предлагает создать/присоединиться к комнате
- **Изменения потребуются**: Добавить экран выбора игры

#### `public/js/screens/coopGame.mjs` (Игровой интерфейс)
- **Проблема**: Содержит хардкод специфичный для Пулпулак
- **Изменения потребуются**: Динамическая адаптация под выбранную игру

### Игровые данные

#### Структура `/games/`
- **Пулпулак**: `games/pulpulak/PulpulakGameConfig.js` - полностью реализован
- **Детектив**: `games/detective/DetectiveGameConfig.js` - готов к использованию
- **Архитектура**: Отличное разделение на модули (storyData, locationData, npcData, questData)

---

## Детальный план реализации

### Этап 1: Создание системы управления играми

#### 1.1 Создать `GameRegistry` (Новый файл: `game/GameRegistry.js`)

**Назначение**: Централизованное управление доступными играми

**Функциональность**:
```javascript
class GameRegistry {
    constructor() {
        this.games = new Map(); // gameId -> GameConfig instance
        this.gameMetadata = new Map(); // gameId -> metadata
    }

    // Автоматическое сканирование папки /games/
    async scanGames() {
        // Поиск всех GameConfig.js файлов
        // Загрузка метаданных каждой игры
        // Регистрация доступных игр
    }

    // Получение конфигурации игры
    getGameConfig(gameId) {
        // Ленивая загрузка при первом обращении
    }

    // Получение списка доступных игр
    getAvailableGames() {
        // Возврат метаданных для UI
    }
}
```

**Метаданные игр**:
```javascript
// Каждая игра должна экспортировать метаданные
{
    id: 'pulpulak',
    name: 'Княжна Пулпулак',
    description: 'Кооперативная приключенческая игра...',
    minPlayers: 2,
    maxPlayers: 2,
    estimatedDuration: '60-90 минут',
    thumbnail: '/assets/games/pulpulak/thumbnail.jpg',
    tags: ['cooperative', 'story', 'medieval']
}
```

#### 1.2 Модификация `server.js`

**Изменения**:
1. Удалить прямой импорт `PulpulakGameConfig`
2. Создать экземпляр `GameRegistry`
3. Передать registry в `SocketHandler`
4. Добавить API endpoints для работы с играми

**Новые endpoints**:
```javascript
// GET /api/games - список доступных игр
app.get('/api/games', (req, res) => {
    res.json(gameRegistry.getAvailableGames());
});

// GET /api/games/:gameId/config - конфигурация конкретной игры
app.get('/api/games/:gameId/config', (req, res) => {
    const config = gameRegistry.getGameConfig(req.params.gameId);
    res.json(config ? config.getClientData() : null);
});
```

### Этап 2: Модификация обработки сокетов

#### 2.1 Обновление `network/socketHandler.js`

**Ключевые изменения**:

1. **Конструктор**:
```javascript
constructor(io, gameRegistry) {
    this.io = io;
    this.gameRegistry = gameRegistry;
    this.gameLogics = new Map(); // gameId -> CoopGameLogic instance
    this.rooms = new Map(); // roomCode -> { gameId, players, gameState }
}
```

2. **Создание комнаты**:
```javascript
handleCreateRoom(socket, { gameId, ...roomData }) {
    // Валидация gameId
    // Создание комнаты с привязкой к игре
    // Инициализация игровой логики для конкретной игры
}
```

3. **Присоединение к комнате**:
```javascript
handleJoinRoom(socket, { roomCode }) {
    // Определение игры по коду комнаты
    // Загрузка соответствующей игровой логики
    // Передача клиенту информации об игре
}
```

#### 2.2 Адаптация игровых сессий

**Структура комнаты**:
```javascript
{
    roomCode: 'ABC123',
    gameId: 'pulpulak',
    players: [
        { id: 'socket1', role: 'princess', outfit: 'princess' },
        { id: 'socket2', role: 'helper', outfit: 'helper' }
    ],
    gameState: {
        currentScene: 'prologue',
        loyaltyScore: 0,
        // ... остальное состояние игры
    },
    createdAt: Date.now(),
    lastActivity: Date.now()
}
```

### Этап 3: Frontend - новый экран выбора игры

#### 3.1 Создание `public/js/screens/gameSelection.mjs`

**Функциональность**:
- Загрузка списка игр с сервера
- Отображение карточек игр с метаданными
- Выбор игры и переход к созданию/присоединению к комнате
- Поддержка фильтрации по тегам

**Структура компонента**:
```javascript
class GameSelectionScreen {
    async init() {
        // Загрузка списка игр
        // Рендеринг интерфейса
    }

    async loadGames() {
        // GET /api/games
    }

    renderGameCard(game) {
        // Карточка игры с превью, описанием, тегами
    }

    handleGameSelect(gameId) {
        // Сохранение выбранной игры в sessionStorage
        // Переход к лобби
    }
}
```

#### 3.2 Модификация `public/js/screens/mainMenu.mjs`

**Изменения навигации**:
```
Главное меню
├── Новая игра → Выбор игры → Лобби
├── Присоединиться → Ввод кода комнаты
├── Правила
└── Настройки
```

#### 3.3 Обновление `public/js/screens/lobby.mjs`

**Адаптация под выбранную игру**:
- Отображение названия и описания выбранной игры
- Динамическая загрузка игро-специфичных элементов UI
- Валидация совместимости версий

### Этап 4: Динамическая адаптация игрового интерфейса

#### 4.1 Модификация `public/js/screens/coopGame.mjs`

**Проблемы для решения**:
- Удалить хардкод имен персонажей (princess/helper)
- Сделать динамическую загрузку игровых констант
- Адаптировать UI под метаданные игры

**Решения**:
```javascript
class CoopGameScreen {
    async init(gameId) {
        // Загрузка конфигурации игры
        this.gameConfig = await this.loadGameConfig(gameId);
        
        // Динамическая настройка UI
        this.setupGameSpecificUI();
    }

    async loadGameConfig(gameId) {
        // GET /api/games/{gameId}/config
    }

    setupGameSpecificUI() {
        // Настройка цветовой схемы
        // Замена текстовых констант
        // Адаптация элементов интерфейса
    }
}
```

#### 4.2 Создание `public/js/gameConfigManager.js`

**Назначение**: Централизованное управление игровыми конфигурациями на клиенте

```javascript
class GameConfigManager {
    constructor() {
        this.configs = new Map();
        this.currentGameId = null;
    }

    async loadGameConfig(gameId) {
        // Кэширование конфигураций
        // Ленивая загрузка
    }

    getCurrentConfig() {
        return this.configs.get(this.currentGameId);
    }

    getGameConstants(gameId) {
        // Возврат констант для конкретной игры
    }
}
```

### Этап 5: Обновление игровых конфигураций

#### 5.1 Стандартизация метаданных

**Требуется добавить в каждый GameConfig**:
```javascript
// games/pulpulak/PulpulakGameConfig.js
class PulpulakGameConfig {
    static getMetadata() {
        return {
            id: 'pulpulak',
            name: 'Княжна Пулпулак',
            description: 'Кооперативная средневековая приключенческая игра',
            minPlayers: 2,
            maxPlayers: 2,
            estimatedDuration: '60-90 минут',
            thumbnail: '/assets/games/pulpulak/thumbnail.jpg',
            roles: [
                { id: 'princess', name: 'Княжна', description: '...' },
                { id: 'helper', name: 'Помощник', description: '...' }
            ],
            features: ['outfit-system', 'loyalty-tracking', 'cooperative-choices'],
            tags: ['cooperative', 'story', 'medieval', 'role-playing']
        };
    }

    getClientData() {
        // Данные безопасные для передачи клиенту
        return {
            metadata: this.constructor.getMetadata(),
            uiConfig: {
                theme: 'medieval',
                primaryColor: '#8B4513',
                // ... другие настройки UI
            }
        };
    }
}
```

#### 5.2 Обновление детективной игры

**Добавить метаданные в `games/detective/DetectiveGameConfig.js`**:
```javascript
static getMetadata() {
    return {
        id: 'detective',
        name: 'Детективное дело',
        description: 'Загадочное расследование в викторианском Лондоне',
        minPlayers: 2,
        maxPlayers: 2,
        estimatedDuration: '45-75 минут',
        thumbnail: '/assets/games/detective/thumbnail.jpg',
        roles: [
            { id: 'detective', name: 'Детектив', description: '...' },
            { id: 'assistant', name: 'Помощник', description: '...' }
        ],
        features: ['investigation', 'evidence-collection', 'deduction'],
        tags: ['mystery', 'investigation', 'victorian', 'cooperative']
    };
}
```

### Этап 6: Система кэширования и оптимизация

#### 6.1 Ленивая загрузка игр

**Оптимизация памяти сервера**:
- Игры загружаются только при первом обращении
- Автоматическая выгрузка неиспользуемых игр
- Конфигурируемые лимиты памяти

**Реализация в GameRegistry**:
```javascript
async getGameConfig(gameId) {
    if (!this.games.has(gameId)) {
        await this.loadGame(gameId);
    }
    
    this.updateLastAccess(gameId);
    return this.games.get(gameId);
}

async loadGame(gameId) {
    // Динамический импорт
    const GameConfigClass = require(`../games/${gameId}/${this.getConfigFileName(gameId)}`);
    const instance = new GameConfigClass();
    
    this.games.set(gameId, instance);
    this.lastAccess.set(gameId, Date.now());
}
```

#### 6.2 Клиентское кэширование

**Стратегия кэширования**:
- Метаданные игр кэшируются в localStorage
- Версионирование для обновлений
- Префетчинг популярных игр

### Этап 7: Миграция существующих функций

#### 7.1 Обработка legacy URL

**Поддержка прямых ссылок**:
```javascript
// Роутинг для обратной совместимости
if (window.location.pathname === '/pulpulak') {
    // Автоматический выбор игры Пулпулак
    gameConfigManager.setCurrentGame('pulpulak');
    navigateToLobby();
}
```

#### 7.2 Миграция состояния комнат

**Обновление существующих комнат**:
- Автоматическое присвоение gameId = 'pulpulak' существующим комнатам
- Graceful degradation для старых клиентов

### Этап 8: Комплексная тестовая стратегия

#### 8.1 Обязательные тесты для каждого компонента

**GameRegistry (`game/__tests__/GameRegistry.test.js`)**:
```javascript
describe('GameRegistry', () => {
    describe('Game scanning', () => {
        test('should automatically discover games in /games/ directory', async () => {
            // Тестирование автосканирования
        });
        
        test('should handle missing game directories gracefully', async () => {
            // Тестирование устойчивости к ошибкам
        });
        
        test('should validate game config interfaces', async () => {
            // Проверка соответствия IGameConfig
        });
    });
    
    describe('Lazy loading', () => {
        test('should load games only when requested', async () => {
            // Тестирование ленивой загрузки
        });
        
        test('should cache loaded games', async () => {
            // Тестирование кэширования
        });
        
        test('should handle loading failures', async () => {
            // Обработка ошибок загрузки
        });
    });
    
    describe('Memory management', () => {
        test('should unload unused games after timeout', async () => {
            // Тестирование автовыгрузки
        });
        
        test('should respect memory limits', async () => {
            // Тестирование лимитов памяти
        });
    });
});
```

**Server API (`__tests__/server.test.js`)**:
```javascript
describe('Multi-game API', () => {
    describe('GET /api/games', () => {
        test('should return list of available games', async () => {
            // Тестирование списка игр
        });
        
        test('should handle empty games directory', async () => {
            // Тестирование edge case
        });
        
        test('should return proper game metadata', async () => {
            // Валидация метаданных
        });
    });
    
    describe('GET /api/games/:gameId/config', () => {
        test('should return game configuration', async () => {
            // Тестирование получения конфигурации
        });
        
        test('should return 404 for non-existent games', async () => {
            // Тестирование ошибок
        });
        
        test('should not expose sensitive data', async () => {
            // Тестирование безопасности
        });
    });
});
```

**SocketHandler (`network/__tests__/socketHandler.test.js`)**:
```javascript
describe('Multi-game Socket Handler', () => {
    describe('Room creation', () => {
        test('should create rooms with gameId', async () => {
            // Тестирование создания комнат
        });
        
        test('should validate gameId before room creation', async () => {
            // Валидация существования игры
        });
        
        test('should isolate rooms by game type', async () => {
            // Тестирование изоляции
        });
    });
    
    describe('Game session management', () => {
        test('should load correct game logic for room', async () => {
            // Тестирование загрузки логики
        });
        
        test('should handle simultaneous sessions of different games', async () => {
            // Множественные сессии
        });
        
        test('should prevent cross-game state interference', async () => {
            // Изоляция состояний
        });
    });
});
```

#### 8.2 Регрессионное тестирование после каждой фазы

**Обязательные регрессионные проверки**:

1. **После каждой серверной модификации**:
   ```bash
   # Запуск всех существующих тестов
   npm test
   
   # Проверка работы игры Пулпулак
   npm run test:integration:pulpulak
   
   # Проверка работы игры Детектив (если активна)
   npm run test:integration:detective
   ```

2. **Специфичные регрессионные тесты**:
   ```javascript
   // game/__tests__/regression.test.js
   describe('Regression tests', () => {
       test('existing Pulpulak rooms should continue working', async () => {
           // Проверка работы существующих комнат
       });
       
       test('all original game flows should remain functional', async () => {
           // Полный цикл оригинальной игры
       });
       
       test('websocket events should maintain backwards compatibility', async () => {
           // Совместимость WebSocket API
       });
   });
   ```

#### 8.3 Интеграционные тесты

**Критичные интеграционные сценарии**:
```javascript
// __tests__/integration/multiGame.test.js
describe('Multi-game integration', () => {
    test('should handle Pulpulak and Detective sessions simultaneously', async () => {
        // Создание двух комнат разных игр
        // Проверка изоляции и корректной работы
    });
    
    test('should maintain game state isolation', async () => {
        // Тестирование отсутствия пересечений состояний
    });
    
    test('should handle server restart with active sessions', async () => {
        // Тестирование восстановления после перезапуска
    });
});
```

#### 8.4 E2E тестирование с Playwright/Cypress

**Пользовательские сценарии**:
```javascript
// e2e/multiGame.spec.js
describe('Multi-game user experience', () => {
    test('complete game selection and play flow', async ({ page }) => {
        // 1. Открыть главное меню
        // 2. Выбрать игру
        // 3. Создать комнату
        // 4. Пригласить второго игрока
        // 5. Играть до завершения
    });
    
    test('switching between games in same session', async ({ page }) => {
        // Тестирование переключения между играми
    });
    
    test('joining room by code for different games', async ({ page }) => {
        // Присоединение по коду к разным типам игр
    });
});
```

#### 8.5 Тестовое покрытие и метрики качества

**Минимальные требования к покрытию**:
- **Модульные тесты**: 90%+ для новых компонентов
- **Интеграционные тесты**: 80%+ для API endpoints
- **E2E тесты**: 100% критичных пользовательских flow

**Автоматизированные проверки**:
```json
// package.json scripts
{
  "test:coverage": "jest --coverage --coverageThreshold='{\"global\":{\"branches\":80,\"functions\":90,\"lines\":90,\"statements\":90}}'",
  "test:regression": "jest --testPathPattern=regression",
  "test:integration": "jest --testPathPattern=integration",
  "test:e2e": "playwright test",
  "test:all": "npm run test:coverage && npm run test:integration && npm run test:e2e"
}
```

#### 8.6 Непрерывная интеграция и проверки

**GitHub Actions workflow**:
```yaml
# .github/workflows/multi-game-ci.yml
name: Multi-game CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
      
      # Модульные тесты
      - name: Run unit tests
        run: npm run test:coverage
      
      # Регрессионные тесты
      - name: Run regression tests
        run: npm run test:regression
      
      # Интеграционные тесты
      - name: Run integration tests
        run: npm run test:integration
      
      # E2E тесты
      - name: Run E2E tests
        run: npm run test:e2e
      
      # Проверка отсутствия регрессий
      - name: Verify backward compatibility
        run: npm run test:backward-compatibility
```

---

## Техническая реализация

### Последовательность внедрения (TDD подход)

#### Фаза 1: GameRegistry с полным покрытием тестами (2-3 дня)

**День 1: Создание тестовой инфраструктуры**
1. Создать `game/__tests__/GameRegistry.test.js` с полным набором тестов
2. Создать mock-игры для тестирования
3. Настроить тестовую среду для изолированного тестирования

**День 2: Реализация GameRegistry**
1. Создать `GameRegistry` следуя TDD (сначала тесты, потом реализация)
2. Реализовать автосканирование игр с тестовым покрытием
3. Добавить ленивую загрузку с тестами производительности
4. **ОБЯЗАТЕЛЬНО**: Регрессионное тестирование - убедиться, что существующие игры работают

**День 3: Интеграционные тесты**
1. Создать интеграционные тесты для GameRegistry
2. Тестирование работы с файловой системой
3. Тестирование обработки ошибок и edge cases

#### Фаза 2: Модификация server.js с тестовым покрытием (2-3 дня)

**День 1: Подготовка тестовой среды**
1. Создать `__tests__/server.test.js` для тестирования API endpoints
2. Настроить supertest для HTTP тестирования
3. Создать тестовые fixtures для различных игровых конфигураций

**День 2: Реализация API endpoints**
1. TDD для `/api/games` endpoint
2. TDD для `/api/games/:gameId/config` endpoint
3. Тестирование обработки ошибок (несуществующие игры, поврежденные конфигурации)
4. **ОБЯЗАТЕЛЬНО**: Регрессионное тестирование - убедиться, что основной сайт работает

**День 3: Интеграция с GameRegistry**
1. Модифицировать server.js для использования GameRegistry
2. Интеграционные тесты server + GameRegistry
3. Тестирование производительности при множественных запросах

#### Фаза 3: Обновление SocketHandler с тестовым покрытием (3-4 дня)

**День 1: Тестовая инфраструктура для сокетов**
1. Создать `network/__tests__/socketHandler.test.js`
2. Настроить socket.io-client для тестирования
3. Создать helper'ы для эмуляции игровых сессий

**День 2: TDD для мультиигровой поддержки**
1. Написать тесты для создания комнат с gameId
2. Написать тесты для присоединения к игро-специфичным комнатам
3. Реализовать функциональность следуя тестам

**День 3: Расширенные сценарии**
1. TDD для одновременных сессий разных игр
2. Тестирование изоляции игровых состояний
3. Тестирование обработки ошибок при недоступных играх

**День 4: Интеграция и регрессия**
1. Интеграционные тесты всей серверной части
2. **КРИТИЧНО**: Полное регрессионное тестирование - все существующие игровые сценарии должны работать
3. Нагрузочное тестирование с множественными играми

#### Фаза 4: Frontend - экран выбора игры с тестами (2-3 дня)

**День 1: Тестовая среда для frontend**
1. Настроить Jest для frontend тестирования
2. Создать `public/js/__tests__/gameSelection.test.js`
3. Настроить моки для API вызовов

**День 2: TDD для GameSelectionScreen**
1. Написать тесты для загрузки списка игр
2. Написать тесты для рендеринга карточек игр
3. Реализовать компонент следуя тестам

**День 3: Интеграция с навигацией**
1. TDD для обновления mainMenu.mjs
2. Тестирование навигации между экранами
3. **ОБЯЗАТЕЛЬНО**: E2E тестирование полного пользовательского flow

#### Фаза 5: Адаптация игрового интерфейса с тестами (3-4 дня)

**День 1: Тестирование динамической адаптации**
1. Создать `public/js/__tests__/gameConfigManager.test.js`
2. Написать тесты для загрузки игровых конфигураций
3. Тестирование кэширования конфигураций

**День 2-3: TDD для адаптации coopGame.mjs**
1. Написать тесты для удаления хардкода
2. Тестирование динамической настройки UI
3. Реализация следуя тестам

**День 4: Полная интеграция**
1. E2E тестирование полного цикла игры
2. **КРИТИЧНО**: Регрессионное тестирование - обе игры (Пулпулак и Детектив) должны полностью работать
3. Тестирование переключения между играми в рамках одной сессии

### Миграционные риски и митигация

#### Высокие риски
1. **Поломка существующей функциональности**
   - *Митигация*: Поэтапное внедрение с флагами
   - *Тестирование*: Полный регресс-тест после каждого этапа

2. **Производительность при множественных играх**
   - *Митигация*: Ленивая загрузка и умное кэширование
   - *Мониторинг*: Метрики использования памяти

#### Средние риски
1. **Сложность синхронизации состояния**
   - *Митигация*: Четкое разделение игрового состояния по комнатам
   - *Тестирование*: Stress-тестирование с множественными сессиями

2. **UI/UX консистентность между играми**
   - *Митигация*: Создание единой дизайн-системы
   - *Валидация*: UX-тестирование с реальными пользователями

### Метрики успеха и критерии приемки

#### Обязательные технические метрики
- **Тестовое покрытие**: 90%+ для новых компонентов, 80%+ для модифицированных
- **Время запуска новой игровой сессии**: < 3 секунд
- **Использование памяти**: не более +30% от текущего базового уровня
- **Регрессионные тесты**: 100% прохождение для всех существующих функций
- **Изоляция игр**: 0 случаев перекрестного влияния между играми

#### Критерии приемки каждой фазы

**Фаза 1 - GameRegistry**:
- ✅ Все тесты GameRegistry проходят
- ✅ Регрессионные тесты показывают отсутствие влияния на существующую функциональность
- ✅ Производительность сканирования игр < 100мс
- ✅ Память: ленивая загрузка не увеличивает базовое потребление

**Фаза 2 - Server API**:
- ✅ API endpoints работают корректно
- ✅ Все существующие роуты продолжают работать
- ✅ Безопасность: нет утечек конфиденциальных данных
- ✅ Производительность API < 200мс response time

**Фаза 3 - SocketHandler**:
- ✅ Множественные игровые сессии работают изолированно
- ✅ Существующие WebSocket события остаются совместимыми
- ✅ Нет памяти утечек при множественных подключениях
- ✅ Все игровые сценарии Пулпулак работают без изменений

**Фаза 4 - Frontend**:
- ✅ UI выбора игры интуитивно понятен
- ✅ Навигация работает без ошибок
- ✅ Существующие закладки/URL продолжают работать
- ✅ Производительность загрузки игровых данных < 1сек

**Фаза 5 - Динамическая адаптация**:
- ✅ Обе игры (Пулпулак и Детектив) полностью функциональны
- ✅ Переключение между играми работает без перезагрузки
- ✅ UI корректно адаптируется под каждую игру
- ✅ Отсутствие визуальных артефактов при смене игр

#### Процедура завершения каждой фазы

**Обязательная последовательность действий:**

1. **Завершение разработки**
   - ✅ Все автоматические тесты проходят
   - ✅ Покрытие кода соответствует требованиям
   - ✅ Регрессионные тесты пройдены

2. **Ручное тестирование**
   - 🛑 **СТОП** - уведомить о готовности фазы
   - 👤 **Ожидание ручного тестирования** - пользователь тестирует приложение
   - ✅ **Получение разрешения** - пользователь подтверждает готовность

3. **Коммит изменений**
   - 📝 Создание подробного коммита с описанием изменений
   - 🔄 Пуш в репозиторий
   - ✅ **Получение разрешения на следующую фазу**

4. **Переход к следующей фазе**
   - 🚀 Начало работы над следующей фазой

#### Блокирующие критерии (НЕ переходим к ручному тестированию если есть)
- 🚫 Любые падения существующих тестов
- 🚫 Регрессии в функциональности Пулпулак
- 🚫 Увеличение времени загрузки > 50%
- 🚫 Покрытие новых компонентов тестами < 90%
- 🚫 Любые утечки памяти

#### Шаблон уведомления о готовности фазы

```
🎯 ФАЗА [X] ЗАВЕРШЕНА: [Название фазы]

✅ Автоматические тесты: ПРОЙДЕНЫ
✅ Регрессионные тесты: ПРОЙДЕНЫ  
✅ Покрытие кода: [X]% (требование выполнено)
✅ Производительность: в норме

🧪 ГОТОВ К РУЧНОМУ ТЕСТИРОВАНИЮ

Что протестировать:
- [Специфичные сценарии для данной фазы]
- [Основные пользовательские flow]
- [Регрессионные проверки]

Команды для запуска:
- npm start
- [дополнительные команды если нужны]

Ожидаю вашего разрешения для коммита и перехода к следующей фазе.
```

---

## Долгосрочная перспектива

### Масштабируемость
- Поддержка пользовательских игр (модовая система)
- API для внешних разработчиков
- Система рейтингов и рекомендаций игр

### Дополнительные функции
- Сохранение прогресса игр
- Система достижений
- Турнирный режим
- Асинхронные игры

### Мониторинг и аналитика
- Популярность игр
- Время сессий по играм
- Точки отказа пользователей
- A/B тестирование новых игр

---

## Заключение

Данный план обеспечивает поэтапную миграцию к мультиигровой системе с минимальными рисками для существующей функциональности. Архитектура уже подготовлена для таких изменений, что значительно упрощает реализацию.

Ключевой принцип миграции - сохранение обратной совместимости и постепенное внедрение новых возможностей. Это позволит пользователям продолжать играть в привычном режиме, постепенно знакомясь с новыми возможностями выбора игр.