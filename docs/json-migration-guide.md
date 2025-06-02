# Гайд по миграции данных игры Pulpulak в JSON формат

## Введение

Этот гайд содержит пошаговые инструкции по приведению данных игры к JSON формату с тестированием на каждом этапе. Миграция позволяет улучшить читаемость, валидацию и интеграцию данных игры.

## Структура миграции

### 📊 Этап 1: Анализ текущей структуры данных ✅

**Проанализированы следующие типы данных:**

1. **Истории (coopStoryData.js)** - сцены с выборами для игроков
2. **Локации (locationData.js)** - места в игре с их свойствами  
3. **NPC (npcData.js)** - персонажи с системой диалогов
4. **Квесты (questData.js)** - задания для персонажей
5. **Логика одежды (outfitLogic.js)** - правила переодевания

**Итого обнаружено:**
- 3 истории (сцены)
- 17 локаций
- 8 NPC
- 2 квеста

### 📐 Этап 2: Создание JSON схем ✅

Созданы JSON Schema для валидации данных:

- `games/pulpulak/schemas/story.schema.json` - схема для историй
- `games/pulpulak/schemas/location.schema.json` - схема для локаций  
- `games/pulpulak/schemas/npc.schema.json` - схема для NPC
- `games/pulpulak/schemas/quest.schema.json` - схема для квестов

### 🔄 Этап 3: Создание конвертеров JS → JSON ✅

**Инструмент:** `games/pulpulak/tools/dataConverter.js`

**Команды для конвертации:**
```bash
# Конвертация всех данных
node games/pulpulak/tools/dataConverter.js

# Результат:
# - games/pulpulak/data-json/storyData.json
# - games/pulpulak/data-json/locationData.json
# - games/pulpulak/data-json/npcData.json
# - games/pulpulak/data-json/questData.json
# - games/pulpulak/conversion-report.json
```

**Результат конвертации:**
```
📈 Сводка:
   - Историй: 3
   - Локаций: 17
   - NPC: 8
   - Квестов: 2
```

### 🔧 Этап 4: Создание загрузчиков JSON данных ✅

Созданы асинхронные загрузчики с валидацией:

- `JsonStoryLoader` - загрузка историй из JSON
- `JsonLocationLoader` - загрузка локаций из JSON
- `JsonNPCLoader` - загрузка NPC из JSON
- `JsonQuestLoader` - загрузка квестов из JSON

**Особенности:**
- Асинхронная загрузка данных
- Встроенная валидация
- Обратная совместимость с синхронными методами
- Кэширование загруженных данных

### ✅ Этап 5: Создание валидаторов JSON данных ✅

**Инструмент:** `games/pulpulak/tools/dataValidator.js`

**Команды для валидации:**
```bash
# Быстрая проверка целостности
node games/pulpulak/tools/dataValidator.js quick

# Полная валидация
node games/pulpulak/tools/dataValidator.js
```

**Результат валидации:**
```
📊 Результаты валидации:
✅ stories: 3 элементов, 0 ошибок
✅ locations: 17 элементов, 0 ошибок  
✅ npcs: 8 элементов, 0 ошибок
✅ quests: 2 элементов, 0 ошибок
✅ cross-references: 0 элементов, 0 ошибок

📈 Общий статус: ✅ ВСЕ ДАННЫЕ ВАЛИДНЫ
📊 Всего элементов: 30, ошибок: 0
```

### 🧪 Этап 6: Тестирование на каждом этапе ✅

**Команда тестирования:**
```bash
npm test
```

**Результат:** ✅ Все основные тесты игры проходят успешно

## Структура файлов после миграции

```
games/pulpulak/
├── data/                    # Оригинальные JS файлы
│   ├── coopStoryData.js
│   ├── locationData.js
│   ├── npcData.js
│   ├── questData.js
│   └── outfitLogic.js
├── data-json/              # Сконвертированные JSON файлы
│   ├── storyData.json
│   ├── locationData.json
│   ├── npcData.json
│   └── questData.json
├── schemas/                # JSON Schema для валидации
│   ├── story.schema.json
│   ├── location.schema.json
│   ├── npc.schema.json
│   └── quest.schema.json
├── data-loaders/          # Загрузчики JSON данных
│   ├── JsonStoryLoader.js
│   ├── JsonLocationLoader.js
│   ├── JsonNPCLoader.js
│   └── JsonQuestLoader.js
├── tools/                 # Утилиты для работы с данными
│   ├── dataConverter.js
│   └── dataValidator.js
├── conversion-report.json  # Отчет о конвертации
└── validation-report.json  # Отчет о валидации
```

## Команды для работы с миграцией

### Базовые команды

```bash
# 1. Конвертация JS в JSON
node games/pulpulak/tools/dataConverter.js

# 2. Быстрая проверка целостности
node games/pulpulak/tools/dataValidator.js quick

# 3. Полная валидация данных
node games/pulpulak/tools/dataValidator.js

# 4. Запуск тестов
npm test
```

### Пример использования загрузчиков

```javascript
// Асинхронная загрузка данных
const JsonStoryLoader = require('./games/pulpulak/data-loaders/JsonStoryLoader');

const loader = new JsonStoryLoader();
const scene = await loader.getScene('coop_awakening');
const allScenes = await loader.getAllScenes();

// Валидация данных
const validation = await loader.validateAllData();
if (!validation.valid) {
    console.error('Ошибки валидации:', validation.errors);
}
```

## Преимущества JSON формата

1. **📖 Читаемость:** JSON более удобен для чтения и редактирования
2. **🔍 Валидация:** Автоматическая проверка структуры через JSON Schema
3. **🔧 Интеграция:** Легкая интеграция с внешними инструментами
4. **📊 Анализ:** Удобный анализ данных игры
5. **🚀 Производительность:** Быстрая загрузка и парсинг
6. **🛡️ Безопасность:** Отсутствие выполнения кода при загрузке

## Следующие шаги

1. **Обновить игровой движок** для использования JSON загрузчиков
2. **Создать инструменты редактирования** данных в JSON формате
3. **Настроить CI/CD валидацию** для автоматической проверки данных
4. **Добавить поддержку локализации** через JSON структуры
5. **Создать визуальные редакторы** для дизайнеров игры

## Отчеты и логи

- **Отчет о конвертации:** `games/pulpulak/conversion-report.json`
- **Отчет о валидации:** `games/pulpulak/validation-report.json`

## Заключение

Миграция данных игры Pulpulak в JSON формат завершена успешно! Все данные валидны, тесты проходят, структура данных улучшена. Система готова к дальнейшему развитию и масштабированию.

---

🎮 **Pulpulak Game Data Migration** - от JS к JSON с любовью ❤️