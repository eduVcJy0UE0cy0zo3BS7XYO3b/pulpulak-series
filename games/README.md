# 🎮 Каталог игр

В этой папке хранятся все игры, работающие на универсальном движке.

## Доступные игры

### 🏰 Княжна Пулпулак (`pulpulak`)
**Кооперативное средневековое приключение**
- 👥 2 игрока: княжна и помощница
- 🎭 Система смены нарядов
- 💬 Сложные диалоги с NPC
- 🗺️ Исследование замка
- ⚔️ Квестовая система

[Подробнее →](./pulpulak/README.md)

### 🔍 Детектив (`detective`)
**Современное расследование**
- 👥 2 игрока: детектив и журналист
- 🕵️ Расследование кражи
- 💼 Профессиональный vs неформальный подход
- 🏙️ Современная городская обстановка
- 🧩 Сбор улик и опрос свидетелей

[Подробнее →](./detective/README.md)

## Структура игры

Каждая игра должна иметь следующую структуру:

```
games/gameid/
├── README.md                # Описание игры
├── GameConfig.js           # Основная конфигурация
├── data/                   # Игровые данные
│   ├── scenes.js          # Сцены и сюжет
│   ├── locations.js       # Локации
│   ├── npcs.js           # Персонажи
│   ├── quests.js         # Квесты (опционально)
│   └── constants.js      # Константы
└── config/                # Настройки игры
    └── settings.js       # Конфигурационные файлы
```

## Создание новой игры

### 1. Создайте структуру папок
```bash
mkdir -p games/mygame/data games/mygame/config
```

### 2. Скопируйте шаблон конфигурации
```bash
cp games/_template/GameConfig.js games/mygame/MyGameConfig.js
```

### 3. Отредактируйте конфигурацию
```javascript
class MyGameConfig extends GameConfigInterface {
    constructor() {
        super();
        this.gameId = 'mygame';
        this.gameName = 'Моя игра';
        // ...
    }
}
```

### 4. Зарегистрируйте в фабрике
```javascript
// В engine/GameEngineFactory.js
this.registerGame('mygame', MyGameConfig);
```

### 5. Запустите и тестируйте
```bash
node -e "
const factory = require('../engine/GameEngineFactory');
const engine = factory.createEngine('mygame');
console.log('Игра создана успешно!');
"
```

## Особенности системы

### 🔧 Универсальный движок
- Один движок для всех игр
- Стандартизированный API
- Автоматическое управление состоянием

### 📦 Полная изоляция
- Каждая игра самодостаточна
- Независимые данные и настройки
- Легко переносить и обновлять

### 🔄 Обратная совместимость
- Старый API полностью поддерживается
- Постепенная миграция
- Без изменений клиентского кода

### 🎯 Модульность
- Добавляйте функции по необходимости
- Переиспользуйте компоненты
- Быстрая разработка

## Требования к игре

### Обязательные элементы
- **GameConfig.js** - основная конфигурация
- **characters** - определение персонажей
- **scenes** - хотя бы одна сцена
- **locations** - хотя бы одна локация
- **startingScene** - начальная сцена

### Опциональные элементы
- **npcs** - неигровые персонажи
- **quests** - система квестов
- **outfits** - система одежды/маскировки
- **dialogue** - сложная система диалогов

## Примеры использования

### Простая игра
```javascript
this.scenes = {
    "start": {
        title: "Начало",
        text: "Добро пожаловать!",
        choices: {
            player1: [{ id: "continue", text: "Продолжить" }],
            player2: [{ id: "continue", text: "Продолжить" }]
        }
    }
};
```

### Игра с NPC
```javascript
this.npcs = {
    "guard": {
        name: "Стражник",
        dialogue: {
            initial: {
                greeting: "Кто идёт?",
                choices: [/* варианты */]
            }
        }
    }
};
```

### Игра с квестами
```javascript
this.quests = {
    "main_quest": {
        title: "Главный квест",
        steps: [
            { description: "Найти ключ" },
            { description: "Открыть дверь" }
        ]
    }
};
```

## Тестирование игр

### Проверка конфигурации
```bash
node -e "
const Config = require('./games/mygame/MyGameConfig');
const config = new Config();
const validation = config.validate();
console.log(validation.valid ? '✅ OK' : '❌ Errors:', validation.errors);
"
```

### Запуск игры
```bash
node -e "
const factory = require('../engine/GameEngineFactory');
const engine = factory.createEngine('mygame');
const gameData = engine.startGame('test', {
    player1: { id: '1', name: 'Игрок 1' },
    player2: { id: '2', name: 'Игрок 2' }
});
console.log('Игра запущена:', gameData.scene.title);
"
```

## Публикация игры

### 1. Убедитесь в готовности
- ✅ Конфигурация валидна
- ✅ Игра запускается
- ✅ Все файлы на месте
- ✅ README.md написан

### 2. Добавьте в каталог
Отредактируйте этот файл, добавив описание своей игры.

### 3. Зарегистрируйте в фабрике
```javascript
// В GameEngineFactory.js
this.registerGame('mygame', MyGameConfig);
```

### 4. Протестируйте
```bash
npm test
node test_migration.js
```

---

🎮 **Создавайте удивительные игры на универсальном движке!**