# Тестирование Pulpulak Game

## Обзор тестовой системы

Проект использует функциональную систему тестирования для Scheme архитектуры:

1. **Jest** для JavaScript интеграции и CI/CD
2. **Simple Scheme Test Runner** для функциональных тестов игровой логики (JavaScript симуляция)

## Запуск тестов

### Основные команды

```bash
# Запуск всех тестов (Jest + Scheme симуляция)
npm test

# Только Scheme тесты
npm run test:scheme

# Быстрые тесты (для разработки)
npm run test:scheme:quick

# Jest в режиме наблюдения
npm run test:watch
```

### Прямой запуск Scheme тестов

```bash
# Через упрощенный runner
node game/simple-test-runner.js
node game/simple-test-runner.js quick
```

## Структура тестов

### Simple Scheme Test Runner (`game/simple-test-runner.js`)

JavaScript симуляция функциональной Scheme архитектуры включает:

- **Утверждения**: `assertEqual`, `assertTrue`, `assertFalse`, `assertNotNull`
- **Игровые функции**: симуляция всех основных функций Scheme игры
- **Тестовые категории**: основные, перемещение, наряды, действия, интеграция
- **Отчетность**: подробная статистика выполнения

### Тестовые файлы

```
game/
├── simple-test-runner.js           # Основной тестовый раннер
├── __tests__/scheme-tests.test.js  # Jest интеграция
├── test-framework.scm              # Оригинальный Scheme фреймворк
└── tests/                          # Оригинальные Scheme тесты
    ├── core-functions.test.scm
    ├── game-logic.test.scm
    ├── integration.test.scm
    └── run-tests.scm
```

### Jest интеграция

- `game/__tests__/scheme-tests.test.js` - Jest обертка для тестов
- `game/simple-test-runner.js` - Упрощенный JavaScript раннер

## Типы тестов

### 1. Core Functions Tests

- Создание игрового состояния
- Управление игроками
- Базовые действия персонажей
- Система нарядов
- Неизменяемость состояния

### 2. Game Logic Tests

- Диалоги с NPC
- Система квестов
- Взаимодействие персонажей
- Управление ходами
- Валидация действий

### 3. Integration Tests

- Полные игровые сценарии
- Многошаговые взаимодействия
- Граничные случаи
- Тесты производительности

## Функциональное тестирование

### Неизменяемость состояния

Все тесты проверяют что игровые функции не мутируют состояние:

```scheme
(let* ((original-state (make-test-game-with-players))
       (result (make-choice original-state "player1" "move_to_throne_room" 'princess))
       (new-state (caddr result)))
  (assert-state-immutable original-state new-state "State should be immutable"))
```

### Симуляция игровых сценариев

Тесты воспроизводят реальные игровые последовательности:

```scheme
(let* ((state (make-test-game-with-players))
       (actions '((princess "explore")
                 (helper "move_to_kitchen")
                 (princess "move_to_kitchen")
                 (helper "swap_outfits")))
       (final-state (simulate-complex-actions state actions)))
  (assert-not-null final-state "Game scenario completed"))
```

### Проверка игрового состояния

Специальные утверждения для валидации игровых объектов:

```scheme
(assert-character-location state 'princess 'throne_room "Princess location")
(assert-character-outfit state 'helper 'princess_dress "Helper outfit")
(assert-character-has-item state 'helper 'voice_medallion "Helper inventory")
```

## CI/CD интеграция

### GitHub Actions / Jest

Jest автоматически запускает Scheme тесты через JavaScript мост:

```javascript
test('Game Logic Tests', async () => {
  const result = runner.runSchemeFunction('run-all-game-logic-tests');
  expect(result).toBeTruthy();
  // Проверка отсутствия ошибок в результатах
});
```

### Быстрые тесты для CI

Для CI/CD доступны быстрые тесты (`npm run test:scheme:quick`):

- Основные функции создания состояния
- Базовое присоединение игроков
- Простые перемещения
- Обмен нарядами

## Отладка тестов

### Verbose режим

```bash
VERBOSE=true npm test
```

### Отладочные тесты

```bash
node game/test-runner.js debug
```

Включает детальный вывод структур данных для анализа проблем.

### Логирование в Scheme

```scheme
(js-log "Debug info: " (value->string some-data))
```

## Лучшие практики

### Написание тестов

1. **Один тест - одна проверка**: каждый тест должен проверять конкретную функциональность
2. **Четкие названия**: `character-movement-updates-location`
3. **Независимость**: тесты не должны зависеть друг от друга
4. **Проверка граничных случаев**: недопустимые входные данные

### Структура теста

```scheme
(make-test "test-name"
  (lambda ()
    (let* ((state (make-test-game-with-players))
           (result (some-game-function state args)))
      (assert-equal expected actual "Clear description"))))
```

### Группировка тестов

```scheme
(define test-suite-name
  (make-test-suite "Test Suite Description"
    (list test1 test2 test3)))
```

## Метрики и отчетность

### Автоматические отчеты

- Количество пройденных/неудачных тестов
- Время выполнения наборов тестов
- Процент успешности

### Анализ результатов

Тестовый фреймворк предоставляет детальную статистику:

```
=== Test Suite: Core Functions ===
[PASS] create-basic-game-state: Game state should be created successfully
[PASS] initial-scene-is-coop-awakening: Initial scene should be coop_awakening
...
Passed: 15, Failed: 0, Total: 15
```

## Расширение тестов

### Добавление новых тестов

1. Создайте тест в соответствующем файле `.test.scm`
2. Добавьте в тестовый набор
3. Обновите `run-tests.scm` если нужно

### Новые утверждения

Добавьте в `test-framework.scm`:

```scheme
(define (assert-custom-condition state expected message)
  (let ((actual (extract-custom-data state)))
    (assert-equal expected actual message)))
```

### Интеграция с внешними системами

Используйте JavaScript мост в `test-runner.js` для интеграции с внешними сервисами или API.