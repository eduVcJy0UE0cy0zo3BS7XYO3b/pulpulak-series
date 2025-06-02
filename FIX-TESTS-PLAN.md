# 🧪 Test Fixing Plan

## 📋 Problems Identified

### 1. **Async/Await Issues**
- `startGame()` is now async but tests call it synchronously
- `makeChoice()` is now async but tests call it synchronously
- `getGameData()` is now async but tests call it synchronously

### 2. **Initialization Issues**
- Tests use `new CoopGameLogic(gameConfig)` but gameConfig needs initialization
- GameConfigFactory tests fail because JS config needs initialization

### 3. **Data Structure Changes**
- Tests expect `gameData.stats` but get undefined
- Tests expect `gameData.choices` but get undefined
- Tests expect `gameData.roomId` but get undefined

## 🛠️ Fix Strategy

### Phase 1: Core Logic Fixes (High Priority)
1. **CoopGameLogic.test.js** - Core game logic tests
2. **integration.test.js** - Integration tests
3. **movement.test.js** - Movement system tests

### Phase 2: Game Feature Tests (Medium Priority)
4. **questSystem.test.js** - Quest system tests
5. **questAvailability.test.js** - Quest availability tests
6. **fullQuestFlow.test.js** - Full quest flow tests
7. **independentDialogues.test.js** - NPC dialogue tests

### Phase 3: JSON System Tests (Medium Priority)
8. **JsonGameIntegration.test.js** - JSON integration tests
9. **GameConfigFactory.test.js** - Factory pattern tests

### Phase 4: Cleanup (Low Priority)
10. Network/API tests (already mostly passing)

## 🔧 Required Changes

### 1. Make Test Methods Async
```javascript
// Before
test('should work', () => {
    const result = gameLogic.startGame(roomId, players);
    expect(result.stats).toBeDefined();
});

// After
test('should work', async () => {
    const result = await gameLogic.startGame(roomId, players);
    expect(result.stats).toBeDefined();
});
```

### 2. Initialize GameConfig in Tests
```javascript
// Before
const gameLogic = new CoopGameLogic(gameConfig);

// After
const gameLogic = new CoopGameLogic(gameConfig);
// gameConfig will be initialized automatically in startGame
```

### 3. Update Data Access Patterns
```javascript
// Before
const gameData = gameLogic.getGameData(roomId);

// After
const gameData = await gameLogic.getGameData(roomId);
```

## 🎯 Execution Order

1. **Start with CoopGameLogic.test.js** - Most critical
2. **Fix integration.test.js** - Catches cross-cutting issues
3. **Work through movement/quest tests** - Feature-specific
4. **Handle JSON tests last** - New features

## 📊 Expected Results

- **Before**: ~50+ failing tests
- **After**: All tests passing
- **Time**: ~2-3 hours of systematic fixes

## 🚀 Let's Begin!