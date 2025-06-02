const CoopGameLogic = require('../coopGameLogic');
const { refreshGameState } = require('./testHelpers');
const MockGameConfig = require('./mocks/MockGameConfig');

describe('Полный цикл прохождения квестов с переодеваниями', () => {
    let gameLogic;
    let gameConfig;
    const roomId = 'TEST_FULL_FLOW';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(async () => {
        gameConfig = new MockGameConfig();
        gameLogic = new CoopGameLogic(gameConfig);
        await gameLogic.startGame(roomId, players);
    });

    test('полное прохождение обоих квестов с переодеваниями', async () => {
        let gameState = gameLogic.games.get(roomId);
        
        console.log('\n=== ЧАСТЬ 1: КНЯЖНА ВЫПОЛНЯЕТ СВОЙ КВЕСТ ===');
        
        // 1. Княжна идёт к советнику за квестом
        console.log('1. Княжна идёт к советнику за квестом');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('throne_room', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        expect(gameState.stats.princess.outfit).toBe('princess_dress');
        
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        
        // Отладка: проверим доступные выборы
        console.log('Доступные выборы:', gameState.npcDialogues.princess?.choices?.map(c => c.id) || 'нет диалога');
        
        let result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_relic', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        console.log('Результат диалога:', result);
        console.log('Квест после диалога:', gameState.quests.princess.active);
        expect(result.success).toBe(true);
        expect(gameState.quests.princess.active.title).toContain('реликвия');
        console.log('   ✅ Квест получен: ' + gameState.quests.princess.active.title);
        
        // 2. Княжна идёт в библиотеку к библиотекарю
        console.log('2. Княжна идёт в библиотеку');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'library';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('library', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        let npcs = gameLogic.getNPCsForLocation('library', gameState, 'princess');
        expect(npcs).toContain('Библиотекарь Марк');
        
        gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        console.log('   ✅ Библиотекарь дал информацию');
        
        
        // 3. Проверяем что библиотекарь переместился в архив
        console.log('3. Библиотекарь переместился в архив');
        npcs = gameLogic.getNPCsForLocation('library', gameState, 'princess');
        expect(npcs).not.toContain('Библиотекарь Марк');
        npcs = gameLogic.getNPCsForLocation('secret_archive', gameState, 'princess');
        expect(npcs).toContain('Библиотекарь Марк');
        console.log('   ✅ Библиотекарь в архиве');
        
        // 4. Княжна идёт в секретный архив
        console.log('4. Княжна идёт в секретный архив');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'secret_archive';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('secret_archive', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
        
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        if (!result.success) {
            console.log('   ❌ Ошибка в архиве:', result.message);
        }
        expect(result.success).toBe(true);
        console.log('   ✅ Получена информация из архива');
        
        
        // 5. Княжна возвращается к советнику
        console.log('5. Княжна возвращается к советнику');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'throne_room';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('throne_room', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        let gameData = await gameLogic.getGameData(roomId);
        let dialogue = gameData.npcDialogues.princess;
        expect(dialogue.choices.some(c => c.text.includes('находках'))).toBe(true);
        
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'report_relic_findings', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        expect(gameState.quests.princess.completed.length).toBe(1);
        console.log('   ✅ Квест княжны завершён!');
        
        console.log('\n=== ЧАСТЬ 2: КНЯЖНА ПЕРЕОДЕВАЕТСЯ И ДЕЛАЕТ КВЕСТ ПОМОЩНИЦЫ ===');
        
        // 6. Княжна идёт к помощнице меняться одеждой
        console.log('6. Княжна и помощница встречаются в спальне');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'princess_chamber';
            draft.stats.helper.location = 'princess_chamber';
            // Обновляем списки NPC для новых локаций
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'princess');
            draft.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'helper');
        });
        gameLogic.games.set(roomId, gameState);
        
        // Проверяем что можно меняться одеждой
        console.log('Локация княжны:', gameState.stats.princess.location);
        console.log('Локация помощницы:', gameState.stats.helper.location);
        console.log('NPC у княжны:', gameState.stats.princess.npcsPresent);
        console.log('NPC у помощницы:', gameState.stats.helper.npcsPresent);
        expect(gameLogic.canSwitchOutfits(gameState, 'princess')).toBe(true);
        
        // Создаём запрос на обмен
        const requestResult = await gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
        expect(requestResult.success).toBe(true);
        
        // Помощница принимает
        const swapResult = await gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
        expect(swapResult.success).toBe(true);
        
        // Обновляем состояние игры после обмена одеждой
        gameState = refreshGameState(gameLogic, roomId);
        
        expect(gameState.stats.princess.outfit).toBe('common_dress');
        expect(gameState.stats.helper.outfit).toBe('princess_dress');
        console.log('   ✅ Одежда поменяна: княжна в простом платье');
        
        // 7. Княжна (в простой одежде) идёт к повару
        console.log('7. Княжна в простой одежде идёт к повару');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'kitchen';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('kitchen', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
        gameData = await gameLogic.getGameData(roomId);
        dialogue = gameData.npcDialogues.princess;
        expect(dialogue.choices.some(c => c.text.includes('травах'))).toBe(true);
        
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_herbs', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        expect(gameState.quests.helper.active.title).toContain('зелье');
        console.log('   ✅ Квест зелья получен княжной');
        
        // 8. Княжна идёт к травнику в сад
        console.log('8. Княжна идёт к травнику');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'garden';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('garden', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'herbalist', 'princess');
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        console.log('   ✅ Травник дал информацию');
        
        // 9. Травник переместился в теплицу
        console.log('9. Травник переместился в теплицу');
        npcs = gameLogic.getNPCsForLocation('greenhouse', gameState, 'princess');
        expect(npcs).toContain('Травник Элиас');
        
        // 10. Княжна идёт в теплицу
        console.log('10. Княжна идёт в теплицу');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'greenhouse';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('greenhouse', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'herbalist', 'princess');
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'collect_herbs', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        if (!result.success) {
            console.log('   ❌ Ошибка:', result.message);
        }
        expect(result.success).toBe(true);
        console.log('   ✅ Травы собраны');
        
        // 11. Княжна возвращается к повару
        console.log('11. Княжна возвращается к повару');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'kitchen';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('kitchen', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
        gameData = await gameLogic.getGameData(roomId);
        dialogue = gameData.npcDialogues.princess;
        console.log('   Доступные выборы у повара:', dialogue.choices.map(c => c.text));
        console.log('   Global quest memory:', gameState.globalQuestMemory);
        console.log('   Helper quest state:', gameState.quests.helper);
        console.log('   Helper quest steps detailed:', gameState.quests.helper.active?.steps);
        expect(dialogue.choices.some(c => c.text.includes('информацию о редких травах') || c.text.includes('травах'))).toBe(true);
        
        result = await gameLogic.processNPCDialogueChoice(roomId, 'alice', 'report_herb_findings', 'princess');
        gameState = refreshGameState(gameLogic, roomId);
        expect(result.success).toBe(true);
        
        // Проверяем завершённые квесты
        console.log('   Завершённые квесты помощницы:', gameState.quests.helper.completed.map(q => q.title));
        console.log('   Активный квест помощницы:', gameState.quests.helper.active);
        
        // У помощницы должен быть завершён квест зелья
        expect(gameState.quests.helper.active).toBeNull();
        console.log('   ✅ Квест зелья завершён!');
        
        console.log('\n=== ЧАСТЬ 3: ОБРАТНАЯ СМЕНА И ПОПЫТКА ПОМОЩНИЦЫ ===');
        
        // 12. Княжна и помощница снова меняются
        console.log('12. Обратная смена одежды');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.location = 'princess_chamber';
            draft.stats.helper.location = 'princess_chamber';
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'princess');
            draft.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'helper');
        });
        gameLogic.games.set(roomId, gameState);
        
        const request2 = await gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
        await gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
        
        // Обновляем состояние игры после обмена одеждой
        gameState = refreshGameState(gameLogic, roomId);
        
        expect(gameState.stats.princess.outfit).toBe('princess_dress');
        expect(gameState.stats.helper.outfit).toBe('common_dress');
        console.log('   ✅ Одежда вернулась к исходной');
        
        // 13. Помощница в княжеской одежде пытается взять квест княжны
        console.log('13. Помощница меняется с княжной и идёт к советнику');
        const request3 = await gameLogic.createOutfitSwapRequest(roomId, 'bob', 'helper');
        await gameLogic.respondToOutfitSwapRequest(roomId, 'alice', true);
        
        // Обновляем состояние игры после обмена одеждой
        gameState = refreshGameState(gameLogic, roomId);
        
        expect(gameState.stats.helper.outfit).toBe('princess_dress');
        
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.helper.location = 'throne_room';
            draft.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('throne_room', gameState, 'helper');
        });
        gameLogic.games.set(roomId, gameState);
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'helper');
        gameData = await gameLogic.getGameData(roomId);
        dialogue = gameData.npcDialogues.helper;
        
        // Квест реликвии не должен быть доступен
        expect(dialogue.choices.some(c => c.text.includes('реликвии'))).toBe(false);
        console.log('   ✅ Квест реликвии недоступен (уже взят)');
        
        // 14. Помощница возвращается и меняется обратно
        console.log('14. Помощница возвращается и меняется обратно');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.helper.location = 'princess_chamber';
            draft.stats.princess.location = 'princess_chamber';
            draft.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'helper');
            draft.stats.princess.npcsPresent = gameLogic.getNPCsForLocation('princess_chamber', gameState, 'princess');
        });
        gameLogic.games.set(roomId, gameState);
        
        const request4 = await gameLogic.createOutfitSwapRequest(roomId, 'bob', 'helper');
        await gameLogic.respondToOutfitSwapRequest(roomId, 'alice', true);
        
        // Обновляем состояние игры после обмена одеждой
        gameState = refreshGameState(gameLogic, roomId);
        
        expect(gameState.stats.helper.outfit).toBe('common_dress');
        
        // 15. Помощница идёт к повару
        console.log('15. Помощница в простой одежде идёт к повару');
        gameState = gameLogic.immerStateManager.updateState(gameState, draft => {
            draft.stats.helper.location = 'kitchen';
            draft.stats.helper.npcsPresent = gameLogic.getNPCsForLocation('kitchen', gameState, 'helper');
        });
        gameLogic.games.set(roomId, gameState);
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        gameData = await gameLogic.getGameData(roomId);
        dialogue = gameData.npcDialogues.helper;
        
        // Квест зелья не должен быть доступен
        expect(dialogue.choices.some(c => c.text.includes('лечебных травах'))).toBe(false);
        console.log('   ✅ Квест зелья недоступен (уже взят)');
        
        console.log('\n=== ИТОГ ===');
        console.log('Состояние квестов:');
        console.log('- Княжна завершила:', gameState.quests.princess.completed.map(q => q.title));
        console.log('- Помощница завершила:', gameState.quests.helper.completed.map(q => q.title));
        console.log('- Глобальная память:', gameState.globalQuestMemory);
        
        // Проверяем итоговое состояние
        expect(gameState.quests.princess.completed.length).toBeGreaterThan(0);
        expect(gameState.globalQuestMemory.princess_lost_relic).toBe(true);
        expect(gameState.globalQuestMemory.helper_secret_potion).toBe(true);
        
        console.log('✅ Княжна выполнила оба квеста');
        console.log('✅ Помощница не может взять уже выполненные квесты');
        console.log('✅ NPC узнают игроков по одежде');
        console.log('✅ Каждый квест можно взять только один раз');
    });
});