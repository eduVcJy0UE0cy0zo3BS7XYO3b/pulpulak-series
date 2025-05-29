const CoopGameLogic = require('../coopGameLogic');

describe('Полный цикл прохождения квестов с переодеваниями', () => {
    let gameLogic;
    const roomId = 'TEST_FULL_FLOW';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        gameLogic.startGame(roomId, players);
    });

    test('полное прохождение обоих квестов с переодеваниями', () => {
        const gameState = gameLogic.games.get(roomId);
        
        console.log('\n=== ЧАСТЬ 1: КНЯЖНА ВЫПОЛНЯЕТ СВОЙ КВЕСТ ===');
        
        // 1. Княжна идёт к советнику за квестом
        console.log('1. Княжна идёт к советнику за квестом');
        gameState.stats.princess.location = 'throne_room';
        expect(gameState.stats.princess.outfit).toBe('princess_dress');
        
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        let result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_relic', 'princess');
        expect(result.success).toBe(true);
        expect(gameState.quests.princess.active.title).toContain('реликвия');
        console.log('   ✅ Квест получен: ' + gameState.quests.princess.active.title);
        
        // 2. Княжна идёт в библиотеку к библиотекарю
        console.log('2. Княжна идёт в библиотеку');
        gameState.stats.princess.location = 'library';
        let npcs = gameLogic.getNPCsForLocation('library', gameState, 'princess');
        expect(npcs).toContain('Библиотекарь Марк');
        
        gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
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
        gameState.stats.princess.location = 'secret_archive';
        
        gameLogic.processNPCInteraction(gameState, 'librarian', 'princess');
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
        expect(result.success).toBe(true);
        console.log('   ✅ Получена информация из архива');
        
        // 5. Княжна возвращается к советнику
        console.log('5. Княжна возвращается к советнику');
        gameState.stats.princess.location = 'throne_room';
        
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'princess');
        let dialogue = gameLogic.getGameData(roomId).npcDialogues.princess;
        expect(dialogue.choices.some(c => c.text.includes('находках'))).toBe(true);
        
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'report_relic_findings', 'princess');
        expect(result.success).toBe(true);
        expect(gameState.quests.princess.completed.length).toBe(1);
        console.log('   ✅ Квест княжны завершён!');
        
        console.log('\n=== ЧАСТЬ 2: КНЯЖНА ПЕРЕОДЕВАЕТСЯ И ДЕЛАЕТ КВЕСТ ПОМОЩНИЦЫ ===');
        
        // 6. Княжна идёт к помощнице меняться одеждой
        console.log('6. Княжна и помощница встречаются в спальне');
        gameState.stats.princess.location = 'princess_chamber';
        gameState.stats.helper.location = 'princess_chamber';
        
        // Проверяем что можно меняться одеждой
        expect(gameLogic.canSwitchOutfits(gameState, 'princess')).toBe(true);
        
        // Создаём запрос на обмен
        const requestResult = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
        expect(requestResult.success).toBe(true);
        
        // Помощница принимает
        const swapResult = gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
        expect(swapResult.success).toBe(true);
        
        expect(gameState.stats.princess.outfit).toBe('common_dress');
        expect(gameState.stats.helper.outfit).toBe('princess_dress');
        console.log('   ✅ Одежда поменяна: княжна в простом платье');
        
        // 7. Княжна (в простой одежде) идёт к повару
        console.log('7. Княжна в простой одежде идёт к повару');
        gameState.stats.princess.location = 'kitchen';
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
        dialogue = gameLogic.getGameData(roomId).npcDialogues.princess;
        expect(dialogue.choices.some(c => c.text.includes('травах'))).toBe(true);
        
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'ask_about_herbs', 'princess');
        expect(result.success).toBe(true);
        expect(gameState.quests.princess.active.title).toContain('зелье');
        console.log('   ✅ Квест зелья получен княжной');
        
        // 8. Княжна идёт к травнику в сад
        console.log('8. Княжна идёт к травнику');
        gameState.stats.princess.location = 'garden';
        
        gameLogic.processNPCInteraction(gameState, 'herbalist', 'princess');
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'start_quest', 'princess');
        expect(result.success).toBe(true);
        console.log('   ✅ Травник дал информацию');
        
        // 9. Травник переместился в теплицу
        console.log('9. Травник переместился в теплицу');
        npcs = gameLogic.getNPCsForLocation('greenhouse', gameState, 'princess');
        expect(npcs).toContain('Травник Элиас');
        
        // 10. Княжна идёт в теплицу
        console.log('10. Княжна идёт в теплицу');
        gameState.stats.princess.location = 'greenhouse';
        
        gameLogic.processNPCInteraction(gameState, 'herbalist', 'princess');
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'collect_herbs', 'princess');
        if (!result.success) {
            console.log('   ❌ Ошибка:', result.message);
        }
        expect(result.success).toBe(true);
        console.log('   ✅ Травы собраны');
        
        // 11. Княжна возвращается к повару
        console.log('11. Княжна возвращается к повару');
        gameState.stats.princess.location = 'kitchen';
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'princess');
        dialogue = gameLogic.getGameData(roomId).npcDialogues.princess;
        expect(dialogue.choices.some(c => c.text.includes('информацию о редких травах'))).toBe(true);
        
        result = gameLogic.processNPCDialogueChoice(roomId, 'alice', 'report_herb_findings', 'princess');
        expect(result.success).toBe(true);
        
        // Проверяем завершённые квесты
        console.log('   Завершённые квесты княжны:', gameState.quests.princess.completed.map(q => q.title));
        console.log('   Активный квест княжны:', gameState.quests.princess.active);
        
        // У княжны должен быть завершён квест зелья
        expect(gameState.quests.princess.active).toBeNull();
        console.log('   ✅ Квест зелья завершён княжной!');
        
        console.log('\n=== ЧАСТЬ 3: ОБРАТНАЯ СМЕНА И ПОПЫТКА ПОМОЩНИЦЫ ===');
        
        // 12. Княжна и помощница снова меняются
        console.log('12. Обратная смена одежды');
        gameState.stats.princess.location = 'princess_chamber';
        gameState.stats.helper.location = 'princess_chamber';
        
        const request2 = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
        gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
        
        expect(gameState.stats.princess.outfit).toBe('princess_dress');
        expect(gameState.stats.helper.outfit).toBe('common_dress');
        console.log('   ✅ Одежда вернулась к исходной');
        
        // 13. Помощница в княжеской одежде пытается взять квест княжны
        console.log('13. Помощница меняется с княжной и идёт к советнику');
        const request3 = gameLogic.createOutfitSwapRequest(roomId, 'bob', 'helper');
        gameLogic.respondToOutfitSwapRequest(roomId, 'alice', true);
        
        expect(gameState.stats.helper.outfit).toBe('princess_dress');
        
        gameState.stats.helper.location = 'throne_room';
        gameLogic.processNPCInteraction(gameState, 'royal_advisor', 'helper');
        dialogue = gameLogic.getGameData(roomId).npcDialogues.helper;
        
        // Квест реликвии не должен быть доступен
        expect(dialogue.choices.some(c => c.text.includes('реликвии'))).toBe(false);
        console.log('   ✅ Квест реликвии недоступен (уже взят)');
        
        // 14. Помощница возвращается и меняется обратно
        console.log('14. Помощница возвращается и меняется обратно');
        gameState.stats.helper.location = 'princess_chamber';
        gameState.stats.princess.location = 'princess_chamber';
        
        const request4 = gameLogic.createOutfitSwapRequest(roomId, 'bob', 'helper');
        gameLogic.respondToOutfitSwapRequest(roomId, 'alice', true);
        
        expect(gameState.stats.helper.outfit).toBe('common_dress');
        
        // 15. Помощница идёт к повару
        console.log('15. Помощница в простой одежде идёт к повару');
        gameState.stats.helper.location = 'kitchen';
        
        gameLogic.processNPCInteraction(gameState, 'cook', 'helper');
        dialogue = gameLogic.getGameData(roomId).npcDialogues.helper;
        
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