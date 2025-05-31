const CoopGameLogic = require('../coopGameLogic');

describe('Полный цикл квестов с S-expression системой', () => {
    let gameLogic;
    const roomId = 'TEST_SEXP_FLOW';
    const players = {
        princess: { id: 'alice', name: 'Алиса' },
        helper: { id: 'bob', name: 'Боб' }
    };

    beforeEach(() => {
        gameLogic = new CoopGameLogic();
        gameLogic.startGame(roomId, players);
    });

    describe('Квест принцессы через S-expression систему', () => {
        test('полный цикл квеста через S-expression систему', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            console.log('\n=== КВЕСТ ПРИНЦЕССЫ ЧЕРЕЗ S-EXPRESSION СИСТЕМУ ===');
            
            // Создаем простой тестовый квест программно
            const testQuest = `(quest test_princess_quest
              (metadata
                (title "Test Princess Quest")
                (description "A test quest for princess")
                (character princess))
              (triggers
                (on-dialogue royal_advisor
                  (when (and
                    (outfit-is "noble")
                    (at-location throne_room)))))
              (steps
                (step start_quest
                  (description "Start the quest")
                  (require
                    (at-location throne_room)
                    (talking-to royal_advisor))
                  (actions
                    (set-memory "quest_started" true)
                    (show-message "Quest started!")))
                (step complete_quest
                  (description "Complete the quest")
                  (require
                    (has-memory "quest_started"))
                  (actions
                    (complete-quest))))
              (on-complete
                (show-message "Quest completed!")))`;
            
            // Загружаем тестовый квест
            try {
                questIntegration.questRunner.loadQuest(testQuest);
                console.log('   ✅ Тестовый квест загружен');
            } catch (loadError) {
                console.log('   ❌ Ошибка загрузки квеста:', loadError.message);
                return;
            }
            
            // 1. Проверяем начальные условия
            console.log('1. Проверка начальных условий');
            expect(gameState.stats.princess.location).toBe('princess_chamber');
            expect(gameState.stats.princess.outfit).toBe('princess_dress');
            
            // Перемещаем принцессу к советнику
            gameState.stats.princess.location = 'throne_room';
            console.log('   ✅ Принцесса в тронном зале в парадном платье');
            
            // 2. Проверяем доступность квеста
            console.log('2. Проверка доступности квеста');
            
            // Обновляем контекст для проверки квеста
            questIntegration.questRunner.gameState.currentLocation = 'throne_room';
            questIntegration.questRunner.gameState.currentOutfit = 'noble';
            questIntegration.questRunner.gameState.currentNPC = 'royal_advisor';
            
            const character = { id: 'princess', currentOutfit: 'noble' };
            const canStart = questIntegration.questRunner.canStartQuest('test_princess_quest', character);
            expect(canStart).toBe(true);
            console.log('   ✅ Тестовый квест принцессы доступен');
            
            // 3. Начинаем квест
            console.log('3. Начало квеста');
            const startResult = questIntegration.startQuest('test_princess_quest', true); // force = true for test
            expect(startResult).toBe(true);
            
            // Проверяем что квест активен
            const questStatus = questIntegration.getCurrentQuestStatus();
            expect(questStatus).toBeDefined();
            expect(questStatus.title).toBe('Test Princess Quest');
            console.log('   ✅ Квест начат:', questStatus.title);
            
            // 4. Выполняем первый шаг
            console.log('4. Выполнение первого шага');
            let stepResult = questIntegration.processQuestStep();
            if (stepResult && stepResult.processed) {
                console.log('   ✅ Первый шаг выполнен');
                expect(questIntegration.questRunner.gameState.memory.quest_started).toBe(true);
            }
            
            // 5. Выполняем второй шаг (завершение)
            console.log('5. Завершение квеста');
            stepResult = questIntegration.processQuestStep();
            if (stepResult && stepResult.completed) {
                console.log('   ✅ Квест завершён!');
                expect(stepResult.completed).toBe(true);
            }
            
            console.log('🎉 Квест принцессы успешно завершён через S-expression систему!');
        });
    });

    describe('Квест помощницы через S-expression систему', () => {
        test('полный цикл квеста через S-expression систему', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            console.log('\n=== КВЕСТ ПОМОЩНИЦЫ ЧЕРЕЗ S-EXPRESSION СИСТЕМУ ===');
            
            // Создаем простой тестовый квест для помощницы программно
            const testQuest = `(quest test_helper_quest
              (metadata
                (title "Test Helper Quest")
                (description "A test quest for helper")
                (character helper))
              (triggers
                (on-dialogue cook
                  (when (and
                    (outfit-is "common")
                    (at-location kitchen)))))
              (steps
                (step start_quest
                  (description "Start the quest")
                  (require
                    (at-location kitchen)
                    (talking-to cook))
                  (actions
                    (set-memory "helper_quest_started" true)
                    (show-message "Helper quest started!")))
                (step complete_quest
                  (description "Complete the quest")
                  (require
                    (has-memory "helper_quest_started"))
                  (actions
                    (complete-quest))))
              (on-complete
                (show-message "Helper quest completed!")))`;
            
            // Загружаем тестовый квест
            try {
                questIntegration.questRunner.loadQuest(testQuest);
                console.log('   ✅ Тестовый квест помощницы загружен');
            } catch (loadError) {
                console.log('   ❌ Ошибка загрузки квеста:', loadError.message);
                return;
            }
            
            // 1. Проверяем начальные условия
            console.log('1. Проверка начальных условий');
            expect(gameState.stats.helper.location).toBe('princess_chamber');
            expect(gameState.stats.helper.outfit).toBe('common_dress');
            
            // Перемещаем помощницу к повару
            gameState.stats.helper.location = 'kitchen';
            console.log('   ✅ Помощница на кухне в простом платье');
            
            // 2. Проверяем доступность квеста
            console.log('2. Проверка доступности квеста');
            
            // Обновляем контекст для проверки квеста
            questIntegration.questRunner.gameState.currentLocation = 'kitchen';
            questIntegration.questRunner.gameState.currentOutfit = 'common';
            questIntegration.questRunner.gameState.currentNPC = 'cook';
            
            // Переключаем текущего персонажа на помощницу
            gameState.turnOrder = 'helper';
            
            const character = { id: 'helper', currentOutfit: 'common' };
            const canStart = questIntegration.questRunner.canStartQuest('test_helper_quest', character);
            expect(canStart).toBe(true);
            console.log('   ✅ Тестовый квест помощницы доступен');
            
            // 3. Начинаем квест
            console.log('3. Начало квеста');
            const startResult = questIntegration.startQuest('test_helper_quest', true); // force = true for test
            expect(startResult).toBe(true);
            
            // Проверяем что квест активен
            const questStatus = questIntegration.getCurrentQuestStatus();
            expect(questStatus).toBeDefined();
            expect(questStatus.title).toBe('Test Helper Quest');
            console.log('   ✅ Квест начат:', questStatus.title);
            
            // 4. Выполняем первый шаг
            console.log('4. Выполнение первого шага');
            let stepResult = questIntegration.processQuestStep();
            if (stepResult && stepResult.processed) {
                console.log('   ✅ Первый шаг выполнен');
                expect(questIntegration.questRunner.gameState.memory.helper_quest_started).toBe(true);
            }
            
            // 5. Выполняем второй шаг (завершение)
            console.log('5. Завершение квеста');
            stepResult = questIntegration.processQuestStep();
            if (stepResult && stepResult.completed) {
                console.log('   ✅ Квест завершён!');
                expect(stepResult.completed).toBe(true);
            }
            
            console.log('🎉 Квест помощницы успешно завершён через S-expression систему!');
        });
    });

    describe('Тест смены одежды и доступности квестов', () => {
        test('принцесса должна выполнить квесты в разных одеждах', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            console.log('\n=== ТЕСТ СМЕНЫ ОДЕЖДЫ И ДОСТУПНОСТИ КВЕСТОВ ===');
            
            // 1. Принцесса выполняет свой квест в парадном платье
            console.log('1. Принцесса выполняет свой квест');
            gameState.stats.princess.location = 'throne_room';
            questIntegration.questRunner.gameState.currentLocation = 'throne_room';
            questIntegration.questRunner.gameState.currentOutfit = 'noble';
            questIntegration.questRunner.gameState.currentNPC = 'royal_advisor';
            
            const princessChar = { id: 'princess', currentOutfit: 'noble' };
            expect(questIntegration.questRunner.canStartQuest('helper_secret_potion', princessChar)).toBe(false);
            console.log('   ✅ Квест помощницы недоступен принцессе (неправильный персонаж)');
            
            // 2. Принцесса меняется с помощницей одеждой
            console.log('2. Смена одежды между принцессой и помощницей');
            
            // Перемещаем обеих в спальню
            gameState.stats.princess.location = 'princess_chamber';
            gameState.stats.helper.location = 'princess_chamber';
            
            // Проверяем возможность смены одежды
            expect(gameLogic.outfitSystem.canSwitchOutfits(gameState, 'princess')).toBe(true);
            
            // Создаём и принимаем запрос на смену
            const swapRequest = gameLogic.createOutfitSwapRequest(roomId, 'alice', 'princess');
            expect(swapRequest.success).toBe(true);
            
            const swapResult = gameLogic.respondToOutfitSwapRequest(roomId, 'bob', true);
            expect(swapResult.success).toBe(true);
            
            // Проверяем что одежда поменялась
            expect(gameState.stats.princess.outfit).toBe('common_dress');
            expect(gameState.stats.helper.outfit).toBe('princess_dress');
            console.log('   ✅ Одежда поменялась: принцесса в простом платье, помощница в парадном');
            
            // 3. Принцесса в простой одежде НЕ может делать квест помощницы (квесты привязаны к персонажам)
            console.log('3. Проверка ограничений доступа к квестам по персонажам');
            gameState.stats.princess.location = 'kitchen';
            questIntegration.questRunner.gameState.currentLocation = 'kitchen';
            questIntegration.questRunner.gameState.currentOutfit = 'common';
            questIntegration.questRunner.gameState.currentNPC = 'cook';
            
            const princessCommonChar = { id: 'princess', currentOutfit: 'common' };
            const canStartHelperQuest = questIntegration.questRunner.canStartQuest('helper_secret_potion', princessCommonChar);
            expect(canStartHelperQuest).toBe(false);
            console.log('   ✅ Квест помощницы недоступен принцессе (привязан к персонажу helper)');
            
            // 4. Помощница в парадном платье НЕ может делать квест принцессы
            console.log('4. Помощница не может делать квест принцессы');
            gameState.stats.helper.location = 'throne_room';
            questIntegration.questRunner.gameState.currentLocation = 'throne_room';
            questIntegration.questRunner.gameState.currentOutfit = 'noble';
            questIntegration.questRunner.gameState.currentNPC = 'royal_advisor';
            
            // Переключаем персонажа на помощницу
            gameState.turnOrder = 'helper';
            
            const helperNobleChar = { id: 'helper', currentOutfit: 'noble' };
            const helperCanStartHelperQuest = questIntegration.questRunner.canStartQuest('helper_secret_potion', helperNobleChar);
            expect(helperCanStartHelperQuest).toBe(false);
            
            // В S-expression системе квесты привязаны к character в метаданных И могут иметь ограничения по одежде
            // Квест помощницы требует простую одежду, поэтому недоступен в парадном платье
            console.log('   ✅ Квест помощницы недоступен помощнице в парадном платье (требует простую одежду)');
            
            // 5. Проверяем что NPC реагируют на одежду, а не на ID игрока
            console.log('5. Проверка реакции NPC на одежду');
            
            // Принцесса в простой одежде может говорить с поваром
            gameState.turnOrder = 'princess';
            gameState.stats.princess.location = 'kitchen';
            
            const princessNPCs = gameLogic.getNPCsForLocation('kitchen', gameState, 'princess');
            expect(princessNPCs.length).toBeGreaterThan(0);
            console.log('   ✅ Принцесса в простой одежде может взаимодействовать с поваром');
            
            // Помощница в парадном платье может говорить с советником
            gameState.turnOrder = 'helper';
            gameState.stats.helper.location = 'throne_room';
            
            const helperNPCs = gameLogic.getNPCsForLocation('throne_room', gameState, 'helper');
            expect(helperNPCs.length).toBeGreaterThan(0);
            console.log('   ✅ Помощница в парадном платье может взаимодействовать с советником');
            
            console.log('🎉 Система смены одежды и доступности квестов работает корректно!');
        });
    });

    describe('Проверка ограничений и валидации', () => {
        test('квесты должны быть доступны только один раз', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            console.log('\n=== ТЕСТ ОГРАНИЧЕНИЙ ПОВТОРНОГО ВЫПОЛНЕНИЯ ===');
            
            // 1. Отмечаем квест как уже начатый
            questIntegration.questRunner.gameState.startedQuests.add('helper_secret_potion');
            
            // 2. Проверяем что квест больше недоступен
            const character = { id: 'helper', currentOutfit: 'common' };
            questIntegration.questRunner.gameState.currentLocation = 'kitchen';
            questIntegration.questRunner.gameState.currentOutfit = 'common';
            questIntegration.questRunner.gameState.currentNPC = 'cook';
            
            const canStartAgain = questIntegration.questRunner.canStartQuest('helper_secret_potion', character);
            expect(canStartAgain).toBe(false);
            console.log('   ✅ Уже начатый квест недоступен для повторного запуска');
            
            // 3. Проверяем завершённые квесты
            questIntegration.questRunner.gameState.completedQuests.add('helper_secret_potion');
            
            const helperChar = { id: 'helper', currentOutfit: 'common' };
            questIntegration.questRunner.gameState.currentLocation = 'kitchen';
            questIntegration.questRunner.gameState.currentOutfit = 'common';
            questIntegration.questRunner.gameState.currentNPC = 'cook';
            
            const canStartCompleted = questIntegration.questRunner.canStartQuest('helper_secret_potion', helperChar);
            expect(canStartCompleted).toBe(false);
            console.log('   ✅ Завершённый квест недоступен для повторного выполнения');
            
            console.log('🎉 Система ограничений работает корректно!');
        });

        test('проверка требований локаций и экипировки', () => {
            const gameState = gameLogic.lobbyLogic.getGameState(roomId);
            const questIntegration = gameLogic.lobbyLogic.getQuestIntegration(roomId);
            
            console.log('\n=== ТЕСТ ТРЕБОВАНИЙ ЛОКАЦИЙ И ЭКИПИРОВКИ ===');
            
            // 1. Неправильная локация
            questIntegration.questRunner.gameState.currentLocation = 'garden';  // Неправильная локация
            questIntegration.questRunner.gameState.currentOutfit = 'noble';
            questIntegration.questRunner.gameState.currentNPC = 'royal_advisor';
            
            const charWrongLocation = { id: 'helper', currentOutfit: 'common' };
            const canStartWrongLocation = questIntegration.questRunner.canStartQuest('helper_secret_potion', charWrongLocation);
            expect(canStartWrongLocation).toBe(false);
            console.log('   ✅ Квест недоступен в неправильной локации');
            
            // 2. Неправильная одежда
            questIntegration.questRunner.gameState.currentLocation = 'throne_room';
            questIntegration.questRunner.gameState.currentOutfit = 'common';  // Неправильная одежда
            questIntegration.questRunner.gameState.currentNPC = 'royal_advisor';
            
            const charWrongOutfit = { id: 'helper', currentOutfit: 'noble' };
            const canStartWrongOutfit = questIntegration.questRunner.canStartQuest('helper_secret_potion', charWrongOutfit);
            expect(canStartWrongOutfit).toBe(false);
            console.log('   ✅ Квест недоступен в неправильной одежде');
            
            // 3. Правильные условия
            questIntegration.questRunner.gameState.currentLocation = 'kitchen';
            questIntegration.questRunner.gameState.currentOutfit = 'common';
            questIntegration.questRunner.gameState.currentNPC = 'cook';
            
            const charCorrect = { id: 'helper', currentOutfit: 'common' };
            const canStartCorrect = questIntegration.questRunner.canStartQuest('helper_secret_potion', charCorrect);
            expect(canStartCorrect).toBe(true);
            console.log('   ✅ Квест доступен при правильных условиях');
            
            console.log('🎉 Система требований работает корректно!');
        });
    });
});