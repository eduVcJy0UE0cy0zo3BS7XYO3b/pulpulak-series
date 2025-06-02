/**
 * NPC Dialogue Processing utility extracted from CoopGameLogic
 * Handles all NPC interaction and dialogue management
 */

class NPCDialogueProcessor {
    /**
     * Process initial NPC interaction (start dialogue)
     * @param {Object} gameState - Current game state
     * @param {string} npcId - NPC identifier
     * @param {string} character - Character interacting with NPC
     * @param {Object} NPCData - NPC data module
     * @param {Object} immerStateManager - Immer state manager
     * @param {Object} gameData - Game data manager
     * @returns {Object} Interaction result
     */
    static processNPCInteraction(gameState, npcId, character, NPCData, immerStateManager, gameData) {
        try {
            const npc = NPCData.getNPC(npcId);
            if (!npc) {
                return { success: false, message: "NPC не найден" };
            }

            // Получаем наряд персонажа
            const outfit = gameState.stats[character].outfit;
            
            // Получаем память NPC и создаем диалог через Immer
            let updatedGameState = gameState;
            if (!gameState.npcMemory[character][npcId]) {
                updatedGameState = immerStateManager.updateState(gameState, draft => {
                    draft.npcMemory[character][npcId] = {};
                });
                gameData.games.set(updatedGameState.roomId, updatedGameState);
            }
            const npcMemory = updatedGameState.npcMemory[character][npcId];
            
            // Получаем диалог в зависимости от наряда, памяти, локации и состояния квеста
            const currentLocation = updatedGameState.stats[character].location;
            const questState = updatedGameState.quests[character];
            const globalQuestMemory = updatedGameState.globalQuestMemory;
            const dialogue = NPCData.getNPCDialogue(npcId, outfit, npcMemory, currentLocation, questState, globalQuestMemory, updatedGameState);
            if (!dialogue) {
                return { success: false, message: "Диалог не найден" };
            }

            // Сохраняем информацию о диалоге для конкретного персонажа
            updatedGameState = immerStateManager.updateState(updatedGameState, draft => {
                draft.npcDialogues[character] = {
                    npcId: npcId,
                    npcName: npc.name,
                    greeting: dialogue.greeting,
                    choices: dialogue.choices,
                    attitude: NPCData.getNPCAttitude(npcId, outfit),
                    activeCharacter: character, // Кто ведет диалог
                    isFollowUp: false // Флаг для дополнительных выборов
                };
            });
            gameData.games.set(updatedGameState.roomId, updatedGameState);

            return { 
                success: true, 
                showDialogue: true,
                message: `Начат диалог с ${npc.name}`
            };
        } catch (error) {
            console.error('Ошибка при взаимодействии с NPC:', error);
            return { 
                success: false, 
                message: `Не удалось начать диалог: ${error.message}` 
            };
        }
    }

    /**
     * Process dialogue choice selection
     * @param {string} roomId - Room identifier
     * @param {string} playerId - Player identifier
     * @param {string} choiceId - Choice identifier
     * @param {string} character - Character making choice
     * @param {Object} NPCData - NPC data module
     * @param {Object} immerStateManager - Immer state manager
     * @param {Object} gameData - Game data manager
     * @param {Object} EffectsProcessor - Effects processor
     * @param {Function} processQuestActionFn - Quest action processor function
     * @param {Function} switchTurnFn - Turn switching function
     * @param {Function} getNPCsForLocationFn - Get NPCs for location function
     * @returns {Object} Choice processing result
     */
    static async processNPCDialogueChoice(roomId, playerId, choiceId, character, NPCData, immerStateManager, gameData, EffectsProcessor, processQuestActionFn, switchTurnFn, getNPCsForLocationFn) {
        let gameState = gameData.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Проверяем, что игрок управляет правильным персонажем
        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        // Проверяем, что есть активный диалог для данного персонажа
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        const npcId = gameState.npcDialogues[character].npcId;
        const outfit = gameState.stats[character].outfit;

        // Получаем память NPC для этого персонажа
        if (!gameState.npcMemory[character][npcId]) {
            gameState.npcMemory[character][npcId] = {};
        }

        // Обрабатываем выбор через NPCData
        const isFollowUp = gameState.npcDialogues[character].isFollowUp || false;
        const currentChoices = isFollowUp ? gameState.npcDialogues[character].choices : [];
        
        // Создаем мутабельную копию памяти NPC для NPCData
        const npcMemoryCopy = JSON.parse(JSON.stringify(gameState.npcMemory[character][npcId]));
        
        // Проверяем, это статический метод (JS версия) или метод экземпляра (JSON версия)
        const result = typeof NPCData.processDialogueChoice === 'function' 
            ? NPCData.processDialogueChoice(
                npcId, 
                choiceId, 
                outfit, 
                npcMemoryCopy,
                isFollowUp,
                currentChoices,
                gameState.stats[character].location
            )
            : await NPCData.processDialogueChoice(
                npcId, 
                choiceId, 
                outfit, 
                npcMemoryCopy,
                isFollowUp,
                currentChoices,
                gameState.stats[character].location
            );
        if (!result) {
            return { success: false, message: "Неверный выбор" };
        }

        // Обновляем память NPC и применяем эффекты
        gameState = immerStateManager.updateState(gameState, draft => {
            draft.npcMemory[character][npcId] = result.updatedMemory;
        });
        
        // Применяем эффекты выбора через EffectsProcessor
        if (result.effects) {
            gameState = EffectsProcessor.applyNPCDialogueEffects(immerStateManager, gameState, character, result.effects);
        }
        gameData.games.set(gameState.roomId, gameState);

        // Обрабатываем квестовые действия
        const questResult = processQuestActionFn(gameState, character, choiceId, result);
        if (questResult && questResult.success && questResult.gameState) {
            // Use Immer to properly merge the quest result state
            gameState = immerStateManager.updateState(gameState, draft => {
                const newState = questResult.gameState;
                // Copy all relevant fields that might have changed
                if (newState.quests) draft.quests = newState.quests;
                if (newState.globalQuestMemory) draft.globalQuestMemory = newState.globalQuestMemory;
                if (newState.stats) draft.stats = newState.stats;
            });
            gameData.games.set(gameState.roomId, gameState);
        }

        // Обновляем NPC в локациях после квестовых действий (NPC могли переместиться)
        gameState = immerStateManager.updateState(gameState, draft => {
            draft.stats.princess.npcsPresent = getNPCsForLocationFn(draft.stats.princess.location, gameState, 'princess');
            draft.stats.helper.npcsPresent = getNPCsForLocationFn(draft.stats.helper.location, gameState, 'helper');
        });
        gameData.games.set(gameState.roomId, gameState);

        // Сохраняем attitude до очистки диалога
        const attitude = gameState.npcDialogues[character]?.attitude;

        // Если есть дополнительные выборы, показываем их
        if (result.next_choices && result.next_choices.length > 0) {
            gameState = immerStateManager.updateState(gameState, draft => {
                draft.npcDialogues[character].choices = result.next_choices;
                draft.npcDialogues[character].greeting = result.response;
                draft.npcDialogues[character].isFollowUp = true;
            });
            gameData.games.set(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success',
                hasFollowUp: true
            };
        } else {
            // Очищаем диалог для данного персонажа и меняем ход
            gameState = immerStateManager.updateState(gameState, draft => {
                draft.npcDialogues[character] = null;
            });
            gameState = switchTurnFn(gameState);
            gameData.games.set(gameState.roomId, gameState);

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success'
            };
        }
    }

    /**
     * Close active NPC dialogue
     * @param {string} roomId - Room identifier
     * @param {string} playerId - Player identifier
     * @param {Object} immerStateManager - Immer state manager
     * @param {Object} gameData - Game data manager
     * @returns {Object} Close dialogue result
     */
    static closeNPCDialogue(roomId, playerId, immerStateManager, gameData) {
        let gameState = gameData.getGame(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        // Находим персонажа, который принадлежит данному игроку
        let character = null;
        for (const [char, player] of Object.entries(gameState.players)) {
            if (player && player.id === playerId) {
                character = char;
                break;
            }
        }

        if (!character) {
            return { success: false, message: "Игрок не найден" };
        }

        // Проверяем, что есть активный диалог для данного персонажа
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        // Закрываем диалог для данного персонажа
        gameState = immerStateManager.updateState(gameState, draft => {
            draft.npcDialogues[character] = null;
        });
        gameData.games.set(gameState.roomId, gameState);

        return { 
            success: true, 
            message: "Диалог закрыт",
            gameState: gameState
        };
    }

    /**
     * Validate if choice is NPC interaction
     * @param {string} choiceId - Choice identifier
     * @returns {Object} Validation result with extracted NPC ID
     */
    static validateNPCInteractionChoice(choiceId) {
        if (!choiceId.startsWith('talk_to_')) {
            return { isNPCInteraction: false };
        }
        
        const npcId = choiceId.replace('talk_to_', '');
        return { 
            isNPCInteraction: true, 
            npcId: npcId 
        };
    }

    /**
     * Check if choice is an NPC interaction
     * @param {string} choiceId - Choice ID to check
     * @returns {boolean} True if NPC interaction choice
     */
    static isNPCInteractionChoice(choiceId) {
        return choiceId.startsWith('talk_to_');
    }
}

module.exports = NPCDialogueProcessor;