const questActionHandlers = {
    'start_noble_quest': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (isNoble && !gameState.globalQuestMemory.princess_lost_relic) {
            // Если персонаж в княжеском наряде, работаем с квестом принцессы
            const questCharacter = 'princess';
            const questResult = gameLogic.startQuest(gameState, questCharacter, 'princess_lost_relic');
            if (questResult.success && questResult.gameState) {
                const progressResult = gameLogic.updateQuestProgress(questResult.gameState, questCharacter, 'get_quest');
                return {
                    success: true,
                    gameState: progressResult.gameState || questResult.gameState
                };
            }
        }
        return { success: false };
    },

    'start_common_quest': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (isCommon && !gameState.globalQuestMemory.helper_secret_potion) {
            // Если персонаж в простом наряде, работаем с квестом помощницы
            const questCharacter = 'helper';
            const questResult = gameLogic.startQuest(gameState, questCharacter, 'helper_secret_potion');
            if (questResult.success && questResult.gameState) {
                const progressResult = gameLogic.updateQuestProgress(questResult.gameState, questCharacter, 'get_quest');
                return {
                    success: true,
                    gameState: progressResult.gameState || questResult.gameState
                };
            }
        }
        return { success: false };
    },

    'progress_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (npcId === 'librarian' && isNoble) {
            // Если персонаж в княжеском наряде, обновляем квест принцессы
            const questCharacter = 'princess';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'search_library');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    },

    'progress_herb_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (npcId === 'herbalist' && isCommon) {
            // Если персонаж в простом наряде, обновляем квест помощницы
            const questCharacter = 'helper';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'find_herbalist');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    },

    'complete_archive_step': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (npcId === 'librarian' && isNoble) {
            // Если персонаж в княжеском наряде, обновляем квест принцессы
            const questCharacter = 'princess';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'talk_to_librarian');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    },

    'complete_princess_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (npcId === 'royal_advisor' && isNoble) {
            // Если персонаж в княжеском наряде, обновляем квест принцессы
            const questCharacter = 'princess';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'return_to_advisor');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    },

    'complete_herb_collection': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (npcId === 'herbalist' && isCommon) {
            // Если персонаж в простом наряде, обновляем квест помощницы
            const questCharacter = 'helper';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'talk_to_herbalist');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    },

    'complete_helper_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (npcId === 'cook' && isCommon) {
            // Если персонаж в простом наряде, обновляем квест помощницы
            const questCharacter = 'helper';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'return_to_cook');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    }
};

// Обработчики для старых choiceId (обратная совместимость)
const legacyHandlers = {
    'ask_about_relic': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (isNoble && !gameState.globalQuestMemory.princess_lost_relic) {
            // Если персонаж в княжеском наряде, работаем с квестом принцессы
            const questCharacter = 'princess';
            const questResult = gameLogic.startQuest(gameState, questCharacter, 'princess_lost_relic');
            if (questResult.success && questResult.gameState) {
                const progressResult = gameLogic.updateQuestProgress(questResult.gameState, questCharacter, 'get_quest');
                return {
                    success: true,
                    gameState: progressResult.gameState || questResult.gameState
                };
            }
        }
        return { success: false };
    },

    'ask_about_herbs': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (isCommon && !gameState.globalQuestMemory.helper_secret_potion) {
            // Если персонаж в простом наряде, работаем с квестом помощницы
            const questCharacter = 'helper';
            const questResult = gameLogic.startQuest(gameState, questCharacter, 'helper_secret_potion');
            if (questResult.success && questResult.gameState) {
                const progressResult = gameLogic.updateQuestProgress(questResult.gameState, questCharacter, 'get_quest');
                return {
                    success: true,
                    gameState: progressResult.gameState || questResult.gameState
                };
            }
        }
        return { success: false };
    },

    'start_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        
        if (npcId === 'librarian' && isNoble) {
            // Если персонаж в княжеском наряде, обновляем квест принцессы
            const questCharacter = 'princess';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'talk_to_librarian');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        } else if (npcId === 'herbalist' && isCommon) {
            // Если персонаж в простом наряде, обновляем квест помощницы
            const questCharacter = 'helper';
            const progressResult = gameLogic.updateQuestProgress(gameState, questCharacter, 'find_herbalist');
            return {
                success: true,
                gameState: progressResult.gameState || gameState
            };
        }
        return { success: false };
    }
};

module.exports = {
    processQuestAction: (gameState, character, choiceId, dialogueResult, gameLogic) => {
        const questAction = dialogueResult?.quest_action || null;
        
        // Сначала проверяем questAction из диалога
        if (questAction && questActionHandlers[questAction]) {
            return questActionHandlers[questAction](gameState, character, gameLogic);
        }
        
        // Затем проверяем старые choiceId для обратной совместимости
        if (legacyHandlers[choiceId]) {
            return legacyHandlers[choiceId](gameState, character, gameLogic);
        }
        
        return false;
    },
    
    // Экспорт для тестирования
    questActionHandlers,
    legacyHandlers
};