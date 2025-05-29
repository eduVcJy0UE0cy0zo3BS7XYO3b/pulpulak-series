const questActionHandlers = {
    'start_noble_quest': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isNoble = outfit === 'princess_dress' || outfit === 'court_dress';
        if (isNoble && !gameState.globalQuestMemory.princess_lost_relic) {
            gameLogic.startQuest(gameState, character, 'princess_lost_relic');
            gameLogic.updateQuestProgress(gameState, character, 'get_quest');
            gameState.globalQuestMemory.princess_lost_relic = true;
            return true;
        }
        return false;
    },

    'start_common_quest': (gameState, character, gameLogic) => {
        const outfit = gameState.stats[character].outfit;
        const isCommon = outfit === 'common_dress' || outfit === 'nightgown';
        if (isCommon && !gameState.globalQuestMemory.helper_secret_potion) {
            gameLogic.startQuest(gameState, character, 'helper_secret_potion');
            gameLogic.updateQuestProgress(gameState, character, 'get_quest');
            gameState.globalQuestMemory.helper_secret_potion = true;
            return true;
        }
        return false;
    },

    'progress_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'librarian' && character === 'princess') {
            gameLogic.updateQuestProgress(gameState, character, 'search_library');
            return true;
        }
        return false;
    },

    'progress_herb_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'herbalist') {
            gameLogic.updateQuestProgress(gameState, character, 'find_herbalist');
            return true;
        }
        return false;
    },

    'complete_archive_step': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'librarian' && character === 'princess') {
            gameLogic.updateQuestProgress(gameState, character, 'talk_to_librarian');
            return true;
        }
        return false;
    },

    'complete_princess_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'royal_advisor' && character === 'princess') {
            gameLogic.updateQuestProgress(gameState, character, 'return_to_advisor');
            return true;
        }
        return false;
    },

    'complete_herb_collection': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'herbalist') {
            gameLogic.updateQuestProgress(gameState, character, 'talk_to_herbalist');
            return true;
        }
        return false;
    },

    'complete_helper_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'cook') {
            gameLogic.updateQuestProgress(gameState, character, 'return_to_cook');
            return true;
        }
        return false;
    }
};

// Обработчики для старых choiceId (обратная совместимость)
const legacyHandlers = {
    'ask_about_relic': (gameState, character, gameLogic) => {
        if (character === 'princess') {
            gameLogic.startQuest(gameState, character, 'princess_lost_relic');
            gameLogic.updateQuestProgress(gameState, character, 'get_quest');
            return true;
        }
        return false;
    },

    'ask_about_herbs': (gameState, character, gameLogic) => {
        if (character === 'helper') {
            gameLogic.startQuest(gameState, character, 'helper_secret_potion');
            gameLogic.updateQuestProgress(gameState, character, 'get_quest');
            return true;
        }
        return false;
    },

    'start_quest': (gameState, character, gameLogic) => {
        const npcId = gameState.npcDialogues?.[character]?.npcId;
        if (npcId === 'librarian' && character === 'princess') {
            gameLogic.updateQuestProgress(gameState, character, 'talk_to_librarian');
            return true;
        } else if (npcId === 'herbalist' && character === 'helper') {
            gameLogic.updateQuestProgress(gameState, character, 'find_herbalist');
            return true;
        }
        return false;
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