const gameConfig = require('../config/gameConfig');

class GameStateManager {
    constructor() {
        this.defaultState = {
            currentScene: 'coop_awakening',
            turnOrder: 'princess',
            chapter: 1
        };
    }

    createInitialState(roomId, players) {
        if (!roomId || !players) {
            throw new Error('Required parameters missing');
        }

        return {
            roomId,
            players,
            ...this.defaultState,
            stats: this.createInitialStats(),
            npcMemory: this.createInitialNPCMemory(),
            quests: this.createInitialQuests(),
            globalQuestMemory: this.createInitialGlobalQuestMemory(),
            // Индивидуальные диалоги для каждого персонажа
            npcDialogues: {
                princess: null,
                helper: null
            },
            // Динамические диалоги от квестовой системы
            dynamicDialogues: {
                princess: {},
                helper: {}
            }
        };
    }

    createInitialStats() {
        return {
            princess: {
                outfit: 'princess_dress',
                ...gameConfig.INITIAL_STATS,
                location: 'princess_chamber',
                npcsPresent: []
            },
            helper: {
                outfit: 'common_dress',
                ...gameConfig.INITIAL_STATS,
                secrets_revealed: 0,
                inventory: ['translation_earrings', 'voice_medallion'],
                location: 'princess_chamber',
                npcsPresent: []
            }
        };
    }

    createInitialNPCMemory() {
        return {
            princess: {},
            helper: {}
        };
    }

    createInitialQuests() {
        return {
            princess: {
                active: null,
                completed: []
            },
            helper: {
                active: null,
                completed: []
            }
        };
    }

    createInitialGlobalQuestMemory() {
        return {
            princess_lost_relic: false,
            helper_secret_potion: false
        };
    }

    validateGameState(gameState) {
        const requiredFields = ['roomId', 'players', 'stats', 'quests'];
        
        for (const field of requiredFields) {
            if (!gameState[field]) {
                throw new Error(`Missing required field: ${field}`);
            }
        }

        const requiredCharacters = ['princess', 'helper'];
        for (const character of requiredCharacters) {
            if (!gameState.stats[character]) {
                throw new Error(`Missing stats for character: ${character}`);
            }
            if (!gameState.quests[character]) {
                throw new Error(`Missing quests for character: ${character}`);
            }
        }

        return true;
    }

    cloneGameState(gameState) {
        return JSON.parse(JSON.stringify(gameState));
    }

    resetCharacterToLocation(gameState, character, location) {
        if (!gameState.stats[character]) {
            throw new Error(`Character ${character} not found`);
        }

        gameState.stats[character].location = location;
        gameState.stats[character].npcsPresent = [];
        
        return gameState;
    }

    switchTurn(gameState) {
        gameState.turnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
        return gameState;
    }

    getCharacterStats(gameState, character) {
        if (!gameState.stats[character]) {
            throw new Error(`Character ${character} not found`);
        }
        return gameState.stats[character];
    }

    updateCharacterStat(gameState, character, statName, value) {
        if (!gameState.stats[character]) {
            throw new Error(`Character ${character} not found`);
        }
        
        gameState.stats[character][statName] = value;
        return gameState;
    }

    addToInventory(gameState, character, item) {
        if (!gameState.stats[character]) {
            throw new Error(`Character ${character} not found`);
        }

        const inventory = gameState.stats[character].inventory;
        if (inventory.length >= gameConfig.MAX_INVENTORY_SIZE) {
            throw new Error('Inventory is full');
        }

        inventory.push(item);
        return gameState;
    }

    removeFromInventory(gameState, character, item) {
        if (!gameState.stats[character]) {
            throw new Error(`Character ${character} not found`);
        }

        const inventory = gameState.stats[character].inventory;
        const index = inventory.indexOf(item);
        
        if (index > -1) {
            inventory.splice(index, 1);
            return true;
        }
        
        return false;
    }

    getGameSummary(gameState) {
        return {
            roomId: gameState.roomId,
            currentScene: gameState.currentScene,
            turnOrder: gameState.turnOrder,
            chapter: gameState.chapter,
            playersConnected: {
                princess: !!gameState.players.princess,
                helper: !!gameState.players.helper
            },
            locations: {
                princess: gameState.stats.princess.location,
                helper: gameState.stats.helper.location
            },
            activeQuests: {
                princess: gameState.quests.princess.active?.title || null,
                helper: gameState.quests.helper.active?.title || null
            }
        };
    }
}

module.exports = GameStateManager;