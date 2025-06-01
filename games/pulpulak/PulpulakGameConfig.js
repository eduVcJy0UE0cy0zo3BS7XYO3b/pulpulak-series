const GameConfigInterface = require('../../engine/interfaces/GameConfig');

// Import game data from local data folder
const CoopStoryData = require('./data/coopStoryData');
const LocationData = require('./data/locationData');
const NPCData = require('./data/npcData');
const QuestData = require('./data/questData');
const { OUTFIT_NAMES, CHARACTER_NAMES } = require('./data/constants');
const PulpulakRequestHandlers = require('./requestHandlers');
const PulpulakOutfitLogic = require('./data/outfitLogic');

/**
 * Configuration for the Pulpulak cooperative adventure game
 */
class PulpulakGameConfig extends GameConfigInterface {
    constructor() {
        super();
        
        // Basic game info
        this.gameId = 'pulpulak_coop';
        this.gameName = 'Pulpulak Cooperative Adventure';
        this.gameVersion = '1.0.0';
        this.maxPlayers = 2;
        
        // Characters
        this.characters = {
            princess: {
                name: 'Княжна',
                description: 'Молодая знатная девушка'
            },
            helper: {
                name: 'Помощница',
                description: 'Простая девушка, помогающая княжне'
            }
        };
        
        // Outfits system
        this.outfits = {
            princess_dress: {
                name: 'Княжеское платье',
                type: 'noble',
                description: 'Богатое платье знатной особы'
            },
            common_dress: {
                name: 'Простое платье',
                type: 'common', 
                description: 'Обычная одежда простолюдинки'
            }
        };
        
        // Story scenes
        this.scenes = this.loadScenes();
        this.startingScene = 'coop_awakening';
        
        // World data
        this.locations = this.loadLocations();
        this.npcs = this.loadNPCs();
        this.quests = this.loadQuests();
        
        // Game features
        this.features = {
            ...this.features, // наследуем базовые features
            outfitSwapping: true,
            dynamicNPCMovement: true
        };
        
        // Initial state
        this.initialState = {
            startingLocation: 'princess_chamber',
            startingOutfits: {
                princess: 'princess_dress',
                helper: 'common_dress'
            },
            startingItems: {
                princess: [],
                helper: ['silver_earrings', 'family_medallion']
            },
            globalMemory: {}
        };
    }

    loadScenes() {
        // Use real game scenes
        const scenes = {};
        const allSceneIds = CoopStoryData.getAllScenes();
        
        allSceneIds.forEach(sceneId => {
            scenes[sceneId] = CoopStoryData.getScene(sceneId);
        });
        
        return scenes;
    }

    loadLocations() {
        // Convert real location data to our format
        const locations = {};
        const locationIds = LocationData.getAllLocations();
        
        locationIds.forEach(locationId => {
            const locationInfo = LocationData.getLocationInfo(locationId);
            if (locationInfo) {
                locations[locationId] = {
                    name: locationInfo.name,
                    description: locationInfo.description,
                    connections: locationInfo.connections.map(conn => conn.id),
                    canChangeOutfit: LocationData.canChangeOutfit(locationId),
                    icon: locationInfo.icon,
                    npcs: this.getBaseNPCsForLocation(locationId)
                };
            }
        });
        
        return locations;
    }

    loadNPCs() {
        // Extract NPCs from the real NPCData  
        const npcs = {};
        
        // Get all NPC IDs by checking what locations have NPCs
        const allNPCIds = new Set();
        const locationIds = LocationData.getAllLocations();
        
        locationIds.forEach(locationId => {
            const locationNPCs = this.getBaseNPCsForLocation(locationId);
            locationNPCs.forEach(npcId => allNPCIds.add(npcId));
        });
        
        // Load each NPC
        allNPCIds.forEach(npcId => {
            const npc = NPCData.getNPC(npcId);
            if (npc) {
                npcs[npcId] = {
                    id: npc.id,
                    name: npc.name,
                    description: npc.description,
                    likesNoble: npc.likesNoble,
                    dialogue: npc.dialogue
                };
            }
        });
        
        return npcs;
    }

    loadQuests() {
        // Load real quest data
        const quests = {};
        const allQuests = QuestData.getAllQuests();
        
        Object.keys(allQuests).forEach(questId => {
            quests[questId] = allQuests[questId];
        });
        
        return quests;
    }

    // Helper method to get base NPCs for location
    getBaseNPCsForLocation(locationId) {
        // Use the real NPCData to get base locations
        try {
            const baseNPCs = NPCData.getNPCsForLocation(locationId);
            return baseNPCs.map(npc => npc.id);
        } catch (error) {
            // Fallback to hardcoded list for the main locations
            const baseLocations = {
                'princess_chamber': [],
                'throne_room': ['royal_advisor', 'guard_captain'],
                'kitchen': ['cook'],
                'garden': ['herbalist'],
                'secret_garden': ['gardener'],
                'armory': ['weaponsmith'],
                'chapel': ['priest'],
                'library': ['librarian'],
                'secret_archive': [],
                'greenhouse': []
            };
            return baseLocations[locationId] || [];
        }
    }

    // Override to implement dynamic NPC movement using real NPCData
    getNPCsForLocation(locationId, gameState = null, character = null) {
        // Use the real NPCData system
        return NPCData.getNPCsForLocation(locationId, gameState, character);
    }

    // Метод для регистрации игровых обработчиков запросов
    registerRequestHandlers(requestManager) {
        if (PulpulakRequestHandlers && typeof PulpulakRequestHandlers.registerHandlers === 'function') {
            PulpulakRequestHandlers.registerHandlers(requestManager);
            console.log('🎮 Registered Pulpulak request handlers');
        }
    }

    // Проверить, можно ли переодеваться (делегируется игре)
    canSwitchOutfits(gameState, character) {
        return PulpulakOutfitLogic.canSwitchOutfits(gameState, character);
    }

    // Проверить, включен ли обмен одеждой
    isOutfitSwappingEnabled() {
        return this.features.outfitSwapping;
    }

    // Получить все доступные наряды
    getOutfits() {
        return this.outfits;
    }

    // === УНИВЕРСАЛЬНАЯ СИСТЕМА ЗАПРОСОВ ===
    
    // Получить динамические выборы для персонажа
    getDynamicChoices(gameState, character) {
        const choices = [];
        
        // Добавляем выбор обмена одеждой если доступен
        if (this.isOutfitSwappingEnabled() && PulpulakOutfitLogic.canRequestOutfitSwap(gameState, character, false)) {
            choices.push(PulpulakOutfitLogic.createOutfitSwapChoice(character));
        }
        
        return choices;
    }
    
    // Проверить, является ли выбор запросом
    isRequestChoice(choiceId) {
        return choiceId === 'request_outfit_swap';
    }
    
    // Получить тип запроса из выбора
    getRequestTypeFromChoice(choiceId) {
        if (choiceId === 'request_outfit_swap') {
            return 'outfit_swap';
        }
        return null;
    }
    
    // Проверить, можно ли создать запрос
    canCreateRequest(gameState, requestType, character, requestData) {
        if (requestType === 'outfit_swap') {
            const canRequest = PulpulakOutfitLogic.canRequestOutfitSwap(gameState, character, false);
            return {
                allowed: canRequest,
                reason: canRequest ? null : 'Cannot swap outfits here'
            };
        }
        
        return { allowed: false, reason: `Unknown request type: ${requestType}` };
    }
    
    // Выполнить запрос
    executeRequest(gameState, request, responseData) {
        if (request.type === 'outfit_swap') {
            const updatedGameState = PulpulakOutfitLogic.executeOutfitSwap(gameState);
            return {
                success: true,
                gameState: updatedGameState,
                message: 'Outfits swapped successfully'
            };
        }
        
        return {
            success: false,
            message: `Unknown request type: ${request.type}`
        };
    }
    
    // === ОБРАТНАЯ СОВМЕСТИМОСТЬ ===
    
    // Создать выбор для обмена одеждой (для обратной совместимости)
    createOutfitSwapChoice(character) {
        return PulpulakOutfitLogic.createOutfitSwapChoice(character);
    }

    // Выполнить обмен одеждой (для обратной совместимости)
    executeOutfitSwap(gameState) {
        return PulpulakOutfitLogic.executeOutfitSwap(gameState);
    }

    // Проверить, может ли персонаж запросить обмен одеждой (для обратной совместимости)
    canRequestOutfitSwap(gameState, character, hasActiveRequest = false) {
        return PulpulakOutfitLogic.canRequestOutfitSwap(gameState, character, hasActiveRequest);
    }
}

module.exports = PulpulakGameConfig;