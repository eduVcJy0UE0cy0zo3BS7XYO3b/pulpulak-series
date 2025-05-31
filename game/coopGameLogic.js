const CoopStoryData = require('./data/coopStoryDataSCM');
const LocationData = require('./data/locationDataSCM');
const NPCData = require('./data/npcDataSCM');
const { OUTFIT_NAMES, CHARACTER_NAMES, CHARACTER_ROLES } = require('./constants');

const LobbyLogic = require('./modules/lobbyLogic');
const OutfitSystem = require('./modules/outfitSystem');
const SceneHandler = require('./modules/sceneHandler');
const ChoiceHandler = require('./modules/choiceHandler');

class CoopGameLogic {
    constructor() {
        this.sceneHandler = new SceneHandler(LocationData, NPCData);
        this.lobbyLogic = new LobbyLogic(this.sceneHandler.getNPCsForLocation.bind(this.sceneHandler));
        this.outfitSystem = new OutfitSystem();
        this.choiceHandler = new ChoiceHandler(this.sceneHandler);
    }

    startGame(roomId, players) {
        const gameState = this.lobbyLogic.startGame(roomId, players);
        return this.getGameData(roomId);
    }

    createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter) {
        const gameState = this.lobbyLogic.getGameState(roomId);
        return this.outfitSystem.createOutfitSwapRequest(roomId, fromPlayerId, fromCharacter, gameState);
    }

    respondToOutfitSwapRequest(roomId, playerId, accepted) {
        const gameState = this.lobbyLogic.getGameState(roomId);
        return this.outfitSystem.respondToOutfitSwapRequest(roomId, playerId, accepted, gameState);
    }

    makeChoice(roomId, playerId, choiceId, character) {
        try {
            const gameState = this.lobbyLogic.getGameState(roomId);
            if (!gameState) {
                return { success: false, message: "Игра не найдена" };
            }

            const playerCharacter = gameState.players[character];
            if (!playerCharacter || playerCharacter.id !== playerId) {
                return { success: false, message: "Вы управляете другим персонажем" };
            }

            const isMovement = choiceId.startsWith('move_to_');
            const isNPCInteraction = choiceId.startsWith('talk_to_');
            if (!isMovement && !isNPCInteraction && choiceId !== 'request_outfit_swap' && gameState.turnOrder !== character) {
                return { success: false, message: "Сейчас не ваш ход" };
            }

            const result = this.choiceHandler.makeChoice(gameState, choiceId, character);
            if (result.success) {
                if (choiceId.startsWith('move_to_')) {
                    this.outfitSystem.cancelOutfitRequest(roomId);
                } else if (!isMovement && !isNPCInteraction && choiceId !== 'request_outfit_swap') {
                    this.lobbyLogic.switchTurn(gameState);
                    this.outfitSystem.cancelOutfitRequest(roomId);
                }

                return {
                    success: true,
                    gameData: this.getGameData(roomId),
                    message: result.message
                };
            }

            return result;
        } catch (error) {
            console.error('Ошибка при обработке выбора:', error);
            return { 
                success: false, 
                message: `Ошибка при выполнении действия: ${error.message}` 
            };
        }
    }

    processNPCDialogueChoice(roomId, playerId, choiceId, character) {
        const gameState = this.lobbyLogic.getGameState(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        const playerCharacter = gameState.players[character];
        if (!playerCharacter || playerCharacter.id !== playerId) {
            return { success: false, message: "Вы управляете другим персонажем" };
        }

        const questIntegration = this.lobbyLogic.getQuestIntegration(roomId);
        const result = this.choiceHandler.processNPCDialogueChoice(gameState, choiceId, character, questIntegration);
        
        if (result.success && !result.hasFollowUp) {
            this.lobbyLogic.switchTurn(gameState);
        }

        return result;
    }

    closeNPCDialogue(roomId, playerId) {
        const gameState = this.lobbyLogic.getGameState(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

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

        return this.choiceHandler.closeNPCDialogue(gameState, character);
    }

    getGameData(roomId) {
        const gameState = this.lobbyLogic.getGameState(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        const princessLocationInfo = LocationData.getLocationInfo(gameState.stats.princess.location);
        const helperLocationInfo = LocationData.getLocationInfo(gameState.stats.helper.location);
        
        const gameData = {
            roomId: roomId,
            players: gameState.players,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            choices: {
                princess: this.choiceHandler.getChoicesForCharacter(gameState, 'princess', sceneData, this.outfitSystem),
                helper: this.choiceHandler.getChoicesForCharacter(gameState, 'helper', sceneData, this.outfitSystem)
            },
            stats: JSON.parse(JSON.stringify(gameState.stats)),
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            locations: {
                princess: princessLocationInfo,
                helper: helperLocationInfo
            },
            activeOutfitRequest: this.outfitSystem.getActiveOutfitRequest(roomId),
            npcDialogues: {
                princess: gameState.npcDialogues?.princess || null,
                helper: gameState.npcDialogues?.helper || null
            },
            quests: {
                princess: {
                    active: gameState.quests.princess.active,
                    completed: gameState.quests.princess.completed.length
                },
                helper: {
                    active: gameState.quests.helper.active,
                    completed: gameState.quests.helper.completed.length
                }
            }
        };

        return gameData;
    }

    getActiveOutfitRequest(roomId) {
        return this.outfitSystem.getActiveOutfitRequest(roomId);
    }

    cancelOutfitRequest(roomId) {
        this.outfitSystem.cancelOutfitRequest(roomId);
    }

    removeGame(roomId) {
        this.lobbyLogic.removeGame(roomId);
        this.outfitSystem.cancelOutfitRequest(roomId);
    }

    generateRequestId() {
        return this.outfitSystem.generateRequestId();
    }

    getCharacterName(character) {
        return CHARACTER_NAMES[character] || character;
    }

    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
    }

    getNPCsForLocation(location, gameState = null, character = null) {
        return this.sceneHandler.getNPCsForLocation(location, gameState, character);
    }
}

module.exports = CoopGameLogic;