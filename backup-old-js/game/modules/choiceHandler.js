const CoopStoryData = require('../data/coopStoryDataSCM');
const LocationData = require('../data/locationDataSCM');
const NPCData = require('../data/npcDataSCM');

class ChoiceHandler {
    constructor(sceneHandler) {
        this.sceneHandler = sceneHandler;
    }

    getChoicesForCharacter(gameState, character, sceneData, outfitSystem) {
        let choices = [];
        
        if (gameState.turnOrder === character) {
            choices = sceneData.choices[character] || [];
            choices = choices.filter(choice => {
                return this.isChoiceAvailable(choice, gameState, character);
            });
        }

        if (outfitSystem.canSwitchOutfits(gameState, character) && !outfitSystem.getActiveOutfitRequest(gameState.roomId)) {
            const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
            choices.push({
                id: 'request_outfit_swap',
                text: '👗 Предложить поменяться одеждой',
                description: `Предложить ${otherCharacter} поменяться нарядами`,
                isOutfitRequest: true
            });
        }

        const movementChoices = this.getMovementChoices(gameState, character);
        choices.push(...movementChoices);

        const npcChoices = this.getNPCInteractionChoices(gameState, character);
        choices.push(...npcChoices);

        return choices;
    }

    makeChoice(gameState, choiceId, character) {
        try {
            if (choiceId === 'request_outfit_swap') {
                return { 
                    success: false, 
                    message: "Используйте отдельный обработчик для запросов обмена одеждой" 
                };
            }

            if (choiceId.startsWith('move_to_')) {
                const targetLocation = choiceId.replace('move_to_', '');
                return this.processMovement(gameState, targetLocation, character);
            }

            if (choiceId.startsWith('talk_to_')) {
                const npcId = choiceId.replace('talk_to_', '');
                return this.processNPCInteraction(gameState, npcId, character);
            }

            const sceneData = CoopStoryData.getScene(gameState.currentScene);
            const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
            
            if (!choice) {
                return { success: false, message: "Неверный выбор" };
            }

            if (choice.effects) {
                this.applyEffects(gameState, choice.effects, character);
            }

            if (choice.nextScene) {
                gameState.currentScene = choice.nextScene;
            }

            return { 
                success: true, 
                message: choice.resultText || "Выбор сделан"
            };
        } catch (error) {
            console.error('Ошибка при обработке выбора:', error);
            return { 
                success: false, 
                message: `Ошибка при выполнении действия: ${error.message}` 
            };
        }
    }

    isChoiceAvailable(choice, gameState, character) {
        return true;
    }

    applyEffects(gameState, effects, character) {
        if (effects.outfit) {
            gameState.stats[character].outfit = effects.outfit;
        }
        if (effects.location) {
            gameState.stats[character].location = effects.location;
            gameState.stats[character].npcsPresent = this.sceneHandler.getNPCsForLocation(effects.location, gameState, character);
        }
        if (effects.awareness) {
            gameState.stats[character].awareness += effects.awareness;
        }
    }

    getMovementChoices(gameState, character) {
        const currentLocation = gameState.stats[character].location;
        const locationInfo = LocationData.getLocationInfo(currentLocation);
        
        if (!locationInfo) return [];
        
        const choices = [];
        
        locationInfo.connections.forEach(connection => {
            choices.push({
                id: `move_to_${connection.id}`,
                text: `${connection.icon} Перейти: ${connection.name}`,
                description: `Отправиться в ${connection.name}`,
                isMovement: true,
                targetLocation: connection.id
            });
        });
        
        return choices;
    }

    processMovement(gameState, targetLocation, character) {
        const characterStats = gameState.stats[character];
        
        const currentConnections = LocationData.getConnections(characterStats.location);
        if (!currentConnections.includes(targetLocation)) {
            return { 
                success: false, 
                message: "Вы не можете попасть туда отсюда" 
            };
        }

        const locationInfo = LocationData.getLocation(targetLocation);
        if (!locationInfo) {
            return { 
                success: false, 
                message: "Неизвестная локация" 
            };
        }

        characterStats.location = targetLocation;
        characterStats.npcsPresent = this.sceneHandler.getNPCsForLocation(targetLocation, gameState, character);

        return { 
            success: true, 
            message: `${character === 'princess' ? 'Княжна' : 'Помощница'} переместилась в ${locationInfo.name}`
        };
    }

    getNPCInteractionChoices(gameState, character) {
        const choices = [];
        const characterLocation = gameState.stats[character].location;
        const npcs = NPCData.getNPCsForLocation(characterLocation, gameState, character);
        
        npcs.forEach(npc => {
            choices.push({
                id: `talk_to_${npc.id}`,
                text: `💬 Поговорить с ${npc.name}`,
                description: npc.description,
                isNPCInteraction: true,
                npcId: npc.id
            });
        });
        
        return choices;
    }

    processNPCInteraction(gameState, npcId, character) {
        try {
            const npc = NPCData.getNPC(npcId);
            if (!npc) {
                return { success: false, message: "NPC не найден" };
            }

            const outfit = gameState.stats[character].outfit;
            
            if (!gameState.npcMemory[character]) {
                gameState.npcMemory[character] = {};
            }
            if (!gameState.npcMemory[character][npcId]) {
                gameState.npcMemory[character][npcId] = {};
            }
            const npcMemory = gameState.npcMemory[character][npcId];
            
            const currentLocation = gameState.stats[character].location;
            const questState = gameState.quests[character];
            const globalQuestMemory = gameState.globalQuestMemory;
            const dialogue = NPCData.getNPCDialogue(npcId, outfit, npcMemory, currentLocation, questState, globalQuestMemory, gameState, character);
            if (!dialogue) {
                return { success: false, message: "Диалог не найден" };
            }

            gameState.npcDialogues[character] = {
                npcId: npcId,
                npcName: npc.name,
                greeting: dialogue.greeting,
                choices: dialogue.choices,
                attitude: NPCData.getNPCAttitude(npcId, outfit),
                activeCharacter: character,
                isFollowUp: false
            };

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

    processNPCDialogueChoice(gameState, choiceId, character, questIntegration) {
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        const npcId = gameState.npcDialogues[character].npcId;
        const outfit = gameState.stats[character].outfit;

        if (!gameState.npcMemory[character]) {
            gameState.npcMemory[character] = {};
        }
        if (!gameState.npcMemory[character][npcId]) {
            gameState.npcMemory[character][npcId] = {};
        }

        const isFollowUp = gameState.npcDialogues[character].isFollowUp || false;
        const currentChoices = isFollowUp ? gameState.npcDialogues[character].choices : [];
        
        const result = NPCData.processDialogueChoice(
            npcId, 
            choiceId, 
            outfit, 
            gameState.npcMemory[character][npcId],
            isFollowUp,
            currentChoices,
            gameState.stats[character].location,
            gameState,
            character
        );
        if (!result) {
            return { success: false, message: "Неверный выбор" };
        }

        gameState.npcMemory[character][npcId] = result.updatedMemory;

        if (result.effects) {
            if (result.effects.item) {
                gameState.stats[character].inventory.push(result.effects.item);
            }
            if (result.effects.info) {
                gameState.stats[character][result.effects.info] = true;
            }
        }

        if (questIntegration) {
            this.processQuestAction(gameState, character, choiceId, result, questIntegration);
        }

        const attitude = gameState.npcDialogues[character]?.attitude;

        if (result.next_choices && result.next_choices.length > 0) {
            gameState.npcDialogues[character].choices = result.next_choices;
            gameState.npcDialogues[character].greeting = result.response;
            gameState.npcDialogues[character].isFollowUp = true;

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success',
                hasFollowUp: true
            };
        } else {
            gameState.npcDialogues[character] = null;

            return { 
                success: true, 
                message: result.response,
                type: attitude === 'hostile' ? 'warning' : 'success'
            };
        }
    }

    closeNPCDialogue(gameState, character) {
        if (!gameState.npcDialogues[character]) {
            return { success: false, message: "Нет активного диалога" };
        }

        gameState.npcDialogues[character] = null;

        return { 
            success: true, 
            message: "Диалог закрыт"
        };
    }

    processQuestAction(gameState, character, choiceId, dialogueResult, questIntegration) {
        // Quest actions are now handled by the S-expression quest engine directly
        // No hardcoded quest action processing needed
    }

    processChoice(gameState, choiceId, character) {
        if (choiceId === 'request_outfit_swap') {
            return { 
                success: false, 
                message: "Используйте отдельный обработчик для запросов обмена одеждой" 
            };
        }

        return this.makeChoice(gameState, choiceId, character);
    }
}

module.exports = ChoiceHandler;