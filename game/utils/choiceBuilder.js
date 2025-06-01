/**
 * Choice building utility extracted from CoopGameLogic
 * Handles building different types of choices for characters
 */

class ChoiceBuilder {
    /**
     * Build scene data for client
     */
    static buildSceneData(sceneData) {
        return {
            title: sceneData.title,
            text: sceneData.text
        };
    }

    /**
     * Build choices data for all characters
     */
    static buildChoicesData(gameState, sceneData, getChoicesForCharacterFn, gameConfig) {
        const choices = {};
        const characters = gameConfig ? gameConfig.getCharacters() : ['princess', 'helper'];
        
        characters.forEach(character => {
            choices[character] = getChoicesForCharacterFn(gameState, character, sceneData);
        });
        
        return choices;
    }

    /**
     * Build locations data for all characters
     */
    static buildLocationsData(gameState, LocationData, gameConfig) {
        const locations = {};
        const characters = gameConfig ? gameConfig.getCharacters() : ['princess', 'helper'];
        
        characters.forEach(character => {
            if (gameState.stats[character]) {
                locations[character] = LocationData.getLocationInfo(gameState.stats[character].location);
            }
        });
        
        return locations;
    }

    /**
     * Build dialogues data for all characters
     */
    static buildDialoguesData(gameState, gameConfig) {
        const dialogues = {};
        const characters = gameConfig ? gameConfig.getCharacters() : ['princess', 'helper'];
        
        characters.forEach(character => {
            dialogues[character] = gameState.npcDialogues && gameState.npcDialogues[character] ? gameState.npcDialogues[character] : null;
        });
        
        return dialogues;
    }

    /**
     * Build quests data for all characters
     */
    static buildQuestsData(gameState, gameConfig) {
        const quests = {};
        const characters = gameConfig ? gameConfig.getCharacters() : ['princess', 'helper'];
        
        characters.forEach(character => {
            if (gameState.quests && gameState.quests[character]) {
                quests[character] = {
                    active: gameState.quests[character].active,
                    completed: gameState.quests[character].completed ? gameState.quests[character].completed.length : 0
                };
            }
        });
        
        return quests;
    }

    /**
     * Get scene choices for a character
     */
    static getSceneChoices(gameState, character, sceneData, isChoiceAvailableFn) {
        if (gameState.turnOrder !== character) {
            return [];
        }
        
        const choices = sceneData.choices[character] || [];
        return choices.filter(choice => isChoiceAvailableFn(choice, gameState, character));
    }

    /**
     * Get special choices (outfit, movement, NPC interaction)
     */
    static getSpecialChoices(gameState, character, getMovementChoicesFn, getNPCInteractionChoicesFn, getDynamicChoicesFn) {
        const choices = [];
        
        // Получаем динамические выборы из игровой конфигурации
        if (getDynamicChoicesFn) {
            const dynamicChoices = getDynamicChoicesFn(gameState, character);
            choices.push(...dynamicChoices);
        }
        
        // Выборы перемещения
        choices.push(...getMovementChoicesFn(gameState, character));
        
        // Выборы взаимодействия с NPC
        choices.push(...getNPCInteractionChoicesFn(gameState, character));
        
        return choices;
    }

    /**
     * Create outfit swap choice
     */
    static createOutfitSwapChoice(character, gameConfigCreateFn) {
        // Делегируем создание выбора в конкретную игру через GameConfig
        if (gameConfigCreateFn) {
            return gameConfigCreateFn(character);
        }
        
        // Fallback к стандартному выбору
        const otherCharacter = character === 'princess' ? 'помощнице' : 'княжне';
        return {
            id: 'request_outfit_swap',
            text: '👗 Предложить поменяться одеждой',
            description: `Предложить ${otherCharacter} поменяться нарядами`,
            isOutfitRequest: true
        };
    }

    /**
     * Get movement choices for a character
     */
    static getMovementChoices(gameState, character, LocationData) {
        const currentLocation = gameState.stats[character].location;
        const locationInfo = LocationData.getLocationInfo(currentLocation);
        
        if (!locationInfo) return [];
        
        const choices = [];
        
        // Добавляем кнопки для перехода в соседние локации
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

    /**
     * Get NPC interaction choices for a character
     */
    static getNPCInteractionChoices(gameState, character, NPCData) {
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

    /**
     * Get character name
     */
    static getCharacterName(character, gameConfig) {
        if (gameConfig) {
            const characterNames = gameConfig.getCharacterNames();
            return characterNames[character] || character;
        }
        return character;
    }
}

module.exports = ChoiceBuilder;