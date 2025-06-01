/**
 * Choice building utility extracted from CoopGameLogic
 * Handles building different types of choices for characters
 */

const { CHARACTER_NAMES } = require('../../games/pulpulak/data/constants');

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
     * Build choices data for both characters
     */
    static buildChoicesData(gameState, sceneData, getChoicesForCharacterFn) {
        return {
            princess: getChoicesForCharacterFn(gameState, 'princess', sceneData),
            helper: getChoicesForCharacterFn(gameState, 'helper', sceneData)
        };
    }

    /**
     * Build locations data for both characters
     */
    static buildLocationsData(gameState, LocationData) {
        return {
            princess: LocationData.getLocationInfo(gameState.stats.princess.location),
            helper: LocationData.getLocationInfo(gameState.stats.helper.location)
        };
    }

    /**
     * Build dialogues data for both characters
     */
    static buildDialoguesData(gameState) {
        return {
            princess: gameState.npcDialogues?.princess || null,
            helper: gameState.npcDialogues?.helper || null
        };
    }

    /**
     * Build quests data for both characters
     */
    static buildQuestsData(gameState) {
        return {
            princess: {
                active: gameState.quests.princess.active,
                completed: gameState.quests.princess.completed.length
            },
            helper: {
                active: gameState.quests.helper.active,
                completed: gameState.quests.helper.completed.length
            }
        };
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
    static getCharacterName(character) {
        return CHARACTER_NAMES[character] || character;
    }
}

module.exports = ChoiceBuilder;