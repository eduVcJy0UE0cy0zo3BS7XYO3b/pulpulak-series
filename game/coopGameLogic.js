const CoopStoryData = require('./coopStoryData');

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
    }

    startGame(roomId, players) {
	const gameData = {
	    roomId: data.roomId,
	    players: room.players,
	    scene: {
		title: "Утреннее пробуждение",
		text: `Утренний свет пробивается сквозь тяжелые шторы княжеской спальни. 
        
        Княжна просыпается и видит рядом незнакомую девушку, очень похожую на неё. 
        Девушка представляется сестрой и предлагает завтрак.
        
        💡 <strong>В комнате никого нет кроме вас двоих! Можно поменяться одеждой.</strong>`
	    },
	    choices: {
		princess: [
		    {
			id: "princess_greet",
			text: "Поприветствовать",
			description: "Тепло поздороваться с 'сестрой'"
		    },
		    {
			id: "princess_suspicious",
			text: "Подозрительно осмотреться",
			description: "Что-то кажется странным..."
		    }
		],
		helper: [
		    {
			id: "helper_explain",
			text: "Объяснить ситуацию",
			description: "Рассказать про родителей и войну"
		    },
		    {
			id: "helper_magic",
			text: "Использовать магию",
			description: "Активировать магические серьги"
		    }
		]
	    },
	    stats: {
		princess: {
		    outfit: 'nightgown', // ночная рубашка
		    loyalty: 50
		},
		helper: {
		    outfit: 'common_dress', // простое платье  
		    trustLevel: 75
		}
	    },
	    currentTurn: 'princess',
	    chapter: 1,
	    location: 'princess_chamber',
	    npcsPresent: [] // никого нет - можно менять одежду
	};

        this.games.set(roomId, gameState);
        return this.getGameData(roomId);
    }

    makeChoice(roomId, playerId, choiceId, character) {
        const gameState = this.games.get(roomId);
        if (!gameState) {
            return { success: false, message: "Игра не найдена" };
        }

        const playerRole = this.getPlayerRole(gameState, playerId);
        if (playerRole !== character) {
            return { success: false, message: "Вы не можете управлять этим персонажем" };
        }

        const result = this.processChoice(gameState, choiceId, character);
        if (result.success) {
            return {
                success: true,
                gameData: this.getGameData(roomId),
                message: result.message
            };
        }

        return result;
    }

    processChoice(gameState, choiceId, character) {
        // Обработка смены одежды
        if (choiceId === 'switch_outfits') {
            return this.handleOutfitSwitch(gameState, character);
        }

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        const choice = sceneData.choices[character]?.find(c => c.id === choiceId);
        
        if (!choice) {
            return { success: false, message: "Неверный выбор" };
        }

        // Применяем эффекты выбора
        if (choice.effects) {
            this.applyEffects(gameState, choice.effects, character);
        }

        // Проверяем, меняется ли сцена
        if (choice.nextScene) {
            gameState.currentScene = choice.nextScene;
        }

        // Меняем очередь хода
        this.switchTurn(gameState);

        return { 
            success: true, 
            message: choice.resultText || "Выбор сделан"
        };
    }

    handleOutfitSwitch(gameState, character) {
	if (!this.canSwitchOutfits(gameState)) {
            return { 
		success: false, 
		message: "Нельзя переодеваться при посторонних!" 
            };
	}

	// Меняем наряды местами используя деструктуризацию
	const { princess, helper } = gameState.stats;
	[princess.outfit, helper.outfit] = [helper.outfit, princess.outfit];

	// Небольшое повышение доверия за совместное действие
	helper.trustLevel = Math.min(100, helper.trustLevel + 5);
	princess.loyalty = Math.min(100, princess.loyalty + 5);

	// НЕ меняем очередь хода для специальных действий
	// this.switchTurn(gameState); - убираем эту строку

	const characterNames = {
            'princess': 'Княжна',
            'helper': 'Помощница'
	};

	return {
            success: true,
            message: `${characterNames[character]} предложила поменяться одеждой. Теперь они выглядят по-другому! Княжна носит ${this.getOutfitName(princess.outfit)}, а помощница - ${this.getOutfitName(helper.outfit)}.`
	};
    }

    canSwitchOutfits(gameState) {
	// Можно менять одежду только когда на локации никого нет кроме героинь
	console.log('🔍 Проверка смены одежды:', {
            location: gameState.location,
            npcsPresent: gameState.npcsPresent,
            canSwitch: gameState.npcsPresent.length === 0
	});
	return gameState.npcsPresent.length === 0;
    }

    applyEffects(gameState, effects, character) {
        Object.keys(effects).forEach(effect => {
            const value = effects[effect];
            
            switch (effect) {
            case 'loyalty':
                gameState.stats.princess.loyalty = Math.max(0, Math.min(100, 
									gameState.stats.princess.loyalty + value));
                break;
            case 'trustLevel':
                gameState.stats.helper.trustLevel = Math.max(0, Math.min(100, 
									 gameState.stats.helper.trustLevel + value));
                break;
            case 'outfit':
                if (character === 'princess') {
                    gameState.stats.princess.outfit = value;
                } else if (character === 'helper') {
                    gameState.stats.helper.outfit = value;
                }
                break;
            case 'flag':
                gameState.flags[value] = true;
                break;
            case 'location':
                gameState.location = value;
                // При смене локации обновляем список NPC
                gameState.npcsPresent = this.getNPCsForLocation(value);
                break;
            }
        });
    }

    getNPCsForLocation(location) {
        const locationNPCs = {
            'princess_chamber': [], // только княжна и помощница
            'throne_room': ['guards', 'courtiers'],
            'kitchen': ['cook', 'servants'],
            'garden': ['gardener'],
            'armory': ['guard_captain'],
            'village_square': ['villagers', 'merchants']
        };
        
        return locationNPCs[location] || [];
    }

    switchTurn(gameState) {
        gameState.turnOrder = gameState.turnOrder === 'princess' ? 'helper' : 'princess';
    }

    getGameData(roomId) {
        const gameState = this.games.get(roomId);
        if (!gameState) return null;

        const sceneData = CoopStoryData.getScene(gameState.currentScene);
        
        return {
            roomId: roomId,
            players: gameState.players,
            scene: {
                title: sceneData.title,
                text: sceneData.text
            },
            choices: {
                princess: this.getChoicesForCharacter(gameState, 'princess', sceneData),
                helper: this.getChoicesForCharacter(gameState, 'helper', sceneData)
            },
            stats: gameState.stats,
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            location: gameState.location,
            npcsPresent: gameState.npcsPresent
        };
    }

    getChoicesForCharacter(gameState, character, sceneData) {
	let choices = [];
	
	// Основные выборы сцены (только для игрока, чей ход)
	if (gameState.turnOrder === character) {
            choices = sceneData.choices[character] || [];
            choices = choices.filter(choice => {
		return this.isChoiceAvailable(choice, gameState, character);
            });
	}

	// Смена одежды доступна ВСЕГДА, если персонажи наедине
	if (this.canSwitchOutfits(gameState)) {
            choices.push({
		id: 'switch_outfits',
		text: '👗 Поменяться одеждой',
		description: `${character === 'princess' ? 'Княжна' : 'Помощница'} сейчас в: ${this.getOutfitName(gameState.stats[character].outfit)}`,
		isOutfitChange: true
            });
	}

	return choices;
    }

    getOutfitName(outfitId) {
        const outfitNames = {
            'nightgown': 'Ночная рубашка',
            'princess_dress': 'Княжеское платье',
            'common_dress': 'Простое платье',
            'court_dress': 'Придворное платье'
        };
        return outfitNames[outfitId] || outfitId;
    }

    isChoiceAvailable(choice, gameState, character) {
        if (!choice.requirements) return true;

        return choice.requirements.every(req => {
            switch (req.type) {
            case 'outfit':
                return gameState.stats[character].outfit === req.value;
            case 'loyalty':
                return gameState.stats.princess.loyalty >= req.value;
            case 'trustLevel':
                return gameState.stats.helper.trustLevel >= req.value;
            case 'flag':
                return gameState.flags[req.value];
            case 'npcsAbsent':
                return gameState.npcsPresent.length === 0;
            default:
                return true;
            }
        });
    }

    getPlayerRole(gameState, playerId) {
        if (gameState.players.princess?.id === playerId) {
            return 'princess';
        }
        if (gameState.players.helper?.id === playerId) {
            return 'helper';
        }
        return null;
    }

    removeGame(roomId) {
        this.games.delete(roomId);
    }
}

module.exports = CoopGameLogic;
