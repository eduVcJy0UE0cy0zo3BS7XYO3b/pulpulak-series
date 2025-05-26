const CoopStoryData = require('./coopStoryData');

class CoopGameLogic {
    constructor() {
        this.games = new Map(); // roomId -> gameState
    }

    startGame(roomId, players) {
	const gameState = {
            roomId: roomId,
            players: players,
            currentScene: 'coop_awakening',
            turnOrder: 'princess',
            chapter: 1,
            location: 'princess_chamber',
            npcsPresent: [], 
            stats: {
		princess: {
                    outfit: 'nightgown'
		},
		helper: {
                    outfit: 'common_dress'
		}
            },
            flags: {}
	};

	gameState.npcsPresent = this.getNPCsForLocation(gameState.location);
	this.games.set(roomId, gameState);
	
	console.log('🎮 Игра создана с состоянием:', {
            stats: gameState.stats,
            helperOutfit: gameState.stats.helper.outfit
	});
	
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
            
            // При смене сцены обновляем локацию если она указана
            const newSceneData = CoopStoryData.getScene(choice.nextScene);
            if (newSceneData.location) {
		gameState.location = newSceneData.location;
		gameState.npcsPresent = this.getNPCsForLocation(newSceneData.location);
            }
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

	// Меняем наряды местами
	const { princess, helper } = gameState.stats;
	[princess.outfit, helper.outfit] = [helper.outfit, princess.outfit];

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
	const canSwitch = gameState.npcsPresent.length === 0;
	console.log('🔍 Проверка смены одежды:', {
            location: gameState.location,
            npcsPresent: gameState.npcsPresent,
            canSwitch: canSwitch,
            currentScene: gameState.currentScene
	});
	return canSwitch;
    }

    applyEffects(gameState, effects, character) {
	Object.keys(effects).forEach(effect => {
            const value = effects[effect];
            
            switch (effect) {
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
		gameState.npcsPresent = this.getNPCsForLocation(value);
		break;
            }
	});
    }

    getNPCsForLocation(location) {
	const locationNPCs = {
            'princess_chamber': [], // спальня - наедине
            'private_quarters': [], // личные покои - наедине  
            'secret_passage': [], // тайный проход - наедине
            'abandoned_tower': [], // заброшенная башня - наедине
            'throne_room': ['guards', 'courtiers'],
            'kitchen': ['cook', 'servants'],
            'garden': ['gardener'],
            'armory': ['guard_captain'],
            'village_square': ['villagers', 'merchants'],
            'great_hall': ['nobles', 'servants']
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
	
	// Создаем глубокую копию stats
	const deepCopyStats = JSON.parse(JSON.stringify(gameState.stats));
	
	const gameData = {
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
            stats: deepCopyStats, // Используем глубокую копию
            currentTurn: gameState.turnOrder,
            chapter: gameState.chapter,
            location: gameState.location,
            npcsPresent: gameState.npcsPresent
	};

	// Добавляем отладочную информацию
	console.log('📊 getGameData создал данные:', {
            original: gameState.stats,
            copied: deepCopyStats,
            helperOutfit: deepCopyStats?.helper?.outfit,
            princessOutfit: deepCopyStats?.princess?.outfit
	});

	return gameData;
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
