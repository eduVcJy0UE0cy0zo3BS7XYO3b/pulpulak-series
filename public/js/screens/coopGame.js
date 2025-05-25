import { loadComponent } from '../utils/helpers.js';

class CoopGame {
    constructor(app) {
        this.app = app;
        this.element = null;
        this.gameData = null;
        this.playerRole = null;
        this.chatVisible = false;
    }

    async show(data) {
        this.gameData = data;
        this.playerRole = this.determinePlayerRole(data);
        
        this.element = await loadComponent('coopGame');
        this.app.container.appendChild(this.element);
        this.setupEventListeners();
        this.updateGame(data);
    }

    hide() {
        if (this.element) {
            this.element.remove();
            this.element = null;
        }
    }

    setupEventListeners() {
        // Покинуть игру
        this.element.querySelector('[data-action="leave-game"]').addEventListener('click', () => {
            if (confirm('Вы уверены, что хотите покинуть игру?')) {
                this.app.leaveRoom(this.gameData.roomId);
                this.app.showScreen('mainMenu');
            }
        });

        // Переключение чата
        this.element.querySelector('[data-action="toggle-chat"]').addEventListener('click', () => {
            this.toggleChat();
        });

        // Отправка сообщения в чат
        this.element.querySelector('[data-action="send-chat"]').addEventListener('click', () => {
            this.sendChatMessage();
        });

        // Enter в чате
        this.element.querySelector('#chat-input').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') {
                this.sendChatMessage();
            }
        });

        // Настройка обработчика чата
        this.app.socketManager.socket.on('chat-message', (data) => {
            this.addChatMessage(data);
        });
    }

    determinePlayerRole(data) {
        const socketId = this.app.socketManager.socket.id;
        if (data.players.princess?.id === socketId) {
            return 'princess';
        } else if (data.players.helper?.id === socketId) {
            return 'helper';
        }
        return null;
    }

    updateGame(data) {
        this.gameData = data;

        // Обновляем ID комнаты
        this.element.querySelector('#current-room-id').textContent = data.roomId;

        // Обновляем заголовок и текст истории
        this.element.querySelector('#scene-title').textContent = data.scene.title;
        this.element.querySelector('#story-text').innerHTML = data.scene.text;

        // Обновляем информацию о локации
        this.updateLocationInfo(data);

        // Обновляем информацию о игроках
        this.updatePlayersInfo(data);

        // Обновляем выборы для каждого персонажа
        this.updateCharacterChoices('princess', data.choices.princess);
        this.updateCharacterChoices('helper', data.choices.helper);
    }

    updateLocationInfo(data) {
        const locationNames = {
            'princess_chamber': 'Спальня княжны',
            'throne_room': 'Тронный зал',
            'kitchen': 'Кухня',
            'garden': 'Сад',
            'armory': 'Арсенал'
        };

        this.element.querySelector('#current-location').textContent = 
            locationNames[data.location] || data.location;

        const npcsElement = this.element.querySelector('#npcs-present');
        if (data.npcsPresent && data.npcsPresent.length > 0) {
            npcsElement.innerHTML = `<br>👥 <em>Присутствуют: ${data.npcsPresent.join(', ')}</em>`;
        } else {
            npcsElement.innerHTML = `<br>🤫 <em>Никого нет поблизости</em>`;
        }
    }

    updatePlayersInfo(data) {
        // Информация о княжне
        this.element.querySelector('#princess-player').textContent = 
            data.players.princess?.name || '-';
        this.element.querySelector('#princess-outfit').textContent = 
            this.getOutfitName(data.stats.princess.outfit);
        this.element.querySelector('#princess-loyalty').textContent = 
            data.stats.princess.loyalty;

        // Информация о помощнице
        this.element.querySelector('#helper-player').textContent = 
            data.players.helper?.name || '-';
        this.element.querySelector('#helper-outfit').textContent = 
            this.getOutfitName(data.stats.helper.outfit);
        this.element.querySelector('#helper-trust').textContent = 
            data.stats.helper.trustLevel;
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

    updateCharacterChoices(character, choices) {
	const choicesDiv = this.element.querySelector(`#${character}-choices`);
	choicesDiv.innerHTML = '';

	const isMyRole = this.playerRole === character;
	const hasChoices = choices && choices.length > 0;

	if (!isMyRole) {
            if (hasChoices) {
		// Показываем только действия смены одежды для "не моего" персонажа
		const outfitChoices = choices.filter(choice => choice.isOutfitChange);
		if (outfitChoices.length > 0) {
                    const header = document.createElement('div');
                    header.className = 'other-player-actions';
                    header.textContent = '🔄 Можете предложить:';
                    choicesDiv.appendChild(header);
                    
                    outfitChoices.forEach(choice => {
			const button = this.createChoiceButton(choice, character);
			choicesDiv.appendChild(button);
                    });
		}
            }
            
            const waitingDiv = document.createElement('div');
            waitingDiv.className = 'waiting-turn';
            waitingDiv.textContent = 'Ход другого игрока...';
            choicesDiv.appendChild(waitingDiv);
            return;
	}

	// Это мой персонаж
	if (!hasChoices) {
            choicesDiv.innerHTML = '<div class="waiting-turn">Ожидание развития сюжета...</div>';
            return;
	}

	// Индикатор хода
	const turnIndicator = document.createElement('div');
	turnIndicator.className = 'turn-indicator';
	turnIndicator.textContent = '🎯 Ваш ход!';
	choicesDiv.appendChild(turnIndicator);

	// Все доступные действия
	choices.forEach(choice => {
            const button = this.createChoiceButton(choice, character);
            choicesDiv.appendChild(button);
	});
    }

    createChoiceButton(choice, character) {
	const button = document.createElement('button');
	button.className = 'choice-button';
	
	if (choice.isOutfitChange) {
            button.classList.add('outfit-switch-btn');
	}
	
	button.innerHTML = `
        <strong>${choice.text}</strong><br>
        <small>${choice.description}</small>
    `;
	
	button.addEventListener('click', () => {
            this.makeChoice(choice.id, character);
	});
	
	if (choice.disabled) {
            button.disabled = true;
            button.innerHTML += `<br><small style="color: #dc3545;">🚫 ${choice.reason}</small>`;
	}
	
	return button;
    }

    createChoiceButton(choice, character) {
	const button = document.createElement('button');
	button.className = 'choice-button';
	
	// Особое оформление для смены одежды
	if (choice.id === 'switch_outfits') {
            button.classList.add('outfit-switch-btn');
	}
	
	button.innerHTML = `
        <strong>${choice.text}</strong><br>
        <small>${choice.description}</small>
    `;
	
	button.addEventListener('click', () => {
            this.makeChoice(choice.id, character);
	});
	
	if (choice.disabled) {
            button.disabled = true;
            button.innerHTML += `<br><small style="color: #dc3545;">🚫 ${choice.reason}</small>`;
	}
	
	return button;
    }

    makeChoice(choiceId, character) {
        console.log(`🎯 Делаем выбор: ${choiceId} для ${character}`);
        this.app.makeChoice(this.gameData.roomId, choiceId, character);

        // Временно блокируем кнопки
        const choicesDiv = this.element.querySelector(`#${character}-choices`);
        const buttons = choicesDiv.querySelectorAll('.choice-button');
        buttons.forEach(btn => {
            btn.disabled = true;
            btn.style.opacity = '0.6';
        });
    }

    toggleChat() {
        this.chatVisible = !this.chatVisible;
        const chatSection = this.element.querySelector('#chat-section');
        
        if (this.chatVisible) {
            chatSection.classList.remove('hidden');
            this.element.querySelector('[data-action="toggle-chat"]').textContent = '💬 Скрыть чат';
        } else {
            chatSection.classList.add('hidden');
            this.element.querySelector('[data-action="toggle-chat"]').textContent = '💬 Чат';
        }
    }

    sendChatMessage() {
        const input = this.element.querySelector('#chat-input');
        const message = input.value.trim();
        
        if (message && this.gameData) {
            this.app.sendChatMessage(this.gameData.roomId, message);
            input.value = '';
        }
    }

    addChatMessage(data) {
        const messagesContainer = this.element.querySelector('#chat-messages');
        const messageDiv = document.createElement('div');
        messageDiv.style.marginBottom = '5px';
        messageDiv.innerHTML = `<strong>${data.playerName}:</strong> ${this.escapeHTML(data.message)}`;
        
        messagesContainer.appendChild(messageDiv);
        messagesContainer.scrollTop = messagesContainer.scrollHeight;
    }

    escapeHTML(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }
}

export default CoopGame;
