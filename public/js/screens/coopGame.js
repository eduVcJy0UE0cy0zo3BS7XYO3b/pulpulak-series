import { loadComponent } from '../utils/helpers.js';

class CoopGame {
    constructor(app) {
        this.app = app;
        this.element = null;
        this.gameData = null;
        this.playerRole = null;
        this.chatVisible = false;
	this.activeOutfitRequest = null; // Добавляем поле для активного запроса
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
	
	this.app.socketManager.socket.on('outfit-request-created', (data) => {
            this.handleOutfitRequestCreated(data);
        });

        this.app.socketManager.socket.on('outfit-request-resolved', (data) => {
            this.handleOutfitRequestResolved(data);
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
        console.log('📊 DEBUG updateGame called with data:', {
            roomId: data.roomId,
            choices: data.choices,
            playerRole: this.playerRole,
            location: data.location,
            npcsPresent: data.npcsPresent
        });
        
        this.gameData = data;
	this.activeOutfitRequest = data.activeOutfitRequest;

        // Обновляем ID комнаты
        this.element.querySelector('#current-room-id').textContent = data.roomId;

        // Обновляем заголовок и текст истории
        this.element.querySelector('#scene-title').textContent = data.scene.title;
        this.element.querySelector('#story-text').innerHTML = data.scene.text;

        // Обновляем информацию о локации
        this.updateLocationInfo(data);

        // Обновляем информацию о игроках
        this.updatePlayersInfo(data);

	this.updateOutfitRequestDisplay();

        // Обновляем выборы для каждого персонажа
        this.updateCharacterChoices('princess', data.choices.princess);
        this.updateCharacterChoices('helper', data.choices.helper);
    }

    updateOutfitRequestDisplay() {
        const requestContainer = this.element.querySelector('#outfit-request-container');
        requestContainer.innerHTML = '';

        if (!this.activeOutfitRequest) {
            return;
        }

        const socketId = this.app.socketManager.socket.id;
        const isTargetPlayer = this.activeOutfitRequest.targetPlayerId === socketId;
        const isRequestInitiator = this.activeOutfitRequest.fromPlayerId === socketId;

        if (isTargetPlayer) {
            // Показываем запрос с кнопками принять/отклонить
            this.showIncomingOutfitRequest();
        } else if (isRequestInitiator) {
            // Показываем, что запрос отправлен и ждем ответа
            this.showOutgoingOutfitRequest();
        }
    }

    getCharacterName(character) {
        return character === 'princess' ? 'Княжна' : 'Помощница';
    }
    
    showIncomingOutfitRequest() {
        const requestContainer = this.element.querySelector('#outfit-request-container');
        const fromCharacterName = this.getCharacterName(this.activeOutfitRequest.fromCharacter);
        
        requestContainer.innerHTML = `
            <div class="outfit-request-notification incoming">
                <div class="request-header">👗 Запрос на обмен одеждой</div>
                <div class="request-message">
                    ${fromCharacterName} предлагает поменяться одеждой!
                </div>
                <div class="request-actions">
                    <button class="btn btn-success request-btn" data-action="accept-outfit-swap">
                        ✅ Принять
                    </button>
                    <button class="btn btn-secondary request-btn" data-action="decline-outfit-swap">
                        ❌ Отклонить
                    </button>
                </div>
            </div>
        `;

        // Добавляем обработчики для кнопок
        requestContainer.querySelector('[data-action="accept-outfit-swap"]').addEventListener('click', () => {
            this.respondToOutfitRequest(true);
        });

        requestContainer.querySelector('[data-action="decline-outfit-swap"]').addEventListener('click', () => {
            this.respondToOutfitRequest(false);
        });
    }

    showOutgoingOutfitRequest() {
        const requestContainer = this.element.querySelector('#outfit-request-container');
        const targetCharacterName = this.getCharacterName(this.activeOutfitRequest.targetCharacter);
        
        requestContainer.innerHTML = `
            <div class="outfit-request-notification outgoing">
                <div class="request-header">👗 Запрос отправлен</div>
                <div class="request-message">
                    Ожидаем ответа от ${targetCharacterName}...
                </div>
                <div class="request-waiting">⏳</div>
            </div>
        `;
    }
    
    updateLocationInfo(data) {
	const locationNames = {
            'princess_chamber': 'Спальня княжны',
            'throne_room': 'Тронный зал',
            'kitchen': 'Кухня',
            'garden': 'Сад',
            'armory': 'Арсенал',
            'private_quarters': 'Личные покои',
            'secret_passage': 'Тайный проход'
	};

	this.element.querySelector('#current-location').textContent = 
            locationNames[data.location] || data.location;

	const npcsElement = this.element.querySelector('#npcs-present');
	if (data.npcsPresent && data.npcsPresent.length > 0) {
            npcsElement.innerHTML = `<br>👥 <em>Присутствуют: ${data.npcsPresent.join(', ')}</em>`;
	} else {
            npcsElement.innerHTML = `<br>🤫 <em>Никого нет поблизости - можно переодеваться!</em>`;
	}
    }

    updatePlayersInfo(data) {
	console.log('🎭 Полученные данные на клиенте:', {
            stats: data.stats,
            helperStats: data.stats.helper,
            princessStats: data.stats.princess
	});

	// Информация о княжне
	this.element.querySelector('#princess-player').textContent = 
            data.players.princess?.name || '-';
	this.element.querySelector('#princess-outfit').textContent = 
            this.getOutfitName(data.stats.princess?.outfit || 'nightgown');

	// Информация о помощнице
	this.element.querySelector('#helper-player').textContent = 
            data.players.helper?.name || '-';
	
	const helperOutfit = data.stats.helper?.outfit;
	console.log('👗 Обновление наряда помощницы:', helperOutfit);
	
	this.element.querySelector('#helper-outfit').textContent = 
            this.getOutfitName(helperOutfit || 'common_dress');
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
        console.log(`🔍 DEBUG updateCharacterChoices for ${character}:`, {
            choices: choices,
            playerRole: this.playerRole,
            isMyRole: this.playerRole === character,
            gameDataChoices: this.gameData.choices,
            outfitChoices: choices?.filter(c => c.isOutfitChange)
        });

        const choicesDiv = this.element.querySelector(`#${character}-choices`);
        choicesDiv.innerHTML = '';

        const isMyRole = this.playerRole === character;
        const hasChoices = choices && choices.length > 0;

        if (!isMyRole) {
            this.renderOtherPlayerChoices(choicesDiv, choices, character, hasChoices);
            return;
        }

        this.renderMyPlayerChoices(choicesDiv, choices, hasChoices);
    }

    renderOtherPlayerChoices(choicesDiv, choices, character, hasChoices) {
        if (hasChoices) {
            const outfitChoices = choices.filter(choice => choice.isOutfitChange);
            console.log(`🎭 DEBUG: Filtered outfit choices for ${character}:`, outfitChoices);
            
            if (outfitChoices.length > 0) {
                this.addChoicesHeader(choicesDiv, '🔄 Можете предложить:');
                this.addChoiceButtons(choicesDiv, outfitChoices, character);
            }
        }
        
        this.addWaitingIndicator(choicesDiv, 'Ход другого игрока...');
    }

    renderMyPlayerChoices(choicesDiv, choices, hasChoices) {
        if (!hasChoices) {
            choicesDiv.innerHTML = '<div class="waiting-turn">Ожидание развития сюжета...</div>';
            return;
        }

        this.addTurnIndicator(choicesDiv);
        this.addChoiceButtons(choicesDiv, choices, null);
    }

    addChoicesHeader(container, text) {
        const header = document.createElement('div');
        header.className = 'other-player-actions';
        header.textContent = text;
        container.appendChild(header);
    }

    addWaitingIndicator(container, text) {
        const waitingDiv = document.createElement('div');
        waitingDiv.className = 'waiting-turn';
        waitingDiv.textContent = text;
        container.appendChild(waitingDiv);
    }

    addTurnIndicator(container) {
        const turnIndicator = document.createElement('div');
        turnIndicator.className = 'turn-indicator';
        turnIndicator.textContent = '🎯 Ваш ход!';
        container.appendChild(turnIndicator);
    }

    addChoiceButtons(container, choices, character) {
        choices.forEach(choice => {
            const button = this.createChoiceButton(choice, character);
            container.appendChild(button);
        });
    }

    createChoiceButton(choice, character) {
        const button = document.createElement('button');
        button.className = 'choice-button';
        
        // Особое оформление для запроса обмена одеждой
        if (choice.id === 'request_outfit_swap') {
            button.classList.add('outfit-request-btn');
        }
        
        button.innerHTML = `
            <strong>${choice.text}</strong><br>
            <small>${choice.description}</small>
        `;
        
        button.addEventListener('click', () => {
            if (choice.id === 'request_outfit_swap') {
                this.requestOutfitSwap(character);
            } else {
                this.makeChoice(choice.id, character);
            }
        });
        
        if (choice.disabled) {
            button.disabled = true;
            button.innerHTML += `<br><small style="color: #dc3545;">🚫 ${choice.reason}</small>`;
        }
        
        return button;
    }

    requestOutfitSwap(character) {
        console.log(`👗 Отправляем запрос обмена одеждой для ${character}`);
        this.app.socketManager.socket.emit('request-outfit-swap', { character: character });
    }

    respondToOutfitRequest(accepted) {
        console.log(`👗 Отвечаем на запрос обмена одеждой: ${accepted}`);
        this.app.socketManager.socket.emit('respond-outfit-swap', { accepted: accepted });
    }

    handleOutfitRequestCreated(data) {
        console.log('👗 Новый запрос обмена одеждой:', data);
        this.updateGame(data.gameData);
        
        // Показываем уведомление
        this.showNotification(data.message, 'info');
    }
    
    showNotification(message, type = 'info') {
        // Простое уведомление - можно заменить на более красивое
        const notification = document.createElement('div');
        notification.className = `game-notification ${type}`;
        notification.textContent = message;
        
        this.element.appendChild(notification);
        
        setTimeout(() => {
            notification.remove();
        }, 4000);
    }

    handleOutfitRequestResolved(data) {
        console.log('👗 Запрос обмена одеждой разрешен:', data);
        this.updateGame(data.gameData);
        
        // Показываем результат
        if (data.accepted) {
            this.showNotification(data.message, 'success');
        } else {
            this.showNotification(data.message, 'warning');
        }
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
