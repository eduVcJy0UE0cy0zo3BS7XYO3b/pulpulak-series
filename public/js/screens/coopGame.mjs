// Mithril is loaded globally from CDN
const m = window.m;
import NotificationManager from '../notificationManager.mjs';

const CoopGame = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
        this.chatVisible = false;
        
        // Setup socket listeners
        this.setupSocketListeners();
    },
    
    getGameData(vnode) {
        // Always prefer app.screenData as it gets updated
        return this.app.screenData || vnode.attrs.data || null;
    },
    
    getPlayerRole(vnode) {
        const data = this.getGameData(vnode);
        return this.determinePlayerRole(data);
    },

    onremove() {
        // Clean up socket listeners
        const socket = this.app.socketManager.socket;
        socket.off('chat-message');
        socket.off('outfit-request-created');
        socket.off('outfit-request-resolved');
    },

    setupSocketListeners() {
        const socket = this.app.socketManager.socket;
        
        socket.on('chat-message', (data) => {
            this.addChatMessage(data);
            m.redraw();
        });

        socket.on('outfit-request-created', (data) => {
            this.handleOutfitRequestCreated(data);
            m.redraw();
        });

        socket.on('outfit-request-resolved', (data) => {
            this.handleOutfitRequestResolved(data);
            m.redraw();
        });
    },

    determinePlayerRole(data) {
        if (!data) return null;
        const socketId = this.app.socketManager.socket.id;
        if (data.players.princess?.id === socketId) {
            return 'princess';
        } else if (data.players.helper?.id === socketId) {
            return 'helper';
        }
        return null;
    },


    leaveGame(vnode) {
        const gameData = this.getGameData(vnode);
        this.app.leaveRoom(gameData.roomId);
        this.app.showScreen('mainMenu');
        NotificationManager.add('Вы покинули игру', 'info');
    },

    toggleChat() {
        this.chatVisible = !this.chatVisible;
    },

    sendChatMessage(vnode) {
        const input = document.getElementById('chat-input');
        const message = input.value.trim();
        const gameData = this.getGameData(vnode);
        if (message && gameData) {
            this.app.sendChatMessage(gameData.roomId, message);
            input.value = '';
        }
    },

    addChatMessage(data) {
        const messagesDiv = document.getElementById('chat-messages');
        if (messagesDiv) {
            const messageEl = document.createElement('div');
            messageEl.className = 'chat-message';
            messageEl.innerHTML = `<strong>${data.sender}:</strong> ${data.message}`;
            messagesDiv.appendChild(messageEl);
            messagesDiv.scrollTop = messagesDiv.scrollHeight;
        }
    },

    handleOutfitRequestCreated(data) {
        // Update game data to include the new request
        if (data.gameData) {
            this.app.screenData = data.gameData;
        }
        m.redraw();
    },

    handleOutfitRequestResolved(data) {
        if (data.accepted) {
            NotificationManager.add('Запрос на обмен одеждой принят!', 'success');
        } else {
            NotificationManager.add('Запрос на обмен одеждой отклонен.', 'info');
        }
        // Update game data if provided
        if (data.gameData) {
            this.app.screenData = data.gameData;
        }
        m.redraw();
    },

    respondToOutfitRequest(accept, vnode) {
        const gameData = this.getGameData(vnode);
        const activeOutfitRequest = gameData?.activeOutfitRequest;
        if (activeOutfitRequest && gameData) {
            this.app.socketManager.socket.emit('respond-outfit-swap', {
                roomId: gameData.roomId,
                requestId: activeOutfitRequest.requestId,
                accepted: accept
            });
        }
    },

    makeChoice(choiceId, character, vnode) {
        const gameData = this.getGameData(vnode);
        if (gameData) {
            // Handle outfit swap request separately
            if (choiceId === 'request_outfit_swap') {
                this.app.socketManager.socket.emit('request-outfit-swap', {
                    roomId: gameData.roomId,
                    character: character
                });
            } else {
                this.app.makeChoice(gameData.roomId, choiceId, character);
            }
        }
    },

    getOutfitName(outfitId) {
        const outfitNames = {
            'nightgown': 'Ночная рубашка',
            'princess_dress': 'Княжеское платье',
            'common_dress': 'Простое платье',
            'court_dress': 'Придворное платье'
        };
        return outfitNames[outfitId] || outfitId;
    },

    getLocationName(locationId) {
        const locationNames = {
            'princess_chamber': 'Спальня княжны',
            'throne_room': 'Тронный зал',
            'kitchen': 'Кухня',
            'garden': 'Сад',
            'armory': 'Арсенал',
            'private_quarters': 'Личные покои',
            'secret_passage': 'Тайный проход'
        };
        return locationNames[locationId] || locationId;
    },

    renderOutfitRequest(vnode) {
        const gameData = this.getGameData(vnode);
        const activeOutfitRequest = gameData?.activeOutfitRequest;
        if (!activeOutfitRequest) return null;

        const socketId = this.app.socketManager.socket.id;
        const isTargetPlayer = activeOutfitRequest.targetPlayerId === socketId;
        const isRequestInitiator = activeOutfitRequest.fromPlayerId === socketId;

        if (isTargetPlayer) {
            const fromCharacterName = activeOutfitRequest.fromCharacter === 'princess' ? 'Княжна' : 'Помощница';
            return m('.outfit-request-notification.incoming', [
                m('.request-header', '👗 Запрос на обмен одеждой'),
                m('.request-message', `${fromCharacterName} предлагает поменяться одеждой!`),
                m('.request-actions', [
                    m('button.btn.btn-success.request-btn', {
                        onclick: () => this.respondToOutfitRequest(true, vnode)
                    }, '✅ Принять'),
                    m('button.btn.btn-secondary.request-btn', {
                        onclick: () => this.respondToOutfitRequest(false, vnode)
                    }, '❌ Отклонить')
                ])
            ]);
        } else if (isRequestInitiator) {
            const targetCharacterName = activeOutfitRequest.targetCharacter === 'princess' ? 'Княжна' : 'Помощница';
            return m('.outfit-request-notification.outgoing', [
                m('.request-header', '👗 Запрос отправлен'),
                m('.request-message', `Ожидаем ответа от ${targetCharacterName}...`),
                m('.request-waiting', '⏳')
            ]);
        }
        return null;
    },

    renderChoiceButton(choice, character, vnode) {
        const playerRole = this.getPlayerRole(vnode);
        const isMyRole = playerRole === character;
        const style = choice.danger ? 'danger' : (choice.safe ? 'success' : 'primary');
        
        return m(`button.btn.btn-${style}`, {
            onclick: () => this.makeChoice(choice.id, character, vnode),
            disabled: !isMyRole && !choice.isOutfitRequest
        }, [
            choice.icon ? `${choice.icon} ` : '',
            choice.text || choice.action || 'Действие'
        ]);
    },

    renderCharacterChoices(character, choices, vnode) {
        const playerRole = this.getPlayerRole(vnode);
        const isMyRole = playerRole === character;
        const hasChoices = choices && choices.length > 0;

        if (!isMyRole) {
            if (hasChoices) {
                const outfitChoices = choices.filter(choice => choice.isOutfitRequest);
                if (outfitChoices.length > 0) {
                    return [
                        m('.other-player-actions', '🔄 Можете предложить:'),
                        outfitChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                    ];
                }
            }
            return m('em', 'Ожидание действий другого игрока...');
        } else {
            if (hasChoices) {
                const outfitChoices = choices.filter(choice => choice.isOutfitRequest);
                const actionChoices = choices.filter(choice => !choice.isOutfitRequest);
                
                return [
                    actionChoices.length > 0 && m('div', [
                        m('.choice-section-header', '⚡ Ваши действия:'),
                        actionChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                    ]),
                    outfitChoices.length > 0 && m('div.mt-2', [
                        m('.choice-section-header', '👗 Смена одежды:'),
                        outfitChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                    ])
                ];
            }
            return m('em', 'Обдумываете следующий шаг...');
        }
    },

    view(vnode) {
        const gameData = this.getGameData(vnode);
        if (!gameData) {
            return m('div', 'Загрузка...');
        }

        const data = gameData;
        const npcsPresent = data.npcsPresent && data.npcsPresent.length > 0;

        return m('div#coop-game-screen.fade-in', [
            // Header
            m('.text-center.mb-2', [
                m('button.btn.btn-secondary', {
                    onclick: () => this.leaveGame(vnode),
                    title: 'Покинуть игру'
                }, '🚪 Выйти'),
                m('span.mx-2', ['Комната: ', m('strong', data.roomId)]),
                m('button.btn.btn-secondary', {
                    onclick: () => this.toggleChat()
                }, '💬 Чат')
            ]),

            // Outfit request container
            m('#outfit-request-container.request-container', this.renderOutfitRequest(vnode)),

            // Story section
            m('.card#story-section', [
                m('.card-header', data.scene.title),
                m('#story-text', m.trust(data.scene.text)),
                m('.location-info', [
                    '📍 ', m('strong', 'Локация: '), this.getLocationName(data.location),
                    m('br'),
                    npcsPresent ? 
                        ['👥 ', m('em', `Присутствуют: ${data.npcsPresent.join(', ')}`)] :
                        ['🤫 ', m('em', 'Никого нет поблизости - можно переодеваться!')]
                ])
            ]),

            // Player panels
            m('.dual-player-layout', [
                // Princess panel
                m('.player-panel.panel-princess', [
                    m('h3', { style: 'background: #ffcccb; color: #8b0000;' }, '👑 Княжна Пулпулак'),
                    m('div', [
                        m('strong', 'Игрок: '), data.players.princess?.name || '-',
                        m('br'),
                        m('strong', 'Наряд: '), this.getOutfitName(data.stats.princess?.outfit || 'nightgown')
                    ]),
                    m('div', this.renderCharacterChoices('princess', data.choices.princess, vnode))
                ]),

                // Helper panel
                m('.player-panel.panel-helper', [
                    m('h3', { style: 'background: #e0f2e7; color: #155724;' }, '🧙‍♀️ Помощница ведьмы'),
                    m('div', [
                        m('strong', 'Игрок: '), data.players.helper?.name || '-',
                        m('br'),
                        m('strong', 'Наряд: '), this.getOutfitName(data.stats.helper?.outfit || 'common_dress')
                    ]),
                    m('div', this.renderCharacterChoices('helper', data.choices.helper, vnode))
                ])
            ]),

            // Chat section
            m(`#chat-section.chat-section${this.chatVisible ? '' : '.hidden'}`, [
                m('.chat-messages#chat-messages'),
                m('.chat-input-container', [
                    m('input#chat-input.chat-input', {
                        type: 'text',
                        placeholder: 'Написать сообщение...',
                        maxlength: 200,
                        onkeypress: (e) => {
                            if (e.key === 'Enter') this.sendChatMessage(vnode);
                        }
                    }),
                    m('button.btn.chat-send', {
                        onclick: () => this.sendChatMessage(vnode)
                    }, 'Отправить')
                ])
            ])
        ]);
    }
};

export default CoopGame;