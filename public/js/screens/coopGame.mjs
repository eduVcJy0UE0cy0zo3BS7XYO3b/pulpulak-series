// Mithril is loaded globally from CDN
const m = window.m;
import NotificationManager from '../notificationManager.mjs';
import { OUTFIT_NAMES, CHARACTER_NAMES } from '../constants.mjs';

const CoopGame = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
        this.chatVisible = false;
        this.dialogueProcessing = false; // Флаг для предотвращения двойных кликов
        this.availableGames = [];
        
        // Load game info for dynamic role names
        this.loadGameInfo();
        
        // Setup socket listeners
        this.setupSocketListeners();
    },

    async loadGameInfo() {
        try {
            const response = await fetch('/api/games');
            if (response.ok) {
                const data = await response.json();
                this.availableGames = data.games || [];
                m.redraw();
            }
        } catch (error) {
            console.error('Failed to load game info:', error);
        }
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
        socket.off('request-created');
        socket.off('request-resolved');
        socket.off('game-state-updated');
    },

    setupSocketListeners() {
        const socket = this.app.socketManager.socket;
        
        socket.on('chat-message', (data) => {
            this.addChatMessage(data);
            m.redraw();
        });

        socket.on('request-created', (data) => {
            this.handleOutfitRequestCreated(data);
            m.redraw();
        });

        socket.on('request-resolved', (data) => {
            this.handleOutfitRequestResolved(data);
            m.redraw();
        });

        socket.on('game-state-updated', (data) => {
            // Сбрасываем флаг обработки диалога при получении обновления состояния
            this.dialogueProcessing = false;
            this.app.screenData = data;
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

    getGameInfo(gameId) {
        if (!this.availableGames) return null;
        return this.availableGames.find(game => game.id === gameId);
    },

    getRoleDisplayName(roleKey, gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            // Fallback to constants for backward compatibility
            return CHARACTER_NAMES[roleKey] || roleKey;
        }

        // Map role slots to actual game roles
        const roleIndex = roleKey === 'princess' ? 0 : 1;
        const role = gameInfo.roles[roleIndex];
        return role ? role.name : CHARACTER_NAMES[roleKey] || roleKey;
    },

    getRoleIcon(roleKey, gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            // Fallback icons
            return roleKey === 'princess' ? '👑' : '🧙‍♀️';
        }

        // Map role slots to actual game roles
        const roleIndex = roleKey === 'princess' ? 0 : 1;
        const role = gameInfo.roles[roleIndex];
        
        // Get icon based on role id
        const iconMap = {
            princess: '👑',
            helper: '🧙‍♀️',
            detective: '🔍',
            journalist: '📰'
        };
        
        return role ? (iconMap[role.id] || '🎭') : (iconMap[roleKey] || '🎭');
    },

    getCharacterDisplayName(character, gameId) {
        return this.getRoleDisplayName(character, gameId);
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
        console.log('👗 MJS: handleOutfitRequestCreated called with:', data);
        // Update game data to include the new request
        if (data.gameData) {
            console.log('👗 MJS: Updating screen data with:', data.gameData);
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
        const activeRequest = gameData?.activeRequest || gameData?.activeOutfitRequest;
        if (activeRequest && gameData) {
            console.log('👗 MJS: Responding to outfit request:', accept);
            this.app.socketManager.socket.emit('respond-request', {
                accepted: accept,
                responseData: {}
            });
        }
    },

    makeChoice(choiceId, character, vnode) {
        const gameData = this.getGameData(vnode);
        if (gameData) {
            // Handle outfit swap request separately
            if (choiceId === 'request_outfit_swap') {
                console.log('👗 MJS: Sending outfit swap request for', character);
                this.app.socketManager.socket.emit('create-request', {
                    requestType: 'outfit_swap',
                    character: character,
                    requestData: {}
                });
            } else {
                this.app.makeChoice(gameData.roomId, choiceId, character);
            }
        }
    },

    getOutfitName(outfitId) {
        return OUTFIT_NAMES[outfitId] || outfitId;
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
        const activeRequest = gameData?.activeRequest || gameData?.activeOutfitRequest;
        
        console.log('👗 MJS: renderOutfitRequest debug:', {
            gameData: gameData,
            activeRequest: activeRequest,
            hasActiveRequest: !!activeRequest
        });
        
        if (!activeRequest) {
            console.log('👗 MJS: No active request found');
            return null;
        }

        const socketId = this.app.socketManager.socket.id;
        const playerRole = this.getPlayerRole(vnode);
        
        console.log('👗 MJS: Request details:', {
            activeRequest: activeRequest,
            playerRole: playerRole,
            socketId: socketId
        });
        
        // Определяем, кто является целью запроса
        const isTargetPlayer = activeRequest.targetCharacter === playerRole;
        const isRequestInitiator = activeRequest.fromCharacter === playerRole;
        
        console.log('👗 MJS: Player status:', {
            isTargetPlayer: isTargetPlayer,
            isRequestInitiator: isRequestInitiator
        });

        if (isTargetPlayer) {
            const gameData = this.getGameData(vnode);
            const gameId = gameData?.gameId || 'pulpulak';
            const fromCharacterName = this.getCharacterDisplayName(activeRequest.fromCharacter, gameId);
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
            const gameData = this.getGameData(vnode);
            const gameId = gameData?.gameId || 'pulpulak';
            const targetCharacterName = this.getCharacterDisplayName(activeRequest.targetCharacter, gameId);
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
        
        // Добавляем логи для отладки
        if (choice.id === 'request_outfit_swap' || (choice.text && choice.text.includes('поменяться одеждой'))) {
            console.log('👗 MJS: Rendering outfit swap button:', {
                choiceId: choice.id,
                character: character,
                playerRole: playerRole,
                isMyRole: isMyRole,
                text: choice.text
            });
        }
        
        return m(`button.btn.btn-${style}`, {
            onclick: () => {
                console.log('👗 MJS: Button clicked:', choice.id, 'for', character);
                this.makeChoice(choice.id, character, vnode);
            },
            disabled: !isMyRole && !choice.isOutfitRequest
        }, [
            choice.icon ? `${choice.icon} ` : '',
            choice.text || choice.action || 'Действие'
        ]);
    },

    renderQuestInfo(character, data) {
        const quest = data.quests && data.quests[character] && data.quests[character].active;
        
        if (!quest) {
            return null;
        }

        const currentStep = quest.steps[quest.currentStep];
        
        return m('.quest-info', { style: 'margin: 10px 0; padding: 10px; background: rgba(255, 255, 255, 0.1); border-radius: 5px; border-left: 3px solid #007bff;' }, [
            m('.quest-title', { style: 'font-weight: bold; color: #007bff;' }, ['📋 ', quest.title]),
            m('.quest-description', { style: 'font-size: 0.9em; margin: 5px 0;' }, quest.description),
            currentStep && m('.quest-current-step', { style: 'font-size: 0.85em; font-style: italic;' }, [
                '🎯 Текущая задача: ', currentStep.description
            ])
        ]);
    },

    renderCurrentPlayerPanel(vnode, data) {
        const playerRole = this.getPlayerRole(vnode);
        
        if (playerRole === 'princess') {
            return m('.single-player-layout', [
                m('.player-panel.panel-princess', [
                    m('h3', { style: 'background: #ffcccb; color: #8b0000;' }, `${this.getRoleIcon('princess', data?.gameId || 'pulpulak')} ${this.getRoleDisplayName('princess', data?.gameId || 'pulpulak')}`),
                    m('div', [
                        m('strong', 'Игрок: '), data.players.princess?.name || '-',
                        m('br'),
                        m('strong', 'Наряд: '), this.getOutfitName(data.stats.princess?.outfit || 'princess_dress'),
                        m('br'),
                        data.locations?.princess && [
                            m('strong', 'Локация: '), 
                            data.locations.princess.icon + ' ' + data.locations.princess.name
                        ]
                    ]),
                    // Информация о локации княжны
                    data.locations?.princess && m('.location-info', { style: 'margin: 10px 0; font-size: 0.9em;' }, [
                        data.stats.princess?.npcsPresent?.length > 0 ? 
                            ['👥 ', m('em', `Присутствуют: ${data.stats.princess.npcsPresent.join(', ')}`)] :
                            data.locations.princess.canChangeOutfit ?
                                ['🤫 ', m('em', 'Никого нет')] :
                                ['👥 ', m('em', 'Публичное место')]
                    ]),
                    // Информация о квесте
                    this.renderQuestInfo('princess', data),
                    m('div', this.renderCharacterChoices('princess', data.choices.princess, vnode))
                ])
            ]);
        } else if (playerRole === 'helper') {
            return m('.single-player-layout', [
                m('.player-panel.panel-helper', [
                    m('h3', { style: 'background: #e0f2e7; color: #155724;' }, `${this.getRoleIcon('helper', data?.gameId || 'pulpulak')} ${this.getRoleDisplayName('helper', data?.gameId || 'pulpulak')}`),
                    m('div', [
                        m('strong', 'Игрок: '), data.players.helper?.name || '-',
                        m('br'),
                        m('strong', 'Наряд: '), this.getOutfitName(data.stats.helper?.outfit || 'common_dress'),
                        m('br'),
                        data.locations?.helper && [
                            m('strong', 'Локация: '), 
                            data.locations.helper.icon + ' ' + data.locations.helper.name
                        ]
                    ]),
                    // Информация о локации помощницы
                    data.locations?.helper && m('.location-info', { style: 'margin: 10px 0; font-size: 0.9em;' }, [
                        data.stats.helper?.npcsPresent?.length > 0 ? 
                            ['👥 ', m('em', `Присутствуют: ${data.stats.helper.npcsPresent.join(', ')}`)] :
                            data.locations.helper.canChangeOutfit ?
                                ['🤫 ', m('em', 'Никого нет')] :
                                ['👥 ', m('em', 'Публичное место')]
                    ]),
                    // Информация о квесте
                    this.renderQuestInfo('helper', data),
                    m('div', this.renderCharacterChoices('helper', data.choices.helper, vnode))
                ])
            ]);
        }
        
        return m('div', 'Загрузка информации о персонаже...');
    },

    renderCharacterChoices(character, choices, vnode) {
        const playerRole = this.getPlayerRole(vnode);
        const isMyRole = playerRole === character;
        const hasChoices = choices && choices.length > 0;

        // Показываем только выборы для текущего игрока
        if (isMyRole && hasChoices) {
            const outfitChoices = choices.filter(choice => choice.isOutfitRequest);
            const movementChoices = choices.filter(choice => choice.isMovement);
            const npcChoices = choices.filter(choice => choice.isNPCInteraction);
            const actionChoices = choices.filter(choice => !choice.isOutfitRequest && !choice.isMovement && !choice.isNPCInteraction);
            
            return [
                actionChoices.length > 0 && m('div', [
                    m('.choice-section-header', '⚡ Ваши действия:'),
                    actionChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                ]),
                npcChoices.length > 0 && m('div.mt-2', [
                    m('.choice-section-header', '💬 Поговорить:'),
                    npcChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                ]),
                outfitChoices.length > 0 && m('div.mt-2', [
                    m('.choice-section-header', '👗 Смена одежды:'),
                    outfitChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                ]),
                movementChoices.length > 0 && m('div.mt-2', [
                    m('.choice-section-header', '🚶 Перемещение:'),
                    movementChoices.map(choice => this.renderChoiceButton(choice, character, vnode))
                ])
            ];
        } else if (isMyRole) {
            return m('em', 'Обдумываете следующий шаг...');
        }
        
        // Не показываем информацию о другом игроке
        return null;
    },

    renderNPCDialogue(vnode) {
        const gameData = this.getGameData(vnode);
        const playerRole = this.getPlayerRole(vnode);
        
        // Получаем диалог для текущего игрока
        const dialogue = gameData.npcDialogues && gameData.npcDialogues[playerRole];
        
        // Показываем диалог только если он есть для данного игрока
        if (!dialogue) return null;
        
        const attitudeClass = dialogue.attitude === 'hostile' ? 'danger' : 'success';
        
        return [
            // Полупрозрачный фон
            m('.npc-dialogue-overlay', {
                style: 'position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0, 0, 0, 0.6); z-index: 999;',
                onclick: (e) => e.stopPropagation() // Предотвращаем закрытие при клике на фон
            }),
            // Само диалоговое окно
            m('.npc-dialogue-modal', {
                style: 'position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); max-width: 500px; z-index: 1000; border-radius: 12px; padding: 25px;'
            }, [
                m('h3', { class: `text-${attitudeClass}` }, dialogue.npcName),
                m('.npc-greeting', dialogue.greeting),
                m('.npc-choices', dialogue.choices.map(choice => 
                    m('button.btn.btn-primary', {
                        style: 'display: block; width: 100%; margin: 12px 0; padding: 12px;',
                        onclick: () => this.respondToNPCDialogue(choice.id, playerRole, vnode),
                        disabled: this.dialogueProcessing
                    }, choice.text)
                )),
                // Кнопка закрытия диалога
                m('button.btn.btn-secondary', {
                    style: 'margin-top: 15px; padding: 8px 16px;',
                    onclick: () => this.closeNPCDialogue(vnode),
                    disabled: this.dialogueProcessing
                }, '❌ Закрыть диалог')
            ])
        ];
    },

    closeNPCDialogue(vnode) {
        const gameData = this.getGameData(vnode);
        const playerRole = this.getPlayerRole(vnode);
        
        // Проверяем, что диалог все еще активен и не обрабатывается
        if (!gameData.npcDialogues[playerRole] || this.dialogueProcessing) {
            return;
        }
        
        this.dialogueProcessing = true;
        
        // Отправляем запрос на закрытие диалога
        this.app.socketManager.socket.emit('close-npc-dialogue');
        
        // Сбрасываем флаг через короткое время
        setTimeout(() => {
            this.dialogueProcessing = false;
        }, 1000);
    },

    respondToNPCDialogue(choiceId, character, vnode) {
        const gameData = this.getGameData(vnode);
        
        // Проверяем, что диалог все еще активен и не обрабатывается
        if (!gameData.npcDialogues[character] || this.dialogueProcessing) {
            return;
        }
        
        this.dialogueProcessing = true;
        
        this.app.socketManager.socket.emit('npc-dialogue-choice', {
            choiceId: choiceId,
            character: character
        });
        
        // Сбрасываем флаг через короткое время
        setTimeout(() => {
            this.dialogueProcessing = false;
        }, 1000);
    },

    view(vnode) {
        const gameData = this.getGameData(vnode);
        if (!gameData) {
            return m('div', 'Загрузка...');
        }

        const data = gameData;

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

            // NPC dialogue modal
            this.renderNPCDialogue(vnode),

            // Outfit request container
            m('#outfit-request-container.request-container', this.renderOutfitRequest(vnode)),

            // Story section
            m('.card#story-section', [
                m('.card-header', data.scene.title),
                m('#story-text', m.trust(data.scene.text))
            ]),

            // Player panel - only show current player's information
            this.renderCurrentPlayerPanel(vnode, data),

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