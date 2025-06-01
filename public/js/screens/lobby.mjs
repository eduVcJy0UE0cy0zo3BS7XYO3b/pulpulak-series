// Mithril is loaded globally from CDN
const m = window.m;

const Lobby = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
        this.gameInfo = null;
        this.loadGameInfo();
    },

    async loadGameInfo() {
        try {
            const response = await fetch('/api/games');
            if (response.ok) {
                const data = await response.json();
                // API returns array directly, not wrapped in {games: [...]}
                this.availableGames = Array.isArray(data) ? data : (data.games || []);
                m.redraw();
            }
        } catch (error) {
            console.error('Failed to load game info:', error);
        }
    },
    
    getRoomData(vnode) {
        // Always prefer app.screenData as it gets updated
        return this.app.screenData || vnode.attrs.data || null;
    },

    getGameInfo(gameId) {
        if (!this.availableGames) return null;
        return this.availableGames.find(game => game.id === gameId);
    },

    getRoleMapping(gameId) {
        const gameInfo = this.getGameInfo(gameId);
        if (!gameInfo || !gameInfo.roles) {
            // Fallback to Pulpulak roles
            return {
                princess: {
                    name: '👑 Княжна Пулпулак',
                    icon: '👑',
                    class: 'role-princess'
                },
                helper: {
                    name: '🧙‍♀️ Помощница ведьмы',
                    icon: '🧙‍♀️',
                    class: 'role-helper'
                }
            };
        }

        // Create dynamic role mapping based on game data
        const roleMapping = {};
        gameInfo.roles.forEach((role, index) => {
            const roleKey = index === 0 ? 'princess' : 'helper'; // Map to existing role slots
            roleMapping[roleKey] = {
                name: `${this.getRoleIcon(role.id)} ${role.name}`,
                icon: this.getRoleIcon(role.id),
                class: `role-${role.id}`,
                description: role.description
            };
        });
        return roleMapping;
    },

    getRoleIcon(roleId) {
        const iconMap = {
            princess: '👑',
            helper: '🧙‍♀️',
            detective: '🔍',
            journalist: '📰'
        };
        return iconMap[roleId] || '🎭';
    },
    

    backToMenu() {
        this.app.showScreen('mainMenu');
    },

    startGame(vnode) {
        const roomData = this.getRoomData(vnode);
        if (roomData) {
            // Support both roomId (legacy) and roomCode (new format)
            const roomId = roomData.roomId || roomData.roomCode;
            this.app.startCoopGame(roomId);
        }
    },

    copyRoomId(roomId) {
        navigator.clipboard.writeText(roomId).then(() => {
            const elem = document.getElementById('room-id-display');
            const originalText = elem.textContent;
            elem.textContent = 'Скопировано!';
            setTimeout(() => {
                elem.textContent = originalText;
            }, 1000);
        }).catch(() => {
            const elem = document.getElementById('room-id-display');
            elem.select();
            document.execCommand('copy');
        });
    },


    createPlayerCard(role, playerData, gameId) {
        const roleMapping = this.getRoleMapping(gameId);
        const info = roleMapping[role];
        
        if (!info) {
            // Fallback if role mapping fails
            const fallbackInfo = {
                name: '🎭 Игрок',
                icon: '🎭',
                class: 'role-generic'
            };
            return this.renderPlayerCard(fallbackInfo, playerData, role);
        }

        return this.renderPlayerCard(info, playerData, role);
    },

    renderPlayerCard(info, playerData, role) {
        const socketId = this.app.socketManager.socket.id;

        if (playerData) {
            return m('.player-card', [
                m('.player-info', [
                    m('.player-avatar', info.icon),
                    m('div', [
                        m('div', m('strong', info.name)),
                        info.description && m('div.role-description', info.description),
                        m('div', `Игрок: ${playerData.name || playerData.id}`),
                        playerData.id === socketId ? m('em', '(Это вы)') : null
                    ])
                ]),
                m('.player-role', { class: info.class }, 'Готов')
            ]);
        } else {
            return m('.player-card', [
                m('.player-info', [
                    m('.player-avatar', '❓'),
                    m('div', [
                        m('div', m('strong', info.name)),
                        info.description && m('div.role-description', info.description),
                        m('div', m('em', 'Ожидание игрока...'))
                    ])
                ]),
                m('.player-role.role-waiting', 'Ожидание')
            ]);
        }
    },

    view(vnode) {
        const roomData = this.getRoomData(vnode);
        const players = roomData?.players || {};
        const gameId = roomData?.gameId || 'pulpulak';
        const gameInfo = this.getGameInfo(gameId);
        const canStart = players.princess && players.helper;
        
        let buttonText, waitingText;
        if (canStart) {
            buttonText = '▶️ Начать игру';
            waitingText = m('em', 'Все игроки готовы!');
        } else {
            buttonText = '⏳ Ожидание игроков';
            waitingText = m('em', 'Ожидание второго игрока...');
        }

        const gameTitle = gameInfo ? gameInfo.name : 'Неизвестная игра';
        const gameDescription = gameInfo ? gameInfo.description : '';

        return m('div#lobby-screen.fade-in', [
            m('button.btn.btn-secondary.mb-2', {
                onclick: () => this.backToMenu()
            }, '← В главное меню'),
            
            m('.lobby-container', [
                m('h2.text-center', `🎮 ${gameTitle}`),
                gameDescription && m('p.text-center.game-description', gameDescription),
                
                m('#room-info.text-center', [
                    m('p', 'Поделитесь этим кодом с другом:'),
                    m('.room-id-display#room-id-display', {
                        onclick: () => this.copyRoomId((roomData?.roomId || roomData?.roomCode) || 'ABCD')
                    }, (roomData?.roomId || roomData?.roomCode) || 'ABCD')
                ]),

                m('#players-list', [
                    this.createPlayerCard('princess', players.princess, gameId),
                    this.createPlayerCard('helper', players.helper, gameId)
                ]),

                m('#lobby-controls.text-center.mt-2', [
                    m('button.btn.btn-primary.btn-large#start-game-btn', {
                        disabled: !canStart,
                        onclick: () => this.startGame(vnode)
                    }, buttonText),
                    m('p.waiting-text', waitingText)
                ])
            ])
        ]);
    }
};

export default Lobby;