// Mithril is loaded globally from CDN
const m = window.m;

const Lobby = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
    },
    
    getRoomData(vnode) {
        // Always prefer app.screenData as it gets updated
        return this.app.screenData || vnode.attrs.data || null;
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


    createPlayerCard(role, playerData) {
        const roleInfo = {
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

        const info = roleInfo[role];
        const socketId = this.app.socketManager.socket.id;

        if (playerData) {
            return m('.player-card', [
                m('.player-info', [
                    m('.player-avatar', info.icon),
                    m('div', [
                        m('div', m('strong', info.name)),
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
        const canStart = players.princess && players.helper;
        
        let buttonText, waitingText;
        if (canStart) {
            buttonText = '▶️ Начать игру';
            waitingText = m('em', 'Все игроки готовы!');
        } else {
            buttonText = '⏳ Ожидание игроков';
            waitingText = m('em', 'Ожидание второго игрока...');
        }

        return m('div#lobby-screen.fade-in', [
            m('button.btn.btn-secondary.mb-2', {
                onclick: () => this.backToMenu()
            }, '← В главное меню'),
            
            m('.lobby-container', [
                m('h2.text-center', '🏰 Лобби игры'),
                
                m('#room-info.text-center', [
                    m('p', 'Поделитесь этим кодом с другом:'),
                    m('.room-id-display#room-id-display', {
                        onclick: () => this.copyRoomId((roomData?.roomId || roomData?.roomCode) || 'ABCD')
                    }, (roomData?.roomId || roomData?.roomCode) || 'ABCD')
                ]),

                m('#players-list', [
                    this.createPlayerCard('princess', players.princess),
                    this.createPlayerCard('helper', players.helper)
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