import { loadComponent } from '../utils/helpers.js';

class Lobby {
    constructor(app) {
        this.app = app;
        this.element = null;
        this.roomData = null;
        this.isHost = false;
    }

    async show(data) {
        this.roomData = data;
        this.isHost = data.isHost || false;
        
        this.element = await loadComponent('lobby');
        this.app.container.appendChild(this.element);
        this.setupEventListeners();
        this.updateLobby(data);
    }

    hide() {
        if (this.element) {
            this.element.remove();
            this.element = null;
        }
    }

    setupEventListeners() {
        // Назад в главное меню
        this.element.querySelector('[data-action="back-to-menu"]').addEventListener('click', () => {
            this.app.showScreen('mainMenu');
        });

        // Запуск игры (только для хоста)
        this.element.querySelector('[data-action="start-coop-game"]').addEventListener('click', () => {
            if (this.isHost && this.roomData) {
                this.app.startCoopGame(this.roomData.roomId);
            }
        });

        // Копирование ID комнаты при клике
        const roomIdDisplay = this.element.querySelector('#room-id-display');
        roomIdDisplay.addEventListener('click', () => {
            navigator.clipboard.writeText(roomIdDisplay.textContent).then(() => {
                const originalText = roomIdDisplay.textContent;
                roomIdDisplay.textContent = 'Скопировано!';
                setTimeout(() => {
                    roomIdDisplay.textContent = originalText;
                }, 1000);
            }).catch(() => {
                // Fallback для старых браузеров
                roomIdDisplay.select();
                document.execCommand('copy');
            });
        });
    }

    updateLobby(data) {
        this.roomData = data;
        
        // Обновляем ID комнаты
        this.element.querySelector('#room-id-display').textContent = data.roomId;
        
        // Обновляем список игроков
        this.updatePlayersList(data.players);
        
        // Обновляем кнопку запуска
        this.updateStartButton(data.players);
    }

    updatePlayersList(players) {
        const playersList = this.element.querySelector('#players-list');
        playersList.innerHTML = '';

        // Карточка княжны
        const princessCard = this.createPlayerCard('princess', players.princess);
        playersList.appendChild(princessCard);

        // Карточка помощницы
        const helperCard = this.createPlayerCard('helper', players.helper);
        playersList.appendChild(helperCard);
    }

    createPlayerCard(role, playerData) {
        const card = document.createElement('div');
        card.className = 'player-card';

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

        if (playerData) {
            card.innerHTML = `
                <div class="player-info">
                    <div class="player-avatar">${info.icon}</div>
                    <div>
                        <div><strong>${info.name}</strong></div>
                        <div>Игрок: ${playerData.name || playerData.id}</div>
                        ${playerData.id === this.app.socketManager.socket.id ? '<em>(Это вы)</em>' : ''}
                    </div>
                </div>
                <div class="player-role ${info.class}">Готов</div>
            `;
        } else {
            card.innerHTML = `
                <div class="player-info">
                    <div class="player-avatar">❓</div>
                    <div>
                        <div><strong>${info.name}</strong></div>
                        <div><em>Ожидание игрока...</em></div>
                    </div>
                </div>
                <div class="player-role role-waiting">Ожидание</div>
            `;
        }

        return card;
    }

    updateStartButton(players) {
        const startBtn = this.element.querySelector('#start-game-btn');
        const waitingText = this.element.querySelector('.waiting-text');
        
        const canStart = players.princess && players.helper;
        
        startBtn.disabled = !canStart || !this.isHost;
        
        if (!this.isHost) {
            startBtn.textContent = '⏳ Ожидание хоста';
            waitingText.innerHTML = '<em>Только создатель комнаты может начать игру</em>';
        } else if (canStart) {
            startBtn.textContent = '▶️ Начать игру';
            waitingText.innerHTML = '<em>Все игроки готовы!</em>';
        } else {
            startBtn.textContent = '⏳ Ожидание игроков';
            waitingText.innerHTML = '<em>Ожидание второго игрока...</em>';
        }
    }
}

export default Lobby;
