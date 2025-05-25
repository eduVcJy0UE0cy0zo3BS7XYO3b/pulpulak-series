import BaseComponent from '../base/BaseComponent.js';

class MainMenuScreen extends BaseComponent {
    template() {
        return `
            <div class="screen main-menu">
                <game-card>
                    <div slot="header">🤝 Совместное приключение</div>
                    <div slot="content">
                        <p>Играйте вдвоем! Один игрок управляет княжной Пулпулак, другой - таинственной "сестрой" (помощницей ведьмы).</p>
                        <p><strong>🎭 Роли:</strong><br>
                        <span class="role-princess">👑 Княжна</span> - главная героиня, ищет правду<br>
                        <span class="role-helper">🧙‍♀️ Помощница</span> - хранит секреты ведьмы</p>
                    </div>
                </game-card>

                <div class="menu-actions">
                    <game-button type="primary" size="large" data-action="create-room">
                        🏠 Создать комнату
                    </game-button>

                    <div class="join-section">
                        <input type="text" id="room-input" class="room-input" 
                               placeholder="ID комнаты" maxlength="10">
                        <game-button type="primary" size="large" data-action="join-room">
                            🚪 Присоединиться
                        </game-button>
                    </div>
                </div>
            </div>
        `;
    }

    styles() {
        return `
            .screen {
                max-width: 600px;
                margin: 0 auto;
                padding: 20px;
                animation: fadeIn 0.5s ease-out;
            }

            .menu-actions {
                display: flex;
                flex-direction: column;
                gap: 20px;
                margin-top: 30px;
            }

            .join-section {
                display: flex;
                flex-direction: column;
                gap: 15px;
                align-items: center;
            }

            .room-input {
                padding: 15px;
                font-size: 18px;
                border: 2px solid var(--primary-dark);
                border-radius: 8px;
                text-align: center;
                text-transform: uppercase;
                width: 200px;
            }

            .room-input:focus {
                outline: none;
                border-color: var(--primary-color);
                box-shadow: 0 0 10px rgba(218, 165, 32, 0.3);
            }

            .role-princess { color: #8b0000; font-weight: bold; }
            .role-helper { color: #155724; font-weight: bold; }

            @keyframes fadeIn {
                from { opacity: 0; transform: translateY(20px); }
                to { opacity: 1; transform: translateY(0); }
            }

            @media (max-width: 768px) {
                .screen { padding: 10px; }
                .room-input { width: 100%; }
            }
        `;
    }

    setupEventListeners() {
        this.on('[data-action="create-room"]', 'click', () => {
            window.app.socket.createRoom();
        });

        this.on('[data-action="join-room"]', 'click', () => {
            const roomId = this.$('#room-input').value.trim().toUpperCase();
            if (roomId) {
                window.app.socket.joinRoom(roomId);
            } else {
                this.showError('Введите ID комнаты!');
            }
        });

        this.on('#room-input', 'keypress', (e) => {
            if (e.key === 'Enter') {
                this.$('[data-action="join-room"]').click();
            }
        });
    }

    showError(message) {
        // Можно создать отдельный компонент для уведомлений
        alert(message);
    }
}

customElements.define('main-menu-screen', MainMenuScreen);
