import { loadComponent } from '../utils/helpers.js';

class MainMenu {
    constructor(app) {
        this.app = app;
        this.element = null;
    }

    async show() {
        this.element = await loadComponent('mainMenu');
        this.app.container.appendChild(this.element);
        this.setupEventListeners();
    }

    hide() {
        if (this.element) {
            this.element.remove();
            this.element = null;
        }
    }

    setupEventListeners() {
        // Создание комнаты
        this.element.querySelector('[data-action="create-room"]').addEventListener('click', () => {
            this.app.createRoom();
        });

        // Присоединение к комнате
        this.element.querySelector('[data-action="join-room"]').addEventListener('click', () => {
            const roomId = this.element.querySelector('#roomInput').value.trim().toUpperCase();
            if (roomId) {
                this.app.joinRoom(roomId);
            } else {
                alert('Введите ID комнаты!');
            }
        });

        // Enter в поле ввода
        this.element.querySelector('#roomInput').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') {
                this.element.querySelector('[data-action="join-room"]').click();
            }
        });
    }
}

export default MainMenu;
