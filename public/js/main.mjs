// Mithril is loaded globally from CDN
const m = window.m;
import SocketManager from './socketManager.js';
import NotificationManager from './notificationManager.mjs';
import Login from './screens/login.mjs';
import MainMenu from './screens/mainMenu.mjs';
import Lobby from './screens/lobby.mjs';
import CoopGame from './screens/coopGame.mjs';

class App {
    constructor() {
        this.socketManager = new SocketManager();
        this.currentScreen = null;
        this.screenData = null;
        this.username = localStorage.getItem('username') || null;
        
        // Current screen component references
        this.screens = {
            login: Login,
            mainMenu: MainMenu,
            lobby: Lobby,
            coopGame: CoopGame
        };
        
        this.init();
    }

    init() {
        console.log('🎮 Инициализация приложения...');
        
        // Setup socket events
        this.setupSocketEvents();
        
        // Check if user is logged in
        if (this.username) {
            this.showScreen('mainMenu');
        } else {
            this.showScreen('login');
        }
        
        console.log('✅ Приложение инициализировано');
    }

    showScreen(screenName, data = null) {
        if (this.currentScreen === screenName && !data) return;
        
        console.log(`🔄 Переход на экран: ${screenName}`);
        
        this.currentScreen = screenName;
        this.screenData = data;
        
        // Mount the component
        const ScreenComponent = this.screens[screenName];
        if (ScreenComponent) {
            m.mount(document.getElementById('app-container'), {
                view: () => [
                    m(ScreenComponent, { app: this, data: this.screenData }),
                    m(NotificationManager.getComponent())
                ]
            });
        } else {
            console.error(`❌ Экран не найден: ${screenName}`);
        }
    }

    setupSocketEvents() {
        const socket = this.socketManager.socket;
        
        // Lobby events
        socket.on('room-created', (data) => {
            console.log('✅ Комната создана:', data);
            this.showScreen('lobby', data);
        });

        socket.on('room-joined', (data) => {
            console.log('✅ Присоединились к комнате:', data);
            this.showScreen('lobby', data);
        });

        socket.on('lobby-update', (data) => {
            console.log('🔄 Обновление лобби:', data);
            if (this.currentScreen === 'lobby') {
                // Update screen data
                this.screenData = { 
                    ...this.screenData, 
                    ...data
                };
                m.redraw();
            }
        });

        // Game events
        socket.on('game-started', (data) => {
            console.log('🎮 Игра началась:', data);
            this.showScreen('coopGame', data);
        });

        socket.on('game-update', (data) => {
            console.log('🔄 Обновление игры:', data);
            if (this.currentScreen === 'coopGame') {
                // Update screen data and redraw
                this.screenData = data;
                m.redraw();
            }
        });

        // Common events
        socket.on('error', (message) => {
            console.error('❌ Ошибка сервера:', message);
            NotificationManager.add(message, 'error', 5000);
        });

        socket.on('player-left', (data) => {
            console.log('👋 Игрок покинул игру:', data);
            if (this.currentScreen === 'lobby') {
                m.redraw();
            } else if (this.currentScreen === 'coopGame') {
                NotificationManager.add(`Игрок ${data.playerName} покинул игру`, 'warning');
                this.showScreen('mainMenu');
            }
        });
    }

    // Socket methods
    createRoom() {
        this.socketManager.createRoom();
    }

    joinRoom(roomId) {
        this.socketManager.joinRoom(roomId);
    }

    startCoopGame(roomId) {
        this.socketManager.startCoopGame(roomId);
    }

    makeChoice(roomId, choiceId, character) {
        this.socketManager.makeChoice(roomId, choiceId, character);
    }

    sendChatMessage(roomId, message) {
        this.socketManager.sendChatMessage(roomId, message);
    }

    leaveRoom(roomId) {
        this.socketManager.leaveRoom(roomId);
    }

    setUsername(username) {
        this.username = username;
        this.socketManager.setUsername(username);
    }

    logout() {
        localStorage.removeItem('username');
        this.username = null;
        this.showScreen('login');
    }
}

// Launch application
document.addEventListener('DOMContentLoaded', () => {
    console.log('🚀 Запуск приложения...');
    window.app = new App();
    window.NotificationManager = NotificationManager; // Make it globally available
});