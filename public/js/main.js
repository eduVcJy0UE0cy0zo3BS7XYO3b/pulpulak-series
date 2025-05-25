import SocketManager from './socketManager.js';
import MainMenu from './screens/mainMenu.js';
import Lobby from './screens/lobby.js';
import CoopGame from './screens/coopGame.js';
import { loadComponent } from './utils/helpers.js';

class App {
    constructor() {
        this.container = document.getElementById('app-container');
        this.currentScreen = null;
        
        // Инициализируем менеджеры
        this.socketManager = new SocketManager();
        
        // Инициализируем экраны
        this.screens = {
            mainMenu: new MainMenu(this),
            lobby: new Lobby(this),
            coopGame: new CoopGame(this)
        };
        
        this.init();
    }

    async init() {
        console.log('🎮 Инициализация приложения...');
        
        // Настраиваем обработчики событий сокетов
        this.setupSocketEvents();
        
        // Показываем главное меню
        await this.showScreen('mainMenu');
        
        console.log('✅ Приложение инициализировано');
    }

    async showScreen(screenName, data = null) {
        if (this.currentScreen === screenName) return;
        
        console.log(`🔄 Переход на экран: ${screenName}`);
        
        // Скрываем текущий экран
        if (this.currentScreen && this.screens[this.currentScreen]) {
            this.screens[this.currentScreen].hide();
        }
        
        // Загружаем и показываем новый экран
        const screen = this.screens[screenName];
        if (screen) {
            await screen.show(data);
            this.currentScreen = screenName;
        } else {
            console.error(`❌ Экран не найден: ${screenName}`);
        }
    }

    setupSocketEvents() {
        const socket = this.socketManager.socket;
        
        // Обработчики для лобби
        socket.on('room-created', (data) => {
            console.log('✅ Комната создана:', data);
            this.showScreen('lobby', { ...data, isHost: true });
        });

        socket.on('room-joined', (data) => {
            console.log('✅ Присоединились к комнате:', data);
            this.showScreen('lobby', { ...data, isHost: false });
        });

        socket.on('lobby-update', (data) => {
            console.log('🔄 Обновление лобби:', data);
            if (this.currentScreen === 'lobby') {
                this.screens.lobby.updateLobby(data);
            }
        });

        // Обработчики для игры
        socket.on('game-started', (data) => {
            console.log('🎮 Игра началась:', data);
            this.showScreen('coopGame', data);
        });

        socket.on('game-update', (data) => {
            console.log('🔄 Обновление игры:', data);
            if (this.currentScreen === 'coopGame') {
                this.screens.coopGame.updateGame(data);
            }
        });

        // Общие обработчики
        socket.on('error', (message) => {
            console.error('❌ Ошибка сервера:', message);
            alert(`Ошибка: ${message}`);
        });

        socket.on('player-left', (data) => {
            console.log('👋 Игрок покинул игру:', data);
            if (this.currentScreen === 'lobby') {
                this.screens.lobby.updateLobby(data);
            } else if (this.currentScreen === 'coopGame') {
                alert(`Игрок ${data.playerName} покинул игру`);
                this.showScreen('mainMenu');
            }
        });
    }

    // Методы для взаимодействия с сокетами
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
}

// Запуск приложения
document.addEventListener('DOMContentLoaded', () => {
    console.log('🚀 Запуск приложения...');
    window.app = new App();
});
