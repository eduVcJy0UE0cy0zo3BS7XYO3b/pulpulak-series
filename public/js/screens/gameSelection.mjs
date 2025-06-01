// Mithril is loaded globally from CDN
const m = window.m;
import NotificationManager from '../notificationManager.mjs';

const GameSelection = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
        this.availableGames = [];
        this.loading = true;
        this.selectedGame = null;
        this.mode = vnode.attrs.data?.mode || 'create'; // 'create' or 'join'
        this.roomId = vnode.attrs.data?.roomId || '';
        
        // Fetch available games from server
        this.fetchAvailableGames();
    },

    async fetchAvailableGames() {
        try {
            this.loading = true;
            m.redraw();

            const response = await fetch('/api/games');
            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const data = await response.json();
            console.log('🎮 Received games data:', data);
            // API returns array directly, not wrapped in {games: [...]}
            this.availableGames = Array.isArray(data) ? data : (data.games || []);
            console.log('🎮 Available games:', this.availableGames);
            
            // Pre-select first game if available
            if (this.availableGames.length > 0) {
                this.selectedGame = this.availableGames[0].id;
                console.log('🎮 Pre-selected game:', this.selectedGame);
            }
            
            this.loading = false;
            m.redraw();
        } catch (error) {
            console.error('❌ Failed to fetch games:', error);
            NotificationManager.add('Не удалось загрузить список игр', 'error');
            this.loading = false;
            m.redraw();
        }
    },

    selectGame(gameId) {
        this.selectedGame = gameId;
        m.redraw();
    },

    getGameInfo(gameId) {
        return this.availableGames.find(game => game.id === gameId);
    },

    proceedWithGame() {
        if (!this.selectedGame) {
            NotificationManager.add('Выберите игру!', 'warning');
            return;
        }

        const gameInfo = this.getGameInfo(this.selectedGame);
        console.log(`🎮 Выбрана игра: ${gameInfo.name} (${this.selectedGame})`);

        if (this.mode === 'create') {
            // Create room with selected game
            this.app.createRoomWithGame(this.selectedGame);
        } else {
            // Join room (game will be determined by room)
            this.app.joinRoom(this.roomId);
        }
    },

    backToMenu() {
        this.app.showScreen('mainMenu');
    },

    createGameCard(game) {
        const isSelected = this.selectedGame === game.id;
        
        return m('.game-card', {
            class: isSelected ? 'selected' : '',
            onclick: () => this.selectGame(game.id)
        }, [
            m('.game-icon', game.icon || '🎮'),
            m('.game-info', [
                m('h3', game.name || 'Неизвестная игра'),
                m('p.game-description', game.description || 'Описание недоступно'),
                m('.game-meta', [
                    m('span.game-players', `👥 ${game.minPlayers || 2}-${game.maxPlayers || 2} игроков`),
                    (game.estimatedPlayTime || game.estimatedDuration) && m('span.game-time', `⏱️ ${game.estimatedPlayTime || game.estimatedDuration}`)
                ]),
                (game.tags && game.tags.length > 0) && m('.game-tags', 
                    game.tags.map(tag => m('span.tag', tag))
                )
            ]),
            isSelected && m('.selection-indicator', '✓')
        ]);
    },

    view() {
        if (this.loading) {
            return m('div#game-selection.menu-container.fade-in', [
                m('button.btn.btn-secondary.mb-2', {
                    onclick: () => this.backToMenu()
                }, '← В главное меню'),
                
                m('.loading-container.text-center', [
                    m('h2', '🎮 Загрузка игр...'),
                    m('.spinner', '⟳')
                ])
            ]);
        }

        if (this.availableGames.length === 0) {
            return m('div#game-selection.menu-container.fade-in', [
                m('button.btn.btn-secondary.mb-2', {
                    onclick: () => this.backToMenu()
                }, '← В главное меню'),
                
                m('.error-container.text-center', [
                    m('h2', '❌ Игры не найдены'),
                    m('p', 'Сервер не возвратил доступных игр. Попробуйте позже.'),
                    m('button.btn.btn-primary', {
                        onclick: () => this.fetchAvailableGames()
                    }, '🔄 Повторить')
                ])
            ]);
        }

        const selectedGameInfo = this.getGameInfo(this.selectedGame);
        const modeText = this.mode === 'create' ? 'создания комнаты' : 'присоединения';
        const buttonText = this.mode === 'create' ? '🏠 Создать комнату' : '🚪 Присоединиться';

        return m('div#game-selection.menu-container.fade-in', [
            m('button.btn.btn-secondary.mb-2', {
                onclick: () => this.backToMenu()
            }, '← В главное меню'),
            
            m('.selection-header.text-center', [
                m('h2', `🎮 Выбор игры для ${modeText}`),
                this.mode === 'join' && m('p', `Присоединение к комнате: ${this.roomId}`)
            ]),

            m('.games-grid', 
                this.availableGames.map(game => this.createGameCard(game))
            ),

            selectedGameInfo && m('.selected-game-info', [
                m('h3', `Выбрана: ${selectedGameInfo.name}`),
                m('p', selectedGameInfo.description),
                selectedGameInfo.tags && m('.game-tags', 
                    selectedGameInfo.tags.map(tag => m('span.tag', tag))
                )
            ]),

            m('.selection-controls.text-center.mt-2', [
                m('button.btn.btn-primary.btn-large', {
                    disabled: !this.selectedGame,
                    onclick: () => this.proceedWithGame()
                }, buttonText),
                
                !this.selectedGame && m('p.help-text', 'Выберите игру из списка выше')
            ])
        ]);
    }
};

export default GameSelection;