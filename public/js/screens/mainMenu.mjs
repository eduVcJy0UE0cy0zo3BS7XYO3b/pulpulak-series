// Mithril is loaded globally from CDN
const m = window.m;
import NotificationManager from '../notificationManager.mjs';

const MainMenu = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
    },

    createRoom() {
        this.app.showGameSelection('create');
    },

    joinRoom() {
        const roomId = document.getElementById('roomInput').value.trim().toUpperCase();
        if (roomId) {
            this.app.showGameSelection('join', roomId);
        } else {
            NotificationManager.add('Введите ID комнаты!', 'warning');
        }
    },

    handleKeyPress(e) {
        if (e.key === 'Enter') {
            this.joinRoom();
        }
    },

    view() {
        return m('div#main-menu.menu-container.fade-in', [
            // User info and logout
            m('.user-info', {
                style: 'text-align: right; margin-bottom: 10px;'
            }, [
                m('span', { style: 'margin-right: 10px;' }, `👤 ${this.app.username}`),
                m('button.btn.btn-secondary', {
                    onclick: () => this.app.logout(),
                    style: 'padding: 5px 10px; font-size: 0.9em;'
                }, '🚪 Выйти')
            ]),
            
            m('.card', [
                m('.card-header', '🎮 Кооперативные игры'),
                m('p', 
                    'Играйте вдвоем в интерактивных текстовых приключениях! Каждая игра предлагает уникальную историю, где игроки берут на себя разные роли и принимают совместные решения.'
                ),
                m('p', [
                    m('strong', '✨ Особенности:'),
                    m('br'),
                    '🎭 Различные роли для каждого игрока',
                    m('br'),
                    '🤝 Совместное принятие решений',
                    m('br'),
                    '📖 Богатые интерактивные сюжеты'
                ])
            ]),
            
            m('button.btn.btn-primary.btn-large', {
                onclick: () => this.createRoom()
            }, '🏠 Создать комнату'),
            
            m('.text-center.mt-2', [
                m('input#roomInput.input-field', {
                    type: 'text',
                    placeholder: 'ID комнаты',
                    maxlength: 10,
                    onkeypress: (e) => this.handleKeyPress(e)
                }),
                m('br'),
                m('button.btn.btn-primary.btn-large', {
                    onclick: () => this.joinRoom()
                }, '🚪 Присоединиться')
            ])
        ]);
    }
};

export default MainMenu;