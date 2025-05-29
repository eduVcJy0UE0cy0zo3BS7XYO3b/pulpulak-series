// Mithril is loaded globally from CDN
const m = window.m;
import NotificationManager from '../notificationManager.mjs';

const Login = {
    oninit(vnode) {
        this.app = vnode.attrs.app;
        this.username = '';
        this.error = '';
    },

    handleLogin() {
        const trimmedUsername = this.username.trim();
        
        if (!trimmedUsername) {
            this.error = 'Пожалуйста, введите имя пользователя';
            return;
        }
        
        if (trimmedUsername.length < 2) {
            this.error = 'Имя должно быть не менее 2 символов';
            return;
        }
        
        if (trimmedUsername.length > 20) {
            this.error = 'Имя не должно превышать 20 символов';
            return;
        }
        
        // Save username to localStorage
        localStorage.setItem('username', trimmedUsername);
        
        // Update app state
        this.app.setUsername(trimmedUsername);
        
        // Go to main menu
        this.app.showScreen('mainMenu');
    },

    handleKeyPress(e) {
        if (e.key === 'Enter') {
            this.handleLogin();
        }
    },

    view() {
        return m('div#login-screen.menu-container.fade-in', [
            m('.card', [
                m('.card-header', '👋 Добро пожаловать!'),
                m('p', 'Введите ваше имя, чтобы начать игру'),
                
                m('.login-form', [
                    m('input#username-input.input-field', {
                        type: 'text',
                        placeholder: 'Ваше имя',
                        maxlength: 20,
                        value: this.username,
                        oninput: (e) => {
                            this.username = e.target.value;
                            this.error = ''; // Clear error on input
                        },
                        onkeypress: (e) => this.handleKeyPress(e),
                        oncreate: (vnode) => {
                            // Auto-focus the input
                            vnode.dom.focus();
                        }
                    }),
                    
                    this.error && m('.error-message', {
                        style: 'color: red; margin-top: 10px;'
                    }, this.error),
                    
                    m('button.btn.btn-primary.btn-large', {
                        onclick: () => this.handleLogin(),
                        style: 'margin-top: 20px;'
                    }, '🚀 Войти в игру')
                ])
            ])
        ]);
    }
};

export default Login;