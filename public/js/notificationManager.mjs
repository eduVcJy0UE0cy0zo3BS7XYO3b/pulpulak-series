// Mithril is loaded globally from CDN
const m = window.m;

class NotificationManager {
    constructor() {
        this.notifications = [];
        this.nextId = 1;
    }

    add(message, type = 'info', duration = 3000) {
        const id = this.nextId++;
        const notification = {
            id,
            message,
            type, // 'info', 'success', 'error', 'warning'
            timestamp: Date.now()
        };
        
        this.notifications.push(notification);
        
        // Auto-remove after duration
        if (duration > 0) {
            setTimeout(() => {
                this.remove(id);
            }, duration);
        }
        
        m.redraw();
        return id;
    }

    remove(id) {
        this.notifications = this.notifications.filter(n => n.id !== id);
        m.redraw();
    }

    clear() {
        this.notifications = [];
        m.redraw();
    }

    getComponent() {
        return {
            view: () => {
                if (this.notifications.length === 0) return null;
                
                return m('.notification-container', {
                    style: 'position: fixed; top: 20px; right: 20px; z-index: 1000; max-width: 400px;'
                }, this.notifications.map(notification => 
                    m('.notification', {
                        key: notification.id,
                        class: `notification-${notification.type} fade-in`,
                        style: `
                            background: ${this.getBackgroundColor(notification.type)};
                            color: white;
                            padding: 15px;
                            margin-bottom: 10px;
                            border-radius: 5px;
                            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
                            cursor: pointer;
                        `,
                        onclick: () => this.remove(notification.id)
                    }, [
                        m('span', notification.message),
                        m('span', {
                            style: 'float: right; margin-left: 10px; opacity: 0.7;'
                        }, '✕')
                    ])
                ));
            }
        };
    }

    getBackgroundColor(type) {
        const colors = {
            info: '#3498db',
            success: '#27ae60',
            error: '#e74c3c',
            warning: '#f39c12'
        };
        return colors[type] || colors.info;
    }
}

export default new NotificationManager();