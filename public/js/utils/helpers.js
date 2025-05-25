// Утилиты для загрузки компонентов и других вспомогательных функций

export async function loadComponent(componentName) {
    try {
        const response = await fetch(`components/${componentName}.html`);
        if (!response.ok) {
            throw new Error(`Не удалось загрузить компонент: ${componentName}`);
        }
        
        const html = await response.text();
        const container = document.createElement('div');
        container.innerHTML = html;
        
        return container.firstElementChild;
    } catch (error) {
        console.error('❌ Ошибка загрузки компонента:', error);
        const errorElement = document.createElement('div');
        errorElement.innerHTML = `<div class="card"><h2>Ошибка загрузки</h2><p>Не удалось загрузить компонент: ${componentName}</p></div>`;
        return errorElement.firstElementChild;
    }
}

export function generateId(length = 8) {
    const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    let result = '';
    for (let i = 0; i < length; i++) {
        result += chars.charAt(Math.floor(Math.random() * chars.length));
    }
    return result;
}

export function formatTime(date) {
    return new Intl.DateTimeFormat('ru-RU', {
        hour: '2-digit',
        minute: '2-digit'
    }).format(date);
}

export function sanitizeHTML(str) {
    const div = document.createElement('div');
    div.textContent = str;
    return div.innerHTML;
}

export function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}
