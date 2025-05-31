const { produce, enableMapSet, setAutoFreeze, enablePatches } = require('immer');

// Включаем поддержку Map/Set если понадобится в будущем
enableMapSet();

// Включаем поддержку патчей для отладки
enablePatches();

// В production замораживаем объекты для дополнительной безопасности
if (process.env.NODE_ENV === 'production') {
    setAutoFreeze(true);
}

class ImmerStateManager {
    constructor() {
        this.enablePatches = process.env.NODE_ENV === 'development';
        this.patchListeners = [];
    }

    /**
     * Безопасное обновление состояния с помощью Immer
     * @param {Object} currentState - Текущее состояние
     * @param {Function} updater - Функция изменения draft состояния
     * @returns {Object} Новое иммутабельное состояние
     */
    updateState(currentState, updater) {
        return produce(currentState, updater, (patches, inversePatches) => {
            if (this.enablePatches) {
                console.log('🔄 State patches:', patches);
                console.log('↩️ Inverse patches:', inversePatches);
                
                // Уведомляем слушателей об изменениях
                this.patchListeners.forEach(listener => {
                    listener(patches, inversePatches);
                });
            }
        });
    }

    /**
     * Создание иммутабельной копии состояния
     * @param {Object} state - Исходное состояние
     * @returns {Object} Иммутабельная копия
     */
    createDraft(state) {
        return produce(state, draft => draft);
    }

    /**
     * Добавить слушатель изменений состояния
     * @param {Function} listener - Функция обработки патчей
     */
    addPatchListener(listener) {
        this.patchListeners.push(listener);
    }

    /**
     * Удалить слушатель изменений состояния
     * @param {Function} listener - Функция для удаления
     */
    removePatchListener(listener) {
        const index = this.patchListeners.indexOf(listener);
        if (index > -1) {
            this.patchListeners.splice(index, 1);
        }
    }

    /**
     * Применить патчи к состоянию (для отладки/восстановления)
     * @param {Object} state - Текущее состояние
     * @param {Array} patches - Массив патчей
     * @returns {Object} Новое состояние
     */
    applyPatches(state, patches) {
        return produce(state, draft => {
            patches.forEach(patch => {
                if (patch.op === 'replace') {
                    this.setPath(draft, patch.path, patch.value);
                } else if (patch.op === 'add') {
                    this.setPath(draft, patch.path, patch.value);
                } else if (patch.op === 'remove') {
                    this.removePath(draft, patch.path);
                }
            });
        });
    }

    /**
     * Установить значение по пути в объекте
     * @param {Object} obj - Объект
     * @param {Array} path - Путь к свойству
     * @param {*} value - Значение
     */
    setPath(obj, path, value) {
        let current = obj;
        for (let i = 0; i < path.length - 1; i++) {
            current = current[path[i]];
        }
        current[path[path.length - 1]] = value;
    }

    /**
     * Удалить значение по пути в объекте
     * @param {Object} obj - Объект
     * @param {Array} path - Путь к свойству
     */
    removePath(obj, path) {
        let current = obj;
        for (let i = 0; i < path.length - 1; i++) {
            current = current[path[i]];
        }
        delete current[path[path.length - 1]];
    }

    /**
     * Проверить, является ли объект иммутабельным (заморожен)
     * @param {Object} obj - Объект для проверки
     * @returns {boolean} true если объект заморожен
     */
    isFrozen(obj) {
        return Object.isFrozen(obj);
    }

    /**
     * Получить статистику использования
     * @returns {Object} Статистика
     */
    getStats() {
        return {
            patchListenersCount: this.patchListeners.length,
            patchingEnabled: this.enablePatches,
            autoFreezeEnabled: process.env.NODE_ENV === 'production'
        };
    }
}

module.exports = ImmerStateManager;