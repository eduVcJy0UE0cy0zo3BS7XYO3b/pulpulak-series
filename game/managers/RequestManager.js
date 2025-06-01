/**
 * RequestManager - Универсальная система обработки запросов
 * Управляет всеми типами запросов между игроками
 */

class RequestManager {
    constructor(gameDataManager, playerDataManager) {
        this.gameData = gameDataManager;
        this.playerData = playerDataManager;
        this.activeRequests = new Map(); // roomId -> { type, data, ... }
        this.requestHandlers = new Map(); // requestType -> handler
    }

    /**
     * Зарегистрировать обработчик для типа запроса
     */
    registerRequestHandler(requestType, handler) {
        if (!handler.validate || !handler.create || !handler.respond) {
            throw new Error(`Обработчик для ${requestType} должен содержать методы: validate, create, respond`);
        }
        this.requestHandlers.set(requestType, handler);
    }

    /**
     * Создать запрос
     */
    createRequest(roomId, requestType, fromPlayerId, fromCharacter, requestData = {}) {
        // Проверяем, есть ли обработчик для данного типа
        const handler = this.requestHandlers.get(requestType);
        if (!handler) {
            return { success: false, message: `Неизвестный тип запроса: ${requestType}` };
        }

        // Проверяем, нет ли активного запроса
        if (this.hasActiveRequest(roomId)) {
            return { success: false, message: "Уже есть активный запрос" };
        }

        // Валидируем запрос через обработчик
        const validation = handler.validate(roomId, fromCharacter, requestData);
        if (!validation.valid) {
            return { success: false, message: validation.message };
        }

        // Создаём запрос через обработчик
        const result = handler.create(roomId, fromPlayerId, fromCharacter, requestData);
        if (!result.success) {
            return result;
        }

        // Сохраняем запрос
        const request = {
            id: this.generateRequestId(),
            type: requestType,
            roomId: roomId,
            fromPlayerId: fromPlayerId,
            fromCharacter: fromCharacter,
            targetPlayerId: result.targetPlayerId,
            targetCharacter: result.targetCharacter,
            data: result.data || {},
            timestamp: Date.now()
        };

        this.activeRequests.set(roomId, request);

        return {
            success: true,
            request: request,
            message: result.message
        };
    }

    /**
     * Ответить на запрос
     */
    respondToRequest(roomId, playerId, accepted, responseData = {}) {
        const request = this.activeRequests.get(roomId);
        if (!request) {
            return { success: false, message: "Запрос не найден" };
        }

        if (request.targetPlayerId !== playerId) {
            return { success: false, message: "Этот запрос не для вас" };
        }

        const handler = this.requestHandlers.get(request.type);
        if (!handler) {
            return { success: false, message: "Обработчик запроса не найден" };
        }

        // Удаляем запрос из активных
        this.activeRequests.delete(roomId);

        // Обрабатываем ответ через обработчик
        return handler.respond(roomId, request, accepted, responseData);
    }

    /**
     * Получить активный запрос для комнаты
     */
    getActiveRequest(roomId) {
        return this.activeRequests.get(roomId) || null;
    }

    /**
     * Проверить, есть ли активный запрос
     */
    hasActiveRequest(roomId) {
        return this.activeRequests.has(roomId);
    }

    /**
     * Отменить активный запрос
     */
    cancelRequest(roomId) {
        const request = this.activeRequests.get(roomId);
        this.activeRequests.delete(roomId);
        return request;
    }

    /**
     * Очистить все запросы для комнаты
     */
    clearRoomRequests(roomId) {
        this.activeRequests.delete(roomId);
    }


    /**
     * ВСПОМОГАТЕЛЬНЫЕ МЕТОДЫ
     */

    generateRequestId() {
        return Math.random().toString(36).substring(2, 15);
    }

    /**
     * Получить статистику запросов (для отладки)
     */
    getRequestStats() {
        return {
            activeRequests: this.activeRequests.size,
            rooms: Array.from(this.activeRequests.keys()),
            requestTypes: Array.from(this.requestHandlers.keys())
        };
    }
}

module.exports = RequestManager;