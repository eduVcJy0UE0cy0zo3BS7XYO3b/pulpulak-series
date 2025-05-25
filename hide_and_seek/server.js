const express = require('express');
const http = require('http');
const socketIo = require('socket.io');

const app = express();
const server = http.createServer(app);
const io = socketIo(server);

app.use(express.static('public'));

const rooms = {};

// Игра в прятки
const hideAndSeekGame = {
    locations: [
	{ id: 'closet', name: '🚪 Шкаф', description: 'Темный шкаф с одеждой' },
	{ id: 'bed', name: '🛏️ Под кроватью', description: 'Пыльное место под кроватью' },
	{ id: 'kitchen', name: '🍳 Кухонный шкафчик', description: 'Шкафчик с посудой' },
	{ id: 'bathroom', name: '🚿 За шторкой в ванной', description: 'За занавеской душа' },
	{ id: 'attic', name: '🪜 Чердак', description: 'Старый пыльный чердак' },
	{ id: 'basement', name: '🔦 Подвал', description: 'Темный подвал с коробками' }
    ]
};

function cleanupRoom(roomId) {
    if (rooms[roomId]) {
	// Удаляем отключенные сокеты
	const activeSockets = [];
	for (const socketId of rooms[roomId].players) {
	    const socket = io.sockets.sockets.get(socketId);
	    if (socket && socket.connected) {
		activeSockets.push(socketId);
	    }
	}
	rooms[roomId].players = activeSockets;
	
	if (rooms[roomId].players.length === 0) {
	    console.log(`Комната ${roomId} удалена - нет активных игроков`);
	    delete rooms[roomId];
	    return false;
	}
    }
    return true;
}

io.on('connection', (socket) => {
    console.log('Игрок подключился:', socket.id);

    socket.on('create-room', () => {
	const roomId = Math.random().toString(36).substring(7);
	rooms[roomId] = {
	    players: [socket.id],
	    gameState: 'waiting',
	    seeker: null,
	    hider: null,
	    hideLocation: null,
	    searchAttempts: 0,
	    maxAttempts: 3,
	    searchedLocations: [],
	    gameResult: null
	};
	socket.join(roomId);
	console.log(`Комната ${roomId} создана игроком ${socket.id}`);
	
	socket.emit('room-created', roomId);
	socket.emit('game-update', {
	    gameState: 'waiting',
	    playerCount: 1,
	    message: 'Ожидание второго игрока...'
	});
    });

    socket.on('join-room', (roomId) => {
	console.log(`Игрок ${socket.id} пытается присоединиться к комнате ${roomId}`);
	
	if (!rooms[roomId]) {
	    socket.emit('error', 'Комната не найдена');
	    return;
	}

	// Очищаем комнату от неактивных соединений
	cleanupRoom(roomId);
	
	// Проверяем, не в комнате ли уже этот игрок
	if (!rooms[roomId].players.includes(socket.id)) {
	    if (rooms[roomId].players.length >= 2) {
		socket.emit('error', 'Комната полна');
		return;
	    }
	    rooms[roomId].players.push(socket.id);
	}
	
	socket.join(roomId);
	console.log(`Игрок ${socket.id} в комнате ${roomId}. Всего игроков: ${rooms[roomId].players.length}`);

	// Если это первый игрок, ждем второго
	if (rooms[roomId].players.length === 1) {
	    socket.emit('game-update', {
		gameState: 'waiting',
		playerCount: 1,
		message: 'Ожидание второго игрока...'
	    });
	    return;
	}

	// Если оба игрока есть, начинаем игру
	if (rooms[roomId].players.length === 2 && rooms[roomId].gameState === 'waiting') {
	    const players = rooms[roomId].players;
	    const seekerIndex = Math.floor(Math.random() * 2);
	    rooms[roomId].seeker = players[seekerIndex];
	    rooms[roomId].hider = players[1 - seekerIndex];
	    rooms[roomId].gameState = 'hiding';
	    
	    console.log(`Игра началась в комнате ${roomId}`);
	    console.log(`Ищущий: ${rooms[roomId].seeker}, Прячущийся: ${rooms[roomId].hider}`);
	    
	    // Отправляем разные сообщения игрокам
	    io.to(rooms[roomId].seeker).emit('game-update', {
		gameState: 'waiting_for_hider',
		role: 'seeker',
		playerCount: 2,
		message: 'Вы - ИЩУЩИЙ! 🔍 Ждите, пока напарник спрячется...',
		maxAttempts: rooms[roomId].maxAttempts
	    });
	    
	    io.to(rooms[roomId].hider).emit('game-update', {
		gameState: 'hiding',
		role: 'hider',
		playerCount: 2,
		message: 'Вы - ПРЯЧУЩИЙСЯ! 🙈 Выберите место, где спрятаться:',
		locations: hideAndSeekGame.locations
	    });
	} else {
	    // Игра уже идет, отправляем текущее состояние
	    const room = rooms[roomId];
	    if (socket.id === room.seeker) {
		socket.emit('game-update', {
		    gameState: room.gameState === 'hiding' ? 'waiting_for_hider' : room.gameState,
		    role: 'seeker',
		    playerCount: 2,
		    message: room.gameState === 'hiding' ? 'Ждите, пока напарник спрячется...' : 'Ищите!',
		    locations: room.gameState === 'seeking' ? hideAndSeekGame.locations : undefined,
		    attempts: room.searchAttempts,
		    maxAttempts: room.maxAttempts,
		    searchedLocations: room.searchedLocations
		});
	    } else if (socket.id === room.hider) {
		socket.emit('game-update', {
		    gameState: room.gameState === 'hiding' ? 'hiding' : 'hidden',
		    role: 'hider',
		    playerCount: 2,
		    message: room.gameState === 'hiding' ? 'Выберите место, где спрятаться:' : 'Вы спрятались, ждите...',
		    locations: room.gameState === 'hiding' ? hideAndSeekGame.locations : undefined
		});
	    }
	}
    });

    socket.on('hide-in-location', (data) => {
	const room = Object.values(rooms).find(r => r.players.includes(socket.id));
	if (!room || room.hider !== socket.id || room.gameState !== 'hiding') return;

	const roomId = Object.keys(rooms).find(key => rooms[key] === room);
	room.hideLocation = data.locationId;
	room.gameState = 'seeking';
	
	const location = hideAndSeekGame.locations.find(loc => loc.id === data.locationId);
	console.log(`Игрок спрятался в: ${location.name}`);

	// Сообщаем прячущемуся
	io.to(room.hider).emit('game-update', {
	    gameState: 'hidden',
	    role: 'hider',
	    message: `Вы спрятались в: ${location.name}. Ждите, пока вас ищут...`,
	    hideLocation: location
	});

	// Начинаем поиск для ищущего
	io.to(room.seeker).emit('game-update', {
	    gameState: 'seeking',
	    role: 'seeker',
	    message: `Начинайте поиск! У вас ${room.maxAttempts} попыток.`,
	    locations: hideAndSeekGame.locations,
	    attempts: room.searchAttempts,
	    maxAttempts: room.maxAttempts,
	    searchedLocations: room.searchedLocations
	});
    });

    socket.on('search-location', (data) => {
	const room = Object.values(rooms).find(r => r.players.includes(socket.id));
	if (!room || room.seeker !== socket.id || room.gameState !== 'seeking') return;

	const roomId = Object.keys(rooms).find(key => rooms[key] === room);
	const location = hideAndSeekGame.locations.find(loc => loc.id === data.locationId);
	
	room.searchAttempts++;
	room.searchedLocations.push(data.locationId);
	
	const found = room.hideLocation === data.locationId;
	
	console.log(`Поиск в: ${location.name}, найден: ${found}`);

	if (found) {
	    room.gameState = 'finished';
	    room.gameResult = 'seeker_wins';
	    
	    io.to(roomId).emit('game-update', {
		gameState: 'finished',
		result: 'seeker_wins',
		message: `🎉 НАЙДЕН! Ищущий нашел прячущегося в: ${location.name}`,
		hideLocation: hideAndSeekGame.locations.find(loc => loc.id === room.hideLocation)
	    });
	    
	} else if (room.searchAttempts >= room.maxAttempts) {
	    room.gameState = 'finished';
	    room.gameResult = 'hider_wins';
	    const hideLocation = hideAndSeekGame.locations.find(loc => loc.id === room.hideLocation);
	    
	    io.to(roomId).emit('game-update', {
		gameState: 'finished',
		result: 'hider_wins',
		message: `😎 ПРЯЧУЩИЙСЯ ПОБЕДИЛ! Он был в: ${hideLocation.name}`,
		hideLocation: hideLocation
	    });
	    
	} else {
	    io.to(room.seeker).emit('game-update', {
		gameState: 'seeking',
		role: 'seeker',
		message: `Не там! Попробуйте еще. Осталось попыток: ${room.maxAttempts - room.searchAttempts}`,
		locations: hideAndSeekGame.locations,
		attempts: room.searchAttempts,
		maxAttempts: room.maxAttempts,
		searchedLocations: room.searchedLocations
	    });

	    io.to(room.hider).emit('game-update', {
		gameState: 'hidden',
		role: 'hider',
		message: `Вас ищут... Попытка ${room.searchAttempts}/${room.maxAttempts}. Пока не нашли! 😊`,
		attempts: room.searchAttempts,
		maxAttempts: room.maxAttempts
	    });
	}
    });

    socket.on('play-again', () => {
	const room = Object.values(rooms).find(r => r.players.includes(socket.id));
	if (!room) return;

	const roomId = Object.keys(rooms).find(key => rooms[key] === room);
	
	// Сброс игры
	const players = room.players;
	const seekerIndex = Math.floor(Math.random() * 2);
	room.seeker = players[seekerIndex];
	room.hider = players[1 - seekerIndex];
	room.gameState = 'hiding';
	room.hideLocation = null;
	room.searchAttempts = 0;
	room.searchedLocations = [];
	room.gameResult = null;

	console.log(`Новая игра в комнате ${roomId}`);
	
	io.to(room.seeker).emit('game-update', {
	    gameState: 'waiting_for_hider',
	    role: 'seeker',
	    message: 'Новая игра! Вы - ИЩУЩИЙ! 🔍 Ждите, пока напарник спрячется...',
	    maxAttempts: room.maxAttempts
	});
	
	io.to(room.hider).emit('game-update', {
	    gameState: 'hiding',
	    role: 'hider',
	    message: 'Новая игра! Вы - ПРЯЧУЩИЙСЯ! 🙈 Выберите место, где спрятаться:',
	    locations: hideAndSeekGame.locations
	});
    });

    socket.on('disconnect', () => {
	console.log('Игрок отключился:', socket.id);
	Object.keys(rooms).forEach(roomId => {
	    if (rooms[roomId] && rooms[roomId].players.includes(socket.id)) {
		console.log(`Удаляем игрока ${socket.id} из комнаты ${roomId}`);
		rooms[roomId].players = rooms[roomId].players.filter(id => id !== socket.id);
		if (rooms[roomId].players.length === 0) {
		    console.log(`Комната ${roomId} удалена - нет игроков`);
		    delete rooms[roomId];
		}
	    }
	});
    });
});

const PORT = process.env.PORT || 3000;
server.listen(PORT, () => {
    console.log(`Сервер запущен на порту ${PORT}`);
});
