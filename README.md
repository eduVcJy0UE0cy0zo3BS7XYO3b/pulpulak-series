# Pulpulak Series - Clojure/ClojureScript Edition

A cooperative multiplayer text adventure game reimplemented in Clojure and ClojureScript.

## Prerequisites

- Java 8 or higher
- [Leiningen](https://leiningen.org/) 2.9.0 or higher

## Development Setup

### Backend Development

1. Start a REPL:
```bash
lein repl
```

2. In the REPL, start the server:
```clojure
(start)
```

The server will run on http://localhost:3000

### Frontend Development with Figwheel

1. Start Figwheel for hot-reloading ClojureScript:
```bash
lein figwheel
```

This will:
- Compile ClojureScript
- Start a Figwheel server on port 3449
- Watch for file changes and auto-reload

2. Open http://localhost:3000 in your browser

### Running Both Backend and Frontend

In separate terminals:

Terminal 1 - Backend:
```bash
lein repl
# Then in REPL: (start)
```

Terminal 2 - Frontend:
```bash
lein figwheel
```

## Production Build

To create a production build:

```bash
# Clean previous builds
lein clean

# Create uberjar with optimized ClojureScript
lein uberjar
```

Run the production jar:
```bash
java -jar target/pulpulak-standalone.jar
```

## Project Structure

```
├── src/
│   ├── clj/           # Clojure backend code
│   │   └── pulpulak/
│   │       ├── server.clj      # HTTP server setup
│   │       ├── websocket.clj   # WebSocket handling
│   │       └── game/           # Game logic
│   ├── cljs/          # ClojureScript frontend code
│   │   └── pulpulak/
│   │       ├── core.cljs       # Entry point
│   │       ├── events.cljs     # Re-frame events
│   │       ├── subs.cljs       # Re-frame subscriptions
│   │       └── views/          # React components
│   └── cljc/          # Shared Clojure/ClojureScript code
├── resources/
│   └── public/        # Static assets
├── dev/               # Development utilities
└── project.clj        # Project configuration
```

## Key Technologies

### Backend
- **Ring**: HTTP server abstraction
- **Compojure**: Routing
- **http-kit**: Web server
- **Sente**: WebSocket library
- **Mount**: State management

### Frontend
- **Reagent**: React wrapper for ClojureScript
- **Re-frame**: State management framework
- **Sente**: WebSocket client

## Game Features

- Real-time multiplayer (2 players)
- Role-based gameplay (Princess/Helper)
- Outfit swapping mechanics
- Loyalty system
- Synchronized story progression

## Development Tips

### REPL-Driven Development

Connect your editor to the REPL for interactive development:

```clojure
;; Reload changed namespaces
(require '[pulpulak.game.logic :as logic] :reload)

;; Test functions directly
(logic/get-scene :intro)
```

### Frontend Hot Reloading

Figwheel automatically reloads ClojureScript changes. For component state preservation, use Reagent atoms:

```clojure
(defonce app-state (r/atom {:count 0}))
```

### Debugging

Enable verbose logging:
```clojure
(require '[taoensso.timbre :as log])
(log/set-level! :debug)
```

## Testing

Run tests:
```bash
lein test
```

Run ClojureScript tests:
```bash
lein doo phantom test once
```