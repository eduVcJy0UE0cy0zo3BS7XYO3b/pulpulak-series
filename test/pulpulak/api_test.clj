(ns pulpulak.api-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pulpulak.server :as server]
            [pulpulak.game.registry-pure :as registry]
            [pulpulak.games.pulpulak-config :as pulpulak-config]
            [cheshire.core :as json]))

(deftest test-health-endpoint
  (testing "Health check endpoint"
    (let [response (server/app (mock/request :get "/health"))]
      (is (= 200 (:status response)))
      (is (= {:status "ok"} (json/parse-string (:body response) true))))))

(deftest test-game-registry-api
  (testing "Game registry functionality"
    ;; Ensure pulpulak game is registered
    (require 'pulpulak.games.pulpulak-config :reload)
    
    (testing "List games"
      (let [registry-state (registry/empty-registry)
            games (registry/list-games registry-state)]
        (is (vector? games))))
    
    (testing "Pure registry operations"
      (let [test-config (pulpulak-config/create-pulpulak-config)
            registry-state (registry/register-game (registry/empty-registry) test-config)
            game (registry/get-game registry-state :pulpulak)]
        (is (some? game))
        (is (= "Pulpulak Series" (registry/get-game-name game)))
        (is (= 2 (get-in (registry/get-game-constants game) [:max-players])))))))

(deftest test-static-file-serving
  (testing "Index page"
    (let [response (server/app (mock/request :get "/"))]
      (is (= 200 (:status response)))))
  
  (testing "CSS files"
    (let [response (server/app (mock/request :get "/css/style.css"))]
      (is (= 200 (:status response))))))

(deftest test-websocket-endpoints
  (testing "WebSocket GET endpoint exists"
    (let [response (server/app (mock/request :get "/chsk"))]
      ;; Will fail without proper WebSocket headers, but endpoint should exist
      (is (not= 404 (:status response)))))
  
  (testing "WebSocket POST endpoint exists"
    (let [response (server/app (mock/request :post "/chsk"))]
      ;; Will fail without proper data, but endpoint should exist
      (is (not= 404 (:status response))))))