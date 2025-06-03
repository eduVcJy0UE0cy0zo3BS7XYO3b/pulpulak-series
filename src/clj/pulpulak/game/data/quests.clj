(ns pulpulak.game.data.quests)

(def quests
  {:princess-lost-relic
   {:id :princess-lost-relic
    :character :princess
    :title "Потерянная королевская реликвия"
    :description "Найти древний амулет, который пропал из сокровищницы"
    :steps [{:id :get-quest
             :description "Поговорить с королевским советником о пропаже"
             :location :throne-room
             :npc :royal-advisor
             :completed false}
            {:id :search-library
             :description "Найти библиотекаря для поиска подсказок"
             :location :library
             :npc :librarian
             :completed false}
            {:id :talk-to-librarian
             :description "Поговорить с библиотекарем о древних записях"
             :location :secret-archive
             :npc :librarian
             :completed false}
            {:id :return-to-advisor
             :description "Вернуться к советнику с информацией"
             :location :throne-room
             :npc :royal-advisor
             :completed false}]
    :current-step 0
    :rewards [:ancient-amulet :royal-gratitude]
    :status :not-started}
   
   :helper-secret-potion
   {:id :helper-secret-potion
    :character :helper
    :title "Секретное зелье исцеления"
    :description "Найти ингредиенты для мощного зелья исцеления"
    :steps [{:id :get-quest
             :description "Получить задание от алхимика"
             :location :alchemist
             :npc :alchemist
             :completed false}
            {:id :find-moonflower
             :description "Найти лунный цветок в тайном саду"
             :location :secret-garden
             :item :moonflower
             :completed false}
            {:id :gather-spring-water
             :description "Набрать воды из фонтана на площади"
             :location :town-square
             :item :spring-water
             :completed false}
            {:id :return-ingredients
             :description "Вернуть ингредиенты алхимику"
             :location :alchemist
             :npc :alchemist
             :completed false}]
    :current-step 0
    :rewards [:healing-potion :alchemist-friendship]
    :status :not-started}
   
   :shared-rescue-cat
   {:id :shared-rescue-cat
    :character :both
    :title "Спасение королевского кота"
    :description "Найти и спасти пропавшего королевского кота"
    :steps [{:id :notice-missing
             :description "Узнать о пропаже кота от слуг"
             :location :servants-hall
             :completed false}
            {:id :search-castle
             :description "Обыскать замок"
             :locations [:library :storage :stable]
             :completed false}
            {:id :find-in-tree
             :description "Найти кота на дереве в саду"
             :location :gardens
             :completed false}
            {:id :rescue-cat
             :description "Спасти кота (требуется сотрудничество)"
             :location :gardens
             :requires-both true
             :completed false}]
    :current-step 0
    :rewards [:royal-cat-friendship :servants-respect]
    :status :not-started}
   
   :princess-diplomatic-mission
   {:id :princess-diplomatic-mission
    :character :princess
    :title "Дипломатическая миссия"
    :description "Наладить отношения с торговцами города"
    :steps [{:id :meet-merchants
             :description "Встретиться с главными торговцами"
             :location :market
             :npcs [:merchant]
             :completed false}
            {:id :negotiate-taxes
             :description "Обсудить налоговые льготы"
             :location :throne-room
             :completed false}
            {:id :deliver-news
             :description "Сообщить торговцам хорошие новости"
             :location :market
             :completed false}]
    :current-step 0
    :rewards [:merchant-loyalty :trade-prosperity]
    :status :not-started}
   
   :helper-underground-mystery
   {:id :helper-underground-mystery
    :character :helper
    :title "Тайна подземелий"
    :description "Исследовать странные звуки из подвалов замка"
    :steps [{:id :hear-rumors
             :description "Услышать слухи от прислуги"
             :location :servants-quarters
             :completed false}
            {:id :investigate-storage
             :description "Исследовать кладовую ночью"
             :location :storage
             :time :night
             :completed false}
            {:id :discover-passage
             :description "Найти секретный проход"
             :location :storage
             :completed false}
            {:id :report-findings
             :description "Доложить о находке"
             :location :throne-room
             :completed false}]
    :current-step 0
    :rewards [:secret-passage-key :guard-trust]
    :status :not-started}}

(def quest-actions
  {:start-noble-quest
   (fn [game-state player-id]
     (-> game-state
         (assoc-in [:quests player-id :princess-lost-relic :status] :active)
         (update-in [:player-states player-id :active-quests] conj :princess-lost-relic)))
   
   :start-herb-quest
   (fn [game-state player-id]
     (-> game-state
         (assoc-in [:quests player-id :helper-secret-potion :status] :active)
         (update-in [:player-states player-id :active-quests] conj :helper-secret-potion)))})

(defn get-quest [quest-id]
  (get quests quest-id))

(defn get-quests-for-character [character]
  (filter #(#{character :both} (:character (val %))) quests))

(defn is-quest-available? [quest-id game-state player-id]
  (let [quest (get-quest quest-id)
        player-role (get-in game-state [:players player-id :role])
        quest-status (get-in game-state [:quests player-id quest-id :status] :not-started)]
    (and quest
         (#{(:character quest) :both} player-role)
         (= quest-status :not-started))))

(defn can-complete-step? [quest-id step-id game-state player-id]
  (let [quest-state (get-in game-state [:quests player-id quest-id])
        step-index (.indexOf (map :id (:steps (get-quest quest-id))) step-id)
        current-step (:current-step quest-state 0)]
    (and quest-state
         (= (:status quest-state) :active)
         (= step-index current-step))))

(defn complete-quest-step [game-state player-id quest-id step-id]
  (if (can-complete-step? quest-id step-id game-state player-id)
    (let [quest (get-quest quest-id)
          step-index (.indexOf (map :id (:steps quest)) step-id)
          next-step (inc step-index)
          quest-complete? (>= next-step (count (:steps quest)))]
      (-> game-state
          (assoc-in [:quests player-id quest-id :steps step-index :completed] true)
          (assoc-in [:quests player-id quest-id :current-step] next-step)
          (assoc-in [:quests player-id quest-id :status] 
                    (if quest-complete? :completed :active))
          (update-in [:player-states player-id :completed-quests] 
                     #(if quest-complete? (conj % quest-id) %))))
    game-state))

(defn get-active-quests [game-state player-id]
  (let [player-quests (get-in game-state [:quests player-id] {})]
    (filter #(= :active (:status (val %))) player-quests)))

(defn get-current-objectives [game-state player-id]
  (let [active-quests (get-active-quests game-state player-id)]
    (map (fn [[quest-id quest-state]]
           (let [quest (get-quest quest-id)
                 current-step (get (:steps quest) (:current-step quest-state))]
             {:quest-id quest-id
              :quest-title (:title quest)
              :objective (:description current-step)
              :location (:location current-step)}))
         active-quests)))