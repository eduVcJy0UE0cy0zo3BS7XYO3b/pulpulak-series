(ns pulpulak.game.data.locations)

(def locations
  {:princess-chamber {:name "Спальня княжны"
                      :description "Уютная спальня с большой кроватью и богатым убранством. Утренний свет пробивается сквозь тяжелые шторы."
                      :connections [:corridor-upper :private-quarters]
                      :can-change-outfit true
                      :icon "🛏️"}
   
   :private-quarters {:name "Личные покои"
                      :description "Небольшая комната для отдыха и чтения. Здесь хранятся личные вещи княжны."
                      :connections [:princess-chamber]
                      :can-change-outfit true
                      :icon "📚"}
   
   :corridor-upper {:name "Верхний коридор"
                    :description "Длинный коридор второго этажа замка. Отсюда можно попасть в различные комнаты."
                    :connections [:princess-chamber :throne-room :stairs-main :library]
                    :can-change-outfit false
                    :icon "🏛️"}
   
   :throne-room {:name "Тронный зал"
                 :description "Величественный зал с высокими потолками и королевским троном. Место официальных приемов."
                 :connections [:corridor-upper :great-hall]
                 :can-change-outfit false
                 :icon "👑"}
   
   :great-hall {:name "Большой зал"
                :description "Просторный зал для пиров и празднеств. Длинные столы и множество стульев."
                :connections [:throne-room :kitchen :stairs-main]
                :can-change-outfit false
                :icon "🍷"}
   
   :kitchen {:name "Кухня"
             :description "Большая кухня замка, где готовят еду для всех обитателей."
             :connections [:great-hall :servants-hall :storage]
             :can-change-outfit false
             :icon "🍳"}
   
   :servants-hall {:name "Зал слуг"
                   :description "Комната отдыха для прислуги. Простая обстановка и деревянные столы."
                   :connections [:kitchen :servants-quarters]
                   :can-change-outfit false
                   :icon "🪑"}
   
   :servants-quarters {:name "Комнаты прислуги"
                       :description "Скромные жилые помещения для слуг замка."
                       :connections [:servants-hall]
                       :can-change-outfit true
                       :icon "🛌"}
   
   :library {:name "Библиотека"
             :description "Огромная библиотека с тысячами книг. Тихое место для учебы и размышлений."
             :connections [:corridor-upper :secret-archive]
             :can-change-outfit false
             :icon "📚"}
   
   :secret-archive {:name "Тайный архив"
                    :description "Скрытая комната с древними манускриптами и секретными документами."
                    :connections [:library]
                    :can-change-outfit true
                    :hidden true
                    :icon "📜"}
   
   :stairs-main {:name "Главная лестница"
                 :description "Широкая мраморная лестница, соединяющая этажи замка."
                 :connections [:corridor-upper :great-hall :entrance-hall]
                 :can-change-outfit false
                 :icon "🏰"}
   
   :entrance-hall {:name "Входной зал"
                   :description "Просторный зал у главного входа в замок. Первое, что видят гости."
                   :connections [:stairs-main :castle-yard :guard-post]
                   :can-change-outfit false
                   :icon "🚪"}
   
   :castle-yard {:name "Двор замка"
                 :description "Открытый двор с фонтаном в центре. Место для прогулок и тренировок."
                 :connections [:entrance-hall :stable :gardens :castle-gate]
                 :can-change-outfit false
                 :icon "⛲"}
   
   :stable {:name "Конюшни"
            :description "Просторные конюшни с лучшими скакунами королевства."
            :connections [:castle-yard]
            :can-change-outfit false
            :icon "🐎"}
   
   :gardens {:name "Сады"
             :description "Прекрасные королевские сады с редкими цветами и фонтанами."
             :connections [:castle-yard :secret-garden]
             :can-change-outfit false
             :icon "🌹"}
   
   :secret-garden {:name "Тайный сад"
                   :description "Уединенный уголок сада, скрытый от посторонних глаз."
                   :connections [:gardens]
                   :can-change-outfit true
                   :hidden true
                   :icon "🌺"}
   
   :guard-post {:name "Караульная"
                :description "Пост охраны у входа в замок. Здесь всегда дежурят стражники."
                :connections [:entrance-hall]
                :can-change-outfit false
                :icon "⚔️"}
   
   :castle-gate {:name "Ворота замка"
                 :description "Массивные ворота, ведущие из замка в город."
                 :connections [:castle-yard :town-square]
                 :can-change-outfit false
                 :icon "🏰"}
   
   :town-square {:name "Городская площадь"
                 :description "Центральная площадь города с рынком и фонтаном."
                 :connections [:castle-gate :market :inn :blacksmith]
                 :can-change-outfit false
                 :icon "🏘️"}
   
   :market {:name "Рынок"
            :description "Шумный рынок с торговцами со всего королевства."
            :connections [:town-square :alchemist]
            :can-change-outfit false
            :icon "🛍️"}
   
   :inn {:name "Таверна"
         :description "Уютная таверна 'Золотой дракон'. Место встреч путешественников."
         :connections [:town-square :inn-room]
         :can-change-outfit false
         :icon "🍺"}
   
   :inn-room {:name "Комната в таверне"
              :description "Простая, но чистая комната для постояльцев."
              :connections [:inn]
              :can-change-outfit true
              :icon "🛏️"}
   
   :blacksmith {:name "Кузница"
                :description "Жаркая кузница, где мастер создает оружие и доспехи."
                :connections [:town-square]
                :can-change-outfit false
                :icon "🔨"}
   
   :alchemist {:name "Лавка алхимика"
               :description "Таинственная лавка с зельями и редкими ингредиентами."
               :connections [:market]
               :can-change-outfit false
               :icon "🧪"}
   
   :storage {:name "Кладовая"
             :description "Подземная кладовая с запасами еды и других припасов."
             :connections [:kitchen]
             :can-change-outfit false
             :icon "📦"}})

(defn get-location [location-id]
  (get locations location-id))

(defn get-connections [location-id]
  (get-in locations [location-id :connections] []))

(defn can-change-outfit? [location-id]
  (get-in locations [location-id :can-change-outfit] false))

(defn is-hidden? [location-id]
  (get-in locations [location-id :hidden] false))

(defn get-visible-locations []
  (into {} (filter (fn [[_ loc]] (not (:hidden loc))) locations)))