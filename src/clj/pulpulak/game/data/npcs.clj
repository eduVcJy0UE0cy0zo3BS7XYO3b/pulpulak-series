(ns pulpulak.game.data.npcs)

(def npcs
  {:royal-advisor
   {:id :royal-advisor
    :name "Королевский советник Эдвард"
    :description "Важный вельможа в богатых одеждах с золотой цепью"
    :likes-noble true
    :dialogue
    {:noble
     {:initial
      {:greeting "Ваше высочество! Рад видеть вас в добром здравии. Надеюсь, утро выдалось приятным?"
       :choices
       [{:id :ask-about-kingdom
         :text "Спросить о делах королевства"
         :response "Дела идут прекрасно, ваше высочество. Казна полна, народ доволен."
         :unlocks :kingdom-talked
         :next-choices
         [{:id :ask-about-taxes
           :text "Спросить о налогах"
           :response "Недавно мы снизили налоги для крестьян. Это помогло экономике."}
          {:id :ask-about-neighbors
           :text "Узнать о соседних королевствах"
           :response "Отношения мирные, но всегда есть те, кто завидует нашему процветанию."}]}
        {:id :ask-about-parents
         :text "Узнать о родителях"
         :response "Их величества сейчас на совете. Обсуждают важные государственные дела."
         :unlocks :parents-talked
         :next-choices
         [{:id :ask-about-council
           :text "Что за дела обсуждают?"
           :response "Торговые соглашения с восточными землями, ваше высочество."}]}
        {:id :ask-about-relic
         :text "Спросить о пропавшей реликвии"
         :response "Ах, да! Древний амулет пропал из сокровищницы. Мне нужна ваша помощь, ваше высочество. Начните с библиотеки - там работает учёный Марк."
         :unlocks :relic-quest-given
         :quest-action :start-noble-quest
         :requires-not :relic-quest-given}]}}
     :commoner
     {:initial
      {:greeting "Эй, ты! Что тебе здесь надо? Прислуге нельзя находиться в тронном зале без дела!"
       :choices
       [{:id :apologize
         :text "Извиниться и уйти"
         :response "Вот и правильно. Иди займись своими обязанностями."
         :effect {:type :leave}}
        {:id :lie-about-errand
         :text "Соврать про поручение"
         :response "Хм... Ладно, но быстро закончи и уходи."
         :unlocks :lied-to-advisor}
        {:id :ask-about-missing-item
         :text "Спросить о пропаже"
         :response "Какая пропажа? Ты о чем? Убирайся отсюда!"
         :requires :overheard-about-relic}]}}}
    :location :throne-room}
   
   :librarian
   {:id :librarian
    :name "Библиотекарь Марк"
    :description "Пожилой учёный в очках, окруженный книгами"
    :likes-noble false
    :dialogue
    {:noble
     {:initial
      {:greeting "О, ваше высочество! Простите беспорядок, я как раз разбираю древние манускрипты."
       :choices
       [{:id :ask-about-books
         :text "Спросить о книгах"
         :response "У нас тысячи томов! История, магия, легенды... Что вас интересует?"}
        {:id :ask-about-relic
         :text "Спросить о древнем амулете"
         :response "Амулет? Да, я читал о нём! Есть записи в тайном архиве. Но туда можно попасть только с помощником."
         :requires :relic-quest-given
         :unlocks :need-helper-for-archive}]}}
     :commoner
     {:initial
      {:greeting "Ах, помощник! Как хорошо, что ты здесь. Мне нужна помощь с книгами."
       :choices
       [{:id :offer-help
         :text "Предложить помощь"
         :response "Отлично! Помоги мне достать книги с верхних полок."
         :unlocks :helped-librarian}
        {:id :ask-about-secret-archive
         :text "Спросить о тайном архиве"
         :response "Тайный архив? Хм... Если княжна просит, я могу показать вход. Но только если ты мне поможешь."
         :requires :need-helper-for-archive
         :effect {:type :unlock-location :location :secret-archive}}]}}}
    :location :library}
   
   :guard-captain
   {:id :guard-captain
    :name "Капитан стражи Гаррет"
    :description "Суровый воин в доспехах с многочисленными шрамами"
    :likes-noble true
    :dialogue
    {:noble
     {:initial
      {:greeting "Ваше высочество! Замок под надёжной защитой, можете не беспокоиться."
       :choices
       [{:id :ask-about-security
         :text "Спросить о безопасности"
         :response "Все входы охраняются. Никто не пройдёт без проверки."}
        {:id :ask-about-town
         :text "Узнать о ситуации в городе"
         :response "В городе спокойно. Хотя ходят слухи о странных событиях на рынке."}]}}
     :commoner
     {:initial
      {:greeting "Стой! Куда направляешься?"
       :choices
       [{:id :say-to-town
         :text "Сказать, что идёшь в город"
         :response "Ладно, проходи. Но веди себя прилично."}
        {:id :say-on-errand
         :text "Сказать, что по поручению княжны"
         :response "По поручению княжны? Хм... Ладно, но я буду следить за тобой."}]}}}
    :location :guard-post}
   
   :merchant
   {:id :merchant
    :name "Торговец Али"
    :description "Хитрый торговец с восточной внешностью"
    :likes-noble false
    :dialogue
    {:noble
     {:initial
      {:greeting "О, ваше высочество! Какая честь! У меня есть прекрасные товары специально для вас!"
       :choices
       [{:id :browse-wares
         :text "Посмотреть товары"
         :response "Шёлка из дальних стран, драгоценности, редкие специи... Всё самое лучшее!"}
        {:id :ask-about-rumors
         :text "Спросить о слухах"
         :response "Слухи? О, я простой торговец, ваше высочество... Но говорят, кто-то ищет древние артефакты."}]}}
     :commoner
     {:initial
      {:greeting "Эй, покупатель! Подходи, не стесняйся! Лучшие цены на рынке!"
       :choices
       [{:id :ask-prices
         :text "Узнать цены"
         :response "Для простых людей - особые цены! Что тебя интересует?"}
        {:id :ask-about-special-items
         :text "Спросить об особых товарах"
         :response "Особые товары? Хм... Есть кое-что, но это не для всех. Нужны связи или много золота."
         :unlocks :knows-about-special-items}]}}}
    :location :market}
   
   :innkeeper
   {:id :innkeeper
    :name "Трактирщица Марта"
    :description "Полная женщина с добрым лицом и фартуком"
    :likes-noble false
    :dialogue
    {:noble
     {:initial
      {:greeting "Ваше высочество в моей скромной таверне! Чем могу услужить?"
       :choices
       [{:id :ask-for-room
         :text "Попросить комнату"
         :response "Конечно! Лучшая комната к вашим услугам, и совершенно бесплатно!"
         :effect {:type :unlock-location :location :inn-room}}
        {:id :ask-about-guests
         :text "Спросить о постояльцах"
         :response "У нас останавливаются путешественники со всего королевства. Все мирные люди."}]}}
     :commoner
     {:initial
      {:greeting "Добро пожаловать! Эль, еда, комната - что желаете?"
       :choices
       [{:id :ask-for-food
         :text "Попросить еды"
         :response "Горячий суп и хлеб - две медяка. Садись, сейчас принесу."}
        {:id :ask-about-news
         :text "Узнать новости"
         :response "Новости? Да много чего! Вчера алхимик искал редкие травы. Что-то важное готовит."
         :unlocks :alchemist-needs-herbs}]}}}
    :location :inn}
   
   :blacksmith
   {:id :blacksmith
    :name "Кузнец Торин"
    :description "Могучий мужчина с обожжёнными руками"
    :likes-noble true
    :dialogue
    {:noble
     {:initial
      {:greeting "Ваше высочество! Редкая честь видеть вас в моей кузнице!"
       :choices
       [{:id :ask-about-work
         :text "Спросить о работе"
         :response "Кую оружие для королевской стражи. Только лучшая сталь!"}
        {:id :commission-item
         :text "Заказать предмет"
         :response "Что угодно для вашего высочества! Опишите, что нужно, и я сделаю."}]}}
     :commoner
     {:initial
      {:greeting "Чего тебе? Я занят, заказы не ждут."
       :choices
       [{:id :offer-help
         :text "Предложить помощь"
         :response "Помощь? Хм... Можешь раздувать мехи, пока я работаю. Заплачу медяком."
         :unlocks :helped-blacksmith}
        {:id :ask-about-special-orders
         :text "Спросить об особых заказах"
         :response "Особые заказы? Да, был один... Но это секрет. Не для твоих ушей."}]}}}
    :location :blacksmith}
   
   :alchemist
   {:id :alchemist
    :name "Алхимик Мерлин"
    :description "Таинственный старик в мантии со странными пятнами"
    :likes-noble false
    :dialogue
    {:noble
     {:initial
      {:greeting "Княжна? В моей лавке? Осторожнее с колбами, некоторые... взрывоопасны."
       :choices
       [{:id :ask-about-potions
         :text "Спросить о зельях"
         :response "У меня есть зелья на все случаи жизни. Исцеление, сила, даже невидимость..."}
        {:id :ask-about-ingredients
         :text "Узнать об ингредиентах"
         :response "Редкие травы, минералы, эссенции... Некоторые достать очень сложно."}]}}
     :commoner
     {:initial
      {:greeting "А, помощник! Может, ты мне поможешь? Мне нужны редкие травы."
       :choices
       [{:id :agree-to-help
         :text "Согласиться помочь"
         :response "Отлично! Мне нужен лунный цветок из тайного сада. Взамен - мощное зелье."
         :quest-action :start-herb-quest
         :requires :alchemist-needs-herbs}
        {:id :ask-about-strange-things
         :text "Спросить о странностях"
         :response "Странности? О да, в последнее время магические потоки нестабильны. Что-то происходит."}]}}}
    :location :alchemist}})

(defn get-npc [npc-id]
  (get npcs npc-id))

(defn get-npcs-at-location [location-id]
  (filter #(= location-id (:location (val %))) npcs))

(defn get-dialogue [npc-id outfit-type dialogue-state]
  (get-in npcs [npc-id :dialogue outfit-type dialogue-state]))

(defn likes-noble? [npc-id]
  (get-in npcs [npc-id :likes-noble] false))