;; NPC data in S-expression format
(npcs
  ;; Royal Advisor
  (npc royal_advisor
    (name "Королевский советник Эдвард")
    (description "Важный вельможа в богатых одеждах с золотой цепью")
    (likes-noble true)
    (base-location throne_room)
    
    (dialogue
      (noble
        (initial
          (greeting "Ваше высочество! Рад видеть вас в добром здравии. Надеюсь, утро выдалось приятным?")
          (choices
            (choice ask_about_kingdom
              (text "Спросить о делах королевства")
              (response "Дела идут прекрасно, ваше высочество. Казна полна, народ доволен.")
              (unlocks kingdom_talked)
              (next-choices
                (choice ask_about_taxes
                  (text "Спросить о налогах")
                  (response "Недавно мы снизили налоги для крестьян. Это помогло экономике."))
                (choice ask_about_neighbors
                  (text "Узнать о соседних королевствах")
                  (response "Отношения мирные, но всегда есть те, кто завидует нашему процветанию."))))
            (choice ask_about_parents
              (text "Узнать о родителях")
              (response "Их величества сейчас на совете. Обсуждают важные государственные дела.")
              (unlocks parents_talked)
              (next-choices
                (choice ask_about_council
                  (text "Что за дела обсуждают?")
                  (response "Торговые соглашения с восточными землями, ваше высочество."))))
            (choice ask_about_relic
              (text "Спросить о пропавшей реликвии")
              (response "Ах, да! Древний амулет пропал из сокровищницы. Мне нужна ваша помощь, ваше высочество.")
              (unlocks relic_quest_given)
              (quest-action start_noble_quest)
              (requires-not relic_quest_given))))
        
        (return
          (greeting "Снова приветствую вас, ваше высочество! Что на этот раз вас интересует?")
          (choices
            (choice continue_kingdom
              (text "Ещё о делах королевства")
              (response "Всегда готов доложить о наших успехах!")
              (requires kingdom_talked)))))
      
      (common
        (initial
          (greeting "*нервно оглядывается* Что вы здесь делаете, простолюдинка? Стража!")
          (choices
            (choice pretend_servant
              (text "Притвориться слугой")
              (response "*прищуривается* Хм... Ладно, но не мешайтесь под ногами!"))
            (choice run_away
              (text "Убежать")
              (response "*кричит* Стража! Держите её!")))))))

  ;; Guard Captain
  (npc guard_captain
    (name "Капитан стражи Рейнальд")
    (description "Суровый воин в доспехах с многочисленными шрамами")
    (likes-noble true)
    (base-location throne_room)
    
    (dialogue
      (noble
        (initial
          (greeting "Ваше высочество! *отдаёт честь* Всё спокойно в замке.")
          (choices
            (choice ask_about_security
              (text "Спросить о безопасности")
              (response "Стража бдительна, ваше высочество. Никто не пройдёт незамеченным."))
            (choice ask_about_guards
              (text "Спросить о стражниках")
              (response "Лучшие воины королевства служат вам, ваше высочество."))))
        
        (return
          (greeting "*кивает* Ваше высочество.")
          (choices
            (choice duty_chat
              (text "Поговорить о службе")
              (response "Служба идёт своим чередом, ваше высочество.")))))
      
      (common
        (initial
          (greeting "*хмурится* Эй, ты! Что делаешь в тронном зале?")
          (choices
            (choice explain_lost
              (text "Сказать, что заблудилась")
              (response "*недоверчиво* Заблудилась? В тронный зал? Проваливай!"))
            (choice act_confident
              (text "Вести себя уверенно")
              (response "*прищуривается* Хм... Ладно, но я буду за тобой следить.")))))))

  ;; Cook
  (npc cook
    (name "Повариха Марта")
    (description "Полная женщина в фартуке, вечно в муке")
    (likes-noble false)
    (base-location kitchen)
    
    (dialogue
      (noble
        (initial
          (greeting "*нервно вытирает руки о фартук* О-ох! Ваше высочество! Простите беспорядок!")
          (choices
            (choice reassure_cook
              (text "Успокоить повариху")
              (response "*немного расслабляется* Вы очень добры, ваше высочество."))
            (choice demand_food
              (text "Потребовать еду")
              (response "*суетится* К-конечно! Сейчас же приготовлю!")
              (unlocks met_cook))))
        
        (return
          (greeting "*кланяется* Ваше высочество, чем могу служить?")
          (choices
            (choice chat_recipes
              (text "Поговорить о рецептах")
              (response "О, у меня есть чудесный рецепт пирога с ягодами!")))))
      
      (common
        (initial
          (greeting "*улыбается* Ох, милая! Проголодалась? Садись, я тебя накормлю!")
          (choices
            (choice accept_food
              (text "Принять угощение")
              (response "*даёт пирожок* Вот, ешь на здоровье!")
              (effects (give-item marta_pie))
              (unlocks ate_together))
            (choice chat_kitchen
              (text "Поболтать о кухне")
              (response "Работы много, но я люблю готовить для людей!"))
            (choice help_cooking
              (text "Предложить помощь")
              (response "Правда поможешь? Вот спасибо! Мне нужна особая помощь - в деревне болезнь, нужны лечебные травы...")
              (unlocks helped_cooking)
              (quest-action start_common_quest)
              (requires-not helped_cooking))))
        
        (return
          (greeting "*радостно* А, это снова ты! Как дела, дорогая?")
          (choices
            (choice talk_about_herbs
              (text "Поговорить о лечебных травах")
              (response "Ах, дорогая! Я так рада, что ты готова помочь! В деревне болезнь, нужно особое лечебное зелье. Знаешь, есть один травник в саду...")
              (quest-action advance_potion_quest)
              (requires helped_cooking))
            (choice more_food
              (text "Попросить ещё еды")
              (response "Конечно, милая! *даёт ещё пирожок*")
              (requires helped_cooking))
            (choice kitchen_gossip
              (text "Послушать сплетни")
              (response "Знаешь, говорят, в подвалах что-то странное происходит...")))))))

  ;; Gardener
  (npc gardener
    (name "Садовник Томас")
    (description "Пожилой мужчина с добрыми глазами и землёй под ногтями")
    (likes-noble false)
    (base-location secret_garden)
    
    (dialogue
      (noble
        (initial
          (greeting "*кланяется* Ваше высочество! Редкая честь видеть вас в саду.")
          (choices
            (choice admire_garden
              (text "Похвалить сад")
              (response "*расцветает от гордости* Благодарю, ваше высочество! Я стараюсь."))
            (choice ask_flowers
              (text "Спросить о цветах")
              (response "Эти розы - гордость нашего сада. Они цветут только при лунном свете."))))
        
        (return
          (greeting "*улыбается* Рад снова видеть вас в саду, ваше высочество.")
          (choices
            (choice garden_secrets
              (text "Спросить о секретах сада")
              (response "В этом саду много тайн... Некоторые цветы помнят времена ваших предков.")))))
      
      (common
        (initial
          (greeting "*добродушно* О, юная леди! Любуешься цветами?")
          (choices
            (choice love_flowers
              (text "Сказать, что любите цветы")
              (response "*улыбается* Вижу, у тебя добрая душа. Вот, возьми эту розу.")
              (effects (give-item garden_rose)))
            (choice help_garden
              (text "Предложить помощь")
              (response "Как мило! Помоги-ка мне полить вон те фиалки.")
              (unlocks helped_gardening))
            (choice see_roses
              (text "Посмотреть розы")
              (response "Конечно! Вот мои любимые розы. Они особенные...")
              (next-choices
                (choice learn_about_herbs
                  (text "Узнать о травах")
                  (response "Ах, травы! У меня есть целая коллекция целебных растений. Лунный цветок растет только в полнолуние, а здесь, в теплице, он цветет всегда!")
                  (quest-action get_moonflower))
                (choice ask_rose_secret
                  (text "Спросить о секрете роз")
                  (response "Эти розы цветут только при лунном свете. Магия природы!"))))))
        
        (return
          (greeting "*машет рукой* Привет, милая! Сад сегодня особенно красив!")
          (choices
            (choice secret_place
              (text "Спросить о тайных местах")
              (response "Хм... Если пообещаешь никому не говорить, покажу тебе кое-что особенное.")
              (requires helped_gardening)
              (effects (reveal-location hidden_grove))))))))

  ;; Librarian
  (npc librarian
    (name "Библиотекарь Марк")
    (description "Худощавый человек в очках, окружённый книгами")
    (likes-noble true)
    (base-location library)
    
    (dialogue
      (noble
        (initial
          (greeting "*поправляет очки* Ваше высочество! Какие знания вас интересуют сегодня?")
          (choices
            (choice history_books
              (text "Спросить об истории")
              (response "У нас прекрасная коллекция исторических хроник. Вот, начните с этой.")
              (effects (give-item history_book)))
            (choice magic_books
              (text "Спросить о магии")
              (response "*понижает голос* Есть... особые книги. Но они не для всех глаз."))
            (choice ask_about_amulet
              (text "Спросить об амулете")
              (response "Амулет Королей? Да, я читал о нём. Говорят, он резонирует с королевскими садами.")
              (requires relic_quest_given))))
        
        (return
          (greeting "*вежливо кивает* Снова ищете знания, ваше высочество?")
          (choices
            (choice more_books
              (text "Попросить ещё книг")
              (response "Конечно! Вот подборка о дипломатии соседних королевств.")))))
      
      (common
        (initial
          (greeting "*строго смотрит поверх очков* Библиотека - место для знати. Что тебе нужно?")
          (choices
            (choice need_learn
              (text "Сказать, что хотите учиться")
              (response "*смягчается* Хм... Тяга к знаниям похвальна. Ладно, можешь взять книгу из общего раздела."))
            (choice looking_someone
              (text "Сказать, что ищете кого-то")
              (response "Здесь только я и книги. Уходи, не мешай работать.")))))))

  ;; Village Elder (in village square)
  (npc village_elder
    (name "Староста Грегор")
    (description "Мудрый старик с длинной седой бородой")
    (likes-noble false)
    (base-location village_square)
    
    (dialogue
      (noble
        (initial
          (greeting "*низко кланяется* Ваше высочество! Чем обязаны такой чести?")
          (choices
            (choice village_welfare
              (text "Спросить о благополучии деревни")
              (response "Мы справляемся, ваше высочество. Урожай в этом году хороший."))
            (choice village_needs
              (text "Узнать о нуждах деревни")
              (response "*осторожно* Ну... новый колодец бы не помешал. Старый почти высох."))))
        
        (return
          (greeting "*кланяется* Ваше высочество, рады снова видеть вас.")
          (choices
            (choice promise_help
              (text "Пообещать помощь с колодцем")
              (response "*глаза загораются надеждой* Правда? О, это было бы чудесно!")
              ;; Removed loyalty system
              ))))
      
      (common
        (initial
          (greeting "*тепло улыбается* А, молодая леди! Добро пожаловать в нашу деревню!")
          (choices
            (choice ask_stories
              (text "Попросить рассказать истории")
              (response "О, я знаю много историй! Садись, расскажу о древних временах."))
            (choice offer_help_village
              (text "Предложить помощь")
              (response "Какая добрая душа! Если хочешь помочь, сходи к колодцу - там нужна помощь."))))
        
        (return
          (greeting "*машет рукой* А, наша добрая помощница! Как поживаешь?")
          (choices
            (choice village_news
              (text "Узнать новости деревни")
              (response "Вчера родился телёнок у Мэри! А ещё торговец приезжал с севера."))))))))