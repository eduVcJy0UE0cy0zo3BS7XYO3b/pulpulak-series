;; Данные локаций игры в S-expression формате
(locations
  (location princess_chamber
    (name "Спальня княжны")
    (description "Уютная спальня с большой кроватью и богатым убранством. Утренний свет пробивается сквозь тяжелые шторы.")
    (connections (corridor_upper private_quarters))
    (can-change-outfit true)
    (icon "🛏️"))
    
  (location private_quarters
    (name "Личные покои")
    (description "Небольшая комната для отдыха и чтения. Здесь хранятся личные вещи княжны.")
    (connections (princess_chamber))
    (can-change-outfit true)
    (icon "📚"))
    
  (location corridor_upper
    (name "Верхний коридор")
    (description "Длинный коридор второго этажа замка. Отсюда можно попасть в различные комнаты.")
    (connections (princess_chamber throne_room stairs_main library))
    (can-change-outfit false)
    (icon "🏛️"))
    
  (location throne_room
    (name "Тронный зал")
    (description "Величественный зал с высокими потолками и королевским троном. Место официальных приемов.")
    (connections (corridor_upper great_hall))
    (can-change-outfit false)
    (icon "👑"))
    
  (location great_hall
    (name "Большой зал")
    (description "Просторный зал для пиров и празднеств. Длинные столы и множество стульев.")
    (connections (throne_room kitchen stairs_main))
    (can-change-outfit false)
    (icon "🏰"))
    
  (location stairs_main
    (name "Главная лестница")
    (description "Широкая мраморная лестница, соединяющая этажи замка.")
    (connections (corridor_upper great_hall corridor_lower))
    (can-change-outfit false)
    (icon "🪜"))
    
  (location corridor_lower
    (name "Нижний коридор")
    (description "Коридор первого этажа. Отсюда можно попасть в служебные помещения.")
    (connections (stairs_main kitchen garden armory chapel))
    (can-change-outfit false)
    (icon "🏛️"))
    
  (location kitchen
    (name "Кухня")
    (description "Большая кухня с печами и столами для приготовления пищи. Пахнет свежим хлебом.")
    (connections (great_hall corridor_lower pantry))
    (can-change-outfit false)
    (icon "🍳"))
    
  (location pantry
    (name "Кладовая")
    (description "Прохладное помещение с запасами еды. Полки уставлены банками и мешками.")
    (connections (kitchen))
    (can-change-outfit true)
    (icon "🥫"))
    
  (location garden
    (name "Сад")
    (description "Прекрасный сад с цветущими розами и уютными беседками. Свежий воздух и пение птиц.")
    (connections (corridor_lower secret_garden greenhouse))
    (can-change-outfit true)
    (icon "🌹"))
    
  (location secret_garden
    (name "Тайный сад")
    (description "Скрытый уголок сада, о котором знают немногие. Идеальное место для уединения.")
    (connections (garden))
    (can-change-outfit true)
    (icon "🌿"))
    
  (location armory
    (name "Оружейная")
    (description "Комната с доспехами и оружием. На стенах висят мечи и щиты.")
    (connections (corridor_lower training_grounds))
    (can-change-outfit false)
    (icon "⚔️"))
    
  (location training_grounds
    (name "Тренировочная площадка")
    (description "Открытая площадка для тренировок. Есть мишени для стрельбы из лука.")
    (connections (armory))
    (can-change-outfit false)
    (icon "🎯"))

  (location chapel
    (name "Часовня")
    (description "Тихое место для молитв и размышлений. Цветные витражи создают мистическую атмосферу.")
    (connections (corridor_lower))
    (can-change-outfit true)
    (icon "⛪"))

  (location library
    (name "Библиотека")
    (description "Большая библиотека с множеством древних книг и свитков. Высокие полки тянутся до потолка.")
    (connections (corridor_upper secret_archive))
    (can-change-outfit false)
    (icon "📚"))

  (location secret_archive
    (name "Секретный архив")
    (description "Скрытая комната за библиотекой. Здесь хранятся самые древние и ценные документы.")
    (connections (library))
    (can-change-outfit true)
    (icon "📜"))

  (location greenhouse
    (name "Теплица")
    (description "Стеклянная теплица полная экзотических растений. Тепло и влажно, множество редких трав.")
    (connections (garden))
    (can-change-outfit true)
    (icon "🌱")))