;; Данные сюжета кооперативной игры в S-expression формате
(story
  (scene coop_awakening
    (title "Утреннее пробуждение")
    (text "Утренний свет пробивается сквозь тяжелые шторы княжеской спальни. 
    
    🎭 <strong>Ситуация:</strong> Вы просыпаетесь и оказываетесь вдвоем в спальне. Княжна и незнакомая девушка, очень похожая на неё.
    
    <div style=\"background: rgba(255,255,0,0.1); padding: 10px; border-radius: 5px; margin: 10px 0;\">
    💡 <strong>Подсказка:</strong> В комнате никого нет кроме вас двоих! Любой из игроков может предложить поменяться одеждой. Используйте систему перемещения для исследования замка.
    </div>")
    (location princess_chamber)
    
    (choices
      (princess
        (choice prepare_morning
          (text "Подготовиться к новому дню")
          (description "Встать и начать утренние процедуры")
          (result-text "Вы встаете и готовитесь к дню")
          (next-scene outfit_discussion)))
      (helper
        (choice suggest_preparation
          (text "Предложить подготовиться")
          (description "Нужно одеться и выйти в замок")
          (result-text "Вы предлагаете начать день")
          (next-scene outfit_discussion)))))

  (scene outfit_discussion
    (title "Выбор нарядов")
    (text "Княжна уже одета в своё прекрасное платье, а помощница в простом наряде. Перед выходом из спальни можно поменяться одеждой.
    
    🎭 <strong>Важно:</strong> Разные NPC по-разному относятся к княжеским и простым нарядам! Экспериментируйте с обменом одежды.")
    (location princess_chamber)
    
    (choices
      (princess
        (choice princess_ready
          (text "Готова к исследованию")
          (description "Отправиться изучать замок")
          (result-text "Вы готовы выйти из спальни")
          (next-scene ready_to_explore)))
      (helper
        (choice helper_prepare
          (text "Подготовиться к выходу")
          (description "Проверить свои вещи")
          (result-text "Вы проверяете серьги и медальон")
          (next-scene ready_to_explore)))))

  (scene ready_to_explore
    (title "Готовы к исследованию")
    (text "Вы готовы выйти из спальни и исследовать замок.
    
    🎭 <strong>Совет:</strong> Используйте кнопки перемещения чтобы перейти в другие локации. В разных местах вас ждут разные NPC!")
    (location princess_chamber)
    
    (choices
      (princess)
      (helper))))