/**
 * ✅ ФИНАЛЬНЫЙ РАБОЧИЙ ПРИМЕР КВЕСТОВОЙ СИСТЕМЫ НА TAU-PROLOG
 */

const pl = require('tau-prolog');

console.log('🎮 === КВЕСТОВАЯ СИСТЕМА НА TAU-PROLOG === 🎮\n');

const session = pl.create();

const questProgram = `
% === ИГРОВОЕ СОСТОЯНИЕ ===
character_location(princess, throne_room).
character_outfit(princess, princess_dress).
character_location(helper, kitchen).
character_outfit(helper, simple_dress).

% === БАЗОВЫЕ ФАКТЫ ===
character_role(princess, royal).
character_role(helper, servant).

npc(royal_advisor, throne_room).
npc(librarian, library).
npc(cook, kitchen).
npc(herbalist, garden).

quest(princess_lost_relic).
quest(helper_secret_potion).
quest_belongs_to(princess_lost_relic, princess).
quest_belongs_to(helper_secret_potion, helper).

% === КЛАССИФИКАЦИЯ ЛОКАЦИЙ ===
royal_location(throne_room).
royal_location(library).
servant_location(kitchen).
servant_location(garden).

% === КВЕСТОВАЯ ЛОГИКА ===

% Квест доступен если персонаж подходит и одежда соответствует
quest_available(Quest, Character) :-
    quest_belongs_to(Quest, Character),
    character_outfit_matches(Character, Quest).

character_outfit_matches(Character, princess_lost_relic) :-
    character_outfit(Character, princess_dress).

character_outfit_matches(Character, helper_secret_potion) :-
    character_outfit(Character, simple_dress).

% Взаимодействие с NPC
can_talk_to_npc(Character, NPC) :-
    character_location(Character, Location),
    npc(NPC, Location).

npc_gives_quest(royal_advisor, Character, princess_lost_relic) :-
    can_talk_to_npc(Character, royal_advisor),
    character_outfit(Character, princess_dress).

npc_gives_quest(cook, Character, helper_secret_potion) :-
    can_talk_to_npc(Character, cook),
    character_outfit(Character, simple_dress).

% Возможность начать квест
can_start_quest(Character, Quest) :-
    quest_available(Quest, Character),
    npc_gives_quest(NPC, Character, Quest).

% === УМНАЯ СИСТЕМА ПОДСКАЗОК ===

% Подсказки на основе контекста
hint_available(Character, go_to_library) :-
    character_location(Character, throne_room),
    character_outfit(Character, princess_dress),
    quest_available(princess_lost_relic, Character).

hint_available(Character, talk_to_cook) :-
    character_location(Character, kitchen),
    character_outfit(Character, simple_dress),
    quest_available(helper_secret_potion, Character).

hint_available(Character, change_to_simple_dress) :-
    character_outfit(Character, princess_dress),
    character_location(Character, Location),
    servant_location(Location).

hint_available(Character, change_to_royal_dress) :-
    character_outfit(Character, simple_dress),
    character_location(Character, Location),
    royal_location(Location).

% === АНАЛИЗ ИГРОВОЙ СИТУАЦИИ ===

% Персонаж находится в подходящей локации для своей роли
role_location_match(Character, good_match) :-
    character_role(Character, royal),
    character_location(Character, Location),
    royal_location(Location).

role_location_match(Character, good_match) :-
    character_role(Character, servant),
    character_location(Character, Location),
    servant_location(Location).

role_location_match(Character, mismatch) :-
    character_role(Character, royal),
    character_location(Character, Location),
    servant_location(Location).

role_location_match(Character, mismatch) :-
    character_role(Character, servant),
    character_location(Character, Location),
    royal_location(Location).

% Рекомендации по действиям
action_recommendation(Character, start_quest(Quest)) :-
    can_start_quest(Character, Quest).

action_recommendation(Character, change_outfit) :-
    hint_available(Character, change_to_simple_dress).

action_recommendation(Character, change_outfit) :-
    hint_available(Character, change_to_royal_dress).

action_recommendation(Character, explore_library) :-
    hint_available(Character, go_to_library).

action_recommendation(Character, talk_to_cook) :-
    hint_available(Character, talk_to_cook).

% === СИМУЛЯЦИЯ ИГРОВЫХ СЦЕНАРИЕВ ===

% Что произойдет если персонаж поменяет одежду
scenario_outfit_change(Character, new_quest_available(Quest)) :-
    character_location(Character, Location),
    character_role(Character, Role),
    alternative_outfit(Role, NewOutfit),
    quest_belongs_to(Quest, Character),
    ((NewOutfit = princess_dress, Quest = princess_lost_relic);
     (NewOutfit = simple_dress, Quest = helper_secret_potion)).

alternative_outfit(royal, simple_dress).
alternative_outfit(servant, princess_dress).
`;

console.log('📚 Загружаем квестовую систему...');
session.consult(questProgram, {
    success: function() {
        console.log('✅ Система загружена!\n');
        runCompleteAnalysis();
    },
    error: function(err) {
        console.log('❌ Ошибка:', err);
    }
});

async function runCompleteAnalysis() {
    console.log('🔍 === ПОЛНЫЙ АНАЛИЗ ИГРОВОЙ СИТУАЦИИ ===\n');
    
    console.log('1. 📍 ТЕКУЩЕЕ СОСТОЯНИЕ:');
    await findAll("character_location(Character, Location)", "Где находятся персонажи");
    await findAll("character_outfit(Character, Outfit)", "Во что одеты персонажи");
    
    console.log('\n2. 🎯 КВЕСТОВЫЙ АНАЛИЗ:');
    await findAll("quest_available(Quest, Character)", "Доступные квесты");
    await findAll("can_start_quest(Character, Quest)", "Готовые к запуску квесты");
    
    console.log('\n3. 💬 СОЦИАЛЬНЫЕ ВЗАИМОДЕЙСТВИЯ:');
    await findAll("can_talk_to_npc(Character, NPC)", "Возможные разговоры");
    await findAll("npc_gives_quest(NPC, Character, Quest)", "NPC готовые дать квесты");
    
    console.log('\n4. 💡 ИНТЕЛЛЕКТУАЛЬНЫЕ ПОДСКАЗКИ:');
    await findAll("hint_available(Character, Hint)", "Умные подсказки системы");
    
    console.log('\n5. 🎭 АНАЛИЗ РОЛЕЙ И ЛОКАЦИЙ:');
    await findAll("role_location_match(Character, Match)", "Соответствие роли и места");
    
    console.log('\n6. 🚀 РЕКОМЕНДАЦИИ ПО ДЕЙСТВИЯМ:');
    await findAll("action_recommendation(Character, Action)", "Что рекомендует делать система");
    
    console.log('\n7. 🔮 ПРОГНОЗИРОВАНИЕ СЦЕНАРИЕВ:');
    await findAll("scenario_outfit_change(Character, Outcome)", "Что случится при смене одежды");
    
    console.log('\n🎉 === ДЕМОНСТРАЦИЯ ЗАВЕРШЕНА ===');
    console.log('\n💭 ВЫВОДЫ:');
    console.log('   ✅ Tau-prolog успешно работает с игровой логикой');
    console.log('   ✅ Система может делать умные выводы о состоянии игры');
    console.log('   ✅ Логические правила позволяют создавать интеллектуальные подсказки');
    console.log('   ✅ Можно моделировать сложные игровые сценарии декларативно');
}

function findAll(query, description) {
    return new Promise((resolve) => {
        console.log(`  📋 ${description}:`);
        session.query(query + '.', {
            success: function(goal) {
                let count = 0;
                
                function getNext() {
                    session.answer(function(answer) {
                        if (answer !== null && answer !== false) {
                            count++;
                            const formatted = session.format_answer(answer);
                            console.log(`     ${count}. ${formatted}`);
                            setTimeout(getNext, 10);
                        } else {
                            if (count === 0) {
                                console.log('     (нет результатов)');
                            }
                            resolve();
                        }
                    });
                }
                
                getNext();
            },
            error: function(err) {
                console.log(`     ❌ ОШИБКА: ${err}`);
                resolve();
            }
        });
    });
}