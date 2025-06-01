/**
 * 👑 КВЕСТ ПРО ПУЛПУЛАК НА TAU-PROLOG 👑
 * Реальная история княжны и её помощницы
 */

const pl = require('tau-prolog');

console.log('👑 === КВЕСТ КНЯЖНЫ ПУЛПУЛАК === 👑\n');

const session = pl.create();

const pulpulakQuest = `
% === ПЕРСОНАЖИ И ИХ РОЛИ ===
character(pulpulak).
character(helper).
character_role(pulpulak, princess).
character_role(helper, servant).

% === ЛОКАЦИИ ЗАМКА ===
location(princess_chamber).
location(throne_room).
location(corridor_upper).
location(corridor_lower).
location(kitchen).
location(garden).
location(library).
location(secret_archive).
location(greenhouse).

% Классификация локаций по статусу
royal_location(princess_chamber).
royal_location(throne_room).
royal_location(library).
royal_location(secret_archive).

servant_location(kitchen).
servant_location(garden).
servant_location(greenhouse).

public_location(corridor_upper).
public_location(corridor_lower).

% === NPC И ИХ МЕСТОНАХОЖДЕНИЕ ===
npc(royal_advisor).
npc(librarian).
npc(cook).
npc(herbalist).

npc_location(royal_advisor, throne_room).
npc_location(librarian, library).
npc_location(cook, kitchen).
npc_location(herbalist, garden).

% === ОДЕЖДА И ЕЁ ЗНАЧЕНИЕ ===
outfit(princess_dress).
outfit(simple_dress).

outfit_grants_access(princess_dress, royal_location).
outfit_grants_access(simple_dress, servant_location).
outfit_grants_access(_, public_location).

% === ТЕКУЩЕЕ СОСТОЯНИЕ ИГРЫ ===
character_location(pulpulak, princess_chamber).
character_location(helper, princess_chamber).
character_outfit(pulpulak, princess_dress).
character_outfit(helper, simple_dress).

% === КВЕСТЫ ===
quest(lost_royal_relic).
quest(secret_healing_potion).

quest_giver(lost_royal_relic, royal_advisor).
quest_giver(secret_healing_potion, cook).

quest_requires_outfit(lost_royal_relic, princess_dress).
quest_requires_outfit(secret_healing_potion, simple_dress).

% Шаги квеста реликвии
quest_step(lost_royal_relic, 1, talk_to_advisor).
quest_step(lost_royal_relic, 2, visit_library).
quest_step(lost_royal_relic, 3, find_librarian_in_archive).
quest_step(lost_royal_relic, 4, return_to_advisor).

% Шаги квеста зелья
quest_step(secret_healing_potion, 1, talk_to_cook).
quest_step(secret_healing_potion, 2, find_herbalist).
quest_step(secret_healing_potion, 3, gather_herbs_in_greenhouse).
quest_step(secret_healing_potion, 4, return_to_cook).

% === МЕХАНИКА ПЕРЕОДЕВАНИЯ ===
% Можно менять одежду только наедине
can_swap_outfits(Character1, Character2) :-
    character_location(Character1, Location),
    character_location(Character2, Location),
    Character1 \\= Character2,
    private_location(Location).

private_location(princess_chamber).

% === ДОСТУПНОСТЬ КВЕСТОВ ===
quest_available(Character, Quest) :-
    character_outfit(Character, RequiredOutfit),
    quest_requires_outfit(Quest, RequiredOutfit),
    quest_giver(Quest, NPC),
    can_talk_to_npc(Character, NPC),
    \\+ quest_completed(Quest).

% === ВЗАИМОДЕЙСТВИЕ С NPC ===
can_talk_to_npc(Character, NPC) :-
    character_location(Character, Location),
    npc_location(NPC, Location),
    has_access_to_location(Character, Location).

has_access_to_location(Character, Location) :-
    character_outfit(Character, Outfit),
    location_type(Location, Type),
    outfit_grants_access(Outfit, Type).

location_type(Location, royal_location) :- royal_location(Location).
location_type(Location, servant_location) :- servant_location(Location).
location_type(Location, public_location) :- public_location(Location).

% === NPC РЕАКЦИИ НА ПЕРСОНАЖЕЙ ===
npc_attitude(royal_advisor, Character, respectful) :-
    character_outfit(Character, princess_dress).

npc_attitude(royal_advisor, Character, confused) :-
    character_outfit(Character, simple_dress).

npc_attitude(cook, Character, welcoming) :-
    character_outfit(Character, simple_dress).

npc_attitude(cook, Character, nervous) :-
    character_outfit(Character, princess_dress).

npc_attitude(librarian, Character, helpful) :-
    character_outfit(Character, princess_dress).

npc_attitude(herbalist, Character, friendly) :-
    character_outfit(Character, simple_dress).

% === ИСТОРИЯ И СЮЖЕТ ===
% Основная проблема королевства
kingdom_problem(missing_ancient_relic).
kingdom_problem(plague_needs_cure).

% Квест решает проблему
quest_solves(lost_royal_relic, missing_ancient_relic).
quest_solves(secret_healing_potion, plague_needs_cure).

% === СИСТЕМА ПОДСКАЗОК ===
hint_available(Character, go_to_throne_room) :-
    character_location(Character, princess_chamber),
    character_outfit(Character, princess_dress),
    \\+ quest_started(lost_royal_relic).

hint_available(Character, go_to_kitchen) :-
    character_location(Character, princess_chamber),
    character_outfit(Character, simple_dress),
    \\+ quest_started(secret_healing_potion).

hint_available(Character, swap_outfits) :-
    character_location(Character, princess_chamber),
    character_location(OtherCharacter, princess_chamber),
    Character \\= OtherCharacter,
    character_outfit(Character, CurrentOutfit),
    character_outfit(OtherCharacter, OtherOutfit),
    CurrentOutfit \\= OtherOutfit.

hint_available(Character, visit_library) :-
    quest_started(lost_royal_relic),
    quest_step_completed(lost_royal_relic, 1),
    \\+ quest_step_completed(lost_royal_relic, 2).

% === СТРАТЕГИЧЕСКОЕ ПЛАНИРОВАНИЕ ===
% Может ли персонаж выполнить квест в текущем состоянии
can_complete_quest(Character, Quest) :-
    quest_available(Character, Quest),
    quest_all_steps_accessible(Character, Quest).

quest_all_steps_accessible(Character, Quest) :-
    character_outfit(Character, Outfit),
    quest_requires_outfit(Quest, Outfit),
    quest_giver(Quest, NPC),
    npc_location(NPC, NPCLocation),
    has_access_to_location(Character, NPCLocation).

% === АНАЛИЗ ИГРОВОЙ СИТУАЦИИ ===
situation_analysis(Character, can_start_main_quest) :-
    character_role(Character, princess),
    can_complete_quest(Character, lost_royal_relic).

situation_analysis(Character, can_start_secret_quest) :-
    can_complete_quest(Character, secret_healing_potion).

situation_analysis(Character, needs_outfit_change) :-
    character_location(Character, princess_chamber),
    \\+ can_complete_quest(Character, _),
    can_swap_outfits(Character, _).

situation_analysis(Character, stuck_wrong_outfit) :-
    character_location(Character, Location),
    \\+ has_access_to_location(Character, Location).

% === ИСТОРИЯ ПЕРСОНАЖЕЙ ===
character_backstory(pulpulak, royal_bloodline).
character_backstory(pulpulak, educated_in_ancient_lore).
character_backstory(helper, common_birth).
character_backstory(helper, skilled_in_herbs).

% Персонаж подходит для квеста по происхождению
character_suited_for_quest(Character, Quest) :-
    character_backstory(Character, royal_bloodline),
    quest(lost_royal_relic),
    Quest = lost_royal_relic.

character_suited_for_quest(Character, Quest) :-
    character_backstory(Character, skilled_in_herbs),
    quest(secret_healing_potion),
    Quest = secret_healing_potion.

% === СЛОЖНАЯ ЛОГИКА: КООПЕРАТИВНЫЕ КВЕСТЫ ===
% Могут ли персонажи выполнить оба квеста, меняясь местами
cooperative_quest_possible :-
    can_swap_outfits(pulpulak, helper),
    character_suited_for_quest(pulpulak, lost_royal_relic),
    character_suited_for_quest(helper, secret_healing_potion).

% Оптимальная стратегия для двух игроков
optimal_strategy(start_with_natural_roles) :-
    character_role(pulpulak, princess),
    character_suited_for_quest(pulpulak, lost_royal_relic),
    character_role(helper, servant),
    character_suited_for_quest(helper, secret_healing_potion).

optimal_strategy(swap_for_efficiency) :-
    cooperative_quest_possible,
    character_location(pulpulak, princess_chamber),
    character_location(helper, princess_chamber).
`;

console.log('📚 Загружаем мир Пулпулак...');
session.consult(pulpulakQuest, {
    success: function() {
        console.log('✅ Мир Пулпулак загружен!\n');
        explorePulpulakWorld();
    },
    error: function(err) {
        console.log('❌ Ошибка:', err);
    }
});

async function explorePulpulakWorld() {
    console.log('🏰 === ИССЛЕДУЕМ МИР КНЯЖНЫ ПУЛПУЛАК ===\n');
    
    console.log('1. 👑 ПЕРСОНАЖИ И ИХ СОСТОЯНИЕ:');
    await findAll("character_location(Character, Location)", "Где находятся персонажи");
    await findAll("character_outfit(Character, Outfit)", "Во что одеты персонажи");
    await findAll("character_role(Character, Role)", "Роли персонажей");
    
    console.log('\n2. 🏰 МИР ЗАМКА:');
    await findAll("npc_location(NPC, Location)", "Где находятся NPC");
    await findAll("royal_location(Location)", "Королевские покои");
    await findAll("servant_location(Location)", "Места для слуг");
    
    console.log('\n3. 💬 СОЦИАЛЬНЫЕ ВЗАИМОДЕЙСТВИЯ:');
    await findAll("can_talk_to_npc(Character, NPC)", "Кто с кем может говорить");
    await findAll("npc_attitude(NPC, Character, Attitude)", "Отношение NPC к персонажам");
    
    console.log('\n4. 🎯 КВЕСТОВЫЙ АНАЛИЗ:');
    await findAll("quest_available(Character, Quest)", "Доступные квесты");
    await findAll("can_complete_quest(Character, Quest)", "Квесты которые можно завершить");
    
    console.log('\n5. 👗 МЕХАНИКА ПЕРЕОДЕВАНИЯ:');
    await findAll("can_swap_outfits(Character1, Character2)", "Кто может поменяться одеждой");
    await findAll("has_access_to_location(Character, Location)", "Доступ к локациям");
    
    console.log('\n6. 💡 УМНЫЕ ПОДСКАЗКИ:');
    await findAll("hint_available(Character, Hint)", "Подсказки системы");
    
    console.log('\n7. 📖 ПРЕДЫСТОРИЯ И ХАРАКТЕРЫ:');
    await findAll("character_backstory(Character, Trait)", "Предыстория персонажей");
    await findAll("character_suited_for_quest(Character, Quest)", "Кто подходит для какого квеста");
    
    console.log('\n8. 🤝 КООПЕРАТИВНАЯ СТРАТЕГИЯ:');
    await checkFact("cooperative_quest_possible", "Возможно ли кооперативное прохождение?");
    await findAll("optimal_strategy(Strategy)", "Оптимальные стратегии");
    
    console.log('\n9. 🧠 АНАЛИЗ ИГРОВОЙ СИТУАЦИИ:');
    await findAll("situation_analysis(Character, Analysis)", "Анализ текущей ситуации");
    
    console.log('\n10. 👥 КОРОЛЕВСТВО И ЕГО ПРОБЛЕМЫ:');
    await findAll("kingdom_problem(Problem)", "Проблемы королевства");
    await findAll("quest_solves(Quest, Problem)", "Какие квесты решают проблемы");
    
    console.log('\n🎭 === ДЕМОНСТРАЦИЯ СЮЖЕТНЫХ СЦЕНАРИЕВ ===');
    
    // Сценарий 1: Пулпулак начинает квест реликвии
    console.log('\n📜 СЦЕНАРИЙ 1: Пулпулак идёт к советнику');
    await simulateScenario([
        'character_location(pulpulak, throne_room)'
    ], [
        "can_talk_to_npc(pulpulak, royal_advisor)",
        "quest_available(pulpulak, lost_royal_relic)",
        "npc_attitude(royal_advisor, pulpulak, Attitude)"
    ]);
    
    // Сценарий 2: Помощница переодевается
    console.log('\n📜 СЦЕНАРИЙ 2: Помощница надевает княжеское платье');
    await simulateScenario([
        'character_outfit(helper, princess_dress)'
    ], [
        "has_access_to_location(helper, throne_room)",
        "quest_available(helper, lost_royal_relic)",
        "npc_attitude(royal_advisor, helper, Attitude)"
    ]);
    
    console.log('\n🎉 === ИСТОРИЯ ПУЛПУЛАК ЗАВЕРШЕНА ===');
    console.log('\n💭 ВЫВОДЫ О МИРЕ ПУЛПУЛАК:');
    console.log('   ✅ Сложная система социальных ролей и одежды');
    console.log('   ✅ Интеллектуальные NPC с разным отношением');
    console.log('   ✅ Кооперативная механика смены ролей');
    console.log('   ✅ Богатая предыстория и мотивация персонажей');
    console.log('   ✅ Логическая связь между квестами и проблемами королевства');
}

async function simulateScenario(newFacts, queries) {
    // Добавляем временные факты
    for (const fact of newFacts) {
        const tempProgram = fact + '.';
        await new Promise(resolve => {
            session.consult(tempProgram, {
                success: () => resolve(),
                error: () => resolve()
            });
        });
    }
    
    // Выполняем запросы
    for (const query of queries) {
        await findAll(query, `   ${query}`);
    }
}

function checkFact(query, description) {
    return new Promise((resolve) => {
        console.log(`  📋 ${description}`);
        session.query(query + '.', {
            success: function(goal) {
                session.answer(function(answer) {
                    if (answer !== null && answer !== false) {
                        console.log(`     ✅ ДА`);
                    } else {
                        console.log(`     ❌ НЕТ`);
                    }
                    resolve();
                });
            },
            error: function(err) {
                console.log(`     ❌ ОШИБКА: ${err}`);
                resolve();
            }
        });
    });
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