% Declare facts of available options
breads([italian_wheat, hearty_italian, honey_oat, parmesan_oregano, flatbread, multigrain]).
meats([ turkey_breast, ham, chicken_breast, roast_beef, tuna, turkey_salami, beefsteak, bacon, meatballs, pepperoni]).
cheeses([processed_cheddar, monterey_cheddar, none]).
vegs([cucumbers, green_bell_peppers, lettuce, red_onions, tomatoes, black_olives, jalapeno, pickles]).
sauces([honey_mustard, yelow_mustard, deli_brown_mustard, sweet_onion, chipotle_southwest, ranch, bbq, chili_sauce, tomato_sauce, mayonnaise]).
vegan_sauces([chili_sauce, tomato_sauce]).
healthy_sauces([honey_mustard, yelow_mustard, deli_brown_mustard, sweet_onion]).
sides([chips, cookies, hashbrowns, energy_bar_and_fruit_crisps, yogurt]). 
drinks([fountain_drinks, dasani_mineral_water, minute_maid_puly_orange_juice, ayataka_japanese_green_tea, coffee, tea]).
salads([cold_cut_trio, chicken_and_bacon_ranch, chicken_teriyaki, egg_mayo, chicken_ham, italian_bmt, meatball_marinara_melt, roast_beef, veggie_party]).
healthy([italian_wheat, hearty_italian, sourdough]).
vegan([italian_wheat, hearty_italian, sourdough]).

print_options([]). % empty list
print_options([H]) :- 
    write(H), 
    write('.'). % last item in list

print_options([H|T]) :- 
    write(H), 
    write(', '), 
    print_options(T), !. % list 

options(breads):-
    breads(L), print_options(L).

options(meats):-
    meats(L), print_options(L).

options(cheeses):-
    cheeses(L), print_options(L).

options(vegs):-
    vegs(L), print_options(L).

options(sauces):-
    sauces(L), print_options(L).

options(vegan_sauces):-
    vegan_sauces(L),print_options(L).

options(healthy_sauces):-
    healthy_sauces(L),print_options(L).

options(sides):-
    sides(L), print_options(L).

options(drinks):-
    drinks(L), print_options(L).

options(salads):-
    salads(L), print_options(L).

check_selection(X, breads):-
    breads(L), member(X,L),!.

check_selection(X, meats):-
    meats(L), member(X,L),!.

check_selection(X, cheeses):-
    cheeses(L), member(X,L),!.

check_selection(X, vegs):-
    vegs(L), member(X,L),!.

check_selection(X, vegan_sauces):-
    vegan_sauces(L), member(X,L),!.

check_selection(X, healthy_sauces):-
    healthy_sauces(L), member(X,L),!.

check_selection(X, sauces):-
    sauces(L), member(X,L),!.

check_selection(X, sides):-
    sides(L), member(X,L),!.

check_selection(X, drinks):-
    drinks(L), member(X,L),!.

check_selection(X,salads):-
    salads(L), member(X,L),!.

selected(X,breads):-
    assert(bread(X)), write(X),nl.

selected(X,meats):-
    assert(meat(X)), write(X),nl.

selected(X,cheeses):-
    assert(cheese(X)), write(X),nl.

selected(X,vegs):-
    assert(veg(X)), write(X),nl.

selected(X,sauces):-
    assert(sauce(X)), write(X),nl.

selected(X,sides):-
    assert(side(X)), write(X),nl.

selected(X,drinks):-
    assert(drink(X)), write(X),nl.

selected(X,salads):-
    assert(salad(X)), write(X),nl.

query_bread:-
    options(breads),nl,
    read(X),
    check_selection(X,breads) -> selected(X,breads);
        write("wrong_selection"),nl,
        query_bread.

query_cheese:-
    options(cheeses),nl,
    read(X),
    check_selection(X,cheeses) -> selected(X,cheeses);
        write("wrong_selection"),nl,
        query_cheese.

query_drink:-
    options(drinks),nl,
    read(X),
    check_selection(X,drinks) -> selected(X,drinks);
        write("wrong_selection"),nl,
        query_drink.

query_meat:-
    options(meats),nl,
    read(X),
    not(X==0) -> 
        (check_selection(X,meats) -> selected(X,meats);
            write("wrong_selection"),nl),
        query_meat;
        true.

query_veg :-
    options(vegs),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, vegs) -> selected(X,vegs); 
            write("wrong_selection"),nl ),      
        query_veg;
        true.

query_healthy_sauce:-
    options(healthy_sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X,healthy_sauces) -> selected(X,sauces);
            write("wrong_selection"),nl),
        query_healthy_sauce;
        true.

query_vegan_sauce:-
    options(vegan_sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, vegan_sauces) -> selected(X,sauces); 
            write("wrong_selection"),nl ),      
        query_vegan_sauce;
        true.

query_sauce:-
    options(sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, sauces) -> selected(X,sauces); 
            write("wrong_selection"),nl ),      
        query_sauce;
        true.

query_side:-
    options(sides),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, sides) -> selected(X,sides); 
            write("wrong_selection"),nl ),      
        query_side;
        true.

query_salad:-
    options(salads),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, salads) -> selected(X,salads); 
            write("wrong_selection"),nl ),      
        query_salad;
        true.

% Declare dynamic predicates to store results
:- dynamic bread/1, meat/1, veg/1, sauce/1, side/1, drink/1, cheese/1, salad/1.

meal_normal:-
    query_bread, query_meat, query_veg, query_cheese, query_sauce, query_side, query_drink.
meal_veggie:-
    query_bread, query_veg, query_cheese, query_sauce, query_side, query_drink.
meal_vegan:-
    query_bread, query_veg, query_vegan_sauce, query_side, query_drink.
meal_healthy:-
    query_bread, query_veg, query_healthy_sauce.
meal_value :-
    query_bread, query_meat, query_veg, query_cheese, query_sauce.
meal_salad:-
    query_salad, query_side, query_drink.

