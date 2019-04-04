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

query(bread):-
    print("Please choose your bread type:"),
    options(breads),nl,
    read(X),
    check_selection(X,breads) -> selected(X,breads);
        write("wrong_selection"),nl,
        query(bread).

query(cheese):-
    print("Please choose your type of cheese:"),
    options(cheeses),nl,
    read(X),
    check_selection(X,cheeses) -> selected(X,cheeses);
        write("wrong_selection"),nl,
        query(cheese).

query(drink):-
    print("Please choose your drink:"),
    options(drinks),nl,
    read(X),
    check_selection(X,drinks) -> selected(X,drinks);
        write("wrong_selection"),nl,
        query(drink).

query(meat):-
    print("Please choose the meats you want one by one(0 to end):"),
    options(meats),nl,
    read(X),
    not(X==0) -> 
        (check_selection(X,meats) -> selected(X,meats);
            write("wrong_selection"),nl),
        query(meat);
        true.

query(veg) :-
    print("Please choose the vegetables you want one by one(0 to end):"),
    options(vegs),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, vegs) -> selected(X,vegs); 
            write("wrong_selection"),nl ),      
        query(veg);
        true.

query(healthy_sauce):-
    print("Please choose the type of sauces you want one by one(0 to end):"),
    options(healthy_sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X,healthy_sauces) -> selected(X,sauces);
            write("wrong_selection"),nl),
        query(healthy_sauce);
        true.

query(vegan_sauce):-
    print("Please choose the type of sauces you want one by one(0 to end):"),
    options(vegan_sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, vegan_sauces) -> selected(X,sauces); 
            write("wrong_selection"),nl ),      
        query(vegan_sauce);
        true.

query(sauce):-
    print("Please choose the type of sauces you want one by one(0 to end):"),
    options(sauces),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, sauces) -> selected(X,sauces); 
            write("wrong_selection"),nl ),      
        query(sauce);
        true.

query(side):-
    print("Please choose the sides you want one by one(0 to end):"),
    options(sides),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, sides) -> selected(X,sides); 
            write("wrong_selection"),nl ),      
        query(side);
        true.

query(salad):-
    print("Please choose the salads you want one by one(0 to end):"),
    options(salads),nl,
    read(X),
    not(X==0) -> 
        ( check_selection(X, salads) -> selected(X,salads); 
            write("wrong_selection"),nl ),      
        query(salad);
        true.

% Declare dynamic predicates to store results
:- dynamic bread/1, meat/1, veg/1, sauce/1, side/1, drink/1, cheese/1, salad/1, meal_type/1.

meal(normal):-
    query(bread), query(meat), query(veg), query(cheese), query(sauce), query(side), query(drink).
meal(veggie):-
    query(bread), query(veg), query(cheese), query(sauce), query(side), query(drink).
meal(vegan):-
    query(bread), query(veg), query(vegan_sauce), query(side), query(drink).
meal(healthy):-
    query(bread), query(veg), query(healthy_sauce).
meal(value) :-
    query(bread), query(meat), query(veg), query(cheese), query(sauce).
meal(salad):-
    query(salad), query(side), query(drink).

start_choose:-
    write("Choose meal type:(normal, veggie, vegan, healthy, value, salad)"),nl,
    read(Type),
    (Type== veggie -> 
        write("meal type ="), write(Type),nl, 
        meal(veggie), assert(meal_type(veggie));
    Type== vegan ->
        write("meal type ="), write(Type),nl, 
        meal(vegan), assert(meal_type(vegan));
    Type== healthy ->
        write("meal type ="), write(Type),nl, 
        meal(healthy), assert(meal_type(healthy));
    Type== value ->
        write("meal type ="), write(Type),nl, 
        meal(value), assert(meal_type(value));
    Type== normal ->    
        write("meal type ="), write(Type),nl, 
        meal(normal), assert(meal_type(normal));
    Type== salad ->
        write("meal type ="), write(Type),nl, 
        meal(salad), assert(meal_type(meal(salad)));
    write("invalid option selected!"),nl,
    start_choose),
    display.

display:- 
    meal_type(Meal),
    write("Meal type selected: "), write(Meal), nl,
    write("Choices selected:"),nl,
    findall(X, bread(X), Breads), atomic_list_concat(Breads, ',',Bread),
    write("Bread selected:"),write(Bread),nl,
    findall(X, meat(X), Meats), atomic_list_concat(Meats, ',',Meat),
    write("Meats selected:"),write(Meat),nl,
    findall(X, cheese(X), Cheeses), atomic_list_concat(Cheeses, ',',Cheese),
    write("Cheese selected:"),write(Cheese),nl,
    findall(X, veg(X), Vegs), atomic_list_concat(Vegs, ',',Veg),
    write("Vegetables selected:"),write(Veg),nl,
    findall(X, sauce(X), Sauces), atomic_list_concat(Sauces, ',',Sauce),
    write("Sauces selected:"),write(Sauce),nl,
    findall(X, side(X), Sides), atomic_list_concat(Sides, ',',Side),
    write("Sides selected:"),write(Side),nl,
    findall(X, salad(X), Salads), atomic_list_concat(Salads, ',',Salad),
    write("Salads selected:"),write(Salad),nl,
    findall(X, drink(X), Drinks), atomic_list_concat(Drinks, ',',Drink),
    write("Drink selected:"),write(Drink),nl.
