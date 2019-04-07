% Declare facts of available options, based on the subway official website
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

% print_options is used to print the items based on the given list.
print_options([]). % empty list
print_options([H]) :- % last item in list
    write(H), 
    write('.'). 

print_options([H|T]) :-  % List with items more than one
    write(H), 
    write(', '), 
    print_options(T), !.% remove the item then print it one by one

% options is used to choose the list based on the arguments given and call the print list command
options(breads):- % Print options for breads
    breads(L), print_options(L). % Get the list and then call the print function with the respective list

options(meats):- % Functionally the same as above but for meats
    meats(L), print_options(L). 

options(cheeses):- % Functionally the same as above but for cheeses
    cheeses(L), print_options(L).

options(vegs):- % Functionally the same as above but for vegetables
    vegs(L), print_options(L).

options(sauces):- % Functionally the same as above but for sauces
    sauces(L), print_options(L).

options(vegan_sauces):- % Functionally the same as above but for vegan sauces
    vegan_sauces(L),print_options(L).

options(healthy_sauces):- % Functionally the same as above but for healthy sauces
    healthy_sauces(L),print_options(L).

options(sides):- % Functionally the same as above but for sides
    sides(L), print_options(L).

options(drinks):- % Functionally the same as above but for drinks
    drinks(L), print_options(L).

options(salads):- % Functionally the same as above but for salads
    salads(L), print_options(L).

% Check selection is used to check whether the given input is inside the list of fact specified
% '!' is used to cut the backtracking and return right away when the 
check_selection(X, breads):- % Check X is in list of breads
    breads(L), member(X,L),!. % For the given list, check whether element is in a list

check_selection(X, meats):- % Check X is in list of meats
    meats(L), member(X,L),!.

check_selection(X, cheeses):- % Check X is in list of cheeses
    cheeses(L), member(X,L),!.

check_selection(X, vegs):- % Check X is in list of vegetables
    vegs(L), member(X,L),!.

check_selection(X, vegan_sauces):- % Check X is in list of vegan sauces
    vegan_sauces(L), member(X,L),!.

check_selection(X, healthy_sauces):- % Check X is in list of healthy sauces
    healthy_sauces(L), member(X,L),!.

check_selection(X, sauces):- % Check X is in list of sauces
    sauces(L), member(X,L),!.

check_selection(X, sides):- % Check X is in list of sides
    sides(L), member(X,L),!.

check_selection(X, drinks):- % Check X is in list of drinks
    drinks(L), member(X,L),!.

check_selection(X,salads):- % Check X is in list of salads
    salads(L), member(X,L),!.

% Selected is used to assert the facts given the item argument(X) and the specific list of fact
selected(X,breads):- % assert a bread fact from the given argument and print the value
    assert(bread(X)), print_selected(breads).

selected(X,meats):- % assert a meat fact from the given argument and print the value
    assert(meat(X)), print_selected(meats).

selected(X,cheeses):- % assert a cheese fact from the given argument and print the value
    assert(cheese(X)), print_selected(cheeses).

selected(X,vegs):- % assert a vegetable fact from the given argument and print the value
    assert(veg(X)), print_selected(vegetables).

selected(X,sauces):- % assert a sauce fact from the given argument and print the value
    assert(sauce(X)), print_selected(sauces).

selected(X,sides):- % assert a side fact from the given argument and print the value
    assert(side(X)),print_selected(sides).

selected(X,drinks):- % assert a drink fact from the given argument and print the value
    assert(drink(X)), print_selected(drinks).

selected(X,salads):- % assert a salad fact from the given argument and print the value
    assert(salad(X)), print_selected(salads).

% Print selected is used to find asserted items of a specific list and then print them.
print_selected(breads):- % print the asserted breads 
    findall(X, bread(X), Breads), atomic_list_concat(Breads, ',',Bread), % Find the items and concat it into one string
    write("Bread selected: "),write(Bread),write("."),nl. % Print the items

print_selected(meats):- % print the asserted meats 
    findall(X, meat(X), Meats), atomic_list_concat(Meats, ',',Meat),
    write("Meats selected: "),write(Meat),write("."),nl.

print_selected(cheeses):- % print the asserted cheeses 
    findall(X, cheese(X), Cheeses), atomic_list_concat(Cheeses, ',',Cheese),
    write("Cheese selected: "),write(Cheese),write("."),nl.

print_selected(vegetables):- % print the asserted vegetables 
    findall(X, veg(X), Vegs), atomic_list_concat(Vegs, ',',Veg),
    write("Vegetables selected: "),write(Veg),write("."),nl.

print_selected(sauces):- % print the asserted sauces 
    findall(X, sauce(X), Sauces), atomic_list_concat(Sauces, ',',Sauce),
    write("Sauces selected: "),write(Sauce),write("."),nl.

print_selected(sides):- % print the asserted sides
    findall(X, side(X), Sides), atomic_list_concat(Sides, ',',Side),
    write("Sides selected: "),write(Side),write("."),nl.

print_selected(salads):- % print the asserted salads 
    findall(X, salad(X), Salads), atomic_list_concat(Salads, ',',Salad),
    write("Salads selected: "),write(Salad),write("."),nl.

print_selected(drinks):- % print the asserted drinks
    findall(X, drink(X), Drinks), atomic_list_concat(Drinks, ',',Drink),
    write("Drink selected: "),write(Drink),write("."),nl.

% Query is used to get the user inputs for each of the options 
query(bread):- % Get the type of bread selected by the user
    print("Please choose your bread type:"),
    options(breads),nl,
    read(X),
    check_selection(X,breads) -> selected(X,breads); % If the input given is valid, then assert the fact
        write("wrong_selection"),nl, 
        query(bread). % The input is invalid, thus it loops back to query the user again

query(cheese):- % Get the type of cheese selected by the user
    print("Please choose your type of cheese:"),
    options(cheeses),nl,
    read(X),
    check_selection(X,cheeses) -> selected(X,cheeses);
        write("wrong_selection"),nl,
        query(cheese).

query(drink):- % Get the type of drink selected by the user
    print("Please choose your drink:"),
    options(drinks),nl,
    read(X),
    check_selection(X,drinks) -> selected(X,drinks);
        write("wrong_selection"),nl,
        query(drink).

query(meat):- % Get the type of meats selected by the user
    print("Please choose the meats you want one by one(0 to end):"),
    options(meats),nl,
    read(X),
    not(X==0) -> 
        (check_selection(X,meats) -> selected(X,meats); % If the input given is valid, then assert the fact
            write("wrong_selection"),nl),
        query(meat); % Loops until the given input is 0 
        true. % Ends the loop if the input is 0

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
    print_selected(breads),
    print_selected(meats),
    print_selected(cheeses),
    print_selected(vegetables),
    print_selected(sauces),
    print_selected(sides),
    print_selected(salads),
    print_selected(drinks).

