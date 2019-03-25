male(prince_charles).
male(prince_andrew).
male(prince_edward).
female(princess_ann).
female(queen_elizabeth).

offspring(queen_elizabeth,prince_charles).
offspring(queen_elizabeth,princess_ann).
offspring(queen_elizabeth,prince_andrew).
offspring(queen_elizabeth,prince_edward).

older(prince_charles,princess_ann).
older(princess_ann,prince_andrew).
older(prince_andrew,prince_edward).
check_older(X,Y):-
    older(X,Y);
    older(X,Z),
    check_older(Z,Y).

precedes(X,Y):-
	male(X),
	male(Y),
	check_older(X,Y);
	male(X),
	female(Y);
	female(X),
	female(Y),
	check_older(X,Y).


compares(<,X,Y):-
	precedes(X,Y).

compares(>,X,Y):-
	not(precedes(X,Y)).


succession_list(X,SuccessionList):-
	findall(Y,offspring(X,Y),ChildrenList),
	predsort(compares,ChildrenList,SuccessionList).
% succession_list(queen_elizabeth,Y).