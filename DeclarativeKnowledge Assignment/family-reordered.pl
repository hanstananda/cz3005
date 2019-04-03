% Use Tabling module to prevent infinity loop in searching
:- use_module(library(tabling)).
:- table parent_of/2.
:- discontiguous 'parent_of tabled'/2.

brother(peter, warren).
brother(jerry,kather).
brother(jerry,stuart).
male(stuart).
male(peter).
male(warren).
male(jerry).
female(kather).
female(ann).
female(maryalice).
sister(kather,jerry).
sister(ann, mayalice).
parent_of(maryalice,jerry).
parent_of(warren,jerry).

father(X,Y):-
    male(X),
    parent_of(X,Y).

mother(X,Y):-
    female(X),
    parent_of(X,Y).


son(X,Y):-
    male(X),
    parent_of(Y,X).

daughter(X,Y):-
    female(X),
    parent_of(Y,X).


grandfather(X,Y):-
    father(X,Z),
    parent_of(Z,Y).

sibling(X,Y):-
    brother(X,Y);
    brother(Y,X);
    sister(X,Y);
    sister(Y,X).

aunt(X,Y):-
    female(X),
    sister(X,Z) | sister(Z,X),
    parent_of(Z,Y).

uncle(X,Y):-
    male(X),
    brother(X,Z) | brother(Z,X),
    parent_of(Z,Y).

cousin(X,Y):-
    (aunt(Z,X)|uncle(Z,X)),
    sibling(Z,Y).

spouse(X,Y):-
    father(X,Z),
    mother(Y,Z).

parent_of(X,Y):-
    parent_of(X,Z),
    sibling(Y,Z).
