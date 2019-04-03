% Use Tabling module to prevent infinity loop in searching
:- use_module(library(tabling)).
:- table parent_of/2.
:- discontiguous 'parent_of tabled'/2.

male(hans).
male(karim).
male(bam).
male(bang).
male(hanliang).
male(a).
female(handayani).
female(sako).
female(liko).
female(lixia).
female(sasa).
brother(karim,liko).
brother(karim,sako).
brother(hanliang,handayani).
brother(michael,hans).
sister(hanliang,handayani).
sister(liko,sako).
sister(liko,karim).
sister(sako,karim).
sister(sako,liko).
parent_of(bang,sako).
parent_of(bang,liko).
parent_of(karim,hans).
parent_of(bam,handayani).
parent_of(bang,karim).
parent_of(handayani,hans).
parent_of(lixia,sasa).
parent_of(hanliang,sasa).


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
