competitor(appy,sumsum).

rival(appy,Y):-
competitor(appy,Y).

smartphone_technology(sumsum,galactica-s3).

business(X,Y):-
smartphone_technology(X,Y).

boss(stevey,appy).
steal(stevey,galactica-s3).

unethical(X):-
boss(X,A),
rival(A,B),
steal(X,Y),
business(B,Y).








