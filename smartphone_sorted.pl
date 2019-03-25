competitor(appy,sumsum).
smartphone_technology(sumsum,galactica-s3).
steal(stevey,galactica-s3).
boss(stevey,appy).
rival(appy,Y):-
competitor(appy,Y).

business(X,Y):-
smartphone_technology(X,Y).

unethical(X):-
boss(X,A),
rival(A,B),
steal(X,Y),
business(B,Y).