from pyswip import Prolog
prolog = Prolog()
prolog.consult('subway.pl')
x = prolog.query("start_choose")
print(x)
