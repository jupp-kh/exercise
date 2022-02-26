% Youssef kharita 
% a)
userDefinedList(nil).
userDefinedList(cons(_, X)) :-userDefinedList(X).

% b)
asPrologList(nil, []).
asPrologList(cons(Z,X),[Y|Res]) :- Z = Y,
				asPrologList(X,Res).

% c)
flatten([[]],[]).
flatten([[]|Nl],Resy) :- flatten(Nl,Resy).
flatten([[X|Resx]|Nl],[Y|Resy]) :- X=Y,
				flatten([Resx|Nl],Resy), !.
% d)
% jeder node hat zwei praedikate eines ist der beliebig zugewiesene Wert 
% und der andere ist eine  liste von nodes 
% kns ist die liste von nodes 
% Kn ist die naechste node in Baum. 
% leaf ist einer node, der keine Kinderknote hat.
% Bsp für eingabe mehrwegBaum(node(12,[node(3,[leaf(4)]),leaf(5)])).

mehrwegBaum([leaf(_)]).
mehrwegBaum(node(_ ,[leaf(_)])).
mehrwegBaum(node(_,[Kn|Kns])) :-
				mehrwegBaum(Kn),
				mehrwegBaum(node(_,Kns)).

% e)
flattenTree([], []).
flattenTree(leaf(W),W).						
flattenTree([Nk|GK],X) :- flattenTree(Nk,F),
			flattenTree(GK,G),
			append(F,G,X). 
flattenTree(node(Y,Res),X) :-flattenTree(Res,F),
		      		append(Y,F,X).			
								  
append([], YS, YS).
append([X|XS], YS, [X|Res]) :- append(XS, YS, Res).

% Aufgabe(8) a)

prime(N) :- N > 1,
		primetest(N,2).

primetest(N,N).
primetest(N,Y) :-  0 < mod(N,Y),
		Z is Y + 1 , 	
		primetest(N, Z).

% b)
only_primes([]).
only_primes([X|XS]) :- prime(X),
			only_primes(XS).

firstGreater([X|_],Y ,0) :- X > Y.
firstGreater([X|XS],Y,I) :- X =< Y, firstGreater(XS ,Y,I2), I is I2 +1.