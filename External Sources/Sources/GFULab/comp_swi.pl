 /*================================================================
    File COMP_SWI.PL
    
    Author: Juan C. Ruiz Anton
            Universitat Jaume I, Castelló, Spain
            January 2004
            
        Predicates for compatibility with SWI-Prolog
  =================================================================*/

 :- module(comp_swi,[eraseall/1, new_counter/2]).

 %----------------------------------------------------------------------
 % eraseall(+Key) is a primitive in arity prolog.
 % It removes all the terms that are stored under the specified Key
 %----------------------------------------------------------------------

 eraseall(K) :-
     recorded(K,_,Ref),
     erase(Ref),
     fail.

 eraseall(_).

 % new_counter(+Key,-Number)

 new_counter(Cat,Old+1) :- flag(Cat,Old,Old+1).

/* A more traditional definition por YAP, etc.
 new_counter(Cat,N) :- 
	recorded(Cat,M,Ref),
	erase(Ref),
	N is M+1,
	recordz(Cat,N).

 new_counter(Cat,1) :- recorda(Cat,1). */


