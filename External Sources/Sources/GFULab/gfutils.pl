 /******************************************************************
    Library of utilities for GFU
 *******************************************************************/

 :- module(gfutils,[prefix/3, lastnv/2, writelist/1, litteral/1,list_to_disj/2, atom_string_list/2, pick/3, lst/1]).

 
 %--------------------------------------------------------------------
 % prefix/3 determines if arg1 is the initial prefix (sublist) of arg2,
 % returning the remainder list arg3 (Sterling & Shapiro, p. 45)
 %--------------------------------------------------------------------

 prefix([],X,X) :- !.	
 prefix([A|B],[A|C],D) :- prefix(B,C,D).


 %--------------------------------------------------------
 % LASTNV: find the last non-variable element of a list 
 %--------------------------------------------------------

 lastnv(T,N) :-
        T =.. [H,Tail],!,
        (   var(Tail) -> N=H,!; lastnv(Tail,N)).
 lastnv(T,T).


 %----------------------------------------------------------------
 %  writelist( +L)
 %   <- Writes the elements of list L, separated by a blank
 %----------------------------------------------------------------

 writelist([]) :- !.
 writelist([A|B]) :- write(A),tab(1),writelist(B).

 %------------------------------------------------
 % litteral( +Expr)
 %   <- Expr is an atom biginning with a letter
 %------------------------------------------------

 litteral(X) :- X @>=a, X @< '{', !. 
 litteral(X) :- X @>='A', X @< '['.
 
 %--------------------------------------------------------------------
 % list_to_disj( ?L, ?D)
 %  <- Creates a disjunctive series (format a;b;c;...;x) from a 
 %     list of atomic elements.( ).
 %     NOTA BENE: This predicate is bidirectional

 list_to_disj([A],A) :- atomic(A),!.
 list_to_disj([A|B],(A;C)) :- list_to_disj(B,C).

 %-------------------------------------------------------------------
 % atom_string_list(+L1,-L2)
 %  <- Transforms a list of atoms into a list of strings, also
 %     ripping off hyphens (used as morpheme separators in 
 %       the input sentence)
 %--------------------------------------------------------------------

 atom_string_list([],[]).
 
 % The hyphen goes off
 atom_string_list(['-'|Rest],SL) :- !, atom_string_list(Rest,SL).
      
 atom_string_list([A|Rest],[A|SL]) :- atom_string_list(Rest,SL).

 %------------------------------------------------------------------- 
 % pick(+E,+L1,-L2)
 % Picks the element E from list L1, returning the remainder L2
 %-------------------------------------------------------------------

 pick(_,Var,_) :- var(Var), !, fail.
 pick(X,[X|Rest],Rest) :-!.
 pick(X,[First|Y],[First|Z]) :- pick(X,Y,Z).

 %-------------------------------------------------------
 %  Checks if its argument is a list
 %  Unlike 'is_list' this predicate also succeeds with
 %  variable-ended lists such as [a,b,c|_]
 %-------------------------------------------------------

 lst(X) :-
    nonvar(X),
    (X = [] ; X= [_|_] ).
