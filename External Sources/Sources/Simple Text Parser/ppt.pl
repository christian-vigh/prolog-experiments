/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * Definition of the ppt predicate for displaying tree structures.
 * This predicate was developed by Gustaf Neumann (see below).
 **/
/* 
   ppt/1, 
   printing Prolog terms in a tree layout for typewriter output.

   Written in Spring 1985 -- Gustaf Neumann 

   (c)1985 Gustaf Neumann, Wirtschaftsuniversitaet Wien,
      Augasse 2-6, 1090 Vienna, Austria *222/31 336-4533. 
      email:  neumann@wu-wien.ac.at  or  neumann@awiwuw11.bitnet

   Permission is granted to use, copy and distribute this program as long 
   (1) this note remains intact, 
   (2) no fees are charged and 
   (3) no further restrictions are imposed.

   The following predicates are not defined within this program:
   -   length(List,Length), 
   -   tab(Exp), 
   -   append(A,B,AB).
   Do not try to print infinite trees :-)

   To show, what this program does, issue the goal: examples. 
*/
?-op(100,xfy,:).

examples:- example(X), ppt(X), nl, nl, write(X), nl,
	wait_for_input, fail.
examples.

example(sin(alpha)/cos(beta-phi)+cos(beta)*tan(360-alpha)).
example(buch(titel(wirschaftsinformatik1),autor(hans_robert, hansen))).
example((a:-b,c,d)).
%example((ppt(X,Y):-Body)):- clause(ppt(X,Y),Body).
example(sentence(np(proper(gustaf)),vp(v(likes),np(proper(prolog))))).
example(sentence(np(det(that),n(man),rel(that,vp(iv(whistle)))),
           vp(tv(tunes),np(det(nil),n(pianos),rel(nil))))).
example(wirtschaftsinformatik(leitung(hans_robert),
           sekretariat(virly,anita),
           assistenten(lore,rony,goeha,gu,margret,andy,stessi))).


/************************************************************************
*                      top level predicate ppt/1                        *
************************************************************************/
ppt(Term):- ppt(Term,arc).
ppt(Term,Arc) :-
      number_vars(Term,0,_),          /* ground all variables in Term     */
     {pos(Term,Pos,C,0-Right)},       /* compute hor. positions of nodes  */
     {inv([Pos],[]:_,H:T,s)},         /* invert structure for printing    */
      posdiff(-(72-Right)//2,0,Tab),  /* compute hor. tab for centering   */
     {print_tree(H:T,[C],Tab,Arc)}.
                                      /* print tree in line printer mode  */

/************************************************************************
*                      Compute Positions of Nodes                       *
************************************************************************/
pos(Head,t(Head,Rel,L,[],0)-[], Nc, N0-Nn):-     /* leaf node         */
     atomic(Head), !,
     string_length(Head,L), Nn is N0+L,
     Rel is L//2,                                /* middle of the node */
     Nc  is (N0+Nn)//2.                          /* center over node   */
pos(X,t(Head,Rel,L,Centers,Adj)-A, Nc, N0-N2):-  /* non-leaf node      */
     X =.. [Head|Args],
     pos_list(Args,A,Centers,N0-N1),
     string_length(Head,L), posdiff(N1-N0,L,Error),
     Adj is (Error+((N1-N0) mod 2))//2,
     N2 is N1+Error,
     Rel is L//2,                                /* middle of the node */
     Nc  is (N0+N2)//2.

pos_list([],   [],      [],         N-N).
pos_list([H],  [A],     [Center],   N-N1) :- !, pos(H,A,Center,N-N1).
pos_list([H|T],[A|Args],[C|Centers],N0-Nn):-
     pos( H,    A,       C,         N0-N1),
     N2 is N1+2, pos_list(T,Args,Centers,N2-Nn).

string_length(X,L):- atomic(X), name(X,S), length(S,L).

posdiff(Expr,L,Adj):- Adj is L-Expr, Adj > 0, !.
posdiff(_,_,0).

/************************************************************************

*                              invert tree                              *
************************************************************************/
inv([Node-Sons|Brothers],List:Deep,[Node|List1]:Deep2,_):-
     inv(Brothers,List:Deep,List1:Deep1,b),
     inv(Sons,Deep1,Deep2,s).
inv([],[]:[],[],s).
inv([],[]:[],[]:_,b).
inv([],E,E,_).

/************************************************************************
*                              print tree                               *
************************************************************************/
print_tree(Node:Deep, Centers, Tab, Arc) :-
     tab(Tab), print_list(Node,0,Centers,Cd), nl,
     {(   Arc  == noarc
      ;    Deep == []
      ;    tab(Tab), marks(Centers,Node,0),
           tab(Tab), horarc(Node,0,_), nl,
           tab(Tab), marks(Cd,0)
     )},
     print_tree(Deep,Cd,Tab,Arc).
print_tree([],[],_,_).

print_list([t(H,Rel,L,Cd,Adj)|R], P0, [C|Centers], Ca) :-
     P is C-Rel, tab(P-P0), write(H), Pn is P+L,
     print_list(R,Pn,Centers,Cr),
     add_to(Cd,Adj,Cda), {append(Cda,Cr,Ca)}.
print_list([],_,[],[]).

/************************************************************************
*                              draw arcs                                *
************************************************************************/
marks([],[],_) :- nl.
marks([H|T],[t(_,_,_,[],_)|R],E) :- !, tab(H-E), write(' '),marks(T,R,H+1).
marks([H|T],[_|R],            E) :-    tab(H-E), write('|'),marks(T,R,H+1).

marks([],_) :- nl.
marks([H|T],E) :- tab(H-E), write('|'), marks(T,H+1).

horarc([], A,A).
horarc([t([],_,_,_,_  )|R],P,P2) :- !, horarc(R,P,P2).
horarc([t(_,_,_,Cd,Adj)|R],P,P2) :-    line(Cd,Adj,P,P1), horarc(R,P1,P2).

line([],   _,E,P) :- P is E.
line([H],  A,E,P) :- !, tab(H+A-E), write('.'), P is H+A+1.
line([H|T],A,E,P) :-    tab(H+A-E), write('.'), line_([H|T],A,H+A+1,P).

line_([],      _,E,P) :- P is E.
line_([H],     A,E,P) :- line_to(H+A-E), P is H+A+1.
line_([_,T|Tt],A,E,P) :- line_to(T+A-E), write('.'), line_([T|Tt],A,T+A+1,P).

line_to(Exp)  :- L is Exp, line_to_(L,'-').
line_to_(L,_) :- L < 1.
line_to_(L,C) :- L >= 1, write(C), L1 is L-1, line_to_(L1,C).

add_to([],_,[]).
add_to([H|T],A,[Ha|Ta]) :- Ha is H+A, add_to(T,A,Ta).

/************************************************************************
*                       misc utility predicates                         *
************************************************************************/
{G} :- G,!.

wait_for_input :- get0(_).
%wait_for_input :- system([clrscrn,more]).

number_vars(Term,N0,N1) :- 
	var(Term), !, 
	name(N0,Digits), name('V',[C]), 
	name(Term,[C|Digits]),
	N1 is N0+1.
number_vars(Term,N0,N0) :- 
	atomic(Term), !.

number_vars(Term,N0,N1) :- 
	Term =.. [_|Args], 
	number_list(Args,N0,N1).

number_list([],N0,N0).
number_list([H|T],N0,N2) :- number_vars(H,N0,N1), number_list(T,N1,N2).
