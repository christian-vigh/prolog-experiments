%--------------------------------------------------------------%
%   Question-answering by clausal resolution                   %
%   (C) 1998 Zdravko Markov                                    %
%--------------------------------------------------------------%
% To start the program type "qa." 
% Every input must end with '.' (dot).
% See the grammar for the vocabulary.

:- op(850,fx,~).  
:- op(900,xfy,#).
:- op(900,xfy,&).
:- op(950,xfy,->).
:- dynamic cl/2.

qa :-
    write('Type help for explanations'), nl,
    repeat,
    read_sentence(S),
    process_sentence(S).

read_sentence(L) :- read(S), c2l(S,L).

process_sentence([end]) :- !,
    write('Good Bye!').
process_sentence([help]) :- !,
    write('The system accepts (words separated by , and ended with .):'),nl,
    write('   Statement - e.g. |: jonh,loves,mary.'),nl,
    write('   Question - e.g. |: who,loves,mary.'),nl,
    write('   showgrammar - print the grammar'),nl,
    write('   help - prints this message'),nl,
    write('   end - ends the program'),nl,
    write('   facts - prints all facts as sentences'),nl,
    write('   rules - prints all rules in clausal form'),nl,fail.
process_sentence([showgrammar]) :- !,
    showgrammar,nl,fail.
process_sentence([facts]) :- !,
    cl(P,[]),
    write_clause(cl(P,[])), fail.
process_sentence([rules]) :- !,
    cl(P,N), N \== [],
    write_clause(cl(P,N)), fail.
process_sentence([who|VP]) :-
    verb_phrase(VP,[],_,F), !,
    cl([F],[]),
    write_clause(cl([F],[])), fail.
process_sentence(S) :-
    sentence(S,[],F), !,
    translate(F,Clauses),
    add_to_database(Clauses),
    resolve, fail.
process_sentence(_) :-
    write('Unrecognized sentence !'), nl, fail.

write_clause(cl(F,[])) :- 
    l2d(F,D),
    sentence(L,[],D),
    c2l(S,L),
    write(S), nl, !.
write_clause(cl(P,N)) :-
    l2d(P,Head), 
    c2l(Body,N),
    write((Head:-Body)), nl.

add_to_database([]).
add_to_database([C|L]) :-
    (call(C),write('Already known: '),write_clause(C);
     write('Adding to database: '),write_clause(C),assertz(C)
    ), !,
    add_to_database(L).

c2l((P,Q),[P|L]) :- L\==[], !, c2l(Q,L).
c2l(P,[P]).
c2l(true,[]).

l2d([P],P).
l2d([P|L],(P#Q)) :- l2d(L,Q).

proper_noun([Sk|A],A,Sk). % Allow any symbol as proper noun


%*---------------------------------------------------%
%  Grammar                                           %
%*---------------------------------------------------%

showgrammar :-
    writeln('sentence --> noun_phrase verb_phrase'),
    nl,
    writeln('noun_phrase --> determiner noun rel_clause'),
    writeln('noun_phrase --> proper_noun'),
    nl,
    writeln('verb_phrase --> trans_verb noun_phrase'),
    writeln('verb_phrase --> intrans_verb'),
    nl,
    writeln('rel_clause --> that verb_phrase'),
    writeln('rel_clause --> or verb_phrase'),
    writeln('rel_clause -->'),
    nl,
    writeln('determiner --> every'),
    writeln('determiner --> a'),
    nl,
    writeln('noun --> man'),
    writeln('noun --> woman'),
    writeln('noun --> student'),
    nl,
    writeln('proper_noun --> john'),
    writeln('proper_noun --> mary'),
    nl,
    writeln('trans_verb --> loves'),
    writeln('trans_verb --> is'),
    nl,
    writeln('intrans_verb --> lives').

sentence(A,C,P) :-
    noun_phrase(A,B,X,P1,P),
    verb_phrase(B,C,X,P1).

noun_phrase(A,B,X,P,P) :-
    proper_noun(A,B,X).
noun_phrase(A,D,X,P1,P) :-
    determiner(A,B,X,P2,P1,P),
    noun(B,C,X,P3),
    rel_clause(C,D,X,P3,P2).

verb_phrase(A,C,X,P) :-
    trans_verb(A,B,X,Y,P1), !,
    noun_phrase(B,C,Y,P1,P).
verb_phrase(A,B,X,P) :-
    intrans_verb(A,B,X,P).

rel_clause(A,A,_,P,P).
rel_clause([that|A],B,X,P1,(P1&P2)) :-
    verb_phrase(A,B,X,P2).
rel_clause([or|A],B,X,P1,(P1#P2)) :-
    verb_phrase(A,B,X,P2).

determiner([every|A],A,X,P1,P2,all(X,(P1->P2))).
determiner([a|A],A,X,P1,is_a(X,X),P1).
determiner([a|A],A,X,P1,P2,exists(X,(P1&P2))).

noun([man|A],A,X,man(X)).
noun([woman|A],A,X,woman(X)).
noun([student|A],A,X,student(X)).

proper_noun([john|A],A,john).
proper_noun([mary|A],A,mary).

trans_verb([loves|A],A,X,Y,loves(X,Y)).
trans_verb([is|A],A,X,Y,is_a(X,Y)).

intrans_verb([lives|A],A,X,lives(X)).

%---------------------------------------------------%
%   Translate FOL formulae into clausal form        %
%---------------------------------------------------%
translate(FOL,Clauses) :-
    implout(FOL,X2),
    negin(X2,X3),
    skolem(X3,X4,[]),
    univout(X4,X5),
    conjn(X5,X6),
    clausify(X6,Clauses,[]).

implout(P->Q,~P1#Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(all(X,P),all(X,P1)) :-
    !, implout(P,P1).
implout(exists(X,P),exists(X,P1)) :-
    !, implout(P,P1).
implout(P&Q,P1&Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(P#Q,P1#Q1) :-
    !, implout(P,P1),
    implout(Q,Q1).
implout(~P,~P1) :-
    !, implout(P,P1).
implout(P,P).

negin(~P,P1) :-
    !, neg(P,P1).
negin(all(X,P),all(X,P1)) :-
    !, negin(P,P1).
negin(exists(X,P),exists(X,P1)) :-
    !, negin(P,P1).
negin(P&Q,P1&Q1) :-
    !, negin(P,P1),
    negin(Q,Q1).
negin(P#Q,P1#Q1) :-
    !, negin(P,P1),
    negin(Q,Q1).
negin(P,P).

neg(~P,P1) :-
    !, negin(P,P1).
neg(all(X,P),exists(X,P1)) :-
    !, neg(P,P1).
neg(exists(X,P),all(X,P1)) :-
    !, neg(P,P1).
neg(P&Q,P1#Q1) :-
    !, neg(P,P1),
    neg(Q,Q1).
neg(P#Q,P1&Q1) :-
    !, neg(P,P1),
    neg(Q,Q1).
neg(P,~P).

skolem(all(X,P),all(X,P1),Vars) :-
    !, skolem(P,P1,[X|Vars]).
skolem(exists(X,P),P2,Vars) :-
    !, gensym(s,S),
    Sk =.. [S|Vars],
    subst(X,Sk,P,P1),
    skolem(P1,P2,Vars).
skolem(P&Q,P1&Q1,Vars) :-
    !, skolem(P,P1,Vars),
    skolem(Q,Q1,Vars).
skolem(P#Q,P1#Q1,Vars) :-
    !, skolem(P,P1,Vars),
    skolem(Q,Q1,Vars).
skolem(P,P,_).

gensym(Char,Atom) :-
    getnum(Char,N),
    numlist(N,L),
    name(Char,[M]),
    name(Atom,[M|L]).

getnum(C,M) :-
    retract(cn(C,N)),
    M is N+1,
    assertz(cn(C,M)),!.
getnum(C,1) :-
    assertz(cn(C,1)).

numlist(N,[M]) :-
    N<10, M is 48+N, !.
numlist(N,[M|T]) :-
    M is N mod 10 + 48,
    N1 is N//10,
    numlist(N1,T).

subst(X,Y,P,Q) :-
    nonvar(P),
    P =.. [F|L1],
    subst1(X,Y,L1,L2),
    Q =.. [F|L2], !.
subst(X,Y,Z,Y) :-
    X == Z, !.
subst(_,_,X,X).

subst1(_,_,[],[]) :- !.
subst1(X,Y,[A|T1],[B|T2]) :-
    subst(X,Y,A,B),
    subst1(X,Y,T1,T2).

univout(all(_,P),P1) :- !,
    univout(P,P1).
univout(P&Q,P1&Q1) :- !,
    univout(P,P1),
    univout(Q,Q1).
univout(P#Q,P1#Q1) :- !,
    univout(P,P1),
    univout(Q,Q1).
univout(P,P).

conjn(P#Q,R) :- !,
    conjn(P,P1),
    conjn(Q,Q1),
    conjn1(P1#Q1,R).
conjn(P&Q,P1&Q1) :- !,
    conjn(P,P1),
    conjn(Q,Q1).
conjn(P,P).

conjn1((P&Q)#R,P1&Q1) :- !,
    conjn(P#R,P1),
    conjn(Q#R,Q1).
conjn1(P#(Q&R),P1&Q1) :- !,
    conjn(P#Q,P1),
    conjn(P#R,Q1).
conjn1(P,P).

clausify(P&Q,C1,C2) :- !,
    clausify(P,C1,C3),
    clausify(Q,C3,C2).
clausify(P,[cl(A,B)|Cs],Cs) :-
    inclause(P,A,[],B,[]), !.
clausify(_,C,C).

inclause(P#Q,A,A1,B,B1) :- !,
    inclause(P,A2,A1,B2,B1),
    inclause(Q,A,A2,B,B2).
inclause(~P,A,A,B1,B) :- !,
    notin(P,A),
    putin(P,B,B1).
inclause(P,A1,A,B,B) :-
    notin(P,B),
    putin(P,A,A1).

notin(X,[X|_]) :-
    !, fail.
notin(X,[_|L]) :-
    !, notin(X,L).
notin(_,[]).

putin(X,[],[X]) :- !.
putin(X,[X|L],[X|L]) :- !.
putin(X,[Y|L],[Y|L1]) :-
    putin(X,L,L1).

%---------------------------------------------------%
%  Clausal resolution                               %
%---------------------------------------------------%
resolve :-
   resolvent(C),
   write('Derived: '),write_clause(C),
   resolve.
resolve.

resolvent(cl(P,N)) :-
   new_resolvent(P,N),
   \+ cl(P,N),   
   assertz(cl(P,N)). 

new_resolvent(P,N) :-
   cl(P1,N1),cl(P2,N2),
   del(L,P1,P3),
   del(L,N2,N3),
   union(P2,P3,P),
   union(N1,N3,N),
   \+ ((member(X,P),
        member(X,N))).

union([],L,L) :- !.
union([H|T],L,[H|V]) :-
   \+ member(H,L), !,
   union(T,L,V).
union([_|T],L,V) :-
   union(T,L,V).

del(X,[X|T],T).
del(X,[Y|T],[Y|V]) :-
   del(X,T,V).
