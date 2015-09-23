%This file implements a simplified version of constructive negation.
%Meta-terms and delayed goals are dealt with correctly
%The fundamental rules used are that not(A and B) = not(A) or (A and not(B))
%                                    not(A or B)  = not(A) and not(B)
:- import(sepia_kernel).

neg(Goal):-
	bagof((Goal:-D),subcall(Goal,D),L),
	neg_test(Goal,L).

%The result is obtained by conjunction of negation of alternate solutions
neg_test(_Goal,[]):- !. %No solutions succeed
neg_test(Goal,[(H:-B)|L]):- neg_t(Goal,(H:-B)), neg_test(Goal,L).

%Either head doesnt bind or it does and body fails
neg_t(Goal,(H:-_)):- neg_eq(Goal,H).
neg_t(G,(G:-Body)):- neg_body(Body).

%Simplified version of neg_body
%If a single goal that can be transformed then do so else delay
neg_body([]):- !, fail.
neg_body([A]):- neg_goal(A), !.
neg_body(L):- L\=[_], not_delay_list(L).

delay not_delay_list(L) if initial(L).
not_delay_list(L):- neg(call_list(L)).

call_list([]).
call_list([H|L]):- call(H), call_list(L).

%Two terms (possibly containing variables and meta terms) are not equal.
neg_eq(A,B):- A==B, !, fail.
neg_eq(A,B):- (meta(A);meta(B)), !, neg_meta(A,B).
neg_eq(A,B):- nonvar(A), nonvar(B), !, neg_term(A,B).
neg_eq(A,B):- !, neq_delay(A,B).

delay neq_delay(A,B) if initial([A,B]).
neq_delay(A,B):- neg_eq(A,B).

%Both parameters are instantiated
%If disagree in primary functor then succeed
%Otherwise agree in primary functor then traverse from left to right
%and either find sub-term that is not equal or set it equal and look
%for one later in structure
%[|] is treated as special because it is result of =..
neg_term(A,B):- atomic(A), atomic(B), !, A\=B.
neg_term(A,B):- atomic(A), !. %not(atomic(B))
neg_term(A,B):- atomic(B), !. %not(atomic(A))
neg_term([A|LA],[B|LB]):-  !, neg_term2(A,B,LA,LB).
neg_term(A,B):- A=..[FA|PA], B=..[FB|PB], 
	(FA=FB->neg_term(PA,PB);true).

%This will generate real choice points
neg_term2(A,B,_LA,_LB):- neg_eq(A,B).
neg_term2(A,B,LA,LB):-   A=B, neg_eq(LA,LB).

%neg_goal deals with goals that have been delayed - the default is to
%just delay the term on the same basis as the original
%the following however transforms terms that can be effectively rewritten
delay neg_goal(A) if var(A).
neg_goal(A):- neg_sepia(A,B), !, call(B).
neg_goal(A):- neg_deter(A,V,An,Vn), !, call(An), neg_eq(V,Vn).
neg_goal(A):- neg_user(A,B), !, call(B).
neg_goal(A):- not_delay(A).

%If this knew the details of the delay condition for A then it could
%be smarter and more efficient
delay not_delay(A) if initial(A).
not_delay(A):- neg(A).

%Builtins that can delay and which can profitably be rewritten appear here
neg_sepia(not_delay(A),A).
neg_sepia(neq_delay(A,B),A=B).

%Deterministic builtins that delay
%Specify the output variable
%What about multiple instances of variables
neg_deter(+(A,B,C),C,+(A,B,Cn),Cn).
neg_deter(*(A,B,C),C,*(A,B,Cn),Cn).
neg_deter(-(A,B),B,-(A,Bn),Bn).
%... and many more
