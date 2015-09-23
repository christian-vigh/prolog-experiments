%   VOCABULARY

noun(frog).
noun(lake).
noun(water).
noun(man).
noun(woman).
noun(cat).
noun(mouse).
noun(fish).

proper(fish).	%  come now!
proper(tom).
proper(jerry).
proper(jim).
proper(fred).
proper(john).

transitive(drinks).
transitive(owns).
transitive(likes).
transitive(loves).
transitive(chased).

intransitive(swims).
intransitive(jumps).
intransitive(lives).
intransitive(squeaks).

preposition(into).
preposition(near).
preposition(in).
preposition(to).

adjective(green).
adjective(blue).
adjective(big).
adjective(small).


%   EXAMPLE	1

s --> np, vp.
s --> Alpha^[np,pP], np, vp/Alpha.
% s --> np, np, vp/np.
% s --> pP, np, vp/pP.

np --> [the], adjs, noun, rel.
np --> [W], {proper(W)}.

adjs --> [W], {adjective(W)}, adjs.
adjs --> [].

noun --> [W], {noun(W)}.

rel --> [that], s/np.
rel --> [].

vp --> [W], {transitive(W)}, np, pPs.
vp --> [W], {intransitive(W)}, pPs.

pPs --> pP, pPs.
pPs --> [].

pP --> [W], {preposition(W)}, np.

t1(N) :- t1(N,S), s(0,0,S,[]).
t1(1, [jim,owns,the,frog]).
t1(2, [the,frog,that,jim,owns,swims]).
t1(3, [jim,swims,in,the,lake,that,the,frog,likes]).
t1(4, [jim,owns,the,frog,that,likes,jim]).
t1(5, [in,the,lake,fred,swims]).
t1(6, [the,green,frog,likes,the,lake,that,fred,swims,in]).
t1(7, [into,the,blue,lake,that,fred,swims,in,the,frog,jumps]).
t1(8, [jim,owns,the,lake,that,the,frog,jumps,into]).


% EXAMPLE	2
% This is Fernando Pereira's XG example.

sentence --> noun_phrase, verb_phrase.
sentence --> Alpha^[noun_phrase,prep_phrase], noun_phrase, verb_phrase/Alpha.

noun_phrase --> [W], {proper(W)}.
noun_phrase --> determiner, noun, ( relative | prep_phrase | [] ).

determiner --> [the].
determiner --> [a].
determiner --> [an].

noun --> [W], {noun(W)}.


verb_phrase --> [W], {transitive(W)}, noun_phrase.
verb_phrase --> [W], {intransitive(W)}, (prep_phrase | []).


relative --> [that], sentence/noun_phrase.

prep_phrase --> [W], {preposition(W)}, noun_phrase.

t2(N) :- t2(N,S), dcsg_phrase(sentence, S).

t2(1, [the,mouse,squeaks]).
t2(2, [the,cat,likes,fish]).
t2(3, [the,cat,chased,the,mouse]).
t2(4, [the,mouse,that,the,cat,that,chased,likes,fish,squeaks]).
% 4: rejected
t2(5, [the,mouse,that,the,cat,that,likes,fish,chased,squeaks]).
t2(6, [to,the,fish,tom,chased,the,mouse]).
t2(7, [to,the,fish,tom,chased,the,mouse,that,squeaks]).
% 7: rejected.  (In fact ungrammatical according to this grammar.)
% if accepted would be (tom) chased (the mouse (that squeaks (to the fish)))
t2(8, [jerry,the,cat,that,likes,fish,chased]). 


%   EXAMPLE	3
%   This is the example in the Dec-10 Prolog manual.

sentence(P) -->
	noun_phrase(X, P1, P),
	verb_phrase(X, P1).
sentence(P) -->
	noun_phrase(Y, Q1, Q),
	noun_phrase(X, P1, P),
	verb_phrase(X, P1)/noun_phrase(Y, Q1, Q).

noun_phrase(X, P, P) -->
	[X], {proper(X)}, !.
noun_phrase(X, P1, P) -->
	determiner(X, P2, P1, P),
	noun(X, P3),
	rel_clause(X, P3, P2).

rel_clause(X, P1, (P1,P2)) -->
	[that], verb_phrase(X, P2).
rel_clause(X, P, P) -->
	[].

determiner(X, P1, P2, each(X,P1,P2)) --> [every].
determiner(X, P1, P2, some(X,P1,P2)) --> [a] | [some].

noun(X, P) --> [W], {noun(W)}, {P =.. [W,X]}.

verb_phrase(X, P) -->
	[W], {transitive(W)},
	noun_phrase(Y, P1, P),
	{P1 =.. [W,X,Y]}.
verb_phrase(X, P) -->
	[W], {intransitive(W)},
	{P =.. [W,X]}.

t3(N) :-
	t3(N,S),
	dcsg_phrase(sentence(P), S),
	numbervars(P, 0, _), 
	write(P).

t3(1, [every,man,that,lives,loves,a,woman]).
t3(2, [some,woman,some,man,that,lives,loves]).
t3(3, [a,woman,lives,that,loves,jim]).
% 3 is not grammatical according to the grammar above
t3(4, [a,woman,that,loves,jim,likes,every,frog,that,swims]).
