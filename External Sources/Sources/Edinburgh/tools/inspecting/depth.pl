%   File   : DEPTH.PL
%   Author : R.A.O'Keefe
%   Updated: 12 March 1984, 23 June 1987
%   Purpose: Find or check the depth of a term.

/*  Many resolution-based theorem provers impose a Depth Bound on the
    terms they create.  Not the least of the reasons for this is to
    stop infinite loops.  This module provides two entry points:

	depth_of_term(Term, Depth)
	depth_bound(Term, Bound)

    depth_of_term calculates the depth of the term, using the definition
	depth(Var) = 0
	dpeth(Const) = 0
	depth(F(T1,...,Tn)) = 1+max(depth(T1),...,depth(Tn))

    Mostly, we couldn't care less what the depth of a term is, provided
    it is below some fixed bound.  depth_bound checks that the depth of
    the given term is below the bound (which is assumed to be an integer
    >= 1), without ever finding out what the depth actually is.
*/

% Example, given the tree
% 
% 		m
% 	       / \
% 	      /   \
% 	    f	    q
% 	   / \     / \
% 	  a   g   /   \
% 		 o      u
% 		/ \    / \
% 		n  p  s  w
% represented by
% tree(
% 	tree(m,
% 		tree(f,tree(a,[],[]),tree(g,[],[])),
% 		tree(q,
% 			tree(o,tree(n,[],[]),tree(p,[],[])),
% 			tree(u,tree(s,[],[]),tree(w,[],[]))
% 		    )
% 	    )
%     ).
% 
% test(D) :-
% 	tree(T),
% 	depth_of_term(T,D).
% 
% % ?- test(D) succeeds with D = 4
% 
% ?- tree(T), depth_bound(T,3)	Fails
% ?- tree(T), depth_bound(T,4)	Succeeds

depth_bound(Compound, Bound) :-
	nonvar(Compound),
	functor(Compound, _, Arity),
	Arity > 0,
	!,
	Bound > 0,		% this is the test!
	Limit is Bound-1,
	depth_bound(Arity, Compound, Limit).

depth_bound(_, _).


depth_bound(0, _, _) :-
	!.

depth_bound(N, Compound, Limit) :-
	arg(N, Compound, Arg),
	depth_bound(Arg, Limit),
	M is N-1,
	!,
	depth_bound(M, Compound, Limit).

depth_of_term(Compound, Depth) :-
	nonvar(Compound),
	functor(Compound, _, Arity),
	Arity > 0,
	!,
	depth_of_term(Arity, Compound, 0, ArgDepth),
	Depth is ArgDepth+1.
depth_of_term(_, 0).

depth_of_term(0, _, Depth, Depth) :- !.

depth_of_term(N, Compound, SoFar, Depth) :-
	arg(N, Compound, Arg),
	depth_of_term(Arg, ArgDepth),
	ArgDepth > SoFar,
	M is N-1,
	!,
	depth_of_term(M, Compound, ArgDepth, Depth).

depth_of_term(N, Compound, SoFar, Depth) :-
	M is N-1,
	depth_of_term(M, Compound, SoFar, Depth).

