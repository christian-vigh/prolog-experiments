%   File   : ARC3.PL
%   Author : R.A.O'Keefe
%   Updated: 9 February 1984
%   Purpose: Implement Mackworth's AC-3 algorithm.
%   Needs  : Util:Assoc.Pl, Util:ListUt.Pl

/*  It is often stated that blind backtracking is highly inefficient, and
    it is thereby implied that Prolog must be highly inefficient.  In his
    article "Consistency in Networks of Relations" (AIJ 8 (1977) 99-118)
    Mackworth presents a series of algorithms of increasing complexity to
    "remedy the thrashing behaviour that nearly always accompanies back-
    tracking", which applies to problems involving unary and binary
    constraints for a fixed number of variables with modest discrete
    domains.  Of course it can readily be extended to problems with higher
    degree relations, which become unary or binary when enough of their
    arguments are filled in.  His algorithms do not constitute a complete
    problem-solving method, but can be used to plan a backtracking or
    other solution so that it will be more efficient.

    He considers three forms of "consistency".  I have just implemented
    the first two in this file.  The reason is that this level of planning
    can be handled using just sets of values, path consistency requires
    data structures for relations.  (I know how to manipulate such data
    structures, but I'd like to keep this simple.)

    For an explanation of why the algorithms work, read Mackworth's paper.

    We are given
	a set of Nodes
	a set of Arcs, represented as (From->To) pairs
	a fixed "node admissibility" relation
		admissible_node(Node, Value)
	a fixed "arc admissibility" relation
		admissible_arc(FromNode, ToNode, FromValue, ToValue)
    We compute
	a set of (Node=PossibleValues) associations
	which is node consistent and arc consistent, but may well not
	be path consistent.
*/

:- public
	arc_consistency_3/3.

:- mode
	arc_consistency_3(+, +, -),
	make_nodes(+, -, -),
	make_graph(+, +, -, -),
	revise_each_arc(+, +, +, -),
	node_consistent_bindings(+, -),
	normalise_arcs(+, -),
	group_arcs_with_same_to_node(+, +, -),
	group_arcs_with_same_to_node(+, +, -, -),
	revise_arc(+, +, +, +, -),
	queue_arcs(+, +, +, -).


arc_consistency_3(Nodes, Arcs, ArcConsistentBindings) :-
	make_nodes(Nodes, NodeSet, InitialBindings),
	make_graph(NodeSet, Arcs, ArcSet, Graph),
	revise_each_arc(ArcSet, Graph, InitialBindings, FinalBindings),
	assoc_to_list(FinalBindings, ArcConsistentBindings).


/*  make_nodes(NodeList, NodeSet, Bindings)
    is given a representation of the set of nodes as an unordered list
    possibly with duplicates and returns a representation as an ordered
    list without duplicates (make_graph will need this).  It also returns
    an initial set of node-consistent bindings for the nodes.  Now we will
    want to fetch and update random elements of this map, and the simplest
    thing to do is to use the existing ASSOC.PL utilities.  The fact that
    setof fails if the set would be empty is *exactly* what we want here.
*/

make_nodes(NodeList, NodeSet, Bindings) :-
	sort(NodeList, NodeSet),
	node_consistent_bindings(NodeSet, NodeValList),
	list_to_assoc(NodeValList, Bindings).


node_consistent_bindings([], []).
node_consistent_bindings([Node|Nodes], [Node-Possible|Bindings]) :-
	setof(Value, admissible_node(Node, Value), Possible), !,
	node_consistent_bindings(Nodes, Bindings).


/*  We shall want to look up all the arcs leading TO a given node.
    We would like that to be fast.  We would also like to eliminate
    self-loops (X->X).  I think it is safe to assume that the arc
    list does not mention any nodes not in the node list, but we
    may have nodes that no arc leads to.  So what we are going to
    build as a representation of the graph is a binary tree mapping
    nodes to the list of arcs leading to that node.  In other
    contexts we would make that the list of node with arcs leading
    to the node, but here we want the arcs so we can push them back
    onto the stack.  We also want a list of arcs.  Just in case an
    arc appears more than once in the list, we use sort rather than
    keysort.  The code for building the list into a tree is taken
    from ASSOC.PL, avoiding the extra keysort.
*/

make_graph(NodeSet, ArcList, ArcSet, GraphTree) :-
	normalise_arcs(ArcList, PairList),
	sort(PairList, ArcSet),
	group_arcs_with_same_to_node(NodeSet, ArcSet, FinalPairs),
	length(FinalPairs, N),
	list_to_assoc(N, FinalPairs, GraphTree, []).


/*  normalise_arcs maps a list of (From->To) pairs to a list of (To-From)
    pairs, omitting any (X->X) pairs it may find.
*/

normalise_arcs([], []) :- !.
normalise_arcs([(X->X)|ArcList], PairList) :- !,
	normalise_arcs(ArcList, PairList).
normalise_arcs([(From->To)|ArcList], [To-From|PairList]) :-
	normalise_arcs(ArcList, PairList).


/*  group_arcs_with_same_to_node(NodeSet, ArcSet, NodeToArcMap)
    takes a list of Nodes, and for each node puts a (Node-Arcs) pair
    in the NodeToArcMap, where Arcs is the subset of the ArcSet that
    has Node as the To-node.  It exploits the fact that the NodeSet
    and ArcSet are both sorted, and the NodeToArcMap will also be
    sorted on the Node key, ready for building into a tree.
*/

group_arcs_with_same_to_node([], [], []).
group_arcs_with_same_to_node([Node|Nodes], ArcSet, [Node-Arcs|NodeToArcMap]) :-
	group_arcs_with_same_to_node(ArcSet, Node, Arcs, RestArcSet),
	group_arcs_with_same_to_node(Nodes, RestArcSet, NodeToArcMap).

group_arcs_with_same_to_node([Node-To|ArcSet], Node, [Node-To|Arcs], Rest) :- !,
	group_arcs_with_same_to_node(ArcSet, Node, Arcs, Rest).
group_arcs_with_same_to_node(Rest, _, [], Rest).


/*  revise_each_binding implements the heart of Mackworth's AC-3:
	Q <- {(i,j) | (i,j) in arcs(G), i =/= j}
	while Q not empty do begin
	    select and delete any arc (k,m) from Q;
	    if REVISE((k,m)) then Q <- Q U {(i,k) | (i,k) in arcs(G),i/=k,i/=m}
	end;
    the Bindings variables play the role of his D-subscript-i, and the ArcSet
    variables play the role of Q.  We exploit Prolog's success-failure: if
    revise_arc fails we just pop the arc from Q, if it succeeds it returns
    the new binding for node k.  Note that arc (i,j) in Mackworth's notation
    corresponds to J-I in our notation.
*/
revise_each_arc([], _, Bindings, Bindings) :- !.
revise_each_arc([M-K|Arcs], Graph, OldBindings, NewBindings) :-
	get_assoc(M, OldBindings, OldM),
	get_assoc(K, OldBindings, OldK),
	revise_arc(OldK, K, OldM, M, NewK),
	NewK \== OldK,
	!,		%  There was at least one deletion
	put_assoc(K, OldBindings, NewK, MidBindings),
	get_assoc(K, Graph, ArcsToK),
	queue_arcs(ArcsToK, M, Arcs, MidArcs),
	revise_each_arc(MidArcs, Graph, MidBindings, NewBindings).
revise_each_arc([_|Arcs], Graph, OldBindings, NewBindings) :-
	revise_each_arc(Arcs, Graph, OldBindings, NewBindings).


/*  revise_arc(OldK, K, OldM, M, NewK)
    checks each value in OldK to see whether there is at least one value
    in OldM which admissible_arc will accept.  If there is, it includes
    that value from OldK in NewK, otherwise it skips it.  So NewK is the
    subset of bindings for K which is compatible with the current bindings
    for M.
*/

revise_arc([], _, _, _, []).
revise_arc([Kval|OldK], K, OldM, M, [Kval|NewK]) :-
	member(Mval, OldM),
	admissible_arc(K, M, Kval, Mval),
	!,	% at least one combination works
	revise_arc(OldK, K, OldM, M, NewK).
revise_arc([_|OldK], K, OldM, M, NewK) :-
	revise_arc(OldK, K, OldM, M, NewK).	% nothing worked


/*  queue_arcs(Arcs, Exclude, OldQueue, NewQueue)
    adds each (To-From) arc from Arcs whose From is not Exclude to OldQueue,
    forming at last a NewQueue.  On reflection, it wasn't necessary to store
    complete arcs in the Graph after all, and I should go back and change it.
    However, storing complete arcs wins in a structure copying system.
*/

queue_arcs([], _, Queue, Queue).
queue_arcs([_-Exclude|Arcs], Exclude, OldQueue, NewQueue) :- !,
	queue_arcs(Arcs, Exclude, OldQueue, NewQueue).
queue_arcs([Arc|Arcs], Exclude, OldQueue, NewQueue) :-
	queue_arcs(Arcs, Exclude, [Arc|OldQueue], NewQueue).


