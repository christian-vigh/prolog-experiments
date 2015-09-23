/*----------------------------------------------------------------------
     
                         -- C N R S -- 
         Laboratoire d'automatique et d'analyse des systemes 
            Groupe Robotique et Intelligence Artificielle  
                   7 Avenue du colonel Roche   
                     31 077 Toulouse Cedex  
     
  Fichier              : cycles
  Fonction             : Determination of simple elementary cycles in a
              strongly connected component of a digraph.

This version based on the algorithm presented in [Read and Tarjan 75],
except that we are processing the strongly connected components of the 
digraph, instead of the digraph itself.

  elementary cycle:     no arc traversed more than once
  simple cycle:     no node visited more than once

  Each node in the scc has the following arguments:
  1. identifier
  2. depth first index
  3. level in the spanning tree

  FILES:           : "IN" is input filename, 
             "OUT" = "IN_cy" i.e. the set of cycles

  Auteur               : Paul Freedman

A note about code efficiency:

All instances of unification with ground atoms which are declared dynamic 
'node/3', 'arc/3', 'scc/2', 'successors/2', etc.
are NOT subject to indexing in SICStus 0.6.
This means that unification with such a ground atom 'foo/1' consumes space
on the control stack with each call to 'foo(X)' to instantiate 'X'.
The solution proposed by Mats Carlsson of SICStus: 
    'foo(X)' to replaced by '( foo(X) -> true )' 
WHEN the enclosing predicate is deterministic.
After such changes, 'find_cycles/4'  is completely deterministic,
as indicate by the control stack before and after, in the
predicate 'process_back_arcs/0'.

------------------------------------------------------------------------*/

:- compile(utilities).

/* Here are the nuts and bolts of the [Read and Tarjan 75] algorithm.   */
/* Each back arc (X,Y) is treated separately.               */
/* The key idea is this: each cycle to be found will traverse (X,Y) */
/*   as the LAST arc.                           */
/* Therefore, nodes which are HIGHER in the scc than 'Y' are ignored.   */
/* The algorithm uses DFS with backtracking to explore all      */
/* outgoing arcs (tree, cross, forward, and back) to find cycles.   */

/* Throw away successor node if it is too shallow in the tree       */
/*  OR if it has already been visited.                  */

not_too_shallow([[Lev,N]|T],Index,Final):-
    Lev < Index,
    !, not_too_shallow(T,Index,Final).
not_too_shallow(Final,_,Final).


not_yet_visited([],_,Final,Final).
not_yet_visited([[_,N]|T],Visited,Current,Final):-
    member_ordered_set(N,Visited), 
    !, not_yet_visited(T,Visited,Current,Final).
not_yet_visited([[_,N]|T],Visited,Current,Final):-
    not_yet_visited(T,Visited,[N|Current],Final).


/* Important hack here: the successors of a node are retrieved      */
/*  indirectly via 'findall', so that no new choice point is        */
/*  introduced which would otherwise restrict garbage collection.   */

suitable_successors(N,Index,Visited,Successors,Num):-
    ( successors(N,L1) -> true ),
    not_too_shallow(L1,Index,L2),
    not_yet_visited(L2,Visited,[],Successors),
    length(Successors,Num).


/*----------------------------------------------------------------------*/

/* The predicates 'process_successors' and 'back_up' work together to   */
/*  expand and contract the partial paths which might become cycles.    */
/* They pass back and forth a list of nodes already visited.        */
/* Since between two nodes there can be at most one arc, no need to */
/*  keep track of arcs already traversed in order to ensure that the    */
/*  the cycles found are always simple and elementary.      */

/* 'V1' = list of nodes visited BEFORE contracting the path */
/* 'V2' = list of nodes visited AFTER contracting the path  */


contract_path([Y],_,[Y],[]):- !.     /* head of back arc, so reset 'V2' */
contract_path([[Z],N|T],V1,[Z|[N|T]],V2):-  /* choice point = 1 */
    !, insert_ordered_set(Z,V1,V2).
contract_path([[Z|T1],N|T2],V1,[Z|Temp],V2):-          /* choice = many */
    !, append([T1],[N|T2],Temp),
    insert_ordered_set(Z,V1,V2).
contract_path([Z,N|T],V1,Path,V2):-     /* no choice point  */
    atomic(N),
    !, delete(Z,V1,New),
    contract_path([N|T],New,Path,V2).
contract_path([Z,L,N|T],V1,Path,V2):-     /*  list 'L' of choice points */
    delete(Z,V1,New),
    contract_path([L,N|T],New,Path,V2).



expand_path(0,[],P,V1,Path,V2):-               /* no successors */
    !, contract_path(P,V1,Path,V2).
expand_path(1,[Z],[N|T],V1,[Z|[N|T]],V2):-          /* just 1 successor */
    !, insert_ordered_set(Z,V1,V2).
expand_path(_,[Z|T1],[N|T2],V1,[Z|Temp],V2):-        /* many successors */
    append([T1],[N|T2],Temp),
    insert_ordered_set(Z,V1,V2).


/*----------------------------------------------------------------------*/

extract_cycle1(Cycle):-
    phigs(true),
    !, stream_for_c(W_stream),
    send_list_to_c([Cycle],W_stream).
extract_cycle1(_).


extract_cycle([],Cycle):-
    !, write('cycle('), writeq(Cycle), write(').'), nl,
    extract_cycle1(Cycle).
extract_cycle([[X|T1]|T2],Cycle):-           /* skip past choice points */
    !, extract_cycle(T2,Cycle).
extract_cycle([X|T],Cycle):- 
    extract_cycle(T,[X|Cycle]).


/* 1st argument = partial path, head is the current frontier node   */
/* 2nd argument = target node, i.e. tail 'X' of back arc(X,Y)       */
/* 3rd argument = depth first index of node 'Y'             */
/* 4th argument = list of nodes visited thus far            */

find_cycles([Y],_,_,[]):- !.
find_cycles([X|T],X,I_Y,Visited):-
    !, extract_cycle([X|T],[]),
    contract_path([X|T],Visited,Path,New_visited),
    find_cycles(Path,X,I_Y,New_visited).
find_cycles([N|T],X,I_Y,Visited):- 
    suitable_successors(N,I_Y,Visited,L,Num),
    expand_path(Num,L,[N|T],Visited,Path,New_visited),
    find_cycles(Path,X,I_Y,New_visited).


/*----------------------------------------------------------------------*/

process_back_arcs:-
    retract(arc(X,Y,back)),
    display('processing a new back'), nl(user_output),
    ( node(Y,I_Y,_) -> true ),
    find_cycles([Y],X,I_Y,[Y]),
    process_back_arcs.
process_back_arcs.


/* To make the cycle finding more efficient, store the successors   */
/*  of a node in an ordered set, ranked by level in the spanning tree.  */

rank_nodes([],Current,Final):-
    !, sort(Current,Final).
rank_nodes([N|T],Current,Final):-
    ( node(N,_,Lev) -> true ),
    rank_nodes(T,[[Lev,N]|Current],Final).


assert_successors([]):- !.
assert_successors([node(X,I,Lev)|T]):-
    findall(Y,arc(X,Y,_),L1),
    rank_nodes(L1,[],L2),
    assert(successors(X,L2)),
    assert_successors(T).
    

/* For each strongly connected component in the spanning forest, find   */
/*   all the back arcs and use their heads as start nodes for cycles.   */ 

process_forest:-
    retract(scc([[],_])),       /* no arcs -> scc just 1 node   */
    !, process_forest.          /* skip on to next scc  */
process_forest:-
    retract(scc(Arcs,Nodes)),       /* select some scc  */
    !, assert_list(Arcs),
    assert_list(Nodes),
    assert_successors(Nodes),
    process_back_arcs,
    retractall(node(_,_,_)),
    retractall(arc(_,_,_)),
    retractall(successors(_,_)),
    process_forest.
process_forest.


/*----------------------------------------------------------------------*/

start:- 
    assert(phigs(false)),
    write(' Filename base: '),
    read(USER),
    name(USER,User_ascii),  /*  ASCII list correponding to "USER"   */
    name('_scc',Scc_ascii),
    append(User_ascii,Scc_ascii,IN_ascii),
    name(IN,IN_ascii),
    consult(IN),               /* strongly connected components */
    name('_cy',Cy_ascii),
    append(User_ascii,Cy_ascii,OUT_ascii),
    name(OUT,OUT_ascii),
    tell(OUT),
    statistics(runtime,_),               /* reset runtime clock */
    process_forest, 
    retract(phigs(_)),
    statistics(runtime,[T_elapsed,_]),  /* forget fractions of msec */
    print_statistics(T_elapsed),
    told.


/*----------------------------------------------------------------------*/

start_from_c:-
    assert(phigs(true)),
    read(USER),
    name(USER,User_ascii),  /*  ASCII list correponding to "USER"   */
    name('_scc',Scc_ascii),
    append(User_ascii,Scc_ascii,IN_ascii),
    name(IN,IN_ascii),
    consult(IN),               /* strongly connected components */
    name('_cy',Cy_ascii),
    append(User_ascii,Cy_ascii,OUT_ascii),
    name(OUT,OUT_ascii),
    tell(OUT),
    statistics(runtime,_),               /* reset runtime clock */
    open_stream_for_c(W_stream),
    assert(stream_for_c(W_stream)),
    process_forest, 
    close_stream_for_c(W_stream),
    handshake,
    retract(stream_for_c(_)),
    retract(phigs(_)),
    statistics(runtime,[T_elapsed,_]),  /* forget fractions of msec */
    print_statistics(T_elapsed),
    told.
