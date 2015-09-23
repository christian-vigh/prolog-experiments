/*----------------------------------------------------------------------
     
                         -- C N R S -- 
         Laboratoire d'automatique et d'analyse des systemes 
            Groupe Robotique et Intelligence Artificielle  
                   7 Avenue du colonel Roche   
                     31 077 Toulouse Cedex  
     
  Fichier              : dfs_scc

 Depth first search to obtain the spanning forest of a directed graph.
 Along the way, decompose each spanning tree in the forest into its 
   constituent strongly connected components 
   (since two nodes can only belong to the same cycle if they 
    belong to the same scc).
 Based on the classic algorithm due to [Tarjan 72], but encoding
 more closely follows description in [Aho, Hopcroft, Ullman 74].

 Each 'node' carries the following arguments:               
   1. identifier (a natural number, starting with 1),       
   2. depth first 'index' (a natural number, starting with 1),
   3. 'level' in the spanning tree
   4. 'lowlink' value (for finding the strongly connected components)   
   5. list of ancestors (for identifying back arcs, cross arcs, etc.)   

 At the start, the arcs are simply labelled '(3,4)'.        
 All the arcs which belong to a given scc are then systematically
   classified into tree arcs, back arcs,  and forward arcs
   (for further processing by the next program "cycles").
  

  FILES:           : "IN" is input filename, 
             "OUT" = "IN_scc", i.e. list of scc's,
                  plus the nodes + their labels

  Auteur               : Paul Freedman


------------------------------------------------------------------------*/

:- compile(utilities).

/* Format node information for passing to the cycle finding program.    */
/*   -> throw away list of ancestors                    */
/* save 'level' for the graphical display (SUNphigs)            */

format_nodes([],Current,Final):-
    !, reverse_list(Current,Final).
format_nodes([H|T],Current,Final):- 
    retract(node(H,I,Lev,Low,_)),
    format_nodes(T,[node(H,I,Lev)|Current],Final).


classify(X,Y,back):- 
    ( node(X,_,_,_,Anc) -> true ),
    ( node(Y,_,_,_,_) -> true ),
    member(Y,Anc).          /* 'Y' is an ancestor of 'X'    */
classify(X,Y,forward):- 
    ( node(X,_,_,_,_) -> true ),
    ( node(Y,_,_,_,Anc) -> true ),
    member(X,Anc).          /* 'X' is an ancestor of 'Y'    */
classify(X,Y,cross).


format_arcs([],_,Final,Final):- !.
format_arcs([X|T],Scc,Current,Final):-
    retract(tree_arc(X,Y)),
    member(Y,Scc),              /* head of arc in this scc */
    !, format_arcs([X|T],Scc,[arc(X,Y,tree)|Current],Final). 
format_arcs([X|T],Scc,Current,Final):-
    retract(arc(X,Y)),  
    member(Y,Scc),
    !, classify(X,Y,Class),         /* back, cross, or forward arc */
    format_arcs([X|T],Scc,[arc(X,Y,Class)|Current],Final). 
format_arcs([X|T],Scc,Current,Final):-
    format_arcs(T,Scc,Current,Final).


/*-----------------------------------------------------------------------*/

/* Now build scc backwards from the nodes on the stack 'visited'.   */
/*  -> root of the scc becomes first node in the scc list.      */

partition(N,[N|T],Current,[N|Current],T):- !.
partition(N,[H|T],Current,Final,Others):-
    partition(N,T,[H|Current],Final,Others).

/*-----------------------------------------------------------------------*/

form_scc1(Nodes,Arcs):-
    phigs(true),
    !, stream_for_c(W_stream),
    send_list_to_c(Nodes,W_stream),
    send_list_to_c(Arcs,W_stream).
form_scc1(_,_).


form_scc(N):-
    retract(visited(Nodes)),
    !, partition(N,Nodes,[],Scc,Others),
    assert(visited(Others)),
    format_arcs(Scc,Scc,[],L1),
    format_nodes(Scc,[],L2),
    write('scc(['), 
    writeq_utility(L1), write('],'), nl,
    write('['), writeq_utility(L2),
    write(']).'), nl, nl,
    form_scc1(L2,L1).


/*-----------------------------------------------------------------------*/

reset_lowlink(N,H,lowlink):-
    ( node(N,_,_,N_Low,_) -> true ),
    ( node(H,_,_,H_Low,_) -> true ),
    N_Low > H_Low,
    !, retract(node(N,I,Lev,_,Anc)),
    assert(node(N,I,Lev,H_Low,Anc)).
reset_lowlink(N,H,index):-
    ( node(N,_,_,N_Low,_) -> true ),
    ( node(H,H_I,_,_,_) -> true ),
    N_Low > H_I,
    !, retract(node(N,I,Lev,_,Anc)),
    assert(node(N,I,Lev,H_I,Anc)).
reset_lowlink(_,_,_).


/* The DFS determination of the spanning forest of the digraph.     */
/* 1st argument = frontier node,                    */
/* 2nd argument = its successors                    */
/* 3rd argument = current level in the spanning tree            */

/* 1st clause: no more successors to visit, and 'lowlink' = 'index' */
/*   -> found root of a new SCC !                   */

dfs(N,[],_):-
    ( node(N,Val,_,Val,_) -> true ),
    !, form_scc(N).
dfs(_,[],_):- !.           /* keep backing up the spanning tree */
dfs(N,[H|T],Lev):-
    retract(node(H,0)),    /*  successor  'H' not yet visited   */
    retract(arc(N,H)),
    !, assert(tree_arc(N,H)),  /* -> new arc in this spanning tree  */
    retract(visited(Nodes)),
    assert(visited([H|Nodes])),
    retract(index(I)),
    J is I + 1,
    assert(index(J)),
    H_Lev is Lev + 1,
    ( node(N,_,_,_,N_Anc) -> true ),    /* list of ancestors    */
    assert(node(H,J,H_Lev,J,[N|N_Anc])),    /* 'lowlink' <- 'index' */
    ( successors(H,S) -> true ),
    dfs(H,S,H_Lev),         /* for tree 'arc(N,H)',     */
    reset_lowlink(N,H,lowlink), /*  compare 2  'lowlink' values */
    dfs(N,T,Lev).
dfs(N,[H|T],Lev):-             /* successor 'H' already visited */
    !, reset_lowlink(N,H,index),   /* for back or cross 'arc(N,H)', */
    dfs(N,T,Lev).           /*   compare 'index' and 'lowlink'  */
dfs(N,[_|T],Lev):-  /* 'H' no longer present, since it belonged to  */
    dfs(N,T,Lev).       /*   some other SCC discovered earlier  */



/*-----------------------------------------------------------------------*/


depth_first_search:-
    retract(node(N,0)), /* start at any node not yet visited    */
    !, retract(index(I)),
    J is I + 1,
    assert(index(J)),
    assert(node(N,J,1,J,[])),   /* root has no ancestors!   */
    ( successors(N,S) -> true ),
    assert(visited([N])),   /* stack of nodes already visited   */
    dfs(N,S,1),         /* start at at level 1      */
    retract(visited(_)),
    depth_first_search. /* proceed to next spanning tree    */
depth_first_search.     /* spanning forest generated        */


/*-----------------------------------------------------------------------*/

findall_successors(X,S):- 
    findall(Y,arc(X,Y),Z),      /* 'Z' = [] if no successors    */
    sort(Z,S).

/* Assign to each node an initial depth first index         */
/*  (value 0 => not yet visited by the depth first search)      */

prepare_nodes([]):- !.
prepare_nodes([H|T]):-
    assert(node(H,0)), 
    findall_successors(H,S),
    assert(successors(H,S)),
    prepare_nodes(T).


dfs_and_scc:-
    findall(N,node(N),Nodes),
    retractall(node(_)),
    prepare_nodes(Nodes),
    assert(index(0)),       /* initialize index for DFS */
    depth_first_search.

/*-----------------------------------------------------------------------*/
    

start:- assert(phigs(false)),
    write(' File with user description: '),
    read(USER),
    consult(USER),
    name(USER,User_ascii),
    name('_scc',Scc_ascii),
    append(User_ascii,Scc_ascii,OUT_ascii),
    name(OUT,OUT_ascii),
    tell(OUT),
    write(':- dynamic scc/1.'), nl, nl,
    statistics(runtime,_),              /* reset runtime clock */
    dfs_and_scc,                 /* includes listing the scc's */
    retract(phigs(_)),
    statistics(runtime,[T_elapsed,_]), /* throw away fractions of msec */
    print_statistics(T_elapsed),      /* timing for combined dfs + scc */
    told.


start_from_c:- 
    assert(phigs(true)),
    read(USER),
    consult(USER),
    name(USER,User_ascii),
    name('_scc',Scc_ascii),
    append(User_ascii,Scc_ascii,OUT_ascii),
    name(OUT,OUT_ascii),
    tell(OUT),
    write(':- dynamic scc/1.'), nl, nl,
    statistics(runtime,_),              /* reset runtime clock */
    open_stream_for_c(W_stream),
    assert(stream_for_c(W_stream)),
    dfs_and_scc,                 /* includes listing the scc's */
    close_stream_for_c(W_stream),
    handshake,
    retract(stream_for_c(_)),
    retract(phigs(_)),
    statistics(runtime,[T_elapsed,_]), /* throw away fractions of msec */
    print_statistics(T_elapsed),      /* timing for combined dfs + scc */
    told.
