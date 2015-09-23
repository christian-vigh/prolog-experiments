/*------------------------------------------------------------------------
     
                         -- C N R S -- 
         Laboratoire d'automatique et d'analyse des systemes 
            Groupe Robotique et Intelligence Artificielle  
                   7 Avenue du colonel Roche   
                     31 077 Toulouse Cedex  
     
  Fichier              : /RIA/vega/users/paul/prolog/sicstus/utilities
  Fonction             :  common utilities for prolog programs
              (many taken directly from SICStus manual, p.97)

  Auteur           :  paul freedman

  Mises a jour:
27/8/90: cuts added most everywhere
30/8/90: cuts modified according to comments from mats carlsson

------------------------------------------------------------------------*/


/* member of an ordered set (no duplicate elements, elements sorted)    */

member_ordered_set(X,[X|_]):- !.              /* 'X' is 1st element */
member_ordered_set(X,[Y|T]):-   /* 'X' > 2nd element, so keep searching */
    X @> Y, !,
    member_ordered_set(X,T).


/* insert an element into an ordered set                                */

insert_ordered_set(X,[Y,Z|T],[Y|Final]):-    /* 'X' > 'Z' -> skip on    */
    X @> Z, !,
    insert_ordered_set(X,[Z|T],Final).
insert_ordered_set(X,[Y|T],[X|[Y|T]]):-      /* 'X' < 'Z' AND 'X' < 'Y' */
    X @< Y, !.
insert_ordered_set(X,[Y,Z|T],[Y,X|[Z|T]]):-  /* 'X' < 'Z' AND 'X' > 'Y' */
    !.  
insert_ordered_set(X,[Y],[Y,X]).          /* 'X' > last element in list */



/*----------------------------------------------------------------------*/

member(X,[X|_]):- !.
member(X,[_|T]):- member(X,T).


membership(X,Y,Z):-          /* 'Z' is an element of 'X' in 'Y' */
    member(Z,X),
    member(Z,Y).

intersection(X,Y,L):-   /* 'L' = elements common to sets 'X' and 'Y'    */
    findall(Z,membership(X,Y,Z),L).


append([],L,L):- !.
append([[]],L,L):- !.
append([X|L1],L2,[X|L3]):-
    append(L1,L2,L3).


delete(X,[X|T],T):- !.  /* fails if 'X' does not belong to list '[H|T]' */
delete(X,[H|T],[H|L]):- delete(X,T,L).
    


last(L,Last,Remainder):-    /* 'Last' is last element in list 'L'   */
    append(Remainder,[Last],L). 


assert_list([]):- !.
assert_list([H|T]):- assert(H), assert_list(T).


write_utility([]):- !.                  /* dummy case for null list     */
write_utility([H|[]]):- !, write(H).    /* no comma or new line here!   */
write_utility([H|T]):- 
    write(H), write(','), nl, write_utility(T).


writeq_utility([]):- !.                 /* dummy case for null list     */
writeq_utility([H|[]]):- !, writeq(H).
writeq_utility([H|T]):- 
    writeq(H), write(','), nl, writeq_utility(T).


reverse_append([],L,L):- !.
reverse_append([X|L1],L2,L3):-
    reverse_append(L1,[X|L2],L3).

reverse_list(L1,L2):-
    reverse_append(L1,[],L2).



/*----------------------------------------------------------------------*/

print_statistics(T_elapsed):-   /* write runtime as an atom, to avoid   */
    nl, nl,         /*  problems when later reading file    */
    write('runtime_'),
    write(T_elapsed), write('msec.'), nl.


probe_statistics:-                    /* write stats to standard output */
        plsys(system('pstat -s')),
    statistics(global_stack,V1), nl(user_output),
    display('global stack: '), display(V1), nl(user_output),
    statistics(local_stack,V2),
    display('local stack: '), display(V2), nl(user_output),
    statistics(trail,V3),
    display('trail: '), display(V3), nl(user_output),
    statistics(choice,V4),
    display('choice: '), display(V4), nl(user_output),
    statistics(core,V5),
    display('core: '), display(V5), nl(user_output),
    statistics(heap,V6),
    display('heap: '), display(V6), nl(user_output),
    nl(user_output).


/*----------------------------------------------------------------------*/

/* now for the phigs related stuff */

open_stream_for_c(W_stream):-
    open('~/phigs/prolog_info',write,W_stream).

close_stream_for_c(W_stream):-
    close(W_stream).

handshake:-
    open('~/phigs/prolog_handshake',write,W_stream),
    close(W_stream).


separator(W_stream):- 
    write(W_stream,'/').

eot(W_stream):- 
    flush_output(W_stream).

send_list_to_c([],W_stream):- !.
send_list_to_c([H|T],W_stream):-
    write(W_stream,H), 
    separator(W_stream),
    send_list_to_c(T,W_stream).


