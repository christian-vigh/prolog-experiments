/*  DLISTS.PL
    Shelved on the 6th of December 1987
*/


/*
    This is my [JNP] revised version of a doubly-linked list-handling
    package sent to the INFO-PROLOG bulletin board by Philip Dart,
    Melbourne University.

    I have made the following changes from his original file:
    (1)  replace % comments by slash-star star-slash
    (2)  remove the mode declarations (Head when Variables).
    (3)  quote atoms like '$dAppend' which were unquoted.

    The original file is in DLISTS.ORIG.                     

    Here follow the original comments:

    Date: 3 Nov 87 03:02:00 GMT
    From: munnari!mulga!philip@uunet.uu.net  (Philip Dart)
    Subject: Doubly-linked list package

    Following the comments [on the bulletin board] about Fortran
    as an AI language, Melbourne University Department of Artificial
    Intelligence has decided to convert all of its Fortran AI programs
    to NU-Prolog. This package has been written as an aid to this
    conversion.                    

    For non-NU-Prolog users, simply comment out the when declarations.

    Doubly-linked list package.
          Why use boring old single-linked lists when doubly-linked
          list could make your list processing applications
          run as never before.

    P.S. Don't forget to turn off the occur-check in your version of Prolog!

    Predicates defined:
      test                         Demonstrate the predicates
      listToD( L, D )              Convert L to D        
      dPrev(D, _)                  Get previous node
      dNext(D, _)                  Get next node
      dHead(D, _)                  Get head of list
      dTail(D, _)                  Get tail of list
      isD(D)                       Is this a doubly-linked list?
      portray(D)                   Portray doubly-linked list
      dAppend(X, Y, Z)             Append for doubly-linked lists
      dAdj(L, R)                   Are these adjacent nodes?
*/


/*  test:
    Demonstrate the conversion of single-linked to double-linked lists.
*/
test :-
        L1 = [1, 2, 3],
        listToD(L1, D1),
        write(L1), write(' <=> '), portray(D1), nl,
        L2 = [4, 5, 6, 7],
        listToD(L2, D2),
        write(L2), write(' <=> '), portray(D2), nl,
        dAppend(D1, D2, D3),
        listToD(L3, D3),
        isD(D3),
        write(L3), write(' <=> '), portray(D3), nl.


/*  dAdj(L+, R+):
    Are these adjacent nodes?
*/
dAdj(L, R) :-
        L = d(_, _, R),
        R = d(L, _, _).


/*  dPrev(D+, P-):
    Get previous node into P.
*/
dPrev(d(L, _, _), L).


/*  dNext(D+, N-).
    Get next node into N.
*/
dNext(d(_, _, R), R).


/*  dHead(D+, Head-):
    Get head of list into Head.
*/
dHead([], []).
dHead(d([], D, R), d([], D, R)).
dHead(d(d(L, D, R), _, _), H) :-
        dHead(d(L, D, R), H).


/*  dTail(D+, Tail-):
    Get tail of list into Tail.
*/
dTail([], []).
dTail(d(L, D, []), d(L, D, [])).
dTail(d(_, _, d(L, D, R)), T) :-
        dTail(d(L, D, R), T).


/*  listToD(List+, D-):
    listToD(List-, D+):
    Convert single to doubly-linked list or vice-versa.
*/
listToD([], []).
listToD( .(H,T), D) :-
        D = d([], H, R),
        '$listToD'(T, D, R).


/*  '$listToD'(List+, _, D-):
or  '$listToD'(List-, _, D+)
*/
'$listToD'([], _, []).
'$listToD'( .(H,T), L, D) :-
        D = d(L, H, R),
        '$listToD'(T, D, R).


/*  isD(D+)
    Is this a doubly-linked list?
*/
isD([]).
isD(D) :-
        D = d([], _, R),
        '$isD'(D, R).


/* '$isD'(_, D+):
*/                    
'$isD'(_, []).
'$isD'(L, D) :-
        D = d(L, V, R),
        '$isD'(D, R).


/*  portray(D)
    Portray doubly-linked list.
*/
portray(D) :-
        nonvar(D),
        D = d([], _, _),
        display('[]:'),
        '$dPrint'(D).


/*  '$dPrint'(D+)
*/                    
'$dPrint'([]) :-
        display('[]').
'$dPrint'(d(_, V, R)) :-
        display(V),
        display(':'),
        '$dPrint'(R).


/*  dAppend(X+, Y, Z-)
or  dAppend(X-, Y, Z+):
    Append for doubly-linked lists.
*/
dAppend(X, [], X).
dAppend([], d([], D, R), d([], D, R)).
dAppend(d(L, X, R), d([], Y, RY), Z) :-
        '$dAppend'([], d(L, X, R), Y, RY, Z).


/*  '$dAppend'(X+, _, _, Z-)
or  '$dAppend'(X-, _, _, Z+):
*/
'$dAppend'(L, d(_, X, []), Y, R, Z) :-
        Z = d(L, X, H),
        H = d(Z, Y, R1),
        '$dAppend1'(H, R, R1).
'$dAppend'(L, d(_, X, d(L1, X1, R1)), Y, RY, d(L, X, RZ)) :-
        Z = d(L, X, RZ),
        '$dAppend'(Z, d(L1, X1, R1), Y, RY, RZ).


/*  '$dAppend'(_, Y, Z+):
or  '$dAppend'(_, Y+, Z):
*/
'$dAppend1'(_, [], []).
'$dAppend1'(L, d(_, D, R), Z) :-
        Z = d(L, D, R1),
        '$dAppend1'(Z, R, R1).
