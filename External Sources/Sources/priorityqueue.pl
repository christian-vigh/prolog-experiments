% Computational Intelligence: a logical approach. 
% Prolog Code. 
% An efficient implementation of Priority Queues.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : pq.pl                                                            %
%   Author : David Poole, poole@cs.ubc.ca                                     %
%   Updated: 26 Septermber 1994                                               %
%   Purpose: Implement priority queues in Prolog.                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(pq, [
	insertPQ/4,
	removePQ/4,
	sizePQ/2,
        emptyPQ/1,
	pq_to_list/2,
	list_to_PQ/2,
	min_of_PQ/5,
	min_of_PQ/3,
        heapsort/2
		 ]).


/*
This provides a new representation for Prolog priority queues that is
more natuaral, more elegant and more efficient than the traditional
implementation of heaps derived from the array-based implementation
designed for procedural languages.

The empty priority queue is represented by "empty". 
A non-empty priority queue is represented recursively as	
"pq(Key,Datum,LPQ,RPQ)" where
Key is the (numeric) key for Datum
LPQ and RPQ are priority queues.  

In our priority queues, we maintain the following invariant for each subtree: 
The Key at the root is the smallest key in the subtree.
The right subtree RPQ has either the same number of data elements or
exactly one more than the left subtree LPQ.  

This invariant insures that both the insert and delete operations have
O(log n) time complexity, where n is the number of elements in the
queue.
*/

%   insertPQ(+OldPQ, +Key, +Datum, -?NewPQ)
%   inserts the new Key-Datum pair into the heap.  

/*
The insertion operation behaves as follows:
insertPQ(OldPQ,Key,Datum,NewPQ) is true if inserting X into queue
OldPQ results in queue NewPQ.  The balance invariant is maintained by
inserting data elements into the left subtree of OldPQ, creating a
new subtree.  This new subtree is made the right subtree of NewPQ,
and the right subtree of OldPQ becomes the new left subtree of NewPQ.
The order invariant is maintained as follows: if the new key is
smaller than the key at the root of the queue, the new element is made
the root, and the old root element is inserted into the left subtree;
otherwise the new element is added to the left subtree.
*/

insertPQ(empty, Key, Datum, pq(Key, Datum,empty,empty)).
insertPQ(pq(Key1,Datum1,LPQ,RPQ),Key,Datum, pq(Key1,Datum1,RPQ,NLPQ)) :-
   Key1 @< Key, !,
   insertPQ(LPQ,Key,Datum,NLPQ).
insertPQ(pq(Key1,Datum1,LPQ,RPQ),Key,Datum, pq(Key,Datum,RPQ,NRPQ)) :-
   Key1 @>= Key,
   insertPQ(LPQ,Key1,Datum1,NRPQ).

/* Deleteion */

% removePQ(+OldPQ, ?Key, ?Datum, -NewPQ) 
% is true Key-Datum is a smallest element of OldPQ, and NewPQ is the
% priority queue resulting from removing that element from priority
% queue OldPQ.
%   If several pairs in the heap have the same key, it is 
%   not defined which of them will come out first.

/*

By the priority queue invariabnts, the root element always has a
smaller key than any element in its subtrees, and so has the smallest
key in the queue, and so is the element returned. Since it is being
deleted, it must be replaced by the next smallest element. To maintain
the balance invariant, the predicate remany(Val,OldPQ,NewPQ) removes a
node from the right subtree (since we always remove from the right to
maintain the invariant) and puts it as the top element in the priority
queue, and we swap subtrees, so that the left subtree is always at
most one element lighter than the right. To maintain the order
invariant, this value is pushed down through the queue with
pushPQ(OldPQ,NewPQ), which pushes the element down until the ordered
property is attained.

*/

removePQ(pq(Key, Datum,empty,PQ),Key, Datum,PQ) :- !.
removePQ( pq(Key, Datum,LPQ,RPQ),Key, Datum,NPQ) 
  :- remanyPQ(Key1, Datum1,RPQ,NRPQ),
     pushPQ(pq(Key1, Datum1,NRPQ,LPQ),NPQ).

/*

The predicate pushPQ(OldPQ,NewPQ) is true if NewPQ is a heap given a
"pseudo-heap" OldPQ. A pseudo-heap is a priority queue, where the
priority queue invariants are all true, with a possible exception of
the ordered property for the root of the tree. If LV, the element in
the left subtree, is smaller than both RV and Val, we exchange LV and
Val, pushing Val down the left subtree.  Otherwise, if Val is smaller
than RV, then we exchange these two, pushing Val down the right
subtree.  Note that we are not adding or deleting elements, so we
don't do any subtree swapping.  Finally, if Val is smaller than either
RV or LV, we are finished.

*/

pushPQ(pq(Key, Datum,pq(KeyL, DatumL,PQLL,PQLR),pq(KeyR, DatumR,PQRL,PQRR)), 
       pq(KeyL, DatumL,NLPQ,pq(KeyR, DatumR,PQRL,PQRR)) ) :-
  KeyL =< KeyR,
  KeyL < Key, !,
  pushPQ(pq(Key, Datum,PQLL,PQLR),NLPQ).

pushPQ(pq(Key, Datum,pq(KeyL, DatumL,PQLL,PQLR),pq(KeyR, DatumR,PQRL,PQRR)), 
       pq(KeyR, DatumR,pq(KeyL, DatumL,PQLL,PQLR),NRPQ )) :-
  KeyR < KeyL,
  KeyR < Key, !,
  pushPQ(pq(Key, Datum,PQRL,PQRR),NRPQ).

pushPQ(pq(Key, Datum,pq(KeyL, DatumL,PQLL,PQLR),pq(KeyR, DatumR,PQRL,PQRR)), 
       pq(Key, Datum,pq(KeyL, DatumL,PQLL,PQLR),pq(KeyR, DatumR,PQRL,PQRR)) ) :-
  Key =< KeyL,
  Key =< KeyR, !.

pushPQ(pq(Key, Datum,empty,pq(KeyR, DatumR,PQRL,PQRR)), 
       pq(Key, Datum,empty,pq(KeyR, DatumR,PQRL,PQRR)) ) :-
  Key =< KeyR, !.

pushPQ(pq(Key, Datum,empty,pq(KeyR, DatumR,PQRL,PQRR)), 
       pq(KeyR, DatumR,empty,pq(Key, Datum,PQRL,PQRR)) ) :-
  KeyR < Key.

pushPQ(pq(Key, Datum,empty,empty),
       pq(Key, Datum,empty,empty)).
/*

We remove an element from a subtree (maintaining the balance
invariant) using remanyPQ(Key,Datum,LQ,RQ).  This predicate finds the
right--most element which either has only a right child, or is itself
the right leaf of a node with two children.  Swapping occurs in the
latter case.

*/

remanyPQ(Key, Datum, pq(Key, Datum,empty,PQ),PQ) :- !.
remanyPQ(Key, Datum, pq(Key1, Datum1,LPQ,RPQ), pq(Key1, Datum1,NLPQ,LPQ)) :-
   remanyPQ(Key, Datum,RPQ,NLPQ).

% list_to_PQ(+L,-PQ) converts list L of Key-Datum pairs into a priority queue.
list_to_PQ(L,PQ) :-
   list_to_PQ(L,empty,PQ).

% list_to_PQ(L,PQ1,PQ2) adds elements of list L of Key-Datum pairs
%  to PQ1 forming PQ2
list_to_PQ([],P,P).
list_to_PQ([K-D|T],P1,P3) :-
   insertPQ(P1,K,D,P2),
   list_to_PQ(T,P2,P3).

%   pq_to_list(+PQ, -List)
%   returns the Key-Datum pairs in PQ as
%   List, sorted into ascending order of Keys.  

pq_to_list(empty,[]).
pq_to_list(PQ,[K-D|T]) :-
   removePQ(PQ,K,D,PQ1),
   pq_to_list(PQ1,T).

% nlist_to_PQ(+L,-PQ) converts list L of numbers into a priority queue.
nlist_to_PQ(L,PQ) :-
   nlist_to_PQ(L,empty,PQ).

% nlist_to_PQ(L,PQ1,PQ2) adds elements of list L of numbers
%  to PQ1 forming PQ2
nlist_to_PQ([],P,P).
nlist_to_PQ([K|T],P1,P3) :-
   insertPQ(P1,K,K,P2),
   nlist_to_PQ(T,P2,P3).

%   pq_to_nlist(+PQ, -List)
%   returns the Keys in PQ as
%   List, sorted into ascending order.  

pq_to_nlist(empty,[]).
pq_to_nlist(PQ,[K|T]) :-
   removePQ(PQ,K,_,PQ1),
   pq_to_nlist(PQ1,T).

% heapsort(+L,-S) sorts list L os numbers into list S
heapsort(L,S) :-
   nlist_to_PQ(L,PQ),
   pq_to_nlist(PQ,S).

% emptyPQ(Q) is true if queue Q is empty
emptyPQ(empty).

% sizePQ(Q,S) is true if queue Q contains S elements.
sizePQ(empty,0).
sizePQ(pq(_,_,L,R),S) :-
   sizePQ(L,S1),
   sizePQ(R,S2),
   S is S1 + S2 + 1 .

% min_of_PQ(+PQ, ?Key, ?Datum)
% returns the Key-Datum pair with smallest Key
min_of_PQ(pq(Key,Datum,_,_),Key,Datum).

