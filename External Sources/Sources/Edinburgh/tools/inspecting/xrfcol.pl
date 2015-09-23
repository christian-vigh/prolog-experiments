/* XRFCOL.PL :  Collecting up module of XREF.

						Dave Bowen
						Updated: 11 March 82
*/

 :- mode callers(+,+,-).   
 :- mode defn_file(+,-).
 :- mode entries(+,+).   
 :- mode exts(+,+,+).    
 :- mode fentries(-). 
 :- mode got_defn(+,-).
 :- mode lt2(+,+,+,+).   
 :- mode lte(+,+).  
 :- mode multiple_defn(+,+,-,-).
 :- mode notin(+,+).
 :- mode partition(+,+,-,-).  
 :- mode qsort(+,-,+).   


/******************************************************************************

Data for collecting up

   $ext(File, Predicate)
			  Held on key: File.
			  $ext(I,P) means P is an external (import) of I

   $entry(File, Predicate)
			  Held on key: File.
			  $entry(I,P) means P is an entry (export) of I

******************************************************************************/


collect :-
   fentries(L),		%  Make list of all predicates with all associated data
   qsort(L,L1,[]),	%  Sort them
   do_output(L1).

				/* Find entries. Search through all encountered
				   predicates */
fentries([e(F,N,f(I,Cs))|L]) :- 
	recorded('$pred',P,Ptr),		% Find a predicate P
	erase(Ptr),
	functor(P,F,N),				% P is F/N
	defn_file(P,I),				% P is defined in file I
	callers(P,I,Cs),			% Cs is list of callers c(F,N)
	((I=undefined,warn(P,'not defined')); true),
	((Cs=[],warn(P,'not called')); true),
	multiple_defn(P,[I],L,L1),		% Any multiple definitions?
	((nonvar(L),warn(P,'multiply defined')); true), !,
	fentries(L1).
fentries([]).

				/* Find I where P is defined, or else
				   set I=undefined.*/
defn_file(P,I) :- 
	got_defn(P,I), !.
defn_file(P,undefined).

				/* Is P defined in a file, or is it known? */
got_defn(P,I) :- 
	recorded(P,'$defn'(I,P),_).
got_defn(P,I) :- 
	recorded(P,'$known'(I),_).

				/* Look for multiple defns of P. List contains
				   all places P is already known to be defined.
				   3rd arg is var & tail of e-list. Instantiate
				   its head to any multiple defn, and return
				   4th arg as new var tail of e-list */

multiple_defn(P,List,[e(F,N,f(I,[]))|L],L1) :-
   got_defn(P,I), notin(I,List), !,
   functor(P,F,N),
   multiple_defn(P,[I|List],L,L1).
multiple_defn(P,_,L,L).

				/* True if X is not in List */
notin(X,List) :- 
	\+ member(X,List).

				/* Return a (possibly empty) list of all
				   callers of the procedure P */
callers(P,I,[c(F,N)|Cs]) :-
	recorded(P,'$caller'(P,F,N,J),Ptr),
	erase(Ptr), !,
	exts(I,J,P),
	callers(P,I,Cs).
callers(_,_,[]).

				/* Record externals. P is defined in I (entry/
				   export), used in J (external/import) */

exts(I,I,_) :- !.			% Do nothing if defn & use in same file
exts(I,undefined,P) :- !,		% Get here if P was mark_interpreted
	entries(I,P).			% Record P is exported from I
exts(I,J,P) :-
	entries(I,P),			% Record P is exported from I
	crecord(J,'$ext'(J,P)).		% Record P is imported to J

				/* Record exports */
entries(undefined,_) :- !.
entries(I,P) :- crecord(I,'$entry'(I,P)).


				/* Quick sort of functor entries. qsort(A,B,C)
				   returns B as the concatenation of sorted
				   A and C (which should already be sorted). */
qsort([X|L],R,R0) :-
	partition(L,X,L1,L2),
	qsort(L2,R1,R0),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

				/* Partition list arg1 on the value of Y
				   into arg3 and arg4 */
partition([X|L],Y,[X|L1],L2) :-
	lte(X,Y), !,
	partition(L,Y,L1,L2).
partition([X|L],Y,L1,[X|L2]) :-
	partition(L,Y,L1,L2).
partition([],_,[],[]).

				/* Comparison predicate for sort */
lte(e(F1,N1,_),e(F2,N2,_)) :-
	lt2(F1,N1,F2,N2).

				/* Order first by functor name, then arity */
lt2(F,N1,F,N2) :- !, N1=<N2.
lt2(F1,_,F2,_) :- F1 @< F2.
