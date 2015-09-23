Date: 27 Jun 86 13:58:00 EDT
From: "CUGINI, JOHN" <cugini@nbs-vms.ARPA>
Subject: new, improved utilities library, as of June 27, 1986

I just finished a moderate overhaul of the library, and thought it
was worth sending out the new version.  There are two main differences:

1. There is a short description of the overall organization at the
   beginning.  Routines have been shuffled around and grouped roughly
   according to the type of entity they manipulate.

2. There is a major new predicate, called "rationalize" which does
   rational arithmetic, roughly a la Common Lisp, and improves
   the handling of exponentiation, for which C-Prolog, er, makes
   some mistakes.  (Try X is 0^(-2) or X is (-2)^2.) 

I'd be interested in knowing about other Prolog libraries you have there - 
if there's an index of libraries, please let me know...

John Cugini

----------------------------------------------------------------------

/* This file contains various little utility predicates,
   some commonly used, some not.  Many are for list
   manipulation.  Furthermore, many of these predicates
   expect certain of their arguments to be instantiated
   upon invocation.  When such restrictions apply it is
   usually the leading arguments which are thought of
   as input (and hence instantiated), and the trailing 
   arguments as output (and hence allowed to be
   uninstantiated).  */

/* There is a coding convention as follows: the user-callable
   version of the predicate has a plain name.  If this predicate
   needs "sub-predicates", based on whether certain arguments are
   instantiated or not, the names of the sub-predicates are formed
   by appending a string of c,v, or x's, where c indicates argument
   must be constant (instantiated), v that it must be a variable,
   and x that it may be either.  
   
   Further, each main predicate is preceded by documentation lines,
   which describe the declarative meaning of the predicate, and which
   arguments must be instantiated.  */ 

/* The overall organization of the library is roughly:
    
    Basic predicates
    Lists
    Structures
    Input/Output
    Sets
    Numeric
    Control
    Extended Logic

   Each is prefaced by a header with lots of asterisks  */

/************** Basic Predicates **************/

/* The following predicates test the type of the term passed
   to them, using the terminology of the C-Prolog manual.  */

term(Term).
simple(Term)   :- atomic(Term); var(Term).
compound(Term) :- not(simple(Term)).
literal(Term)  :- nonvar(Term), 
		  functor(Term, Name, _), 
		  atom(Name).
float(Term)    :- number(Term), not(integer(Term)).

/* islist(X) iff X is a list.  */

islist([]) :- !.
islist([_|_]).

/* member(Elem, List) iff Elem is a member of List */

member(Elem, [Elem | _]). 
member(Elem, [_ | Rest_of_list]) :- member(Elem, Rest_of_list).

/* member_rest(Elem, List, Rest) iff Elem is a member of List and
   Rest is the rest of the list following Elem  */

member_rest(Elem, [Elem | Rest], Rest).
member_rest(Elem, [_ | Rest], Rest_rest) :-
  member_rest(Elem, Rest, Rest_rest).


/************** Lists **************/

/* append(First_part, Second_part, List) iff List is the
   concatenation of the first two arguments.  */

append([], List, List).
append([Elem | First_part], Second_part, [Elem | List]) :- 
  append(First_part, Second_part, List).

/* delete(Elem, Old_list, New_list) iff New_list equals Old_list except 
   for the removal of any occurrences of Elem.  NG if arg2 is var.  */

delete(Elem, Old, New) :-
   (var(Elem) -> (remove_dupl(Old, Smaller_old),
		  member(Elem, Smaller_old)
		 ); 
		  Elem=X        /* dummy statement to fill else-part  */
   ),
   delete_c(Elem, Old, New).

delete_c(_, [], []).
delete_c(Elem1, [Elem2 | Rest_of_old], New_list) :- 
   Elem1 = Elem2 -> (delete_c(Elem1, Rest_of_old, New_list)
		    );
		    (New_list = [Elem2 | Rest_of_new],
		     delete_c(Elem1, Rest_of_old, Rest_of_new)
		    ).

/* delete_all(Del_list, Old_list, New_list) iff New_list equals Old_list
   except for the removal of any occurrences of any elements of Del_list.
   NG if arg1 or 2 is var.  */

delete_all([], L, L).
delete_all([E | R_del], Old_list, New_list) :- 
   delete(E, Old_list, M),
   delete_all(R_del, M, New_list).

/* remove_dupl(List, Shriven_list) iff Shriven_list equals List
   in same order, sans leading duplicate members.  Ie, only the
   rightmost of duplicate members remain.  NG if arg1 is var.  */

remove_dupl([],[]).
remove_dupl([Elem | Rest_list], Rest_shriven) :-
  member(Elem, Rest_list),
  !,
  remove_dupl(Rest_list, Rest_shriven).
remove_dupl([Elem | Rest_list], [Elem | Rest_shriven]) :-
  remove_dupl(Rest_list, Rest_shriven).

/* no_dupls(List) iff List is a list with no duplicate elements. 
   NG if arg1 is var.  */

no_dupls([]).
no_dupls([Elem | Rest]) :-
   not(member(Elem, Rest)),
   no_dupls(Rest).

/* last(Elem, List) iff Elem is the last element in List */

last(Elem, [Elem]).
last(Elem, [_ | Rest]) :- last(Elem, Rest).

/* next_to(X,Y,L) iff X and Y are adjacent in list L */

next_to(X, Y, [X,Y | _]).
next_to(X, Y, [_ | Rest]) :- next_to(X, Y, Rest).

/* reverse(List1, List2) iff List1 is List2 in reverse order.
   NG if arg1 is var.   */

reverse([], []).
reverse([Head | Tail], List) :- 
  reverse(Tail, Liat), append(Liat, [Head], List).

/* efface(Elem, Old_list, New_list) iff New_list = Old_list 
   with first occurrence of Elem removed.  NG if more than 
   one arg is var.  */

efface(Elem, [Elem | Rest], Rest) :- !.
efface(Elem, [Non_elem | Old_rest], [Non_elem | New_rest]) :- 
  not(Elem = Non_elem),
  efface(Elem, Old_rest, New_rest).

/* list_incr(Elem, List, Bigger_list) iff Bigger_list = List plus
   Elem inserted somewhere.  NG if all args are var.  */

list_incr(Elem, List, [Elem | List]).
list_incr(Elem, [Non_elem | List], [Non_elem | Bigger_list]) :-
   list_incr(Elem, List, Bigger_list).

/* subst(Old_elem, Old_list, New_elem, New_list) iff New_list equals 
   Old_list except for the substitution of New_elem for any 
   occurrences of Old_elem.  NG for arg1;2;3 var.  */

subst(_, [], _, []).
subst(Old_elem, [Old_elem | Rest_of_old], 
      New_elem, [New_elem | Rest_of_new]) :- 
	 !, subst(Old_elem, Rest_of_old, New_elem, Rest_of_new).
subst(Old_elem, [Non_elem | Rest_of_old], 
      New_elem, [Non_elem | Rest_of_new]) :-
	    subst(Old_elem, Rest_of_old, New_elem, Rest_of_new).

/* prefix(Part, Whole) iff Part is a leading substring of Whole */

prefix([], _).
prefix([Elem | Rest_of_part], [Elem | Rest_of_whole]) :- 
  prefix(Rest_of_part, Rest_of_whole).

/* suffix(Part, Whole) iff Part is a trailing substring of Whole */

suffix(List, List) :- islist(List).
suffix(Part, [Elem | Rest_of_whole]) :- suffix(Part, Rest_of_whole).

/* sublist(List, Start, End, Sublist) iff Sublist is a contiguous
   sub-list within List, starting at position Start, and ending at 
   position End.  Note that [] is a valid sublist, so for 
   List = [1,2], valid solutions are:

	 Start  End   Sublist
	 1      0     []
	 1      1     [1]
	 1      2     [1,2]
	 2      1     []
	 2      2     [2]
	 3      2     []

  NG if arg1 is var.  */

sublist(List, Start, End, Sublist) :-
  prefix(Sublist, List),
  Start = 1,
  length(Sublist, End).

sublist([Elem | Rest], Start, End, Sublist) :-
  sublist(Rest, Startx, Endx, Sublist),
  Start is Startx + 1,
  End is Endx + 1.

/*  matchlist(List1, List2, List3) iff List1 and List2 are sorted
    instantiated lists and List3 is their intersection, also
    sorted.  NG is Arg1 or Arg2 is var or unsorted.  */

matchlist([], List, []).
matchlist([Elem | Rest], [], []).
matchlist([Elem | Rest1], [Elem | Rest2], [Elem | Rest3]) :-
  matchlist(Rest1, Rest2, Rest3), !.
matchlist([Elem1 | Rest1], [Elem2 | Rest2], Rest3) :-
  (Elem1 @< Elem2 -> matchlist(Rest1, [Elem2 | Rest2], Rest3);
		     matchlist([Elem1 | Rest1], Rest2, Rest3)),
  !.

/* list_length(List, Number) iff List has Number elements.  
   NG if arg1 is var.  Note that, apparently, C-Prolog has an
   undocumented evaluable predicate, length(List, Number).  */

list_length([], 0).
list_length([Elem | Rest], Number) :-
  list_length(Rest, N_minus),
  Number is N_minus + 1.

/* position(List, Elem, Number) iff Elem is in position Number
   in the List.  NG if arg1 is var.  */

position([Elem | Rest], Elem, 1).
position([_    | Rest], Elem, Number) :-
   var(Number) -> (position(Rest, Elem, N_minus),
		   Number is N_minus + 1);
		  (N_minus is Number - 1, 
		   position(Rest, Elem, N_minus),
		   !).

/* repeat_list(Elem, Number, List) iff List is a list of Elem 
   repeated Number times.  No var restrictions.  */

repeat_list(Elem, Number, List) :-
  (nonvar(Number), var(List)) -> 
      repeat_list_xcv(Elem, Number, List);
      repeat_list_xxx(Elem, Number, List).

repeat_list_xcv(Elem, 0, []).
repeat_list_xcv(Elem, N, [Elem | Rest]) :-
  N_minus is N - 1,
  !,
  N_minus > -1,
  repeat_list_xcv(Elem, N_minus, Rest).

repeat_list_xxx(Elem, 0, []).
repeat_list_xxx(Elem, N, [Elem | Rest]) :- 
  repeat_list_xxx(Elem, N_minus, Rest),
  N is N_minus + 1.

/* permute(List1, List2) iff List1 is a permutation of List2.
   NG if arg1 is var.  */

permute(Whole, [Elem | Rest_of_part]) :-
     islist(Whole),
     member(Elem, Whole),
     efface(Elem, Whole, Reduced_whole),
     permute(Reduced_whole, Rest_of_part). 

permute([], []). 

/* reduce(Bin_op, List, Ans) iff Ans is the result of applying
   Bin_op, left-associatively, to the elements of List.   NG if
   arg1 or arg2 is var, or if List contains fewer than two
   elements. If ID is an identity element for Bin_op, and the List
   may contain only one element, invoke with: reduce(Bin_op, [ID |
   List], Ans).   To allow a null list to return ID, invoke
   with: reduce(Bin_op, [ID, ID | List], Ans). */ 

reduce(Bin_op, [El1, El2 | Rest], Ans) :-
  Callit =.. [Bin_op, El1, El2, Temp],
  Callit,
  (Rest = [] ->
     Ans = Temp;
     reduce(Bin_op, [Temp | Rest], Ans)
  ).

/* maplist(Pred, Old, New) iff for each corresponding element in
   Old and New, Pred(Old, New) is true.  NG if arg1 is var.  */

maplist(_, [], []).
maplist(Pred, [Elem_old | Rest_of_old], [Elem_new | Rest_of_new]) :- 
  Pred_call =.. [Pred, Elem_old, Elem_new],  /* constructs  predicate */
  Pred_call,                         /* invokes constructed predicate */
  maplist(Pred, Rest_of_old, Rest_of_new).

/* Note that for string compares (eg, "abc" < "def") and atom
   compares (atom1 < atom2), the evaluable predicates @<, @>,
   etc., work fine.  */

/* maxlist(List, Max) iff Max is the highest value in a List of numbers.
   NG if arg1 is var.  */

maxlist([Elem], Elem).
maxlist([Elem | Rest], Max) :-
   maxlist(Rest, Rmax),
   (Elem > Rmax -> Max = Elem; Max = Rmax).

/* minlist(List, Min) iff Min is the lowest value in a List of numbers.
   NG if arg1 is var.  */

minlist([Elem], Elem).
minlist([Elem | Rest], Min) :-
   minlist(Rest, Rmin),
   (Elem < Rmin -> Min = Elem; Min = Rmin).

/* bestlist(List, Pred, Best) iff Best is the best value in a List, 
   according to some binary predicate Pred(A,B), which succeeds iff
   A is better than B.  NG if arg1 or arg2 is var.  */

bestlist([Elem], _, Elem).
bestlist([Elem | Rest], Pred, Best) :-
   bestlist(Rest, Pred, Rbest),
   Call =.. [Pred, Elem, Rbest],
   (Call -> Best = Elem; Best = Rbest).

/* all(Pred, List) iff Pred is a single-place predicate true of
   all members of the list.  NG if either arg is var. */

all(Pred, []) :- atom(Pred).

all(Pred, [Elem | Rest]) :-
  Callit =.. [Pred, Elem],
  Callit,
  all(Pred, Rest).

/* some(Pred, List) iff Pred is a single-place predicate true of
   at least one member of the list.  NG if either arg is var. */

some(Pred, List) :- atom(Pred), some_cx(Pred, List).

some_cx(Pred, [Elem | Rest]) :-
  (Callit =.. [Pred, Elem],
   Callit);
  some_cx(Pred, Rest).

/* notall(Pred, List) iff Pred is a single-place predicate false of
   at least one member of the list.  NG if either arg is var. */

notall(Pred, List) :- atom(Pred), not(all(Pred,List)).

/* none(Pred, List) iff Pred is a single-place predicate true of
   none of the members of the list.  NG if either arg is var. */

none(Pred, List) :- atom(Pred), not(some(Pred,List)).

/* string_of(Alphabet, String) iff String is a list composed of
   elements of the non-redundant non-null Alphabet.  
   NG if both args are var. */

string_of(Alphabet, String) :-
  var(Alphabet) -> remove_dupl(String, Alphabet);
		   (Alphabet = [_|_],       /* ie non-null */
		    no_dupls(Alphabet), 
		    string_of_cv(Alphabet, String)
		   ).

string_of_cv(Alphabet, []).
string_of_cv(Alphabet, [Elem | Rest]) :-
  string_of_cv(Alphabet, Rest),
  member(Elem, Alphabet).

/* sort_all(In_list, Out_list) iff Out_list is a sorting of In_list, 
   saving duplicates.  The Xing allows the use of keysort, which
   doesn't delete duplicates.  */

sort_all(In, Out) :-
  sort_all_xit(In, Xed_list),
  keysort(Xed_list, Sorted_xed_list),
  sort_all_xit(Out, Sorted_xed_list).

/* sort_all_xit(List, Xed_list) iff each element of list is matched
   by an xed element in Xed_list.   */

sort_all_xit([],[]).
sort_all_xit([Elem | In_rest], [Elem-x | Out_rest]) :-
  sort_all_xit(In_rest, Out_rest), !.

/* tot_sort(In_list, Out_list) iff Out_list is a total sorting
   of In_list.  This means that not only are the elements of
   In_list sorted, but that any of those elements which are lists
   are also transformed by sorting, and so on.  Thus:
   tot_sort([1,[a3,a2,a3,a1],2,1], [1,2,[a1,a2,a3]]). Recall that 
   sort eliminates duplicates, and so Out_list might be shorter 
   than In_list.  Tot_sort can be thought of as providing a
   normalized form for multi-level sets.  NG if arg1 is var.  */

tot_sort(In, Out) :-
  norm_elem(In, Normalized_list),
  sort(Normalized_list, Out).

/* norm_elem(In, Out) iff Out is the same list as In, except that
   any elements of In which are themselves lists, are transformed
   by tot_sorting.  NG if arg1 is var.  */

norm_elem([],[]).
norm_elem([In_elem | In_rest], [Out_elem | Out_rest]) :-
  (islist(In_elem) -> tot_sort(In_elem, Out_elem); In_elem = Out_elem),
  norm_elem(In_rest, Out_rest),
  !.

/* merge(L1, L2, L3) iff L3 is a random merge of L1 and L2, ie
   order is preserved within L1 and L2 but not between them.
   This is like a combination: pick 2 of 5.  Eg, for L1=[1,2,3],
   and L2=[a,b], L3 can be [1,2,a,3,b], [a,1,2,b,3],... 
   NG if L3 and (L1 or L2) are var.  */

merge([], X, X).

merge([E | R], [], [E | R]).

merge([E1 | R1], [E2 | R2], [E1 | R3]) :-   /* pick from L1 */
  merge(R1, [E2 | R2], R3).

merge([E1 | R1], [E2 | R2], [E2 | R3]) :-   /* pick from L2 */
  merge([E1 | R1], R2, R3).

/****************** Structures *****************/

/*  tree_position(Term, Subterm, Location) iff Term is a
    non-variable containing Subterm, at the Location, which
    is a list of numbers corresponding to position, eg, for
    Term = a(b,c,d(e,f),g), the solutions are:
 
    Subterm             Location
    -------             --------
    a(b,c,d(e,f),b)     []
    b                   [1]
    c                   [2]
    d(e,f)              [3]
    e                   [3,1]
    f                   [3,2]
    b                   [4]                    */

tree_position(Term, Sub, []) :- 
  nonvar(Term),
  Term = Sub.
tree_position(Term, Sub, [Number | Sub_pos]) :-
  Term =.. [Func | Args],
  position(Args, Elem, Number),
  tree_position(Elem, Sub, Sub_pos).

/* contains(Term, Sub) iff Term is a non-variable containing Sub,
   either immediately or indirectly.  NG if arg1 is var.  */

contains(Term, Sub) :- 
  nonvar(Term),
  Term = Sub.
contains(Term, Sub) :-
  Term =.. [Func | Args],
  member(Elem, Args),
  contains(Elem, Sub).

/* compound_contains(Term, Tester, Count) iff Count is the number of
   terms, compound or atomic, within Term for which Tester is true.
   Tester is either the name of a unary predicate, or a list of predicate
   name, followed by arguments.  */

compound_contains(Term, Tester, Count) :-
  (Tester = [Pred | Args] -> true;
			     (Pred = Tester, Args = [])),
  Test =.. [Pred, Term | Args],
  !,
  (Test -> N=1; N=0),
  (simple(Term) -> Count = N;
		   (Term =.. [Functor | Arglist],
		    compound_contains_list(Arglist, Pred, Args, Subcount),
		    Count is Subcount + N)).

compound_contains_list([], _, _, 0).
compound_contains_list([Elem | Rest], Pred, Args, Subcount) :-
  compound_contains(Elem, [Pred | Args], Elemcount),
  compound_contains_list(Rest, Pred, Args, Restcount),
  Subcount is Elemcount + Restcount.

/* atomic_contains(Term, Tester, Count) iff Count is the number of
   atomic terms or vars within Term for which Tester is true.  Tester is
   either the name of a unary predicate, or a list of predicate
   name, followed by arguments.  */

atomic_contains(Term, Tester, Count) :-
  (Tester = [Pred | Args] -> true;
			     (Pred = Tester, Args = [])),
  !,
  (simple(Term) -> (Test =.. [Pred, Term | Args],
		      (Test -> Count=1; Count=0)
		   );
		   (Term =.. Termlist,
		    atomic_contains_list(Termlist, Pred, Args, Count)
		   )).

atomic_contains_list([], _, _, 0).
atomic_contains_list([Elem | Rest], Pred, Args, Count) :-
  atomic_contains(Elem, [Pred | Args], Elemcount),
  atomic_contains_list(Rest, Pred, Args, Restcount),
  Count is Elemcount + Restcount.

/************** Input and Output **************/

/* readline(List) iff user enters characters of list, followed
   by a carriage return (whose ASCII code is 10, under VMS).  */

readline(List) :-
  get0(Char),
  (Char=10 -> List = [];
	      List = [Char | Rest], readline(Rest)).

/* tree_print(Term) prints term in tree-fashion, indented by two.  */

tree_print(Term) :-
  nl,
  tree_print_sub(Term,0,'  ').

tree_print_sub(Term, Depth, Prefix) :-
  print_prefix(Depth, Prefix),
  (var(Term) -> (Name = Term, Arglist = []);
		Term =.. [Name | Arglist]),
  print(Name), nl,
  Depth_plus is Depth+1,
  print_args(Arglist, Depth_plus, Prefix).

print_prefix(0,_).
print_prefix(Depth, Prefix) :-
  Depth > 0,
  print(Prefix),
  Depth_minus is Depth-1,
  print_prefix(Depth_minus, Prefix).

print_args([],             Depth, _).
print_args([First | Rest], Depth, Prefix) :-
  tree_print_sub(First, Depth, Prefix),
  print_args(Rest, Depth, Prefix).

/**************** Sets *****************/

/* Following predicates treat lists as sets.  Note that only
outer brackets are interpreted as set-constructors.  Any inner
nested brackets are interpreted as ordered lists - thus these
sets are "flat"; they do not contain other sets.  Eg, the
set: [a,b,[1,2,1]] has a list as its 3rd element and is
distinct from: [a,b,[2,1]], but *not* from [b,b,[1,2,1],a,a],
since duplication and order at the highest (set) level are
insignificant.

To treat lists as multi-level sets, perform tot_sort on them, eg
both [a,b,[1,2,1]] and [a,b,[2,1]] map to: [a,b,[1,2]], which
may be thought of as a normalized form for multi-level sets.

Thus, the only easy choice is to treat all inner brackets as
list-constructors (default) or as set-constructors (using
tot_sort).  To explicitly distinguish and therefore allow
both kinds, a structure must be set up, something like:
set(List) to be interpreted as a set.  Then, eg:

set([1,2,[d,d,c,a]])       3rd element is list

set([1,2,set([d,d,c,a])])  3rd element is set

[1,2,1]       (ordered) list of 3 integers

set([1,2,1])  (unordered) set of 2 integers (= set([1,2]))

Set structure would accept unordered/duplicate elements, but
assume their insignificance.  Conversion predicate might be:

set_list(Set, List) :- Set =.. [set, List].            */

/* subset(Part, Whole) iff Part is an subset of Whole.
   NG if both args are var.  */

subset(Part, Whole) :-
  var(Whole) -> (var(Part) -> fail;
			      subset_cv(Part, Whole)
		);
		(remove_dupl(Whole, Shorn_whole),
		 (var(Part) -> subset_vc(Part, Shorn_whole);
			       subset_cc(Part, Shorn_whole))
		).

subset_vc([Elem | Rest_part], [Elem | Rest_whole]) :- 
  subset_vc(Rest_part, Rest_whole).
subset_vc(        Rest_part,  [Elem | Rest_whole]) :- 
  subset_vc(Rest_part, Rest_whole).
subset_vc([],[]).

subset_cv(Part, Whole) :- remove_dupl(Part, Whole).

subset_cc([], Whole) :- islist(Whole).
subset_cc([Elem | Rest_of_part], Whole) :-
  member(Elem, Whole),
  subset_cc(Rest_of_part, Whole).

/* intersection(S1, S2, Ans) iff Ans is the intersection of S1 and S2.
   NG if arg1;2 is var.  */

intersection(S1, S2, Ans) :-
   remove_dupl(S1, Better_S1),
   (var(Ans) -> intersection_1(Better_S1, S2, Ans);
		intersection_2(Better_S1, S2, Ans)
   ).

intersection_1([], S2, []).
intersection_1([E | R1], S2, Ans) :-
   member(E, S2) -> (Ans = [E | RA],
		     intersection_1(R1, S2, RA)
		    );
		     intersection_1(R1, S2, Ans).

intersection_2(S1, S2, Ans) :-
   intersection_1(S1, S2, X),
   set_equal(X, Ans).

/* union(S1, S2, Ans) iff Ans is the union of S1 and S2.
   NG if arg1;2 is var.  */

union(S1, S2, Ans) :-
   var(Ans) -> union_1(S1, S2, Ans);
	       union_2(S1, S2, Ans).

union_1(S1, S2, Ans)  :- append(S1, S2, X),
			 remove_dupl(X, Ans).

union_2(S1, S2, Ans) :- union_1(S1, S2, X),
			set_equal(X, Ans).

/* set_diff(S1, S2, Ans) iff Ans is the set difference S1 - S2.
   NG if arg1;2 is var.  */

set_diff(S1, S2, Ans) :-
   remove_dupl(S1, Better_S1),
   (var(Ans) -> set_diff_1(Better_S1, S2, Ans);
		set_diff_2(Better_S1, S2, Ans)
   ).

set_diff_1(S1, S2, Ans) :- delete_all(S2, S1, Ans).

set_diff_2(S1, S2, Ans) :- set_diff_1(S1, S2, X),
			   set_equal(X, Ans).

/* set_equal(A,B) iff A and B contain the same elements (set equality). */

set_equal(A, B) :-
  nonvar(A), nonvar(B), set_equal_cc(A, B);
     var(A),    var(B), A = B;
     var(A), nonvar(B), remove_dupl(B, A);
  nonvar(A),    var(B), remove_dupl(A, B).

set_equal_cc(A, B) :- subset(A,B), subset(B,A).

/*  disjoint(A,B) iff A and B have no common element.  
    NG if arg1;2 is var. */

disjoint(A,B) :- not(joint(A,B)).

/*  joint(A,B) iff A and B have at least one common element.  
    NG if arg1;2 is var.  */

joint(A,B) :- member(E,A), member(E,B).

/* set_plus(S1, S2, Both) iff S1 and S2 are disjoint, and their
   union equals Both.  NG if Both and (S1 or S2) are var.  */

set_plus(S1, S2, Both) :-
  nonvar(S1) -> (nonvar(S2) -> set_plus_ccx(S1, S2, Both);
			       set_plus_vxc(S2, S1, Both)
		);
		set_plus_vxc(S1, S2, Both).

set_plus_ccx(S1, S2, Both) :- disjoint(S1, S2), union(S1, S2, Both).

set_plus_vxc(S1, S2, Both) :-
  nonvar(Both),
  subset(S2, Both),
  set_diff(Both, S2, S1).  

/* powerset(Set, Power) iff Power is the power set of Set, ie, a list
   of all subsets of Set.  NG if arg1 is var.  */

powerset(Set, Power) :-
  remove_dupl(Set, Shrunk_set),
  powerset_xx(Shrunk_set, Power).

powerset_xx([], [[]]).
powerset_xx([Elem | Rest], Power) :-
  powerset_xx(Rest, Sub_Power),
  double_list(Elem, Sub_Power, Power).

double_list(New_elem, [Single], [Single, [New_elem | Single]]) :- !.
double_list(New_elem, [Elem | Rest], [Elem, [New_elem | Elem] | List]) :-
  double_list(New_elem, Rest, List).

/* partition(S1,S2) iff S2 is a partition of S1, ie S2 is a set of
   non-null pairwise disjoint sets, whose union = S1.
   NG if both args are var.   */

partition(S1, S2) :-
  nonvar(S1), nonvar(S2),        partition_cc(S1,      S2);
     var(S1), nonvar(S2),        partition_vc(S1,      S2);
  nonvar(S1),    var(S2), 
       remove_dupl(S1, Slim_S1), partition_cv(Slim_S1, S2).

partition_vc(S1, S2) :-
  not(member([], S2)),      /* partition members must be non-null */
  flatten(S2, S1),          /* take all elements of all members   */
  no_dupls(S1).             /* ensure pairwise disjoint           */

partition_cc(S1, S2) :-
  partition_vc(Test, S2),
  set_equal(Test, S1).

partition_cv([Elem], [[Elem]]).
partition_cv([Elem | Rest], S2) :-
  partition_cv(Rest, Sub_S2),      /* take a partition of set minus elem */
  ( S2 = [[Elem] | Sub_S2];        /* either add a new singleton member 
				      containing elem  */
    (list_incr(Sub_S2_Elem, Sub_S2_Less_1, Sub_S2),
				   /* or take one of the old members     */
     S2 = [[Elem | Sub_S2_Elem] | Sub_S2_Less_1]
    )                              /* and add elem to it                 */
  ).

/* flatten(Plump, Flat) iff Plump is a list of lists, and Flat is the
   concatenation of those lists.  Flatten flattens out only one
   level, eg flatten([[a,b], [c, [d,e]]], [a,b,c,[d,e]]) is true.  
   NG if arg1 is var.  */

flatten([List], List) :- islist(List).
flatten([Elem | Rest_plump], Flat) :-
   flatten(Rest_plump, Rest_flat),
   append(Elem, Rest_flat, Flat).

/* closure2(List1, Pred2, List2) iff List2 is the closure of List1
   according to the binary predicate Pred2, for which the first
   operand is the "old" element and the second is the "new" or
   generated element.  NG if arg1 or arg2 is var. */

closure2(List1, Pred2, List2) :-
  sort(List1, Sorted_list1),
  setof(Arg2, Arg1^Test^(member(Arg1, Sorted_list1), 
			 Test =.. [Pred2, Arg1, Arg2],
			 Test),
	New_list),
  append(Sorted_list1, New_list, Combined_list),
  sort(Combined_list, Sorted_combined_list),
  (Sorted_combined_list = Sorted_list1 
       -> List2 = Sorted_combined_list;
	  closure2(Sorted_combined_list, Pred2, List2)).

/* closure3(List1, Pred3, List2) iff List2 is the closure of List1
   according to the ternary predicate Pred3, for which the first 2
   operands are the "old" elements and the third is the "new" or
   generated element.  NG if arg1 or arg2 is var. */

closure3(List1, Pred3, List2) :-
  sort(List1, Sorted_list1),
  setof(Arg3, Arg1^Arg2^Test^(member(Arg1, Sorted_list1),
			      member(Arg2, Sorted_list1),
			      Test =.. [Pred3, Arg1, Arg2, Arg3],
			      Test),
	New_list),
  append(Sorted_list1, New_list, Combined_list),
  sort(Combined_list, Sorted_combined_list),
  (Sorted_combined_list = Sorted_list1 
       -> List2 = Sorted_combined_list;
	  closure3(Sorted_combined_list, Pred3, List2)).

/***************** Numeric *****************/ 

/* numvar(X, Limit) iff X and Limit are non-negative integers,
   with X =< Limit.  In generative mode, solutions are produced
   from zero to higher values.  Either, both, or neither argument
   may be instantiated.  NG if arg1 is fraction and arg2 var.  */ 

numvar(X,Limit) :- var(Limit),    natural(Limit), numvar(X, Limit).
numvar(X,Limit) :- nonvar(Limit), natural(X), ((X>Limit, !, fail); true).

/* natural(X) iff X is a non-negative integer. */

natural(X) :- nonvar(X), integer(X), X>=0;
	      var(X), gen_integer(X, 0).

/* gen_integer(X,Seed) generates integers, incrementing from Seed.
   NG if arg1 is nonvar or arg2 is var.  */

gen_integer(X, Seed) :-
  var(X),
  integer(Seed),
  gen_integer_vc(X, Seed).

gen_integer_vc(X, Seed) :- 
  X is Seed;
  (New_seed is Seed+1,
   gen_integer_vc(X, New_seed)).

/* random(Max, N) instantiates N to a random integer between
   1 and Max.  NG if arg1 is var.  */

seed(13).

random(R,N) :-
	retract(seed(S)),
	N is (S mod R) +1,
	NewSeed is (125*S+1) mod 4096,
	asserta(seed(NewSeed)),!.

/* rationalize(Exprs, R_Exprs) iff Exprs is a numeric expression,
   and R_Exprs is a mostly evaluated form of the expression.
   Rational numbers are preserved and reduced to lowest terms,
   or integers if possible.  If no floating-point numbers are in
   the expression, the results are exact, except for raising to a
   fractional power.  NG if Exprs is not a fully instantiated
   numeric expression.  Routines for handling exponentiation
   override some C-Prolog defaults.  X^0 always equals 1, even for
   0^0.  A negative base is allowed with an integer power, so
   (-2)^4 = 16, and (-2)^5 = -32.  Negative powers are handled
   correctly, eg, 2^(-3) = 1/8, (-2)^(-3) = -1/8, (2/3)^(-3) =
   27/8. A negative base to a fractional power fails, as does zero 
   to a negative power. */

rationalize(Exprs, Exprs) :- number(Exprs), !.

/* handle unary ops */

rationalize(-(Exprs), R_Exprs) :- rationalize(Exprs*(-1), R_Exprs).

rationalize(+(Exprs), R_Exprs) :- rationalize(Exprs, R_Exprs).

/* all other unary ops to be handled by regular evaluation.  */

rationalize(Exprs, R_Exprs) :-
  Exprs =.. [Un_op, Opnd1],
  Un_op \== '+',
  Un_op \== '-',
  rationalize(Opnd1, R_Opnd1),
  Eval =.. [Un_op, R_Opnd1],
  R_Exprs is Eval.

/* handle binary ops */

rationalize(Exprs, R_Exprs) :-
  Exprs =.. [Bin_op, Opnd1, Opnd2],
  rationalize(Opnd1, R_Opnd1),
  rationalize(Opnd2, R_Opnd2),
  (plain_eval(Bin_op, R_Opnd1, R_Opnd2)
	-> (Eval =.. [Bin_op, R_Opnd1, R_Opnd2], R_Exprs is Eval, !);
	   (rationalize_1(Bin_op, R_Opnd1, R_Opnd2, R_Exprs), !)
  ).

/* plain_eval is true if the expression can be evaluated directly,
   either because: 1) the result will be exact (eg integer subtraction),
   or 2) the result is (probably) not rational anyway (eg, when
   a floating-point number is an operand, or raising to a fractional
   power).  */

plain_eval(Bin_op, R_Opnd1, R_Opnd2) :-
  not(member(Bin_op, [+,-,*,/,^])).

plain_eval(Bin_op, R_Opnd1, R_Opnd2) :-
  Bin_op \== '^',
  (float(R_Opnd1); float(R_Opnd2)).

plain_eval(Bin_op, R_Opnd1, R_Opnd2) :-
  integer(R_Opnd1), integer(R_Opnd2), 
  member(Bin_op, [+,-,*]).

plain_eval(^, Base, Power) :- 
  Base > 0,
  (not(integer(Power));
   Power =:= 0;
   float(Base);
   (number(Base), Power > 0)
  ).

rationalize_1(^, 0, Power, _) :-
  Power < 0,
  nl, print('Error: raising zero to negative power.'), nl,
  !, fail.

rationalize_1(^, 0, Power, 0) :-
  Power > 0.

rationalize_1(^, _, 0, 1).

rationalize_1(^, Base, Power, R_Exprs) :-
  Base < 0,
  not(integer(Power)),
  nl, print('Error: raising negative to fractional power.'), nl,
  !, fail.

rationalize_1(^, Base, Power, R_Exprs) :-
  Base < 0,
  float(Base),
  Mag is (-Base) ^ Power, !,
  (Power mod 2 =:= 0 -> R_Exprs is  Mag;
			R_Exprs is -Mag).

rationalize_1(Bin_op, R_Opnd1, R_Opnd2, R_Exprs) :-
  (integer(R_Opnd1) -> (Num1 = R_Opnd1, Den1 = 1);
			Num1/Den1 = R_Opnd1),
  (integer(R_Opnd2) -> (Num2 = R_Opnd2, Den2 = 1);
			Num2/Den2 = R_Opnd2),
  rationalize_2(Bin_op, Num1, Den1, Num2, Den2, Num, Den),
  (Den =:= 0 -> (nl, print('Error: divide by zero'), nl,
		 !, fail
		);
		reduced(Num/Den, R_Exprs)
  ),
  !.

rationalize_2(+, Num1, Den1, Num2, Den2, Num, Den) :-
  Num is Num1*Den2 + Num2*Den1,
  Den is Den1*Den2.

rationalize_2(-, Num1, Den1, Num2, Den2, Num, Den) :-
  Num is Num1*Den2 - Num2*Den1,
  Den is Den1*Den2.

rationalize_2(*, Num1, Den1, Num2, Den2, Num, Den) :-
  Num is Num1*Num2,
  Den is Den1*Den2.

rationalize_2(/, Num1, Den1, Num2, Den2, Num, Den) :-
  Num is Num1*Den2,
  Den is Den1*Num2.

rationalize_2(^, Num1, 1, Num2, 1, Num, Den) :-
  abs(Num1, Base),
  abs(Num2, Power),
  Mag is Base^Power,
  ((Num1 >= 0; Power mod 2 =:= 0) -> Sign = 1; Sign = -1),
  (Num2 < 0 -> (Num = Sign,      Den = Mag);
	       (Num is Sign*Mag, Den = 1)).

rationalize_2(^, Num1, Den1, Num2, 1, Num, Den) :-
  rationalize_2(^, Num1, 1, Num2, 1, NumNum, NumDen),
  rationalize_2(^, Den1, 1, Num2, 1, DenNum, DenDen),
  Num is NumNum*DenDen,
  Den is NumDen*DenNum,
  !.

/* reduced(X, Y) iff Y is X reduced to lowest terms.  Y is an
   integer if the denominator would be 1.  NG if arg1 is var.  */

reduced(X,X) :- integer(X), !.
reduced(N1/D1, Ans) :-
  N1 mod D1 =:= 0,
  Ans is N1/D1, !.
reduced(N1/D1, N2/D2) :-
  gcd(N1,D1,D),
  abs(D, AbsD),
  N2 is N1/AbsD,
  D2 is D1/AbsD, !.

/* gcd(X, Y, Ans) iff Ans is the greatest common divisor of the
   integers X and Y.  NG if arg1 or arg2 is var.  */

gcd(X, 0, X) :- !.
gcd(0, X, X) :- !.
gcd(X, Y, Ans) :-
  L is Y mod X,
  gcd(L, X, Ans), !.

/* abs(X, Y) iff Y is the absolute value of X.  Ng if arg1 is var.  */

abs(X,Y) :-
  X < 0 -> Y is -X; Y is X.

/****************** Control *****************/

/* loop(Pred) repeatedly invokes Pred until it fails.  Loop always
   fails and so is executed only for side-effects.  If Pred has
   several clauses, enclose in parens.  A typical use might be:
      loop((pred(X,Y), write(X), write('  '), write(Y), nl)).
   to write out all solutions.  */

loop(Pred) :-
  repeat,
  (Pred; (!, fail)),     /*  Pred succeeds, or kill loop  */
  fail.                  /*  Force repetition.            */

/* invoke(Pred_list) creates a predication from Pred_list and
   invokes it.  Pred_list must be instantiated.  The reason for
   the 2nd arg is that otherwise invoke will quit after first
   success with all nonvars.  Even if all args are nonvar, 
   users may wish to re-invoke by re-instantiating Pred_list.  */

invoke(Pred_list, Pred_call) :- Pred_call=..Pred_list, Pred_call.

/************** Logic and Meta-Logic ***************/

/* counter_eg(If, Then) iff there exists some instantiation of If
   and Then such that If is true and Then is false.  If and Then
   must be predicates.  Their arg-lists may contain vars, but the
   predicates themselves must be specified.  Eg:
   counter_eg((gender(X,male), gender(Y,female)), taller(X,Y)).
   may be understood as "Are there any counter-examples to the rule
   that if X is male and Y is female, then X is taller than Y?"  */

counter_eg(If, Then) :- If, not(Then).

/* implies(If, Then) iff If and Then form a true implication for
   this DB, ie there are no counter-examples.  */

implies(If, Then) :- not(counter_eg(If, Then)).

/* The following stuff is meant to provide some capability for expressing
   disjunction and negation.  The three predicates visible at the user
   level are:

   or(L) - to ask if this is a true disjunction; defined herein, but
	   invoked by the user.

   ground_or(L) - to tell the system that this is a true disjunction,
	   ie, at least one disjunct is true.  Defined by the user
	   as part of the DB.

   false(P) - says that P is definitely false (not just not provable,
	   a la "not" in normal Prolog).  Defined by the user as part 
	   of the DB. 

   start_or. - the user has to invoke this to fire up the system,
	   before he starts asking things.
 
   eg, if the user, in the DB, says:

   ground_or([e,f,g,h,i]).
   false(f).
   false(g).
   ground_or([a,b,c]) :- d.
   d.
   false(b).
   false(a) :- w; d.

   and then invokes start_or, the system will be able to conclude 
   that the following succeed:

   c.                    (because (a or b or c) is true and a and b are 
			  both false)
   or([x1,d,x2]).        (because d is a true disjunct)
   or([f,g,t,h,r,e,i]).  (because it contains a true disjunct: [e,f,g,h,i]).
   or([e,h,i])           (because its residue from [e,f,g,h,i] is [f,g] all 
			  of which are false.)

   It's probably not wise for "ground_or" itself to depend on "or".

*/

/* or(L) iff L is a list of disjuncts, at least one of which is true.  
   NG if L is var.  */

or(L) :- nonvar(L), L = [_|_], (or_1(L); or_2(L); or_3(L)).

/* or_1 tries to find a true individual disjunct.
   or_2 tries to find a subset of L already known to be true. 
   or_3 tries to find a superset of L already known to be true,
	and then show the rest are false.  */

or_1([E | R]) :- E; or_1(R).
or_2(L1)      :- ground_or(L2), subset(L2, L1).
or_3(L)       :- ground_or(Ground_list),
		 set_plus(L, Residue, Ground_list),
		 false_list(Residue).

/* false_list(L) iff all members of L are provably false. */

false_list([]).
false_list([E1 | Rest]) :- false(E1), false_list(Rest).

/* start_or always fails, but in the meantime, it builds clauses
   for each disjunct of the ground_or's.  */

start_or :- 
  clause(ground_or(Disjuncts), Antecedents),
  set_plus([One_disjunct], Rest, Disjuncts),
  assertz((One_disjunct :- Antecedents, false_list(Rest))),
  fail.   

/* Some meta-logical facilities coming up.  */

/* kb_object(Type, Head, Tail) iff there is a clause "Head :- Tail."
   in the current program.  If the Tail=true, and the Head is
   composed of all constants, Type = fact, otherwise Type = rule.  */


kb_object(Type, Head, Tail) :-
  current_predicate(_, Head),
  clause(Head, Tail),
  ((Tail = true, constant_term(Head)) -> Type = fact;
					 Type = rule).

/* constant_term(Term) iff Term is currently instantiated and all
   of its arguments, sub-arguments, etc. are constant. */

constant_term(Term) :- 
  atomic(Term) -> true;
		  (nonvar(Term),
		   Term =.. [Functor | Args],
		   all(constant_term, Args)).

/*  End of Prolog Utilities */


