/*  CUGINI.PL
    Shelved on the 12th of December 1987.
    Updated on the 30th of July, 1988, with a new copy from John Cugini.                 
*/


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   */
/*                                                                         */
/*     NBS/ICST Prolog Utility Library                                     */
/*     version date: December 8, 1987                                      */
/*                                                                         */
/*     developed by:                                                       */
/*                                                                         */
/*     John Cugini <cugini@icst-ecf.arpa>                                  */
/*     Institute for Computer Sciences and Technology                      */
/*     National Bureau of Standards                                        */
/*                                                                         */
/*     Product of US Government: not subject to copyright                  */
/*                                                                         */
/*  This file contains various utility predicates, some commonly used,     */
/*  some not.  They deal with lists, structures, I/O, sets, numeric        */
/*  facilities, and some extensions of logic and control.  This library    */
/*  is written in and for the C-Prolog dialect of Prolog.                  */
/*                                                                         */
/*  Many of these predicates expect certain of their arguments to be       */
/*  instantiated upon invocation.  When such restrictions apply it is      */
/*  usually the leading arguments which are thought of as input (and       */
/*  hence instantiated), and the trailing arguments as output (and hence   */
/*  allowed to be uninstantiated).                                         */
/*                                                                         */
/*  There is a coding convention: the user-callable version of the         */
/*  predicate has a plain name.  If this predicate needs sub-predicates,   */
/*  based on whether certain arguments are instantiated or not, the names  */
/*  of the sub-predicates are formed by appending a string of c,v, or      */
/*  x's, where c indicates argument must be constant (instantiated), v     */
/*  that it must be a variable, and x that it may be either.               */
/*                                                                         */
/*  Further, each main predicate is preceded by documentation lines,       */
/*  which describe the declarative meaning of the predicate, and which     */
/*  arguments must be instantiated.                                        */
/*                                                                         */
/*  The overall organization of the library is:                            */
/*                                                                         */
/*      Basic predicates                                                   */
/*      Lists                                                              */
/*      Structures                                                         */
/*      Input/Output                                                       */
/*      Sets                                                               */
/*      Numeric                                                            */
/*      Control                                                            */
/*      Extended Logic                                                     */
/*                                                                         */
/*  Each section is prefaced by a header with lots of asterisks            */
/*                                                                         */
/*  [JNP...]                                                               */
/*  I've replaced %% comments by star-slash ones. To undo the replacement  */
/*  globally edit out all star-slashes, and replace all slash-stars by     */
/*  percents.                                                              */
/*                                                                         */
/*  I've also replaced all ~ by ^ (they were used for exponentiation).     */
/*  That they were ~ may have been a character translation occurring in    */
/*  file transfer from John, since they were ^ anyway in the older version */
/*  that Bert Shure sent me.                                               */
/*                                                                         */
/*  And I've added to 'delete' a change that Bert Shure made to the older  */
/*  version.                                                               */
/*                                                                         */
/*  Some Prologs may lack the (X->Y;Z) construction which this library     */
/*  uses. You can define it by:                                            */
/*      (X -> Y); Z  :-  X, !, Y.                                          */
/*      (X -> Y); Z  :- !, Z.                                              */
/*                                                                         */
/*  Other problems you may have with portability:                          */
/*                                                                         */
/*  (1)  'float' assumes a predicate called number(T), which succeeds iff  */
/*  its argument is a number (integer or real). Its name may be different  */
/*  on other systems.                                                      */
/*                                                                         */
/*  (2)  character codes 10 (end-of-line) and 26 (end-of-file) in          */
/*  'readline'.                                                            */
/*                                                                         */
/*  (3)  character codes in 'print_string'.                                */
/*                                                                         */
/*  (4)  put(46) and the filename in 'ed'.                                 */
/*                                                                         */
/*  (5)  the filename in 'full_name'.                                      */
/*                                                                         */
/*  [...JNP]                                                               */
/*                                                                         */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


/*  ************ Basic Predicates *************                            */

/*  The following predicates test the type of the term passed              */
/*  to them, using the terminology of the C-Prolog manual.                 */

term(Term).
simple(Term)   :- atomic(Term); var(Term).
compound(Term) :- not(simple(Term)).
literal(Term)  :- nonvar(Term),
                  functor(Term, Name, _),
                  atom(Name).
float(Term)    :- number(Term), not(integer(Term)).
rational(Term) :- integer(Term); ratio(Term).
ratio(Num/Den) :- integer(Num), integer(Den).

/*  constant_term(Term) iff Term is currently instantiated and all         */
/*  of its arguments, sub-arguments, etc. are constant.                    */

constant_term(Term) :-
  atomic(Term) -> true;
                  (nonvar(Term),
                   Term =.. [Functor | Args],
                   all(constant_term, Args)).

/*  instantiation(Term, Type) iff Type describes the instantiation state   */
/*  of Term: constant (completely ground), partial, or var.                */

instantiation(Term, Type) :-
  var(Term)           -> Type = var;
  constant_term(Term) -> Type = constant;
                         Type = partial.

/*  ************* Lists **************                                     */

/*  islist(X) iff X is a list.  (Really just checks for [] or that         */
/*  main functor is '.').                                                  */

islist([]) :- !.
islist([_|_]).

/*  is_real_list(X) iff X is a properly formed list.  Does not allow       */
/*  "dotted" lists, a la [a|b], as does islist.                            */

is_real_list([]) :- !.
is_real_list([_ | Rest]) :- is_real_list(Rest).

/*  non_null_list(List) iff List is a real list containing at least        */
/*  one element.                                                           */

non_null_list([_ | Rest]) :- is_real_list(Rest).

/*  member(Elem, List) iff Elem is a member of List.                       */

member(Elem, [Elem | _]).
member(Elem, [_ | Rest_of_list]) :- member(Elem, Rest_of_list).

/*  member_rest(Elem, List, Rest) iff Elem is a member of List and         */
/*  Rest is the rest of the list following Elem.                           */

member_rest(Elem, [Elem | Rest], Rest).
member_rest(Elem, [_ | Rest], Rest_rest) :-
  member_rest(Elem, Rest, Rest_rest).

/*  append(First_part, Second_part, List) iff List is the                  */
/*  concatenation of the first two arguments.                              */

append([], List, List).
append([Elem | First_part], Second_part, [Elem | List]) :-
  append(First_part, Second_part, List).

/*  append_n(L1, L2) iff L1 is a list of lists, which, if concatenated,    */
/*  is L2.  When L1 is var, no null lists are generated for it, even       */
/*  though logically they could appear anywhere.  Thus, this isn't         */
/*  symmetric, in that L1=[[],[1]] will generate L2=[1], but not           */
/*  vice-versa.  Note especially that L1=[[]] will generate L2=[], but     */
/*  not the reverse.  L1 and L2 can be partially instantiated, and this    */
/*  usually works.  NG if both args are var.                               */

/* avoid generating the null list in arg1.                                 */

append_n([List1], List2) :-
  not((var(List1), List2 == [])),
  List1=List2.

append_n([SL1, SL2 | Rest], List) :-
  (var(SL1) -> V=t; V=f),
  append(SL1, Right_part, List),
  not((V=t, SL1=[])),
  append_n([SL2 | Rest], Right_part).

/*  delete(Elem, Old_list, New_list) iff New_list equals Old_list except   */
/*  for the removal of all occurrences of Elem.  NG if arg2 is var.        */
/* The "true" in the else part of the "if-then" was added by Bert Shure on */
/*   July 7, 87 - to avoid an error from the BIM_Prolog compiler           */

delete(Elem, Old, New) :-
   (var(Elem) -> (remove_dupl(Old, Smaller_old),
                  member(Elem, Smaller_old)
                 );
                  true /*Elem=X        dummy statement to fill else-part   */
   ),
   delete_c(Elem, Old, New).

delete_c(_, [], []).
delete_c(Elem1, [Elem2 | Rest_of_old], New_list) :-
   Elem1 = Elem2 -> (delete_c(Elem1, Rest_of_old, New_list)
                    );
                    (New_list = [Elem2 | Rest_of_new],
                     delete_c(Elem1, Rest_of_old, Rest_of_new)
                    ).

/*  delete_all(Del_list, Old_list, New_list) iff New_list equals Old_list  */
/*  except for the removal of any occurrences of any elements of Del_list. */
/*  NG if arg1 or 2 is var.                                                */

delete_all([], L, L).
delete_all([E | R_del], Old_list, New_list) :-
   delete(E, Old_list, M),
   delete_all(R_del, M, New_list).

/*  remove_dupl(List, Shriven_list) iff Shriven_list equals List           */
/*  in same order, sans leading duplicate members.  Ie, only the           */
/*  rightmost of duplicate members remain.  NG if arg1 is var.             */

remove_dupl([],[]).
remove_dupl([Elem | Rest_list], Rest_shriven) :-
  member(Elem, Rest_list),
  !,
  remove_dupl(Rest_list, Rest_shriven).
remove_dupl([Elem | Rest_list], [Elem | Rest_shriven]) :-
  remove_dupl(Rest_list, Rest_shriven).

/*  no_dupls(List) iff List is a list with no duplicate elements.          */
/*  NG if arg1 is var.                                                     */

no_dupls([]).
no_dupls([Elem | Rest]) :-
   not(member(Elem, Rest)),
   no_dupls(Rest).

/* ordered(List) iff List is a list whose elements are in non-             */
/* decreasing order.  NG if List is var.                                   */

ordered([]).
ordered([Elem]).
ordered([Elem1, Elem2 | Rest]) :-
  Elem1 @=< Elem2,
  !,
  ordered([Elem2 | Rest]).

/*  last(Elem, List) iff Elem is the last element in List.                 */

last(Elem, [Elem]).
last(Elem, [_ | Rest]) :- last(Elem, Rest).

/*  next_to(X,Y,L) iff X and Y are adjacent in list L.                     */

next_to(X, Y, [X,Y | _]).
next_to(X, Y, [_ | Rest]) :- next_to(X, Y, Rest).

/*  precedes(A,B,List) iff A and B are both members of List and A precedes B*/
/*  within the List.  NG if List is var.                                   */

precedes(A,B,List) :-
  member_rest(A,List,Rest),
  member(B,Rest).

/*  succeeds(A,B,List) iff A and B are both members of List and A follows B*/
/*  within the List.  NG if List is var.                                   */

succeeds(A,B,List) :-
  precedes(B,A,List).

/*  naive_reverse(List1, List2) iff List1 is List2 in reverse order.       */
/*  naive_reverse takes n^2 + 3*n + 2 steps for a list of n elements.      */
/*  NG if arg1 is var.                                                     */

naive_reverse([], []) :- !.
naive_reverse([Head | Tail], List) :-
  naive_reverse(Tail, Liat), append(Liat, [Head], List).

/*  reverse(List1, List2) iff List1 is List2 in reverse order.             */
/*  reverse takes 2*n + 2 steps for a list of n elements.                  */
/*  NG if both args are var.                                               */

reverse(List1,List2) :- reverse_1(List1, [], List2).

reverse_1([], List2, List2) :- !.
reverse_1([Head | Tail], So_far, List2) :-
  reverse_1(Tail, [Head | So_far], List2).

/*  efface(Elem, Old_list, New_list) iff New_list = Old_list               */
/*  with first occurrence of Elem removed.  NG if more than                */
/*  one arg is var.                                                        */

efface(Elem, [Elem | Rest], Rest) :- !.
efface(Elem, [Non_elem | Old_rest], [Non_elem | New_rest]) :-
  not(Elem = Non_elem),
  efface(Elem, Old_rest, New_rest).

/*  insert(Elem, List, Bigger_list) iff Bigger_list = List plus            */
/*  Elem inserted somewhere.  This can also be used to select              */
/*  non-deterministically Elem from Bigger_list and return Elem            */
/*  and the remaining List.  NG if all args are var.                       */

insert(Elem, List, [Elem | List]).
insert(Elem, [Non_elem | List], [Non_elem | Bigger_list]) :-
   insert(Elem, List, Bigger_list).

/*  subst(Old_elem, Old_list, New_elem, New_list) iff New_list equals      */
/*  Old_list except for the substitution of New_elem for any               */
/*  occurrences of Old_elem.  NG for arg1;2;3 var.                         */

subst(_, [], _, []).
subst(Old_elem, [Old_elem | Rest_of_old],
      New_elem, [New_elem | Rest_of_new]) :-
         !, subst(Old_elem, Rest_of_old, New_elem, Rest_of_new).
subst(Old_elem, [Non_elem | Rest_of_old],
      New_elem, [Non_elem | Rest_of_new]) :-
            subst(Old_elem, Rest_of_old, New_elem, Rest_of_new).

/*  prefix(Part, Whole) iff Part is a leading substring of Whole.          */

prefix([], _).
prefix([Elem | Rest_of_part], [Elem | Rest_of_whole]) :-
  prefix(Rest_of_part, Rest_of_whole).

/*  suffix(Part, Whole) iff Part is a trailing substring of Whole.         */

suffix(List, List) :- islist(List).
suffix(Part, [Elem | Rest_of_whole]) :- suffix(Part, Rest_of_whole).

/*  trim(List, Elem, Ans) iff Ans is List with all leading and             */
/*  trailing occurrences of Elem removed.  NG if arg1 or arg2              */
/*  is var.                                                                */

trim(List, Elem, Ans) :-
  trim_left(List, Elem, Temp),
  trim_right(Temp, Elem, Ans).

/*  trim_left(List, Elem, Ans) iff Ans is List with all leading            */
/*  occurrences of Elem removed.  NG if arg1 or arg2 is var.               */

trim_left([Elem | Rest], Elem, Ans) :-
  trim_left(Rest, Elem, Ans), !.
trim_left(List, _, List).

/*  trim_right(List, Elem, Ans) iff Ans is List with all trailing          */
/*  occurrences of Elem removed.  NG if arg1 or arg2 is var.               */

trim_right([],  _, [])  :- !.
trim_right([Elem | Rest], Elem, []) :-
  trim_right(Rest, Elem, []), !.
trim_right([Arb | Rest], Elem, [Arb | Rest_Trim]) :-
  trim_right(Rest, Elem, Rest_Trim).

/*  sublist(List, Start, End, Sublist) iff Sublist is a contiguous         */
/*  sub-list within List, starting at position Start, and ending at        */
/*  position End.  Note that [] is a valid sublist, so for                 */
/*  List = [1,2], valid solutions are:                                     */
/*                                                                         */
/*         Start  End   Sublist                                            */
/*         1      0     []                                                 */
/*         1      1     [1]                                                */
/*         1      2     [1,2]                                              */
/*         2      1     []                                                 */
/*         2      2     [2]                                                */
/*         3      2     []                                                 */
/*                                                                         */
/*  NG if arg1 is var.                                                     */

sublist(List, Start, End, Sublist) :-
  prefix(Sublist, List),
  Start = 1,
  length(Sublist, End).

sublist([Elem | Rest], Start, End, Sublist) :-
  sublist(Rest, Startx, Endx, Sublist),
  Start is Startx + 1,
  End is Endx + 1.

/*   matchlist(List1, List2, Common, New1, New2) iff List1 and             */
/*   List2 are sorted instantiated lists (possibly with repetitions)       */
/*   and Common is a list of their matching elements, and New1 and New2    */
/*   are List1 and List2 minus the matching elements in Common, eg:        */
/*   matchlist([1,2,3,3,3,4,5,5,6], [3,3,4,4,5,5], [3,3,4,5,5],            */
/*             [1,2,3,6],           [4]) is true.  NG if Arg1 or Arg2      */
/*   is var or unsorted.                                                   */

matchlist([], List2, [], [], List2) :- !.

matchlist([Elem | Rest], [], [], [Elem | Rest], []) :- !.

matchlist([Elem | Rest1], [Elem | Rest2], [Elem | Com_Rest], New1, New2) :-
  matchlist(Rest1, Rest2, Com_Rest, New1, New2), !.

matchlist([El1 | Rest1], [El2 | Rest2], Com, New1, New2) :-
  (El1 @< El2 ->
     (matchlist(Rest1, [El2 | Rest2], Com, New_Rest1, New2),
                       New1 = [El1 | New_Rest1]);
     (matchlist([El1 | Rest1], Rest2, Com, New1, New_Rest2),
                       New2 = [El2 | New_Rest2])
  ),
  !.


/*  list_length(List, Number) iff List has Number elements.                */
/*  NG if arg1 is var.  Note that, apparently, C-Prolog has an             */
/*  undocumented evaluable predicate, length(List, Number).                */

list_length([], 0).
list_length([Elem | Rest], Number) :-
  list_length(Rest, N_minus),
  Number is N_minus + 1.

/*  position(List, Elem, Number) iff Elem is in position Number            */
/*  in the List.  NG if arg1 is var.                                       */

position([Elem | Rest], Elem, 1).
position([_    | Rest], Elem, Number) :-
   var(Number) -> (position(Rest, Elem, N_minus),
                   Number is N_minus + 1);
                  (N_minus is Number - 1,
                   position(Rest, Elem, N_minus),
                   !).

/*  repeat_list(Elem, Number, List) iff List is a list of Elem             */
/*  repeated Number times.  No var restrictions.                           */

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

/*  circular_list(List, Circle) iff Circle is a circular (infinite) list   */
/*  formed from List, eg. if List = [1,2,3], Circle = [1,2,3,1,2,3,1,2,...].*/
/*  Exercise extreme caution - do not try to print Circle, nor rely on it  */
/*  to terminate recursion.  NG if arg1 is var.                            */

circular_list(List,Circle) :-
  de_tail(List, Circle, Tail),
  Tail = Circle.

/*  de_tail(List1, List2, Tail) iff List1 = List2, except for the          */
/*  terminating [] in List1, which is replaced by the uninstantiated       */
/*  variable Tail in List2.                                                */

de_tail([Elem], [Elem | Tail], Tail) :- !.
de_tail([Elem | Rest], [Elem | Var_rest], Tail) :-
  de_tail(Rest, Var_rest, Tail).

/*  permute(List1, List2) iff List1 is a permutation of List2.             */
/*  NG if arg1 is var.                                                     */

permute(Whole, [Elem | Rest_of_part]) :-
     insert(Elem, Reduced_whole, Whole),
     permute(Reduced_whole, Rest_of_part).
permute([], []).

/*  reduce_left(Bin_op, List, Ans) iff Ans is the result of applying       */
/*  Bin_op, left-associatively, to the elements of List.   NG if           */
/*  arg1 or arg2 is var, or if List contains fewer than two                */
/*  elements.  Bin_op must take three arguments and return its             */
/*  result in the third, based on the value of the first two.              */
/*  If ID is an identity element for Bin_op, and the List                  */
/*  may contain only one element, invoke with:                             */
/*  reduce_left(Bin_op, [ID | List], Ans).   To allow a null list to       */
/*  return ID, invoke with: reduce_left(Bin_op, [ID, ID | List], Ans).     */

reduce_left(Bin_op, [El1, El2 | Rest], Ans) :-
  Callit =.. [Bin_op, El1, El2, Temp],
  Callit,
  (Rest = [] ->
     Ans = Temp;
     reduce_left(Bin_op, [Temp | Rest], Ans)
  ).

/*  reduce_right(Bin_op, List, Ans) - just like reduce_left, except        */
/*  it's right-associative.                                                */

reduce_right(Bin_op, [El1, El2], Ans) :-
  Callit =.. [Bin_op, El1, El2, Ans],
  Callit,
  !.

reduce_right(Bin_op, [Elem | Rest], Ans) :-
  reduce_right(Bin_op, Rest, Sub_ans),
  Callit =.. [Bin_op, Elem, Sub_ans, Ans],
  Callit.

/*  maplist(Pred, Old, New) iff for each corresponding element in          */
/*  Old and New, Pred(Old, New) is true.  NG if arg1 is var.               */

maplist(_, [], []).
maplist(Pred, [Elem_old | Rest_of_old], [Elem_new | Rest_of_new]) :-
  Pred_call =.. [Pred, Elem_old, Elem_new],  /*   constructs  predicate    */
  Pred_call,                         /*   invokes constructed predicate    */
  maplist(Pred, Rest_of_old, Rest_of_new).

/*  maplist_2(Pred, L1, L2, L3) iff L1, L2, and L3 are lists of equal length*/
/*  and for each element (E1, E2, E3) in corresponding positions in the    */
/*  lists, Pred(E1, E2, E3) is true.  Thus, E3 should be a function of E1  */
/*  and E2 - if there is more than one solution, only the first will be    */
/*  used.  NG if arg1 is var.  Whether arg2 or arg3 can be var depends on  */
/*  the nature of Pred.                                                    */

maplist_2(_, [], [], []).
maplist_2(Pred, [Elem1 | Rest1], [Elem2 | Rest2], [Elem3 | Rest3]) :-
  Pred_call =.. [Pred, Elem1, Elem2, Elem3],
  Pred_call,
  !,
  maplist_2(Pred, Rest1, Rest2, Rest3).

/*  for_all(Op_list, Pre_list, Post_list) iff a predicate is               */
/*  successfully invoked for each member of Op_List.  Each                 */
/*  predicate is formed by pre-pending Pre_list to a member                */
/*  of Op_List, appending Post_list to it, and then forming                */
/*  the corresponding functor.                                             */
/*                                                                         */
/*  Thus, for_all([a,b,c], [wiggle,x], [y]) will invoke:                   */
/*       wiggle(x, a, y)                                                   */
/*       wiggle(x, b, y)                                                   */
/*       wiggle(x, c, y).                                                  */
/*                                                                         */
/*  and for_all([number, atomic], [], [A]) will invoke:                    */
/*       number(A)                                                         */
/*       atomic(A).                                                        */

for_all([], _, _).
for_all([Op | Rest_Ops], Pre_list, Post_list) :-
  append(Pre_list, [Op | Post_list], Pred_list),
  Pred =.. Pred_list,
  !,
  Pred,
  for_all(Rest_Ops, Pre_list, Post_list).

/*  maxlist(List, Max) iff Max is the highest value in a List of numbers.  */
/*  NG if arg1 is var.                                                     */

maxlist([Elem], Elem).
maxlist([Elem | Rest], Max) :-
   maxlist(Rest, Rmax),
   (Elem > Rmax -> Max = Elem; Max = Rmax).

/*  minlist(List, Min) iff Min is the lowest value in a List of numbers.   */
/*  NG if arg1 is var.                                                     */

minlist([Elem], Elem).
minlist([Elem | Rest], Min) :-
   minlist(Rest, Rmin),
   (Elem < Rmin -> Min = Elem; Min = Rmin).

/*  bestlist(List, Pred, Best) iff Best is the best value in a List,       */
/*  according to some binary predicate Pred(A,B), which succeeds iff       */
/*  A is better than B.  NG if arg1 or arg2 is var.                        */

bestlist([Elem], _, Elem).
bestlist([Elem | Rest], Pred, Best) :-
   bestlist(Rest, Pred, Rbest),
   Call =.. [Pred, Elem, Rbest],
   (Call -> Best = Elem; Best = Rbest).

/*  all(Pred, List) iff Pred is a single-place predicate true of           */
/*  all members of the list.  NG if either arg is var.                     */

all(Pred, []) :- atom(Pred).

all(Pred, [Elem | Rest]) :-
  Callit =.. [Pred, Elem],
  Callit,
  all(Pred, Rest).

/*  some(Pred, List) iff Pred is a single-place predicate true of          */
/*  at least one member of the list.  NG if either arg is var.             */

some(Pred, List) :- atom(Pred), some_cx(Pred, List).

some_cx(Pred, [Elem | Rest]) :-
  (Callit =.. [Pred, Elem],
   Callit);
  some_cx(Pred, Rest).

/*  notall(Pred, List) iff Pred is a single-place predicate false of       */
/*  at least one member of the list.  NG if either arg is var.             */

notall(Pred, List) :- atom(Pred), not(all(Pred,List)).

/*  none(Pred, List) iff Pred is a single-place predicate true of          */
/*  none of the members of the list.  NG if either arg is var.             */

none(Pred, List) :- atom(Pred), not(some(Pred,List)).

/*  all_same(List) iff all the elements of the list are identical, as      */
/*  tested by the '==' operator.                                           */

all_same([]) :- !.
all_same([Elem]) :- !.
all_same([E1, E2 | Rest]) :-
  E1 == E2,
  all_same([E2 | Rest]).

/*  all_diff(List) iff no two elements of the list are identical, as       */
/*  tested by the '==' operator.                                           */

all_diff(List) :-
  not(matching_pair(List, Matcher)).

/*  matching_pair(List, Matcher) iff there is a matching pair of elements, */
/*  E1 and E2 within List such that E1==E2==Matcher.                       */

matching_pair([Elem | Rest], Matcher) :-
  (member(Matcher, Rest),
   Elem == Matcher);
  matching_pair(Rest, Matcher).

/*  string_of(Alphabet, String) iff String is a list composed of           */
/*  elements of the non-redundant non-null Alphabet.                       */
/*  NG if both args are var.                                               */

string_of(Alphabet, String) :-
  var(Alphabet) -> remove_dupl(String, Alphabet);
                   (Alphabet = [_|_],       /*   ie non-null               */
                    no_dupls(Alphabet),
                    string_of_cv(Alphabet, String)
                   ).

string_of_cv(Alphabet, []).
string_of_cv(Alphabet, [Elem | Rest]) :-
  string_of_cv(Alphabet, Rest),
  member(Elem, Alphabet).

/*  sort_all(In_list, Out_list) iff Out_list is a sorting of In_list,      */
/*  saving duplicates.  The Xing allows the use of keysort, which          */
/*  doesn't delete duplicates.                                             */

sort_all(In, Out) :-
  sort_all_xit(In, Xed_list),
  keysort(Xed_list, Sorted_xed_list),
  sort_all_xit(Out, Sorted_xed_list).

/*  sort_all_xit(List, Xed_list) iff each element of list is matched       */
/*  by an xed element in Xed_list.                                         */

sort_all_xit([],[]).
sort_all_xit([Elem | In_rest], [Elem-x | Out_rest]) :-
  sort_all_xit(In_rest, Out_rest), !.

/*  tot_sort(In_list, Out_list) iff Out_list is a total sorting            */
/*  of In_list.  This means that not only are the elements of              */
/*  In_list sorted, but that any of those elements which are lists         */
/*  are also transformed by sorting, and so on.  Thus:                     */
/*  tot_sort([1,[a3,a2,a3,a1],2,1], [1,2,[a1,a2,a3]]). Recall that         */
/*  sort eliminates duplicates, and so Out_list might be shorter           */
/*  than In_list.  Tot_sort can be thought of as providing a               */
/*  normalized form for multi-level sets.  NG if arg1 is var.              */

tot_sort(In, Out) :-
  norm_elem(In, Normalized_list),
  sort(Normalized_list, Out).

/*  norm_elem(In, Out) iff Out is the same list as In, except that         */
/*  any elements of In which are themselves lists, are transformed         */
/*  by tot_sorting.  NG if arg1 is var.                                    */

norm_elem([],[]).
norm_elem([In_elem | In_rest], [Out_elem | Out_rest]) :-
  (islist(In_elem) -> tot_sort(In_elem, Out_elem); In_elem = Out_elem),
  norm_elem(In_rest, Out_rest),
  !.

/*  ordered_merge(List1, List2, List3) iff List3 is the ordered merging    */
/*  of List1 and List2.  If more than one arg is var, returns only a       */
/*  single solution.  To find all decompositions of a list into two        */
/*  lists, use merge.  NG if all args are var.                             */

ordered_merge([], List, List) :-
  islist(List),
  !.

ordered_merge([Elem1 | Rest1], [], [Elem1 | Rest1]) :- !.

ordered_merge([Elem1 | Rest1], [Elem2 | Rest2], [Elem1 | Rest3]) :-
  (var(Elem2) -> true; Elem1 @< Elem2),
  !,
  ordered_merge(Rest1, [Elem2 | Rest2], Rest3).

ordered_merge(List1, [Elem2 | Rest2], [Elem2 | Rest3]) :-
  ordered_merge(List1, Rest2, Rest3).

/*  merge(List1, List2, List3) iff List3 is a random merge of List1 and    */
/*  List2, ie, order is preserved within List1 and List2 but not between   */
/*  them.  This is like a combination: pick 2 of 5.  Eg, for               */
/*  List1=[1,2,3], and List2=[a,b], List3 can be [1,2,a,3,b],              */
/*  [a,1,2,b,3],... NG if List3 and (List1 or List2) are var.              */

merge([], List, List) :- islist(List).

merge([Elem | Rest], [], [Elem | Rest]).

/*   pick from List1                                                       */
merge([Elem1 | Rest1], [Elem2 | Rest2], [Elem1 | Rest3]) :-
  merge(Rest1, [Elem2 | Rest2], Rest3).

/*   pick from List2                                                       */
merge([Elem1 | Rest1], [Elem2 | Rest2], [Elem2 | Rest3]) :-
  merge([Elem1 | Rest1], Rest2, Rest3).

/*  ***************** Structures ****************                          */

/*  atom_append(A,B,C) iff C is the deterministic concatenation            */
/*  of the atoms A and B.  NG if arg1 or 2 is var.                         */

atom_append(A,B,C) :-
  atom(A),    atom(B),
  name(A,AN), name(B,BN),
  append(AN,BN,CN),
  name(C,CN).

/*  full_name(Term, Name) iff Name is the name of Term, which may be       */
/*  atomic or compound.  NG if both args are var.                          */

full_name(Term, Name) :-
  atomic(Term),
  !,
  name(Term, Name).

full_name(Term, Name) :-
  var(Term),
  telling(Cur_output),
  tell('x.x'),
  print_string(Name),
  print_string("."),
  told,
  tell(Cur_output),
  seeing(Cur_input),
  see('x.x'),
  read(Term),
  seen,
  !,
  see(Cur_input).

full_name(Term, Name) :-
  Term =.. [Functor, Arg1 | Arglist],
  name(Functor, Func_name),
  full_name(Arg1, Arg1_name),
  full_name_list(Arglist, Arglist_name),
  append_n([Func_name, "(", Arg1_name, Arglist_name, ")"], Name).

full_name_list([], []) :- !.
full_name_list([Arg1 | Arg_rest], Arglist_name) :-
  full_name(Arg1, Arg1_name),
  full_name_list(Arg_rest, Arg_rest_name),
  append_n([",", Arg1_name, Arg_rest_name], Arglist_name).

/*   tree_position(Term, Subterm, Location) iff Term is a                  */
/*   non-variable containing Subterm, at the Location, which               */
/*   is a list of numbers corresponding to position, eg, for               */
/*   Term = a(b,c,d(e,f),g), the solutions are:                            */
/*                                                                         */
/*    Subterm             Location                                         */
/*    -------             --------                                         */
/*    a(b,c,d(e,f),b)     []                                               */
/*    b                   [1]                                              */
/*    c                   [2]                                              */
/*    d(e,f)              [3]                                              */
/*    e                   [3,1]                                            */
/*    f                   [3,2]                                            */
/*    g                   [4]                                              */

tree_position(Term, Sub, []) :-
  nonvar(Term),
  Term = Sub.
tree_position(Term, Sub, [Number | Sub_pos]) :-
  Term =.. [Func | Args],
  position(Args, Elem, Number),
  tree_position(Elem, Sub, Sub_pos).

/*  contains(Term, Sub) iff Term is a non-variable containing Sub,         */
/*  either immediately or indirectly.  NG if arg1 is var.                  */

contains(Term, Sub) :-
  nonvar(Term),
  Term = Sub.
contains(Term, Sub) :-
  Term =.. [Func | Args],
  member(Elem, Args),
  contains(Elem, Sub).

/*  compound_contains(Term, Tester, Count) iff Count is the number of      */
/*  terms, compound or atomic, within Term for which Tester is true.       */
/*  Tester is either the name of a unary predicate, or a list of predicate */
/*  name, followed by arguments.                                           */

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

/*  atomic_contains(Term, Tester, Count) iff Count is the number of        */
/*  atomic terms or vars within Term for which Tester is true.  Tester is  */
/*  either the name of a unary predicate, or a list of predicate           */
/*  name, followed by arguments.                                           */

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

/*  ************* Input and Output *************                           */

/*  copy_file(Input, Output) simply copies from Input to Output, as        */
/*  a sequence of characters (ie, it is character-oriented, not            */
/*  term-oriented.                                                         */

copy_file(Input, Output) :-
  seeing(Old_input),
  telling(Old_output),
  see(Input),
  tell(Output),
  copy_file_1,
  seen,
  told,
  see(Old_input),
  tell(Old_output).

copy_file_1 :-
  repeat,
  get0(Char),
  (Char = 26 -> true; (put(Char), fail)).

/*  readline(List) iff List is the string of characters in                 */
/*  the input stream from the current position up to, but                  */
/*  not including the next end of line.  If end of file is                 */
/*  encountered, the tail of the List is end_of_file, rather               */
/*  than [].                                                               */

readline(List) :-
  get0(Char),
  (Char=10 -> List = [];
     (Char=26 -> List = end_of_file;
                (List = [Char | Rest], readline(Rest))
  )  ).

/*  readword(Word) iff the next string of contiguous graphic               */
/*  characters in the input stream is Word.                                */

readword([Char | Rest]) :-
  first_char(Char),
  rest_word(Rest).

/*  first_char(Char) iff the next graphic character in the input           */
/*  stream is Char.                                                        */

first_char(Char) :-
  get0(N),
  (graphic(N) -> Char=N; first_char(Char)).

/*  rest_word(Rest) iff Rest is a contiguous string of graphic             */
/*  characters, starting at the current position in the                    */
/*  input stream.                                                          */

rest_word(Rest) :-
  get0(N),
  (graphic(N) -> (Rest = [N | RestRest], rest_word(RestRest));
                  Rest = []).

/*  graphic(N) iff N is a graphic, visible character.                      */

graphic(N) :- N > 32, N < 128.

/*   user_pick(List, Selection) iff Selection is a member of the list      */
/*   chosen by the user.  NG if arg1 is var.                               */

user_pick(List, Selection) :-
  List = [E | R],
  nl, print('Select one of the following: '),
  user_pick_1(List, 1),
  nl,
  print('Enter the number of your preferred entry, '),
  print('terminated by a period.'),
  nl,
  print('Anything besides a valid number will select none of the above.'),
  nl,
  read(Num),
  !,
  integer(Num),
  position(List, Selection, Num).

user_pick_1([], _).
user_pick_1([Elem | Rest], N) :-
  nl, print('  '),
  (N < 10 -> print(' '); true),
  print(N),
  print(' - '),
  print(Elem),
  N1 is N+1,
  user_pick_1(Rest, N1).

/*  user_yes_no(Prompt) iff the user responds affirmatively to the Prompt. */
/*  The Prompt should be a yes or no type question, suitable for printing, */
/*  user_yes_no('Do you wish to continue?').  The current input stream     */
/*  is assumed to be set correctly.                                        */

user_yes_no(Prompt) :-
  nl, print(Prompt),
  print('  (respond with "y." or "n.")'), nl,
  read(Ans),
  !,
  (Ans = y -> true;
    (Ans = n -> (!,fail);
      (user_yes_no(Prompt)
  ) ) ).

/*  print_string(S) succeeds if S is a list of printable integers, and     */
/*  it prints the string as a side-effect to the current output stream.    */

print_string([]).
print_string(String) :-
  name(N,String),
  print(N).

/*  tree_print(Term) prints term in tree-fashion, indented by two.         */

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

/*  tree_list_print(Term) prints Term just like tree_print, except that    */
/*  it keeps lists flat (at the same level), rather than indenting.        */
/*  Further it uses '[' and ']' to delimit the lists instead of the        */
/*  true internal functor '.'.                                             */

tree_list_print(Term) :-
  nl,
  tree_list_print_sub(Term,0,'  ').

tree_list_print_sub(Term, Depth, Prefix) :-
  print_prefix(Depth, Prefix),
  (var(Term) -> (Name = Term, Arglist = []);
     (non_null_list(Term) ->
         (Name = '[', Term = Arglist);
         Term =.. [Name | Arglist]
  )  ),
  print(Name), nl,
  D_plus is Depth+1,
  list_print_args(Arglist, D_plus, Prefix),
  (Name == '[' -> (print_prefix(Depth, Prefix), print(']'), nl); true).

list_print_args([],             Depth, _).
list_print_args([First | Rest], Depth, Prefix) :-
  tree_list_print_sub(First, Depth, Prefix),
  list_print_args(Rest, Depth, Prefix).

/*  *************** Sets ****************                                  */

/*  Following predicates treat lists as sets.  Note that only              */
/*  outer brackets are interpreted as set-constructors.  Any inner         */
/*  nested brackets are interpreted as ordered lists - thus these          */
/*  sets are "flat"; they do not contain other sets.  Eg, the              */
/*  set: [a,b,[1,2,1]] has a list as its 3rd element and is                */
/*  distinct from: [a,b,[2,1]], but *not* from [b,b,[1,2,1],a,a],          */
/*  since duplication and order at the highest (set) level are             */
/*  insignificant.                                                         */
/*                                                                         */
/*  To treat lists as multi-level sets, perform tot_sort on them, eg       */
/*  both [a,b,[1,2,1]] and [a,b,[2,1]] map to: [a,b,[1,2]], which          */
/*  may be thought of as a normalized form for multi-level sets.           */
/*                                                                         */
/*  Thus, the only easy choice is to treat all inner brackets as           */
/*  list-constructors (default) or as set-constructors (using              */
/*  tot_sort).  To explicitly distinguish and therefore allow              */
/*  both kinds, a structure must be set up, something like:                */
/*  set(List) to be interpreted as a set.  Then, eg:                       */
/*                                                                         */
/*  set([1,2,[d,d,c,a]])      : 3rd element is list                        */
/*                                                                         */
/*  set([1,2,set([d,d,c,a])]) : 3rd element is set                         */
/*                                                                         */
/*  [1,2,1]                   : (ordered) list of 3 integers               */
/*                                                                         */
/*  set([1,2,1])              : (unordered) set of 2 integers (= set([1,2]))*/
/*                                                                         */
/*  Set structure would accept unordered/duplicate elements, but           */
/*  assume their insignificance.  Conversion predicate might be:           */
/*                                                                         */
/*  set_list(Set, List) :- Set =.. [set, List], is_real_list(List).        */

/*  subset(Part, Whole) iff Part is an subset of Whole.                    */
/*  NG if both args are var.                                               */

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

/*  intersection(S1, S2, Ans) iff Ans is the intersection of S1 and S2.    */
/*  NG if arg1;2 is var.                                                   */

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

/*  union(S1, S2, Ans) iff Ans is the union of S1 and S2.                  */
/*  NG if arg1;2 is var.                                                   */

union(S1, S2, Ans) :-
   var(Ans) -> union_1(S1, S2, Ans);
               union_2(S1, S2, Ans).

union_1(S1, S2, Ans)  :- append(S1, S2, X),
                         remove_dupl(X, Ans).

union_2(S1, S2, Ans) :- union_1(S1, S2, X),
                        set_equal(X, Ans).

/*  set_diff(S1, S2, Ans) iff Ans is the set difference S1 - S2.           */
/*  NG if arg1;2 is var.                                                   */

set_diff(S1, S2, Ans) :-
   remove_dupl(S1, Better_S1),
   (var(Ans) -> set_diff_1(Better_S1, S2, Ans);
                set_diff_2(Better_S1, S2, Ans)
   ).

set_diff_1(S1, S2, Ans) :- delete_all(S2, S1, Ans).

set_diff_2(S1, S2, Ans) :- set_diff_1(S1, S2, X),
                           set_equal(X, Ans).

/*  set_equal(A,B) iff A and B contain the same elements (set equality).   */

set_equal(A, B) :-
  nonvar(A), nonvar(B), set_equal_cc(A, B);
     var(A),    var(B), A = B;
     var(A), nonvar(B), remove_dupl(B, A);
  nonvar(A),    var(B), remove_dupl(A, B).

set_equal_cc(A, B) :- subset(A,B), subset(B,A).

/*   disjoint(A,B) iff A and B have no common element.                     */
/*   NG if arg1;2 is var.                                                  */

disjoint(A,B) :- not(joint(A,B)).

/*   joint(A,B) iff A and B have at least one common element.              */
/*   NG if arg1;2 is var.                                                  */

joint(A,B) :- member(E,A), member(E,B).

/*  set_plus(S1, S2, Both) iff S1 and S2 are disjoint, and their           */
/*  union equals Both.  NG if Both and (S1 or S2) are var.                 */

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

/*  powerset(Set, Power) iff Power is the power set of Set, ie, a list     */
/*  of all subsets of Set.  NG if arg1 is var.                             */

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

/*  Here's an alternative version of powerset, perhaps a bit less          */
/*  efficient, but easier to understand.  It is deliberately               */
/*  commented out, so as not to conflict with the above.                   */
/*  NG if arg1 is var.                                                     */
/*                                                                         */
/*  powerset(Set, Power) :- setof(Sub, subset(Sub, Set), Power).           */

/*  partition(S1, S2) iff S2 is a partition of S1, ie S2 is a set of       */
/*  non-null pairwise disjoint sets, whose union = S1.                     */
/*  NG if both args are var.                                               */

partition(S1, S2) :-
  nonvar(S1), nonvar(S2),        partition_cc(S1,      S2);
     var(S1), nonvar(S2),        partition_vc(S1,      S2);
  nonvar(S1),    var(S2),
       remove_dupl(S1, Slim_S1), partition_cv(Slim_S1, S2).

partition_vc(S1, S2) :-
  not(member([], S2)),      /*   partition members must be non-null        */
  append_n(S2, S1),         /*   take all elements of all members          */
  no_dupls(S1).             /*   ensure pairwise disjoint                  */

partition_cc(S1, S2) :-
  partition_vc(Test, S2),
  set_equal(Test, S1).

partition_cv([Elem], [[Elem]]).
partition_cv([Elem | Rest], S2) :-
  partition_cv(Rest, Sub_S2),      /*   take a partition of set minus elem */
  ( S2 = [[Elem] | Sub_S2];        /*   either add a new singleton member  */
                                   /*   containing elem                    */
    (insert(Sub_S2_Elem, Sub_S2_Less_1, Sub_S2),
                                   /*   or take one of the old members     */
     S2 = [[Elem | Sub_S2_Elem] | Sub_S2_Less_1]
    )                              /*   and add elem to it                 */
  ).

/*  closure_n(List1, Pred/Arity, List2) iff List2 is the closure of        */
/*  List1 according to the predicate Pred/Arity, for which the *last*      */
/*  n-1 operands are the "old" elements and the *first* operand is         */
/*  the "new" or generated element.  (n =< 10).  NG if arg1 or arg2 is var.*/

closure_n(List1, Pred/Arity, List2) :-
  integer(Arity),
  Arity < 11,
  Arity > 1,
  sort(List1, SL1),
  gen_varlist(Arity, VL),
  VL = [Arg1 | Rest_args],
  Test =.. [Pred | VL],
  sub_closure(SL1, Test, VL, List2).

sub_closure(List1, Test, [Arg1 | Rest_args], List2) :-
  build_test(Rest_args, Test, List1, Setof_test),
  (setof(Arg1, Setof_test, New_list)
     ->
       (append(List1, New_list, Combined_list),
        sort(Combined_list, Sorted_combined_list),
        (Sorted_combined_list = List1
             -> List2 = Sorted_combined_list;
                sub_closure(Sorted_combined_list, Test, [Arg1 | Rest_args],
 List2)
       ));
       List2 = List1
  ).

gen_varlist(0, []) :- !.
gen_varlist(Arity, [Var | Rest]) :-
  A_minus is Arity-1,
  gen_varlist(A_minus, Rest).

build_test(VL, Test, List1, Setof_test) :-
  build_pred(VL, Test, List1, Pred),
  build_quantifiers(VL, Pred, Setof_test).

build_pred([], Test, _, Test) :- !.
build_pred([V | Rest], Test, List, (member(V, List), Sub_pred)) :-
  build_pred(Rest, Test, List, Sub_pred).

build_quantifiers([], P, P) :- !.
build_quantifiers([V | Rest], Pred, V^Sub_pred) :-
  build_quantifiers(Rest, Pred, Sub_pred).

/*  **************** Numeric ****************                              */

/*  between(X,Lo,Hi) iff X is an integer between Lo and Hi.  NG if         */
/*  Lo or Hi is var.                                                       */

between(X, Lo, Hi) :-
  (integer(Lo) -> LL is Lo; LL is floor(Lo)+1),
  HH is floor(Hi),
  Lim is HH-LL,
  !,
  numvar(XX, Lim),
  X is XX+LL.

/*  numvar(X, Limit) iff X and Limit are non-negative integers,            */
/*  with X =< Limit.  In generative mode, solutions are produced           */
/*  from zero to higher values.  Either, both, or neither argument         */
/*  may be instantiated.  NG if arg1 is fraction and arg2 var.             */

numvar(X,Limit) :- var(Limit),    natural(Limit), numvar(X, Limit).
numvar(X,Limit) :- nonvar(Limit), natural(X), ((X>Limit, !, fail); true).

/*  natural(X) iff X is a non-negative integer.                            */

natural(X) :- nonvar(X), integer(X), X>=0;
              var(X), gen_integer(X, 0).

/*  gen_integer(X,Seed) generates integers, incrementing from Seed.        */
/*  NG if arg1 is nonvar or arg2 is var.                                   */

gen_integer(X, Seed) :-
  var(X),
  integer(Seed),
  gen_integer_vc(X, Seed).

gen_integer_vc(X, Seed) :-
  X is Seed;
  (New_seed is Seed+1,
   gen_integer_vc(X, New_seed)).

/*  random(Max, N) instantiates N to a random integer between              */
/*  1 and Max.  NG if arg1 is var.                                         */

seed(13).

random(R,N) :-
        retract(seed(S)),
        N is (S mod R) +1,
        NewSeed is (125*S+1) mod 4096,
        asserta(seed(NewSeed)),!.

/*  rationalize(Exprs, R_Exprs) iff Exprs is a numeric expression,         */
/*  and R_Exprs is a mostly evaluated form of the expression.              */
/*  Rational numbers are preserved and reduced to lowest terms,            */
/*  or integers if possible.  If no floating-point numbers are in          */
/*  the expression, the results are exact, except for raising to a         */
/*  fractional power.  NG if Exprs is not a fully instantiated             */
/*  numeric expression.  Routines for handling exponentiation              */
/*  override some C-Prolog defaults.  X^0 always equals 1, even for        */
/*  0^0.  A negative base is allowed with an integer power, so             */
/*  (-2)^4 = 16, and (-2)^5 = -32.  Negative powers are handled            */
/*  correctly, eg, 2^(-3) = 1/8, (-2)^(-3) = -1/8, (2/3)^(-3) =            */
/*  27/8. A negative base to a fractional power fails, as does zero        */
/*  to a negative power.                                                   */

rationalize(Exprs, Exprs) :- number(Exprs), !.

/*  handle plus and minus unary ops                                        */

rationalize(-(Exprs), R_Exprs) :- rationalize(Exprs*(-1), R_Exprs).

rationalize(+(Exprs), R_Exprs) :- rationalize(Exprs, R_Exprs).

/*  all other unary ops to be handled by regular evaluation.               */

rationalize(Exprs, R_Exprs) :-
  Exprs =.. [Un_op, Opnd1],
  Un_op \== '+',
  Un_op \== '-',
  rationalize(Opnd1, R_Opnd1),
  Eval =.. [Un_op, R_Opnd1],
  R_Exprs is Eval.

/*  handle binary ops                                                      */

rationalize(Exprs, R_Exprs) :-
  Exprs =.. [Bin_op, Opnd1, Opnd2],
  rationalize(Opnd1, R_Opnd1),
  rationalize(Opnd2, R_Opnd2),
  (plain_eval(Bin_op, R_Opnd1, R_Opnd2)
        -> (Eval =.. [Bin_op, R_Opnd1, R_Opnd2], R_Exprs is Eval, !);
           (rationalize_1(Bin_op, R_Opnd1, R_Opnd2, R_Exprs), !)
  ).

/*  plain_eval is true if the expression can be evaluated directly,        */
/*  either because: 1) the result will be exact (eg integer subtraction),  */
/*  or 2) the result is (probably) not rational anyway (eg, when           */
/*  a floating-point number is an operand, or raising to a fractional      */
/*  power).                                                                */

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

/*   reduced(Exprs, R_Exprs) iff R_Exprs is Exprs reduced to               */
/*   lowest terms.  R_Exprs is an integer if the denominator               */
/*   would be 1.  NG if arg1 is var.                                       */

reduced(Exprs,Exprs) :- integer(Exprs), !.
reduced(Num/Den, Ans) :-
  Num mod Den =:= 0,
  Ans is Num/Den, !.
reduced(Num_In/Den_In, Num_Out/Den_Out) :-
  gcd(Num_In, Den_In, GCD),
  Num_Temp is Num_In/GCD,
  Den_Temp is Den_In/GCD,
  (Den_Temp < 0 -> (Num_Out is -Num_Temp, Den_Out is -Den_Temp);
                   (Num_Out is  Num_Temp, Den_Out is  Den_Temp)
  ),
  !.

/*  gcd(X, Y, Ans) iff Ans is the greatest common divisor of the           */
/*  integers X and Y.  NG if arg1 or arg2 is var.                          */

gcd(X, 0, X) :- !.
gcd(0, X, X) :- !.
gcd(X, Y, Ans) :-
  L is Y mod X,
  gcd(L, X, Ans), !.

/*  abs(X, Y) iff Y is the absolute value of X.  Ng if arg1 is var.        */

abs(X,Y) :-
  X < 0 -> Y is -X; Y is X.

/*  ***************** Control ****************                             */

/*  loop(Pred) repeatedly invokes Pred until it fails.  Loop always        */
/*  fails and so is executed only for side-effects.  If Pred has           */
/*  several clauses, enclose in parens.  A typical use might be:           */
/*     loop((pred(X,Y), write(X), write('  '), write(Y), nl)).             */
/*  to write out all solutions.                                            */

loop(Pred) :-
  repeat,
  (Pred; (!, fail)),     /*    Pred succeeds, or kill loop                 */
  fail.                  /*    Force repetition.                           */

/*  invoke(Pred_list) creates a predication from Pred_list and             */
/*  invokes it.  Pred_list must be instantiated.  The reason for           */
/*  the 2nd arg is that otherwise invoke will quit after first             */
/*  success with all nonvars.  Even if all args are nonvar,                */
/*  users may wish to re-invoke by re-instantiating Pred_list.             */

invoke(Pred_list, Pred_call) :- Pred_call=..Pred_list, Pred_call.

/*  ************* Extended Logic **************                            */

/*  counter_eg(If, Then) iff there exists some instantiation of If         */
/*  and Then such that If is true and Then is false.  If and Then          */
/*  must be predicates.  Their arg-lists may contain vars, but the         */
/*  predicates themselves must be specified.  Eg:                          */
/*  counter_eg((gender(X,male), gender(Y,female)), taller(X,Y)).           */
/*  may be understood as "Are there any counter-examples to the rule       */
/*  that if X is male and Y is female, then X is taller than Y?"           */

counter_eg(If, Then) :- If, not(Then).

/*  implies(If, Then) iff If and Then form a true implication for          */
/*  this DB, ie there are no counter-examples.                             */

implies(If, Then) :- not(counter_eg(If, Then)).

/*  The following stuff is meant to provide some capability for expressing */
/*  disjunction and negation.  The three predicates visible at the user    */
/*  level are:                                                             */
/*                                                                         */
/*    or(L) - to ask if this is a true disjunction; defined herein, but    */
/*            invoked by the user.                                         */
/*                                                                         */
/*    ground_or(L) - to tell the system that this is a true disjunction,   */
/*            ie, at least one disjunct is true.  Defined by the user      */
/*            as part of the DB.                                           */
/*                                                                         */
/*    false(P) - says that P is definitely false (not just not provable,   */
/*            a la "not" in normal Prolog).  Defined by the user as part   */
/*            of the DB.                                                   */
/*                                                                         */
/*    start_or. - the user has to invoke this to fire up the system,       */
/*            before he starts asking things.                              */
/*                                                                         */
/*  eg, if the user, in the DB, says:                                      */
/*                                                                         */
/*    ground_or([e,f,g,h,i]).                                              */
/*    false(f).                                                            */
/*    false(g).                                                            */
/*    ground_or([a,b,c]) :- d.                                             */
/*    d.                                                                   */
/*    false(b).                                                            */
/*    false(a) :- w; d.                                                    */
/*                                                                         */
/*  and then invokes start_or, the system will be able to conclude         */
/*  that the following succeed:                                            */
/*                                                                         */
/*    c.                    (because (a or b or c) is true and a and b are */
/*                           both false)                                   */
/*    or([x1,d,x2]).        (because d is a true disjunct)                 */
/*    or([f,g,t,h,r,e,i]).  (because it contains a true disjunct: [e,f,g,h,i]).*/
/*    or([e,h,i])           (because its residue from [e,f,g,h,i] is [f,g] all*/
/*                           of which are false.)                          */
/*                                                                         */
/*  It's probably not wise for "ground_or" itself to depend on "or".       */


/*  or(L) iff L is a list of disjuncts, at least one of which is true.     */
/*  NG if L is var.                                                        */

or(L) :- nonvar(L), L = [_|_], (or_1(L); or_2(L); or_3(L)).

/*  or_1 tries to find a true individual disjunct.                         */
/*  or_2 tries to find a subset of L already known to be true.             */
/*  or_3 tries to find a superset of L already known to be true,           */
/*       and then show the rest are false.                                 */

or_1([E | R]) :- E; or_1(R).
or_2(L1)      :- ground_or(L2), subset(L2, L1).
or_3(L)       :- ground_or(Ground_list),
                 set_plus(L, Residue, Ground_list),
                 false_list(Residue).

/*  false_list(L) iff all members of L are provably false.                 */

false_list([]).
false_list([E1 | Rest]) :- false(E1), false_list(Rest).

/*  start_or always fails, but in the meantime, it builds clauses          */
/*  for each disjunct of the ground_or's.                                  */

start_or :-
  clause(ground_or(Disjuncts), Antecedents),
  set_plus([One_disjunct], Rest, Disjuncts),
  assertz((One_disjunct :- Antecedents, false_list(Rest))),
  fail.

/*  Some meta-logical facilities coming up.                                */

/*  kb_object(Type, Head, Tail) iff there is a clause "Head :- Tail."      */
/*  in the current program.  If the Tail=true, and the Head is             */
/*  composed of all constants, Type = fact, otherwise Type = rule.         */

kb_object(Type, Head, Tail) :-
  current_predicate(_, Head),
  clause(Head, Tail),
  ((Tail = true, constant_term(Head)) -> Type = fact;
                                         Type = rule).

/*  assert1(Term) iff there is exactly one matching instance of Term       */
/*  asserted in the DB.  Term should be at least partially instantiated.   */

assert1(Term) :-
  repeat,
  (retract(Term) -> fail;
                    assert(Term)
  ),
  !.

/*  ed allows for some very primitive run-time program modification.       */
/*  The user can enter terms, facts or rules, without the "extra"          */
/*  parentheses needed by assert, and ed then asserts and writes out       */
/*  the term for later editing into the permanent program.                 */

ed :-
   nl, print('Enter fact or rule ([] to quit).'), nl,
   read(T),
   (T = [] -> true; (ed_1(T), !, ed)).

ed_1(T) :-
  assert(T),
  tell('x.tmp'),
  nl, writeq(T), put(46), nl,
  tell(user).

/*   End of Prolog Utilities                                               */
