The ersatz quicksort implementation which seems to be in every
library has a very high overhead.  I say ``ersatz'' because quicksort
as defined by Hoare uses structure modification and random access.
I do not think that quicksort can ever perform well without these
features.  In contrast, merge sort is a natural sorting method for
linked lists and does not modify structure.

I've derived two methods for implementing merge sort in PROLOG.  Both
give somewhat better performance than ersatz quicksort when benchmarked
under compiled Quintus PROLOG.  They also outperform Quintus's builtin
sort procedure.  I'd like to share with you my most elegant version.
My fastest version is longer, but is available on request - or I can
submit it to the list if there is sufficient interest.

Comments are very welcome.


% Package: merge
% Method: double recurse down, binary merge back
% Version: optimizations removed for publication
% Date: July 1985

% Export:
%	merge_sort( -L, +S )    :- list S is sorted version of list L.
%	merge_2( -L1, -L2, +S ) :- list S is merge of sorted lists L1 & L2.
% Import:
%	less( -X, -Y )    - defines desired sorting order
%	length( +N, -L ) :- N is length of list L	% standard lib

% merge_sort( -L, +S ) :- list S is sorted version of list L.
merge_sort( L, S ) :-
	length(L, N),
	merge__go( N, L, S, [] ).

% merge__go( -N, -L, +S, +R ) :-
%	S is the first N elements of L, sorted,
%	and R is the rest of L thereafter.
merge__go( 0, L, [], L ) :- !.
merge__go( 1, [X|L], [X], L ) :- !.
merge__go( N, L, S, R ) :-				% N >= 2
	N1 is N >> 1,  merge__go( N1, L,  S1, R1 ),	% N1 > 0
% or	N1 is N  / 2,  merge__go( N1, L,  S1, R1 ),	% if / truncates
	N2 is N  - N1, merge__go( N2, R1, S2, R  ), 	% N2 > 0
	N  is N1 + N2, merge_2( S2, S1, S ).

% merge_2( L1, L2, S ) :- list S is merge of sorted lists L1 & L2,
%	L2 must not be empty.
merge_2( [], S, S ) :- !.
merge_2( [X|L1], [Y|L2], [X|L] ) :- less(X,Y), !, merge_2( L1, [Y|L2], L ).
merge_2( L1, [Y|L2], [Y|L] ) :- merge_2( L2, L1, L ).
