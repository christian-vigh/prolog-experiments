%   File   : /usr/lib/prolog/samsort
%   Author : Richard A. O'Keefe
%   Updated: 1 June 84
%   Purpose: a sorting routine which exploits existing order


samsort([], []) :- !.
samsort(List, Sorted) :-
	samsort(List, [], 0, Sorted).

samsort([], Stack, R, Sorted) :- !,
	samfuse(Stack, 0, [Sorted]).
samsort([Head|Tail], Stack, R, Sorted) :-
	sam_run(Tail, [Head|Queue], [Head|Queue], Run, Rest),
	succ(R, S),
	samfuse([Run|Stack], S, NewStack),
	samsort(Rest, NewStack, S, Sorted).


samfuse([A,B|Rest], K, Ans) :-
	0 is K mod 2, !,
	J is K div 2,
	merge(A, B, C), !,
	samfuse([C|Rest], J, Ans).
samfuse(Stack, _, Stack).


sam_run([], Run, [_], Run, []) :- !.
sam_run([Head|Tail], QH, QT, Run, Rest) :-
	sam_run(QH, QT, Head, Tail, Run, Rest).

sam_run([Ah|At], Qt, H, T, Run, Rest) :-
	H @< Ah, !,
	sam_run(T, [H,Ah|At], Qt, Run, Rest).
sam_run(Qh, [Qt], H, T, Qh, [H|T]) :-
	H @< Qt, !.
sam_run(Qh, [_,H|Nt], H, T, Run, Rest) :-
	sam_run(T, Qh, [H|Nt], Run, Rest).




