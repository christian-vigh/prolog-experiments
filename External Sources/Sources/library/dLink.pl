% Following the comments about Fortran as an AI language,
% Melbourne University Department of Artificial Intelligence
% has decided to convert all of its Fortran AI programs to NU-Prolog.
% This package has been written as an aid to this conversion.

% For non-NU-Prolog users, simply comment out the when declarations.

% Doubly-linked list package.
%       Why use boring old single-linked lists when doubly-linked
%       list could make your list processing applications
%       run as never before.

% ?- dAdj(L, R) when L and R.           % Are these adjacent nodes?
% ?- dPrev(D, _) when D.                        % Get previous node.
% ?- dNext(D, _) when D.                        % Get next node.
% ?- dHead(D, _) when D.                        % Get head of list
% ?- dTail(D, _) when D.                        % Get tail of list
% ?- isD(D) when D.                     % Is this a doubly-linked list?
% ?- portray(D) when ever.              % Portray doubly-linked list
% ?- dAppend(X, Y, Z) when X or Z.      % Append for doubly-linked lists

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

?- dAdj(L, R) when L and R.             % Are these adjacent nodes?
dAdj(L, R) :-
	L = d(_, _, R),
	R = d(L, _, _).

?- dPrev(D, _) when D.                  % Get previous node.
dPrev(d(L, _, _), L).

?- dNext(D, _) when D.                  % Get next node.
dNext(d(_, _, R), R).

?- dHead(D, _) when D.                  % Get head of list
dHead([], []).
dHead(d([], D, R), d([], D, R)).
dHead(d(d(L, D, R), _, _), H) :-
	dHead(d(L, D, R), H).

?- dTail(D, _) when D.                  % Get tail of list
dTail([], []).
dTail(d(L, D, []), d(L, D, [])).
dTail(d(_, _, d(L, D, R)), T) :-
	dTail(d(L, D, R), T).

?- listToD(List, D) when List or D.     % Convert single to doubly-linked list
listToD([], []).
listToD(H.T, D) :-
	D = d([], H, R),
	$listToD(T, D, R).

?- $listToD(List, _, D) when List or D.
$listToD([], _, []).
$listToD(H.T, L, D) :-
	D = d(L, H, R),
	$listToD(T, D, R).

?- isD(D) when D.                       % Is this a doubly-linked list?
isD([]).
isD(D) :-
	D = d([], _, R),
	$isD(D, R).

?- $isD(_, D) when D.
$isD(_, []).
$isD(L, D) :-
	D = d(L, V, R),
	$isD(D, R).

?- portray(D) when ever.                % Portray doubly-linked list
portray(D) :-
	nonvar(D),
	D = d([], _, _),
	display('[]:'),
	$dPrint(D).

?- $dPrint(D) when D.
$dPrint([]) :-
	display('[]').
$dPrint(d(_, V, R)) :-
	display(V),
	display(':'),
	$dPrint(R).

?- dAppend(X, Y, Z) when X or Z.        % Append for doubly-linked lists
dAppend(X, [], X).
dAppend([], d([], D, R), d([], D, R)).
dAppend(d(L, X, R), d([], Y, RY), Z) :-
	$dAppend([], d(L, X, R), Y, RY, Z).

?- $dAppend(X, _, _, Z) when X or Z.
$dAppend(L, d(_, X, []), Y, R, Z) :-
	Z = d(L, X, H),
	H = d(Z, Y, R1),
	$dAppend1(H, R, R1).
$dAppend(L, d(_, X, d(L1, X1, R1)), Y, RY, d(L, X, RZ)) :-
	Z = d(L, X, RZ),
	$dAppend(Z, d(L1, X1, R1), Y, RY, RZ).

?- $dAppend(_, Y, Z) when Y or Z.
$dAppend1(_, [], []).
$dAppend1(L, d(_, D, R), Z) :-
	Z = d(L, D, R1),
	$dAppend1(Z, R, R1).

% P.S. Don't forget to turn off the occur-check in your version of Prolog!
