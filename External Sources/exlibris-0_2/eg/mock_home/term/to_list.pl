to_list( Either, List ) :-
	( (var(Either);(Either\=[_H|_T],Either\==[]) ) ->
		List = [Either]
		;
		List = Either
	).
