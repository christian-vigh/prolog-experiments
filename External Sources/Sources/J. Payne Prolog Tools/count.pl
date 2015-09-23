%    count.pl
%    
%    Information about contents of a valid Prolog file
%    Any files prefixed ":- consult(file)" will be count-ed too.
%    The information given is
%    	Number of predicates
%    	Number of clauses in each predicate
%    
%    
%    The output from "count('count.pl')" is as follows:
%    
%    5 predicates
%    count/1:   3 clauses in count.pl
%    count/2:   2 clauses in count.pl
%    summarise/0:   2 clauses in count.pl
%    dump/1:   1 clauses in count.pl
%    look_at/2:   3 clauses in count.pl

count([File|Others]) :-
	!,
	dump(File),
	count(Others).

count([]) :-
	!,
	summarise.

count(File) :-
	dump(File),
	summarise.

summarise :-
	recorded(pred_count,I,_),
	write(I), write(' predicates'), nl,
	recorded(pred_name,Name,_),
	recorded(Name,[Arity,N,File],_),
	write(Name), write('/'), write(Arity), write(':   '),
	write(N), write(' clauses in '), write(File), nl,
	fail.

summarise.

dump(File) :-
	seeing(Seeing),
	see(File),
	!,
	read(Term),
	look_at(Term,File),
	seen,
	see(Seeing).

look_at(end_of_file,_) :- !.

look_at((:- consult(Different)),File) :-
	!,
	dump(Different),
	read(Another_thing),
	look_at(Another_thing,File).

look_at(Thing,File) :-
	count(Thing,File),
	read(Another_thing),
	look_at(Another_thing,File).

count((Head :- _),File) :-
	!,
	count(Head,File).

count(Head,File) :-

	/* This clause is long but straighforward.
	   1. Get the functor name for the clause and its arity
	   2. Count the number of occurrences of functor/arity
	*/

	functor(Head,Func_name,Arity),
	(
		recorded(Func_name, [Arity,I,File], Akey) ->
		(	J is I + 1,
			record(Func_name, [Arity,J,File], _),
			erase(Akey)
		)
	;
		record(Func_name, [Arity,1,File], _),
		(
			recorded(pred_name, Func_name, _)
		;
			record(pred_name, Func_name, _)
		),
		(
			recorded(pred_count, A, Bkey) ->
			(
				B is A + 1,
				record(pred_count, B, _),
				erase(Bkey)
			)
		;
			record(pred_count,1,_)
		)
	),
	!.
