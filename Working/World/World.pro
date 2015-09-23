object(Name) :-
	nonvar(Name), kr$object(Name),
	object_error(already_defined, object, Name),
	!.
object(Name) :-
	nonvar(Name), 
	assert( kr$object(Name) ),
	create_object(Name, kr$object, kr$object_template, [ObjectName]).
object(Name) :-
	kr$object(Name).


create_object(Name, KRName, Template, ParmList) :-
	Func =.. [Template | ParmList],
	not( create_body(Name, KRName, Template, Func, ParmList) ).

create_body(Name, KRName, Template, Func, ParmList) :-
	clause( Func, Body ),
	string_termq(Str, Body),
	string_tokens(Str, BodyList),
	create_string_body(BodyList, Name, KRName, ParmList, Result),
	write(Result), nl,
	string_termq(Result, NewTerm),
	assert( NewTerm ),
	fail.

create_string_body(BodyList, Name, KRName, ParmList, Result ) :-
	Func =.. [Name | ParmList],
	string_termq(StrFunc, Func),
	string_tokens(StrFunc, Head),
	create_object_body(BodyList, Name, KRName, Body), 
	stringlist_concat([StrFunc, ':-' | Body], Result),
	!.
	
create_object_body([], _, _, []) :- !.
create_object_body([keyword_name|Tail], Name, KRName, [Name|Result]) :-
	create_object_body(Tail, Name, KRName, Result).
create_object_body([Head|Tail], Name, KRName, [Head|Result]) :-
	create_object_body(Tail, Name, KRName, Result).
	

kr$object_template(Word) :-
	nonvar(Word), kr$entity(keyword_name, Word), 
	object_error(already_defined, keyword_name, Word),
	!.
kr$object_template(Word) :-
	nonvar(Word), 
	assert( kr$entity( keyword_name, Word ) ).
kr$object_template(Word) :-
	kr$entity(keyword_name, Word).


kr$relation_template(Name, Ordinality, KindA, KindB ) :-
	nonvar(Name),
	kr$relation
	relation_ordinality(Ordinality),
	relation_kind(KindA),
	relation_kind(KindB),


	
object_error(already_defined, Type, Name) :-
	write('error : the '), write(Type), write(' '''), write(Name),
	write(''' is already defined.'),
	nl.