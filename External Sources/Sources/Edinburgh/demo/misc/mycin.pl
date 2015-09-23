%	Mycin(Prolog)	- a Prolog rational reconstruction.
%	Waterloo Prolog version by Peter Hammond 1980.
%	converted to Dec-10 Prolog by Richard O'Keefe 1981.

%	A consultation session is initiated by the goal start_session.

start_session :- therapy_required, !, consult.
start_session :- write('The patient needs no therapy.'), nl.

therapy_required :- f(getans(0, initiator, Ans)), Ans = yes.

consult :- getdata(0, PatientData), write(PatientData), nl.

%	The hierarchical nature of the structure of PatientData is reflected
%	in the recursive definition of getdata below.

getdata(Class4, Hypos) :-
	Class3 is Class4-1,
	current(Class3, Entity),
	last_class(Class4, Attribute), !,
	gethypos(Entity, Attribute, Hypos).
getdata(Class1, [Class1DataItem | OtherClass1Data]) :-
	Class1 < 4,
	gennew(Class1, Entity),
	getdescr(Class1, Entity, Descr),
	Class2 is Class1+1,
	getdata(Class2, Class2Data),
	Class1DataItem = w(Entity, Descr, Class2Data),
	check_for_more(Class1, OtherClass1Data).

%	gennew generates a new entity of the required class and also
%	prints a heading announcing the new entity.

gennew(Class, Entity) :-
	genno(Class, NewNo),
	data_class(Class, ClassName, Details),
	Entity = ClassName-NewNo,	% e.g. patient-1
	genheader(Class, Entity).

%	Each time a new entity is required genno generates a suitable
%	cardinal for the entity.

genno(Class, NewNo) :-
	retract(number(Class, OldNo)), !,
	NewNo is OldNo+1,
	assert(number(Class, NewNo)).
genno(Class, 1) :-
	\+ number(Class, _),
	assert(number(Class, 1)).

genheader(ClassNo, Entity) :-
	repwrite('  ', ClassNo), repwrite('- ', 8),
	write(Entity), repwrite(' -', 8), nl.

repwrite(Object, Times) :-
	Times > 0, Left is Times-1,
	write(Object), !,
	repwrite(Object, Left).
repwrite(Object, 0).

%	getdescr asks the user to supply information of a background nature
%	for a particular entity, and can also cause a message to be printed
%	announcing the name of the first entity in the next class in the hierarchy.

getdescr(Class, Entity, Descr) :-
	data_class(Class, Name, Details),
	getlist(Class, Details, Descr),
	genmess(Class, Entity).

getlist(Class, [Item | OtherItems], [Ans | OtherAns]) :-
	getans(Class, Item, Ans),
	getlist(Class, OtherItems, OtherAns).
getlist(Class, [], []).

%	current returns the current entity of a particular class.

current(Class, Entity) :-
	number(Class, No),	%	fails if none yet
	data_class(Class, ClassName, Details),
	Entity = ClassName-No.

%	getans asks the user for and reads the value of an item of background
%	data.  Each question is preceded by a new question number.

getans(Class, Item, Ans) :-
	genno(question, Q),
	repwrite('  ', Class),
	display('('), display(Q), display(') '),
	question(Item), ttyflush,
	read(Ans), nl.

%	check_for_more asks the user if there is another entity of a
%	particular class to consider.

check_for_more(Class, OtherData) :-
	data_class(Class, ClassName, Details),
	display('Is there another '),
	display(ClassName), display('? '), ttyflush,
	read(Ans),
	consider(Ans, Class, OtherData).

consider(no,  Class, []).
consider(yes, Class, OtherData) :- getdata(Class, OtherData).

%	same causes evidence to be gathered which bears on a particular
%	value of a clinical parameter, and succeeds if the CF supporting
%	this value is greater than 200.

same(Entity, Attribute, ReqVal, CFmi) :-
	get(Entity, Attribute, ReqVal, CFmi), !,
	CFmi > 200.

%	get collects together the hypotheses relevant to determining the
%	value of a clinical parameter and finds the largest of the CF-s
%	supporting the possible values.

get(Entity, Attribute, ReqVal, CFmi) :-
	gethypos(Entity, Attribute, Hypos),
	intersect(Hypos, ReqVal, Intersect),
	maxhyp(Intersect, CFmi).

gethypos(Entity, Attribute, Hypos) :-
	(   know(Entity, Attribute, Hypos)
	;   deduce(Entity, Attribute, Hypos)
	;   askfor(Entity, Attribute, Hypos)
	),
	Hypos \== [].

%	deduce collects all the evidence for the value of a parameter,
%	merges evidence for the same value into one hypothesis, and
%	stores the information obtained.

deduce(Entity, Attribute, Hypos) :-
	bagof(v(Val, Cf), rule_check(Entity, Attribute, Val, Cf), H),
	merge(H, Hypos),
	assert(know(Entity, Attribute, Hypos)).

%	rule_check investigates a rule and calculates the Cf of the
%	deduction when the rule succeeds.

rule_check(Entity, Attribute, Value, Cf) :-
	rule(RuleNo, Entity, Attribute, Value, C, Tally),
	Cf is Tally*C/1000.

merge([], []).
merge([v(unk,1000)], []).
merge([H | Rest], [H1 | Rest1]) :-
	compare(H, Rest, H1, Tser1),
	merge(Tser1, Rest1).

compare(R, [], R, []).
compare(v(Val, Cf1), [v(Val, Cf2) | U], R, W) :-
	Cf3 is (1000-Cf1)*Cf2/1000 + Cf1,
	compare(v(Val, Cf3), U, R, W).
compare(v(Val1, Cf1), [v(Val2, Cf2) | U], R, [v(Val2, Cf2) | W]) :-
	Val1 \== Val2,
	compare(v(Val1, Cf1), U, R, W).

intersect([v(Val, Cf) | H], [Val | Rest], [v(Val, Cf) | H1]) :- !,
	intersect(H, Rest, H1).
intersect([v(Val, Cf) | H], [Alt | Rest], H1) :- !,
	intersect(H, [Alt], X),
	intersect([v(Val, Cf) | H], Rest, Y),
	union(X, Y, H1).
intersect([], List, []).
intersect(Hypos, [], []).

union([], Y, Y).
union(X, [], X).
union([R], Y, [R|Y]).

maxhyp([], 0).
maxhyp([v(Val, Cf) | H], CFmi) :-
	maxhyp(H, C1),
	(C1 > Cf, !, CFmi = C1; CFmi = Cf).

%	askfor causes the user to be asked for the value of a clinical
%	parameter.  The answer is read and checked to see if it is one
%	of the question's possible answers.

askfor(Entity, Attribute, [v(ActVal, Cf)]) :-
	question(Entity, Attribute),
	readans(Answer1, Cf1),
	check_for_query(Answer1, Cf1, A, C),
	check_ans(Attribute, A, C, ActVal, Cf),
	assert(know(Entity, Attribute, [v(ActVal, Cf)])).

check_ans(Attribute, A1, C1, A1, C1) :-
	parameter(Attribute, Expected),
	member(A1, Expected), !.
check_ans(Attribute, A1, C1, A, C) :-
	parameter(Attribute, Expected),
	display('Please enter one of the following:'), nl,
	write_list(Expected),
	readans(Answer2, Cf2),
	check_ans(Attribute, Answer2, Cf2, A, C).

question(Entity, Attribute) :-
	display('	Please enter the '), display(Attribute),
	display(' of '), write(Entity), display(': '), ttyflush.

%	readans is used to read the user's reply to a request for the value
%	of a clinical parameter.  The value and its certainty factor are
%	both read.  The default Cf is 1000.

readans(Answer, Cf) :-
	read(Reply),
	name(Reply, Text),
	gram(Answer, Cf, Text, []).

	gram(Answer, Cf) --> space, word(A), {name(Answer, A)},
			     (   "(", space, number(C), ")", space,
			         {name(Cf, C)} ; {Cf = 1000}).

		space --> [C], {C =< 32}, space | [].

		word([C|R]) --> [C], {C > 32, C =\= "("}, word(R).
		word(  [] ) --> space.

		number([C|R]) --> [C], {C >= "0", C =< "9"}, number(R).
		number(  [] ) --> space.

member(X, [X|_]).
member(X, [_|R]) :- member(X, R).


write_list([X|Y]) :-
	write(X), nl, write_list(Y).
write_list([]) :-
	nl.

min([X], X).
min([X|Y], Z) :-
	min(Y, U),
	( X < U, !, Z=X ; Z=U ).

%	T H E   E X P L A N A T I O N   S Y S T E M

%	check_for_query is called after the user is asked to give a parameter
%	value.  If he said WHY or RULE the explanation system is entered.

check_for_query(Answer, Cf, A, C) :-
	(   Answer = why  ;  Answer = rule   ), !,
	answer_query(Answer),
	get_nearest(Rule),
	report(Answer, Rule),
	check_again_for_query(A, C).
check_for_query(Answer, Cf, Answer, Cf).

answer_query(why) :-		% he can't be serious!
	display('To determine the genus of the organism.'), nl.
answer_query(rule) :-
	display('The current rule is:'), nl.

check_again_for_query(A, C) :-
	f(readans(A1, C1)),
	test(A1, C1, A, C).

test(why, 1000, A, C) :- !, fail.
test(A, C, A, C) :- A \== why.

get_nearest(rule(N, E, A, R, C, T)) :-
	ancestors(AllOfThem),
	member(rule(N, E, A, R, C, T), AllOfThem).

report(why,  Rule) :- explain(Rule).
report(rule, Rule) :- clause(Rule, Body), translate(Rule, Body).

%	explain prints the known parameter values in a rule, those still
%	to be determined, and the deduction which could result.

explain(Rule) :-
	clause(Rule, Body),
	divide(Body, KnownPrems, UnknownPrems),
	write_known(KnownPrems),
	translate(Head, UnknownPrems).

divide(','(A,B), ','(A, OtherKnown), Unknown) :-
	known(A),
	divide(B, OtherKnown, Unknown).
divide(','(A,B), true, ','(A,B)) :-
	\+ known(A).
divide(A, true, A) :-
	\+ known(A).

write_known(true).
write_known(','(A,B)) :-
	display('	It is known that:'), nl,
	writeprems(','(A,B)),
	display('	therefore...'), nl.

writeprems(true).
writeprems(','(A,B)) :-
	transpred(A),
	writeprems(B).
writeprems(A) :-
	transpred(A).

%	translate gives a simple english translation of a rule.

translate(Head, Body) :-
	display('	If :'), nl,
	writeprems(Body),
	display('	Then :'), nl,
	transpred(Head).

transpred(min(M,N)).
transpred(same(E, A, R, C)) :-
	current(3, Entity),
	display('	    the '), write(A),
	display(' of '), write(Entity),
	display(' is '), write(R), nl.	%  the genus of organism-1 is [staph]
transpred(rule(N, E, A, V< C, T)) :-
	current(3, Entity),
	display('	    there is '),
	give_evidence(C),
	display(' evidence that the '), nl,
	display('	    '), write(A),
	display(' of '), write(E),
	display(' is '), write(V), nl,
	display('		(rule '),
	write(N), display(')'), nl.

known(same(E,A,R,C)) :-
	know(E, A, Hypos),
	intersect(Hypos, R, I),
	maxhyp(I, CFmi),
	CFmi > 200.

ruleno(N) :-
	display('		(Rule '),
	display(N), display(')'), nl.

writeval([M]) :- write(M).

give_evidence(C) :-
	C > 800, display('strongly suggestive')
    ;	C > 400, display('suggestive')
    ;	C < 401, display('weakly suggestive')
	.

%	T H E   K N O W L E D G E   B A S E

%	data_class lets the user delcare the classes of data objects,
%	their names, and the background details required.

data_class(0, patient, [name, sex, age]).
data_class(1, infection, [inftype, infdate]).
data_class(2, culture, [cultsite, cultdate]).
data_class(3, organism, []).
last_class(4, genus).

genmess(0, Entity).
genmess(1, Entity) :-
	display('	The most recent culture associated with '),
	write(Entity),
	display(' will be referred to as:'), nl.
genmess(2, Entity) :-
	display('	The first significant organism from '),
	write(Entity),
	display(' will be referred to as:'), nl.
genmess(3, Entity).

%	question defines the questions which will elicit the values of
%	particular details from the user.

question(name) :-
	display('Patient''s name: ').
question(sex) :-
	display('Patient''s sex : ').
question(age) :-
	display('Patient''s age : ').

question(initiator) :-
	display('Have you been able to obtain positive cultures from a
site at which the patient has an infection? ').
question(inftype) :-
	display('What is the infection? ').
question(infdate) :-
	display('When did this infection first appear? ').
question(cultsite) :-
	display('What site did the specimen of this culture come from? ').
question(cultdate) :-
	 display('When was this culture obtained? ').

%	parameter records the possible values of a clinical parameter.

parameter(genus, [unk,strep,neiss,bact,staph,coryn]).
parameter(gramstain, [unk,pos,neg]).
parameter(morphology, [unk,rod,coccus]).
parameter(conformation, [unk,singles,longchains,shortchains]).
parameter(aerobicity, [unk,anaerobic,facul]).

%	T H E   * R U L E *   B A S E

rule(35,	Entity,	genus,	bact,	600,	Tally) :-
	same(Entity, gramstain, [neg], Cf1),
	same(Entity, morphology, [rod], Cf2),
	same(Entity, aerobicity, [anaerobic], Cf3),
	min([Cf1,Cf2,Cf3], Tally).

rule(9,		Entity,	genus,	neiss,	800,	Tally) :-
	same(Entity, gramstain, [neg], Cf1),
	same(Entity, morphology, [coccus], Cf2),
	min([Cf1,Cf2], Tally).

rule(306,	Entity,	genus,	staph,	700, 	Tally) :-
	same(Entity, gramstain, [pos], Cf1),
	same(Entity, morphology, [coccus], Cf2),
	same(Entity, conformation, [singles], Cf3),
	min([Cf1,Cf2,Cf3], Tally).

rule(412,	Entity, genus,	strept,	950,	Tally) :-
	same(Entity, gramstain, [pos], Cf1),
	same(Entity, morphology, [coccus], Cf2),
	same(Entity, conformation, [longchains], Cf3),
	min([Cf1,Cf2,Cf3], Tally).

rule(413,	Entity,	genus,	strept,	800,	Tally) :-
	same(Entity, gramstain, [pos], Cf1),
	same(Entity, morphology, [coccus], Cf2),
	same(Entity, conformation, [shortchains], Cf3),
	min([Cf1,Cf2,Cf3], Tally).


f(X) :- call(X), !.		%  awful name!



%	T H E   * E N D *

