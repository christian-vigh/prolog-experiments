
% optimized using freeze
solve(X):-               
	init(1,CL),
    time(0,T0),
	gen(CL),
	test(CL),
    time(T0,_),
	write('  SOLUTION:'),nl,
	pr(CL).

solveall:-
	init(1,CL),
    time(0,T0),
	bagof(CL,(gen(CL),test(CL)),L),
    time(T0,_),
	write('  ALL SOLUTIONS:'),nl,
	prall(L).


init(N,[couple(N,_,_,_,[_,_,_,_,_,_])|T]):-
	N<12 ->	N1 is N+1, init(N1,T) |
		T=[].

gen(CL):-
	transpose(CL,CLT), % CLT is used to constraint check columns
                           % this makes constraints checking easier.
	simple_clues(CL),
	constraints(CL,CLT),
	important_clues(CL,CLT),
	other_clues(CL,CLT).

test(CL):-
	check_names_and_purchases(CL,[],[],[],[]).

check_names_and_purchases([],_,_,_,_).
check_names_and_purchases([couple(_,MX,FX,NX,PX)|T],ML,FL,NL,PL):-
	\+ member(MX,ML),
	\+ member(FX,FL),
	\+ member(NX,NL),
	\+ member(PX,PL),
	check_names_and_purchases(T,[MX|ML],[FX|FL],[NX|NL],[PX|PL]).


% Optimized ordering of clues.

simple_clues(CL):-
% The following simple clues contains information that can be entered
% straight away.
	clue3(CL),
	clue6(CL),
	clue11(CL),
	clue13(CL),
	clue17(CL),
	clue20(CL).

important_clues(CL,CLT):-
% These clues contains information about many couples and will reduce
% the search space substantially.
% Check constraints after each clue.
	clue24(CL),
	constraints(CL,CLT),
	clue10(CL),
	constraints(CL,CLT),
	clue4(CL),
	constraints(CL,CLT).

other_clues(CL,CLT):-
% The rest of the clues. Check constraints now and then.
	clue18(CL),
	clue26(CL),
	constraints(CL,CLT),
	clue16(CL),
	clue19(CL),
	clue15(CL),
	constraints(CL,CLT),
	clue2(CL),
	clue5(CL),
	clue7(CL),
	clue8(CL),
	clue9(CL),
	constraints(CL,CLT),
	clue12(CL),
	clue14(CL),
	clue21(CL),
	clue22(CL),
	clue23(CL),
	constraints(CL,CLT),
	clue25(CL),
	clue1(CL),
	name_clue(CL),
	constraints(CL,CLT).


name_clue(CL):-
	member(couple(_,bob,_,_,_),CL),
	member(couple(_,_,elizabeth,_,_),CL),
	member(couple(_,_,_,stanton,_),CL).

clue1(CL):-
	before(couple(_,_,_,craig,[_,_,_,_,_,y]),
	       couple(N1,_,_,murphy,_),CL),
	N1=\=12.

clue2(CL):-
	member(couple(_,_,_,collin,[y,n,y,n,y,y]),CL).

clue3(CL):-
	member(couple( 8,_,_,_,[_,y,_,_,_,_]),CL),
	member(couple(10,_,_,_,[_,y,_,_,_,_]),CL).

clue4(CL):-
	consec([couple(_,_,_,smith,_),
	        couple(_,gary,_,_,_),
	        couple(_,_,_,_,[_,y,_,_,_,y]),
	        couple(_,_,_,swain,_),
	        couple(_,bill,_,_,_)],CL).

clue5(CL):-
	member(couple(_,_,geraldine,_,[_,_,_,_,n,n]),CL).

clue6(CL):-
	member(couple(12,_,_,_,[_,_,_,n,_,_]),CL).
	
clue7(CL):-
	member(couple(_,tom,_,_,[_,y,_,_,_,_]),CL).

clue8(CL):-
	member(couple(_,_,_,marshall,[_,_,n,n,_,_]),CL).

clue9(CL):-
	member(couple(_,_,evelyn,_,[y,_,n,_,_,_]),CL).

clue10(CL):-
	consec([couple(_,_,martha,_,_),
		couple(_,jack,_,_,_),
	        couple(_,_,_,_,[y,y,y,n,n,y]),
	        couple(_,_,_,_,[_,n,_,n,_,_]),
	        couple(_,_,margaret,_,_)], CL).

clue11(CL):-
	member(couple(1,_,_,_,[_,_,y,_,_,_]),CL),
	member(couple(2,_,_,_,[_,_,y,_,_,_]),CL),
	member(couple(3,_,_,_,[_,_,y,_,_,_]),CL),
	member(couple(4,_,_,_,[_,_,y,_,_,_]),CL),
	member(couple(5,_,_,_,[_,_,y,_,_,_]),CL).

clue12(CL):-
	member(couple(_,chuck,_,_,[n,_,_,_,_,_]),CL).

clue13(CL):-
	member(couple(1,_,_,_,[_,_,_,_,n,_]),CL),
	member(couple(2,_,_,_,[_,_,_,_,n,_]),CL),
	member(couple(4,_,_,_,[_,_,_,_,n,_]),CL).

clue14(CL):-
	member(couple(_,_,eleanor,_,[_,_,n,_,_,_]),CL).

clue15(CL):-
	member(couple(N1,allen,_,_,[n,_,_,_,_,n]),CL),
	member(couple(N2,_,_,anthony,[n,_,_,_,_,_]),CL),
	N1=\=N2.

clue16(CL):-
	member(couple(N1,john,_,_,[_,_,_,_,y,y]),CL),
	member(couple(N2,_,cheryl,_,[_,_,_,_,y,y]),CL),
	N2=\=10,
	N2=\=12,
	N2=\=N1.

clue17(CL):-
	member(couple(9,_,_,douglas,[n,_,_,_,n,_]),CL).
	
clue18(CL):-
	consec([couple(_,adam,_,_,[_,_,_,_,_,n]),
	        couple(_,_,_,day,_)],CL).

clue19(CL):-
	member(couple(_,steve,_,_,[_,y,_,y,y,_]),CL).

clue20(CL):-
	member(couple(10,_,_,_,[n,_,_,_,_,_]),CL),
	member(couple(11,_,_,_,[n,_,_,_,_,_]),CL),
	member(couple(12,_,_,_,[n,_,_,_,_,_]),CL).
	
clue21(CL):-
	member(couple(_,_,_,jones,[_,_,_,_,n,_]),CL).

clue22(CL):-
	member(couple(_,_,susan,_,[_,_,_,y,_,_]),CL).

clue23(CL):-
	member(couple(_,george,_,_,[_,_,_,_,y,_]),CL).

clue24(CL):- % optimized
	member(couple(N2,_,_,craig,[n,_,_,_,_,_]),CL),
	member(couple(N1,_,dorothy,_,[n,_,_,_,_,_]),CL),
	N1=\=N2,
	member(couple(N3,joe,_,_,[n,_,_,_,_,_]),CL),
	N3=\=N1, N3=\=N2,
	member(couple(N4,_,rosalyn,_,[n,_,_,_,n,_]),CL),
	N4=\=N1, N4=\=N2, N4=\=N3.

clue25(CL):-
	member(couple(_,_,_,oconnor,[_,_,y,_,y,_]),CL).

clue26(CL):-
	consec([couple(_,_,sandra,_,[_,_,_,_,n,_]),
	        couple(_,_,cathleen,_,_)],CL).


% Constraints on the number of items
%  -  Every couple bought 4 different items out of 6
%  -  There are 8 copies of each item
% These constraints are checked

constraints(CL,CLT):-
% always check purchases (row) and items (columns)
	check_purchases(CL),
	check_items(CLT).
	

%   check_purchases(X) and check_items(X) check
% purchases (rows) and items (columns) using constraints on the 
% number of items purchased and the number of items available.
check_purchases([]).
check_purchases([couple(_,_,_,_,[X|T1])|T]):-
	check(T1,4,2),
	check_purchases(T).

check_items([]).
check_items([[X|T1]|T]):-
	check(T1,8,4),
	check_items(T).

%     check(L,Ny,Nn) checks L (row or column).
% L is the (tail of) a row (purchase) or column (items).
% Ny (Nn) is the maximum number of y's (n's) remaining.
check([],0,0).
check([],_,_):-
	write(' *** check error ***'),nl,fail.
check(P,Ny,Nn):-
	freeze(X,P=[X|T]).
	(X=y ->
		Ny>0,
		Ny1 is Ny-1, check(T,Ny1,Nn))|
	 X=n ->
		Nn>0,
		Nn1 is Nn-1, check(T,Ny,Nn1)).

% Support predicates

% transpose the couple matrix (O(n**2))
% transpose(M,Tr)  Tr == transpose_purchases_part_of(M)
transpose([],[]).
transpose([],[[]|T]):-
	transpose([],T).
transpose([couple(_,_,_,_,P)|T],Tr):-
	tr_row(P,diff(Tr,TrT)),
	transpose(T,TrT).
% transpose a row
% tr_row(Row,TrR)   TrR == transpose(Row)
tr_row([],diff([],[])).
tr_row([X|T],diff([[X|XT]|T1],[XT|T2])):-
	tr_row(T,diff(T1,T2)).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

before(C1,C2,[C1|T]):-
	member(C2,T).
before(C1,C2,[_|T]):-
	before(C1,C2,T).

consec([X|T1],[X|T2]):-
	head_sublist(T1,T2).
consec(S,[_|T]):-
	consec(S,T).

head_sublist([],_).
head_sublist([X|T1],[X|T2]):-
	head_sublist(T1,T2).

pr([]).
pr([H|T]):-
	write(H), nl,
	pr(T).	

prall([]).
prall([H|T]):-
	pr(H),nl,
	prall(T).

time(TIN,TOUT):-
	TIN=0 -> statistics(runtime,[TOUT,_]) |
		statistics(runtime,[TOUT,_]),
		T is TOUT-TIN,
		write('  Extime = '), write(T), write(' ms'), nl.

