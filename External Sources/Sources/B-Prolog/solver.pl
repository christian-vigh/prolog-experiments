/* A constraint solver implemented in B-Prolog for CP'05 Solver Competition */
/* by Neng-Fa Zhou, June 12, 2005, at Monash University on leave from CUNY. */
/* Thanks to Mark Wallace for his comments and corrections                  */
/* Version 1.1, run with B-Prolog 6.7.5 or up                               */
/* The submitted version is twice as fast thanks to two built-ins in C.     */
main:-
    get_main_args(L),
    process_args(L,BaseName),
    solve(BaseName).

process_args([],InFile):-format("usage: solver FileName",[]).
process_args([BaseName|Args],BaseName):-
    process_args_aux(Args).

process_args_aux(['>',OutFile|Args]):-!,tell(OutFile).
process_args_aux(_).

solve(BaseName):-
    atom_codes(BaseName,String),
    append(String,".pl",String2),
    atom_codes(InFile,String2),
    (exists(InFile)->true;format("File not found:~w~n",[InFile]),halt),
    see(InFile),
    cputime(Start),
    read((comp_problem(Vars):-Body)),
    seen,
    call(Body),
    cputime(Mid),
    top(Vars),
    cputime(End),
    InstallTime is Mid-Start,
    Time is End-Mid,
    format("~ninstallation time: ~w, search time: ~w~n",[InstallTime,Time]).

test:-
    comp_problem(Vars), % problem instance, loaded separately
    top(Vars).

all:-
    comp_problem(Vars), % problem instance, loaded separately
    top(Vars),fail.

top(Vars):-
    garbage_collect,
    solve_vars(Vars).

solve_vars(Vars):-
    labeling_ffc(Vars),
    write('SAT'),nl,
    write_list(Vars),nl.
solve_vars(Vars):-
    write('UNSAT'),nl.

write_list([]):-true : true.
write_list([X|Xs]):-true :
    write(X),write(' '),
    write_list(Xs).

% wait until the constraint becomes unary, and then exclude no good values
% from the free variable argument  
conflict_constraint(Rel,Constr),no_vars_gt(1,1),{ins(Constr)} => true.
conflict_constraint(Rel,Constr) =>
    project_constr_on_one_arg(Constr,GroundArgs,1,I,Xi),
    Key=k(I,GroundArgs),
    conflict_constraint_action(Rel,Key,Xi).

conflict_constraint_action(Rel,Key,Xi):-
    hashtable_get(Rel,Key,Elm) :
    Elm=nogood(NoGoodValues),
    Xi notin NoGoodValues.
conflict_constraint_action(Rel,Key,Xi):-true : true.

% For a conflict relation [T1,...,Tn], Rel is a hashtable where the key of an element has the form k(I,T) 
% and the value is a list of no good integers [A1,...,Ak]. T is a tuple in the projection of the original 
% relation onto columns except column I.
conflict_rel(Rel,Tuples,N):-
    new_hashtable(Rel,N),
    project_rel_on_one_argument(Tuples,Rel).

conflict_rel(Rel,Tuples):-
    length(Tuples,N),
    conflict_rel(Rel,Tuples,N).

project_rel_on_one_argument([],Rel):-true : true.
project_rel_on_one_argument([T|Ts],Rel):-true :
    project_tuple_on_one_argument([],T,1,Rel),
    project_rel_on_one_argument(Ts,Rel).

project_tuple_on_one_argument(Left,[],I,Rel):-true : true.
project_tuple_on_one_argument(Left,[X|Xs],I,Rel):-true :
    rev_append(Left,Xs,Key),
    (hashtable_get(Rel,k(I,Key),Value)->
     Value=nogood(Elms),
     list_insert_into_sorted(Elms,X,Elms1), %built-in Elms1 is sorted after X is sorted
     setarg(1,Value,Elms1); % dirty, but to avoid O(n) insertion
     hashtable_put(Rel,k(I,Key),nogood([X]))),
    I1 is I+1,
    project_tuple_on_one_argument([X|Left],Xs,I1,Rel).
    
rev_append([],Ys,Zs):-true : Zs=Ys.
rev_append([X|Xs],Ys,Zs):-true :
    rev_append(Xs,[X|Ys],Zs).
    
% Support constraint propagator. 
% Maintain interval consistency when a constraint is non-binary
% Maintain arc consistency when it turns into binary
support_constraint(Rel,Constr):-
    Rel=support(DomainVector,MMTable,PRs),
    preprocess_column_intervals(Constr,1,MMTable),
    generate_interval_propagators(Constr,Constr,1,MMTable),
    wait_until_binary(DomainVector,PRs,Constr).

% Preprocess the domains to make the constraint interval consistent
preprocess_column_intervals([],I,MMTable):-true : true.
preprocess_column_intervals([X|Xs],I,MMTable):-true :
    hashtable_get(MMTable,I,minmax(Min,Max)),
    domain(X,Min,Max),
    I1 is I+1,
    preprocess_column_intervals(Xs,I1,MMTable).

generate_interval_propagators([],Constr,_,MMTable):-true : true.
generate_interval_propagators([X|Xs],Constr,I,MMTable):- true :
    interval_propagator(X,Constr,I,MMTable),
    I1 is I+1,
    generate_interval_propagators(Xs,Constr,I1,MMTable).

% X is the Ith argument in Constr
interval_propagator(X,Constr,I,MMTable),var(X),{ins(X)} => true.
interval_propagator(X,Constr,I,MMTable) =>
     reduce_intervals_1(X,Constr,I,1,MMTable).

reduce_intervals_1(X,[],I,J,MMTable):-true : true.
reduce_intervals_1(X,[Y|Ys],I,J,MMTable):-I=:=J :
    J1 is J+1,
    reduce_intervals_1(X,Ys,I,J1,MMTable).
reduce_intervals_1(X,[Y|Ys],I,J,MMTable):-true :
    hashtable_get(MMTable,k(I,X,J),minmax(Min,Max)),
    domain(Y,Min,Max), 
    J1 is J+1,
    reduce_intervals_1(X,Ys,I,J1,MMTable).
		  
% Do nothing if the constraint is non-binary
wait_until_binary(DomainVector,PRs,Constr),no_vars_gt(1,2),{ins(Constr)} => true.
wait_until_binary(DomainVector,PRs,Constr) =>
    project_constr_on_two_args(Constr,GroundArgs,1,I,Xi,J,Xj),
    hashtable_get(PRs,k(I,J,GroundArgs),PairsTable),
    forward_checking_propagator(PairsTable,I,Xi,J,Xj),
    forward_checking_propagator(PairsTable,J,Xj,I,Xi),
    (var(Xi),var(Xj)->
        arg(I,DomainVector,Di),
        arg(J,DomainVector,Dj),
        new_hashtable(Counters),
        build_support_counters(Di,I,Xi,Xj,PairsTable,Counters),
        build_support_counters(Dj,J,Xj,Xi,PairsTable,Counters),
        arc_propagator(Counters,PairsTable,I,Xi,J,Xj),
        arc_propagator(Counters,PairsTable,J,Xj,I,Xi);
     true).

build_support_counters([],I,Xi,Xj,PairsTable,Counters):-true : true.
build_support_counters([E|Es],I,Xi,Xj,PairsTable,Counters):-true ?
    hashtable_get(PairsTable,k(I,E),Value),!,
    Value=values(SupportE),
    count_in(SupportE,Xj,0,CountE),
    (CountE=:=0->domain_set_false(Xi,E);
     hashtable_put(Counters,k(I,E),count(CountE))),
    build_support_counters(Es,I,Xi,Xj,PairsTable,Counters).
build_support_counters([_|Es],I,Xi,Xj,PairsTable,Counters):-true :
    build_support_counters(Es,I,Xi,Xj,PairsTable,Counters).

count_in([L..U|Es],X,Count0,Count):-true :
    count_in_interval(L,U,X,Count0,Count1),
    count_in(Es,X,Count1,Count).
count_in([E|Es],X,Count0,Count):-b_DM_TRUE_cc(X,E) :
    Count1 is Count0+1,
    count_in(Es,X,Count1,Count).
count_in([E|Es],X,Count0,Count):-true :
    count_in(Es,X,Count0,Count).
count_in([],X,Count0,Count):-true : Count=Count0.

count_in_interval(L,U,X,Count0,Count):-L>U : Count=Count0.
count_in_interval(L,U,X,Count0,Count):-b_DM_TRUE_cc(X,L) :
    Count1 is Count0+1,L1 is L+1,
    count_in_interval(L1,U,X,Count1,Count).
count_in_interval(L,U,X,Count0,Count):-true :
    L1 is L+1,
    count_in_interval(L1,U,X,Count0,Count).

% whenever an element Ei is excluded from Xi, if the constraint (Xi,Xj) is functional
% then the counterpart Ej of Ei in Xj must be excluded as well 
arc_propagator(Counters,PairsTable,I,Xi,J,Xj),var(Xi),var(Xj),{dom_any(Xi,Ei)} =>
    arc_propagator_action(Counters,PairsTable,I,Ei,J,Xj).
arc_propagator(Counters,PairsTable,I,Xi,J,Xj) => true.

arc_propagator_action(Counters,PairsTable,I,Ei,J,Xj):-
    hashtable_get(PairsTable,k(I,Ei),HElm),!,
    HElm=values(SupportEi),
    decrement_counters(Counters,J,SupportEi,Xj).
arc_propagator_action(Counters,PairsTable,I,Ei,J,Xj).

decrement_counters(Counters,J,_,Xj):-nonvar(Xj) : true.
decrement_counters(Counters,J,[],Xj):-true : true.
decrement_counters(Counters,J,[L..U|Es],Xj):-true :
   decrement_counters_interval(Counters,J,L,U,Xj),
   decrement_counters(Counters,J,Es,Xj).
decrement_counters(Counters,J,[E|Es],Xj):-true :
    decrement_counter(Counters,J,E,Xj),
    decrement_counters(Counters,J,Es,Xj).

decrement_counters_interval(Counters,J,L,U,Xj):-L>U : true.
decrement_counters_interval(Counters,J,L,U,Xj):-true :
    decrement_counter(Counters,J,L,Xj),
    L1 is L+1,
    decrement_counters_interval(Counters,J,L1,U,Xj).

decrement_counter(Counters,J,E,Xj):-
    b_DM_TRUE_cc(Xj,E) :  % E is in the domain of Xj
    hashtable_get(Counters,k(J,E),Counter),
    Counter=count(Count),
    (Count=:=1->domain_set_false(Xj,E);
     NewCount is Count-1,
     setarg(1,Counter,NewCount)).
decrement_counter(Counters,J,E,Xj):-true : true.

% Once Xi or Xj is instantiated, the other one can be instantiated 
% as well if the constraint is functional 
forward_checking_propagator(PairsTable,I,Xi,J,Xj),var(Xi),{ins(Xi)} => true.
forward_checking_propagator(PairsTable,I,Xi,J,Xj) =>
    (var(Xj)->
       hashtable_get(PairsTable,k(I,Xi),values(Values)),
       Xj :: Values
     ;       
       (I<J -> hashtable_get(PairsTable,k(0,Xi,Xj),_)
        ;
        hashtable_get(PairsTable,k(0,Xj,Xi),_))).

% For a support relation [T1,...,Tn], Rel is a structure support(DVector,MinMaxHashtable,PRs)
% where DVector is a vector of domains, MinMaxHashtable contains an element for each value 
% (i,x) and column j (j\=i) that stores the min and max of the support values in column j, and 
% PRs is a hashtable that stores several projected binary relations from the original relation. 
% Each element in PRs has the key (I,J,T) (where T is a tuple in the projected relation on
% columns other than I and J) and the value is a hashtable that contains support pairs
%      [elm(k(I,A1),SupportA1),...,elm(k(I,Ak),SupportAk),
%       elm(k(J,B1),SupportB1),...,elm(k(J,Bl),SupportBl)]
support_rel(Rel,Domains,Tuples):-
    length(Tuples,N),
    support_rel(Rel,Domains,Tuples,N).

support_rel(Rel,Domains,Tuples,N):-
    DomainVector=..[domains|Domains],
    Rel=support(DomainVector,MMTable,PRs),
    new_hashtable(MMTable,N),
    new_hashtable(PRs,N),
    project_rel_on_two_args(Tuples,MMTable,PRs).
%    write(MMTable),nl.
%    write(PRs),nl.

project_rel_on_two_args([],MMTable,PRs):-true : true.
project_rel_on_two_args([T|Ts],MMTable,PRs):-true :
    project_tuple_on_two_args(T,1,MMTable,[],PRs),
    project_rel_on_two_args(Ts,MMTable,PRs).

project_tuple_on_two_args([X],I,MMTable,Left,PRs):-true : 
    update_min_max_column(I,X,MMTable).
project_tuple_on_two_args([X|Xs],I,MMTable,Left,PRs):-true :
    I1 is I+1,
    update_min_max_column(I,X,MMTable),
    project_tuple_on_two_args(Xs,I,X,I1,MMTable,Left,PRs),
    project_tuple_on_two_args(Xs,I1,MMTable,[X|Left],PRs).

update_min_max_column(I,X,MMTable):-
    hashtable_get(MMTable,I,MM),!,
    MM=minmax(Min,Max),
    (X<Min->setarg(1,MM,X);true),
    (X>Max->setarg(2,MM,X);true).
update_min_max_column(I,X,MMTable):-
    hashtable_put(MMTable,I,minmax(X,X)).

project_tuple_on_two_args([],I,X,J,MMTable,Left,PRs):-true : true.
project_tuple_on_two_args([Y|Ys],I,X,J,MMTable,Left,PRs):-true :
    (hashtable_get(MMTable,k(I,X,J),MMj)->
     MMj=minmax(MinJ,MaxJ),
     (Y<MinJ->setarg(1,MMj,Y);true),
     (Y>MaxJ->setarg(2,MMj,Y);true);
     hashtable_put(MMTable,k(I,X,J),minmax(Y,Y))),
    (hashtable_get(MMTable,k(J,Y,I),MMi)->
     MMi=minmax(MinI,MaxI),
     (X<MinI->setarg(1,MMi,X);true),
     (X>MaxI->setarg(2,MMi,X);true);
     hashtable_put(MMTable,k(J,Y,I),minmax(X,X))),
    rev_append(Left,Ys,Key),
    (hashtable_get(PRs,k(I,J,Key),Pairs)->
     register_pairs(I,X,J,Y,Pairs);
     new_hashtable(Pairs),
     register_pairs(I,X,J,Y,Pairs),
     hashtable_put(PRs,k(I,J,Key),Pairs)),
    J1 is J+1,
    project_tuple_on_two_args(Ys,I,X,J1,MMTable,[Y|Left],PRs).

register_pairs(I,X,J,Y,Pairs):-
    (hashtable_get(Pairs,k(I,X),SupportX)->
     SupportX=values(ElmsX),
     list_insert_into_sorted(ElmsX,Y,ElmsX1),
     setarg(1,SupportX,ElmsX1);
     hashtable_put(Pairs,k(I,X),values([Y]))),
    (hashtable_get(Pairs,k(J,Y),SupportY)->
     SupportY=values(ElmsY),
     list_insert_into_sorted(ElmsY,X,ElmsY1),
     setarg(1,SupportY,ElmsY1);
     hashtable_put(Pairs,k(J,Y),values([X]))),
    (hashtable_get(Pairs,k(0,X,Y),_)->true;
     hashtable_put(Pairs,k(0,X,Y),_)).
    
% utilities
% Given a constraint [X1,...,Xn], extract the free variable Xi 
% from the constraint and return [X1,...,Xi-1,Xi+1,...,Xn] and Xi.
% If no argument is free, then treat Xn as free.
project_constr_on_one_arg([X],GroundArgs,I0,I,Xi):-true : GroundArgs=[],Xi=X,I=I0.
project_constr_on_one_arg([Arg|Args],GroundArgs,I0,I,Xi):-var(Arg) :
    I=I0, Xi=Arg, GroundArgs=Args.
project_constr_on_one_arg([Arg|Args],GroundArgs,I0,I,Xi):-true :
    GroundArgs=[Arg|GroundArgs1],
    I1 is I0+1,
    project_constr_on_one_arg(Args,GroundArgs1,I1,I,Xi).
    
project_constr_on_two_args([X1,X2],GroundArgs,I0,I,Xi,J,Xj):-true :
     GroundArgs=[],Xi=X1,Xj=X2,I=I0,J is I0+1.
project_constr_on_two_args([X|Xs],GroundArgs,I0,I,Xi,J,Xj):-var(X) :
     Xi=X, I=I0, I1 is I0+1,
     project_constr_on_one_arg(Xs,GroundArgs,I1,J,Xj).
project_constr_on_two_args([X|Xs],GroundArgs,I0,I,Xi,J,Xj):-true :
     GroundArgs=[X|GroundArgs1],
     I1 is I0+1,
     project_constr_on_two_args(Xs,GroundArgs1,I1,I,Xi,J,Xj).

