%   File   : PROLOG.TYP
%   Author : R.A.O'Keefe
%   Updated: 23 June 1983
%   Purpose: Type definitions for Prolog & utilities

%   the two built in types are
%       void            -- type of goals, truth values
%       any             -- matches anything at all

:- type integer -->             %  integers and expressions
        integer + integer
    |   integer - integer
    |   integer * integer
    |   integer //integer       % was /, should be div
    |   integer mod integer
    |   integer /\ integer      %  bitwise and
    |   integer \/ integer      %  bitwise or
    |   integer << integer      %  left shift
    |   integer >> integer      %  right shift
    |   + integer
    |   - integer
    |   [integer|any].



:- type list(T) --> [] | [T|list(T)].
:- type dbref --> ''.           %  this is a sort of abstract data type
:- type op --> xf | yf | yfx | xfx | xfy | fy | fx.
:- type order --> < | = | > .   %  for compare
:- type pair(X,Y) --> X-Y.      %  for keysort

:- pred {void,void},            %  , = conjunction
        {void;void},            %  ; = disjunction
        {void->void}.           %  -> = if-then

:- pred abolish(any, integer).
:- pred revive(any, integer).
:- pred incore(void).
:- pred asserta(void, dbref).
:- pred asserta(void).
:- pred assertz(void, dbref).
:- pred assertz(void).
:- pred retract(void).
:- pred clause(void, void, dbref).
:- pred clause(void, void).
:- pred recorda(any, any, dbref).
:- pred recordz(any, any, dbref).
:- pred recorded(any, any, dbref).
:- pred instance(dbref, any).
:- pred erase(dbref).
:- pred true.
:- pred length(list(_), integer).
:- pred name(any, list(integer)).
:- pred op(integer, op, any).
:- pred var(any).
:- pred atom(any).
:- pred !.
:- pred statistics.
:- pred statistics(any, any).
:- pred functor(any, any, integer).
:- pred call(void).
:- pred expand_term(any, any).
:- pred debug.
:- pred debugging.
:- pred display(any).
:- pred get(integer).
:- pred get0(integer).
:- pred leash(any).
:- pred nl.
:- pred nodebug.
:- pred print(any).
:- pred put(integer).
:- pred skip(integer).
:- pred tab(integer).
:- pred trace.
:- pred ttyflush.
:- pred ttyget(integer).
:- pred ttyget0(integer).
:- pred ttynl.
:- pred ttyput(integer).
:- pred ttyskip(integer).
:- pred ttytab(integer).
:- pred write(any).
:- pred writeq(any).
:- pred ancestors(list(void)).
:- pred depth(integer).
:- pred maxdepth(integer).
:- pred subgoal_of(void).
:- pred abort.
:- pred arg(integer, any, any).
:- pred assert(void).
:- pred atomic(any).
:- pred bagof(T, void, list(T)).
:- pred break.
:- pred close(any).
:- pred compare(order, any, any).
:- pred compile(any).
:- pred consult(any).
:- pred current_atom(any).
:- pred current_functor(any,any).
:- pred current_predicate(any,any).
:- pred current_op(integer, op, any).
:- pred fail.
:- pred fileerrors.
:- pred gc.
:- pred gcguide(any).
:- pred halt.
:- pred integer(any).
:- pred keysort(list(pair(X,Y)), list(pair(X,Y))).
:- pred listing.
:- pred listing(any).
:- pred log.
:- pred nofileerrors.
:- pred nogc.
:- pred nolog.
:- pred nonvar(any).
:- pred numbervars(any, integer, integer).
:- pred phrase(any, list(_)).
:- pred prompt(any, any).
:- pred read(any).
:- pred reconsult(any).
:- pred rename(any, any).
:- pred repeat.
:- pred restore(any).
:- pred save(any).
:- pred see(any).
:- pred seeing(any).
:- pred seen.
:- pred setof(T, void, list(T)).
:- pred sort(list(T), list(T)).
:- pred tell(any).
:- pred telling(any).
:- pred told.
:- pred trimcore.
:- pred plsys(any).
:- pred 'LC'.
:- pred 'NOLC'.
:- pred spy void.
:- pred nospy void.
:- pred \+ void.
:- pred T = T.
:- pred integer is integer.
:- pred T == T.
:- pred T \== T.
:- pred any =.. list(any).
:- pred integer < integer.
:- pred integer > integer.
:- pred integer =< integer.
:- pred integer >= integer.
:- pred any @< any.
:- pred any @=< any.
:- pred any @>= any.
:- pred any @> any.
:- pred any^void.
:- pred integer =\= integer.
:- pred integer =:= integer.

%       From here on belong to UTIL.

:- pred &(void, void).
:- pred \=(T, T).
:- pred \\(void, void).
:- pred any(list(void)).
:- pred append(list(T), list(T), list(T)).
:- pred apply(any, list(any)).
:- pred binding(any, void).
:- pred casserta(void).
:- pred cassertz(void).
:- pred cgensym(any, any).
:- pred check_exists(any).
:- pred checkand(any, any).
:- pred checklist(any, list(_)).
:- pred clean.
:- pred close(any, any).
:- pred concat(any, any, any).
:- pred continue.
:- pred convlist(any, list(_), list(_)).
:- pred delete(any).
:- pred diff(T, T).
:- pred disjoint(list(_)).
:- pred edit(any).
:- pred error(any, any, any).
:- pred eval(void).
:- pred eval(integer, integer).
:- pred file_exists(any).
:- pred findall(T, void, list(T)).
:- pred flag(any, T, T).
:- pred for(integer, void).
:- pred forall(void, void).
:- pred gcc(void).
:- pred gensym(any, any).
:- pred intersect(list(T), list(T), list(T)).
:- pred last(T, list(T)).
:- pred listtoset(list(T), list(T)).
:- pred mapand(any, any, any).
:- pred maplist(any, list(_), list(_)).
:- pred member(T, list(T)).
:- pred nextto(T, T, list(T)).
:- pred nmember(T, list(T), integer).
:- pred nobt(void).
:- pred not(void).
:- pred number(any).
:- pred numlist(integer, integer, list(integer)).
:- pred occ(any, any, integer).
:- pred open(any).
:- pred open(any, any).
:- pred pairfrom(list(T), T, T, list(T)).
:- pred perm(list(T), list(T)).
:- pred perm2(T, T, T, T).
:- pred read_in(list(T)).
:- pred redo(any).
:- pred remove_dups(list(T), list(T)).
:- pred rev(list(T), list(T)).
:- pred select(T, list(T), list(T)).
:- pred seteq(list(T), list(T)).
:- pred some(any, list(_)).
:- pred subgoal(any, void).
:- pred subset(list(T), list(T)).
:- pred subst(any, T, T).
:- pred subtract(list(T), list(T), list(T)).
:- pred sumlist(list(integer), integer).
:- pred thnot(void).
:- pred tidy(any, any).
:- pred tlim(integer).
:- pred ton(any).
:- pred toff.
:- pred toff(any).
:- pred trace(any, integer).
:- pred trace(any, any, integer).
:- pred union(list(T), list(T), list(T)).
:- pred variables(any, list(T)).
:- pred writef(any).
:- pred writef(any, any).
