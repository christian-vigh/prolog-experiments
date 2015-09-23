%   File   : pl/system
%   Author : R.A.O'Keefe
%   Updated: Friday March 2nd, 1984, 6:39:09 pm
%   Purpose: Table of built-in predicates.

/*  This table exists for two reasons.
    1.  A user who can't remember what's available can just say listing(system).
    2.  Tools such as debuggers and cross-referencers can tell the difference
	between the user's predicates and the system's, they should not trace
	system predicates.
    Note that there are a couple of dozen predicates (mostly beginning with $)
    which the user is not particularly supposed to know about, though it can be
    very handy to get at some of them.

    BEWARE: In the SRI versions of C-Prolog the "shell" predicate is called
    "system".  "system" is the established name for THIS operation, and the
    EdAI versions once had only "shell".  If you don't like "shell" as the name
    of the "execute operating system command" command, use any name you like
    which doesn't clash with these nor with the library predicates.
    Keep this up to date as new predicates are added!
*/

system(&(_,_)).
system('C'(_,_,_)).
system('LC').
system('NOLC').
system([_|_]).
system(\+(_)).
system(_ < _).
system(_ = _).
system(_ =:= _).
system(_ =< _).
system(_ == _).
system(_ =\= _).
system(_ > _).
system(_ >= _).
system(_ @< _).
system(_ @=< _).
system(_ @> _).
system(_ @>= _).
system(_ \= _).
system(_ \== _).
system(_ ^ _).
system(_ =.. _).
system(abolish(_,_)).
system(abort).
system(all_float(_,_)).
system(append(_)).
system(arg(_,_,_)).
system(assert(_)).
system(assert(_,_)).
system(asserta(_)).
system(asserta(_,_)).
system(assertz(_)).
system(assertz(_,_)).
system(atom(_)).
system(atomic(_)).
system(bagof(_,_,_)).
system(break).
system(call(_)).
system(cd(_)).
system(chtype(_,_,_)).
system(clause(_,_)).
system(clause(_,_,_)).
system(close(_)).
system(compare(_,_,_)).
system(consult(_)).
system(current_atom(_)).
system(current_functor(_,_)).
system(current_line_number(_)).
system(current_line_number(_,_)).
system(current_op(_,_,_)).
system(current_predicate(_,_)).
system(db_reference(_)).
system(debug).
system(debugging).
system(display(_)).
system(erase(_)).
system(erased(_)).
system(exists(_)).
system(expand_term(_,_)).
system(expanded_exprs(_,_)).
system(fail).
system(fileerrors).
system(findall(_,_,_)).
system(findall(_,_,_,_)).
system(functor(_,_,_)).
system(get(_)).
system(get0(_)).
system(ground(_)).
system(halt).
system(instance(_,_)).
system(integer(_)).
system(is(_,_)).
system(is(_,_,_)).
system(is(_,_,_,_)).
system(is_op(_,_,_,_,_)).
system(keysort(_,_)).
system(leash(_)).
system(leash(_,_)).
system(length(_,_)).
system(lib(_)).
system(lib(_,_)).
system(libdirectory(_)).		% USER MODIFIABLE
system(listing(_)).
system(listing).
system(merge(_,_,_)).
system(msort(_,_)).
system(name(_,_)).
system(nl).
system(nodebug).
system(nofileerrors).
system(nonvar(_)).
system(nospy(_)).
system(not(_)).
system(note_lib(_)).
system(number(_)).
system(numbervars(_,_,_)).
system(occurs_check(_,_)).
system(once(_)).
system(op(_,_,_)).
system(phrase(_,_)).
system(plus(_,_,_)).
system(portray(_)).			% USER MODIFIABLE
system(portray_clause(_)).
system(primitive(_)).
system(print(_)).
system(prompt(_,_)).
system(put(_)).
system(read(_)).
system(read(_,_)).
system(reconsult(_)).
system(record(_,_,_)).
system(recorda(_,_,_)).
system(recorded(_,_,_)).
system(recordz(_,_,_)).
system(rename(_,_)).
system(repeat).
system(retract(_)).
system(retractall(_)).
system(save(_)).
system(save(_,_)).
system(see(_)).
system(seeing(_)).
system(seeing(_,_)).
system(seeing(_,_)).
system(seen).
system(setof(_,_,_)).
system(sh).
system(shell(_)).
system(skip(_)).
system(sort(_,_)).
system(spy(_)).
system(statistics).
system(succ(_,_)).
system(system(_)).
system(tab(_)).
system(tell(_)).
system(telling(_)).
system(telling(_,_)).
system(telling(_,_)).
system(term_expansion(_,_)).		% USER MODIFIABLE
system(told).
system(trace).
system(true).
system(ttyflush(_)).
system(ttyflush).
system(ttyget(_)).
system(ttyget0(_)).
system(ttynl).
system(ttyput(_)).
system(ttyskip(_)).
system(ttytab(_)).
system(unify(_,_)).
system(unknown(_,_)).
system(var(_)).
system(write(_)).
system(writeq(_)).




