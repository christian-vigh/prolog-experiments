%Copyright 1992,1993
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%This file contins all routines that know how an interval is represented 
%within a meta_term and that integers are allowed.
%The other routines in xinterval.pl all operate on upper and lower bounds 
%that have already been extracted from an interval

%An interval is held in a meta_term of the form ii(L,H)
%L is the lower bound and H the upper.
%minf and pinf are used for minus infinity and plus infinity respectively
%Individual integers are allowed and an integer I is equivalent to ii(I,I)
%In 2's complement arithmetic the most negative integer is not allowed
%and is explicitly forbidden in the code in this file (other files
%happily ignore it)

%Extract a sensible upper and lower bound from almost anything
:-mode(ival(?,-,-)).
ival(I,L,H):- meta(I), !, meta_term(I,ii(L,H)).
ival(I,L,H):- var(I), !, L=minf, H=pinf. %Red cut, not a meta term
ival(I,L,H):- integer(I), !, 
	(I<xminint 
          ->(L=minf,H=minf) %Hide integers with no positive representation 
	  ; (L=I, H=I)
        ).

%Ensure that A is an integer or interval.  If a variable then force to
%interval without firing constraints (this is an important performance
%potimization especially atstart up)
iinteger_check(A):- integer(A), !.
iinteger_check(A):- meta(A), !, meta_term(A,ii(_,_)).
%This does not fire constraints (important at beginning when things are
%being set up)
iinteger_check(A):- var(A), !, meta_term(A,ii(minf,pinf)).

%Set a variable to an interval.
%Interesect with prior interval and fail if result empty
:-mode(iset(?,++,++,++)).
iset(I,minf,pinf,_N):- !, iinteger_check(I).
iset(I,L,H,_N):- integer(I), !, xgeq(I,L), xleq(I,H).
iset(I,J,J,N):- integer(J), !, 
	debug_set(I,J,J,N),
	I=J. %This may trigger unififcation I think this is OK
iset(I,L,H,N):- meta(I), !, %assume is in correct form of meta-term
	    meta_term(I,ii(Li,Hi)),
	    xmax(L,Li,Lo), xmin(H,Hi,Ho), xleq(Lo,Ho),
	    meta_term(T,ii(Lo,Ho)),
	    (Li=Lo,Hi=Ho 
	      -> true 
	      ;  (debug_set(I,Lo,Ho,N)
	         ,meta_bind(I,T)
	         ,eclipse(wake_delayed_goals(I,bound))
	         )
	    ).
iset(I,L,H,N):- var(I), !, debug_set(I,L,H,N), 
	%meta_term does not seem to fire constraints waiting on initial/1
	%conditions and meta_bind only workson things that are meta terms
	%but it does fire conditions hence this complicated footwork
%	meta_term(T,ii(L,H)), meta_term(I,ii(_,_)), meta_bind(I,T).
	meta_term(I,ii(L,H)), wake_delayed_goals(I,bound).

%Is the interval OK to split (ii(minf,minf) and ii(pinf,pinf) are not).
xspok(V):- meta(V), ival(V,L,H), (xgt(H,minf);xlt(L,pinf)), !.

%Unification exception handlers - see Sepia manual
meta_meta_unify(A,B):- debug_ii(unify,unify(A,B),N), m_m_u(A,B,N).

:-set_error_handler(10,meta_meta_unify/2).

m_m_u(A,B,N):- A==B, !, debug_i(done,N).
m_m_u(A,B,N):- integer(A), !, B=A, debug_i(done,N).
m_m_u(A,B,N):- integer(B), !, B=A, debug_i(done,N).
m_m_u(A,B,N):-
	meta_term(A,ii(Avl,Avh)),
	iset(B,Avl,Avh,N),
	%By this point A may have been modified by propogated bindings
	%It may even be an integer by now!!
	mmu(A,B,N), debug_i(done,N).

mmu(A,B,_N):- A==B, !.
mmu(A,B,_N):- integer(A), !, B=A.
mmu(A,B,N):- debug_i(bind,N), 
	     meta_bind(A,B), 
	     eclipse(wake_delayed_goals(A,bound)).

%This must not call iset/3 because iset/3 implicitly calls this in some cases
%by doing bindings (also meta_meta_unify/2)
meta_term_unify(A,I):- A==I, !.
meta_term_unify(A,I):-
	integer(I), meta_term(A,ii(Al,Ah)), !,
	xgeq(I,Al), xgeq(Ah,I),
	debug_ii(unify,meta_term_unify(A,I),_),
	meta_bind(A,I),
	eclipse(wake_delayed_goals(A,bound)).
%Very subtle case - two unifications can occur within primitive sequence 
%(hence no calls to delayed goals) which lead to two calls to 
%meta_term_unify but the second time with A bound to something real!!
meta_term_unify(A,I):-
	integer(I), integer(A), !, 
	A=I.
:-set_error_handler(11,meta_term_unify/2).

:- export portray/2.
%Print intervals
portray(Stream,Term):-
	meta(Term), 
	meta_term(Term,ii(L,H)), !,
	write(Stream,Term), %put out the name of the variable
	portr(Stream,L,H).
portray(Stream,Term):-
	integer(Term), Term<xminint, !, %We hide such integers from the user
	portr(Stream,minf,minf).

portr(Stream,L,H):-
	printf(Stream,"[",[]), 
	plh(Stream,L,H),
	printf(Stream,"]",[]).

plh(Stream,pinf,pinf):- !, printf(Stream,"+..",[]).
plh(Stream,minf,minf):- !, printf(Stream,"..-",[]).
plh(Stream,L,H):-
	pl(Stream,L),
	printf(Stream,"..",[]),
	pl(Stream,H).

pl(_Stream,minf).
pl(_Stream,pinf).
pl(Stream,I):- integer(I), printf(Stream,"%d",[I]).


