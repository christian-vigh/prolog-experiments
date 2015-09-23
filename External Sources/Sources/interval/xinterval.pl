%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%These routines do the arithmetic on extended integers (integer + infinity)
%pinf === positive infinity 
%minf === negative infinity
%ii(minf,minf) and ii(pinf,pinf) are allowed intervals (being respectively 
%an interval of very negative or very positive integers)

%Almost all routines are deterministic

%Warning:there is care being taken here to get this stuff to execute fast
%some code is a litle less than elegant
%Have added mode declarations and lots of cuts for efficiency

%The following set of routines do arithmetic on upper and lower bounds

%Split an interval into two parts
%Always split on 0 if possible otherwise take midpoint
%It is assumed that the lower bound is strictly less than the upper bound
%As a covenience in tracing the splits are always most negative interval
%first
:-mode(xisp(++,++,-,-)).
%Contains 0, split into negative, 0 and positive
xisp(L,H,Ln,Hn):- xlt0(L), xgt0(H), !, ((Ln=L,Hn= -1);(Ln=0,Hn=0);(Ln=1,Hn=H)).
%Lower bound of 0
xisp(0,H,Ln,Hn):- !, ((Ln=0,Hn=0);(Ln=1,Hn=H)).
%Upper bound of 0
xisp(L,0,Ln,Hn):- !, ((Ln=L,Hn= -1);(Ln=0,Hn=0)).
%lower bound of minf
xisp(minf,H,Ln,Hn):- !, ((Ln=minf,Hn=minf);(xminint(Ln),Hn=H)).
%Upper bound of pinf
xisp(L,pinf,Ln,Hn):- !, ((xmaxint(Hn),Ln=L);(Ln=pinf,Hn=pinf)).
%Integer cases
%Split an all negative interval
xisp(L,H,Ln,Hn):- xlt0(H), !, xispm_neg(L,H,M,N), ((Ln=L,Hn=M);(Ln=N,Hn=H)).
%Split an all positive interval
xisp(L,H,Ln,Hn):- xgt0(L), !, xispm_pos(L,H,M,N), ((Ln=L,Hn=M);(Ln=N,Hn=H)).

%Split a negative interval not containing 0
%It is guaranteed that I<=M<N<=J
%Care is taken here that the intermediate calculations and results never 
%underflow (this is delicate code)
:-mode(xispm_neg(++,++,-,-)).
xispm_neg(I,J,M,N):- 
	debug_check((integer(I), integer(J), I<J, J<0)),
	N is I//2 + J//2, M is N-1,
	debug_check((I=<M,M<N,N=<J)).

%Split a positive interval not containing 0
%It is guaranteed that I<=M<N<=J
%Care is taken here that the intermediate calculations and results never 
%overflow (this is delicate code)
:-mode(xispm_pos(++,++,-,-)).
xispm_pos(I,J,M,N):- 
	debug_check((integer(I), integer(J), I<J, I>0)),
	M is I//2 + J//2, N is M+1,
	debug_check((I=<M,M<N,N=<J)).

%xadd(A,B,C) C := A+B
%Assume arguments valid
%Add two lower bounds
%The tricky case is minf+pinf=minf for two lower bounds
:-mode(xadd_ll(++,++,-)).
%Warning:the order of these clauses and those in succeeding predicates is 
%crucial
xadd_ll(minf,_,minf):-!.
xadd_ll(_,minf,minf):-!.
%The bracketing is important to prevent overflow
xadd_ll(pinf,I,K):- integer(I), I<0, !, K is (xmaxint+I)+1.
xadd_ll(pinf,_,pinf):-!.
xadd_ll(I,pinf,K):- integer(I), I<0, !, K is (xmaxint+I)+1.
xadd_ll(_,pinf,pinf):-!.
xadd_ll(I,J,K):- debug_check((integer(I), integer(J))), mach_add(I,J,K).

%Partial evaluation should do wonders here
:-mode(xadd_hh(++,++,-)).
%Assume arguments valid
%Add two upper bounds
%The tricky case is minf+pinf=pinf for two upper bounds
xadd_hh(I,J,K):- xneg(I,Im), xneg(J,Jm), xadd_ll(Im,Jm,Km), xneg(Km,K).

%xsub_lh(A,B,C) C := A-B
:-mode(xsub_lh(++,++,-)).
%pinf-pinf=minf and minf-minf=minf for lower bounds
xsub_lh(I,J,K):- xneg(J,Jm), xadd_ll(I,Jm,K).

%xsub_hl(A,B,C) C := A-B
:-mode(xsub_hl(++,++,-)).
%pinf-pinf=pinf and minf-minf=pinf for upper bounds
xsub_hl(I,J,K):- xneg(I,Im), xadd_ll(Im,J,Km), xneg(Km,K).

%B = A+1 where A is an upper bound and B is a lower bound
:-mode(xinc_hl(++,-)).
xinc_hl(minf,B):-!, xminint(B).
xinc_hl(A,B):- xmaxint(A),!, B=pinf.
xinc_hl(A,B):- integer(A), !, B is A+1.

%B = A+1 where A and B are both lower bounds
:-mode(xinc_ll(++,-)).
xinc_ll(minf,B):-!, B=minf.
xinc_ll(pinf,B):-!, B=pinf.
xinc_ll(A,B):- xmaxint(A),!, B=pinf.
xinc_ll(A,B):- integer(A), !, B is A+1.

%B = A-1 where A is a lower bound and B is an upper bound
:-mode(xdec_lh(++,-)).
xdec_lh(pinf,B):-!, xmaxint(B).
xdec_lh(A,B):- xminint(A),!, B=minf.
xdec_lh(A,B):- integer(A), !, B is A-1.

%B = A-1 where A and B are both upper bounds
:-mode(xdec_hh(++,-)).
xdec_hh(minf,B):-!, B=minf.
xdec_hh(pinf,B):-!, B=pinf.
xdec_hh(A,B):- xminint(A),!, B=minf.
xdec_hh(A,B):- integer(A), !, B is A-1.

:-mode(xneg(++,-)).
%B = -A
xneg(pinf,minf):-!.
xneg(minf,pinf):-!.
xneg(I,J):- J is -I.

%C=max(A,B)
:-mode(xmax(++,++,-)).
xmax(A,B,B):- xgeq(B,A), !.
xmax(A,B,A):- xgt(A,B), !.

%C=min(A,B)
:-mode(xmin(++,++,-)).
xmin(A,B,A):- xgeq(B,A), !.
xmin(A,B,B):- xgt(A,B), !.

%A>=B
:-mode(xgeq(++,++)).
xgeq(pinf,minf):-!.
xgeq(pinf,I):-integer(I), !.
xgeq(pinf,pinf):-!.
xgeq(I,J):- integer(I), integer(J), !, I>=J.
xgeq(I,minf):-integer(I), !.
xgeq(minf,minf):-!.

%A=<B
xleq(A,B):- xgeq(B,A).

%A>B
:-mode(xgt(++,++)).
xgt(pinf,I):-integer(I), !.
xgt(pinf,minf):-!.
xgt(I,minf):-integer(I), !.
xgt(I,J):- integer(I), integer(J), !, I>J.

%A<B
xlt(A,B):- xgt(B,A).

%xgeq0
%Test an UPPER bound to see if it is greater than or equal to zero
:-mode(xgeq0(++)).
xgeq0(pinf):-!.
xgeq0(I):- integer(I), I>=0.

%xgt0
%Test an UPPER bound to see if it is greater than zero
:-mode(xgt0(++)).
xgt0(pinf):-!.
xgt0(I):- integer(I), I>0.

%xleq0
%Test a LOWER bound to see if it is less than or equal to zero
:-mode(xleq0(++)).
xleq0(minf):-!.
xleq0(I):- integer(I), I=<0.

%xlt0
%Test a LOWER bound to see if it is less than zero
:-mode(xlt0(++)).
xlt0(minf):-!.
xlt0(I):- integer(I), I<0.

%Set second interval smaller as result of being unequal to first
%If first is an integer (upper and lower bound the same) then can 
%delete one integer from the end of the interval
%xineq

:-mode(xineq(++,++,++,++,-,-)).
xineq(Av,Av,Av,Bvh,Bwl,Bvh):- integer(Av), !, xinc_hl(Av,Bwl).
xineq(Av,Av,Bvl,Av,Bvl,Bwh):- integer(Av), !, xdec_lh(Av,Bwh).
xineq(_Avl,_Avh,Bvl,Bvh,Bvl,Bvh).

%Set second interval smaller as result of being unequalto first
%xineq_force
:-mode(xineq_force(++,++,++,++,-,-)).
xineq_force(Av,Av,Bvl,Bvh,Bwl,Bwh):- integer(Av), !, 
	xineq_force1(Av,Bvl,Bvh,Bwl,Bwh).
xineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xleq(Avl,Bvl), xlt(Avh,Bvh), xgeq(Avh,Bvl), !, 
	xineq_force2(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
xineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xlt(Bvl,Avl), xlt(Avh,Bvh), !,
	xineq_force3(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
xineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xlt(Bvl,Avl), xleq(Bvh,Avh), xleq(Avl,Bvh), !, 
	xineq_force4(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
%Default case B is contained inside A or A and B are disjoint
xineq_force(_Avl,_Avh,Bvl,Bvh,Bvl,Bvh). 

:-mode(xineq_force1(++,++,++,-,-)).
xineq_force1(Av,Av,Bvh,Bwl,Bvh):- !, xinc_hl(Av,Bwl).
xineq_force1(Av,Bvl,Av,Bvl,Bwh):- !, xdec_lh(Av,Bwh).
xineq_force1(Av,Bvl,Bvh,Bwl,Bwh):- xgt(Av,Bvl), xgt(Bvh,Av), !, 
%       this generates a real choice-point the cases above are to prevent
%       it being created when not necessary
	(Bwl=Bvl,xdec_lh(Av,Bwh);(xinc_hl(Av,Bwl),Bwh=Bvh)).
xineq_force1(_Av,Bvl,Bvh,Bvl,Bvh).

:-mode(xineq_force2(++,++,++,++,-,-)).
% A          |           |
% B               |         |
%                 XXXXXXXXYYY
xineq_force2(_Avl,Avh,Bvl,_Bvh,Bvl,Avh).
xineq_force2(_Avl,Avh,_Bvl,Bvh,Bwl,Bvh):- xinc_hl(Avh,Bwl).

:-mode(xineq_force3(++,++,++,++,-,-)).
% A          |      |
% B     |                |
%       XXXXXYYYYYYYYZZZZZ
xineq_force3(Avl,_Avh,Bvl,_Bvh,Bvl,Bwh):- xdec_lh(Avl,Bwh).
xineq_force3(Avl,Avh,_Bvl,_Bvh,Avl,Avh).
xineq_force3(_Avl,Avh,_Bvl,Bvh,Bwl,Bvh):- xinc_hl(Avh,Bwl).

:-mode(xineq_force4(++,++,++,++,-,-)).
% A      |             |
% B   |          |
%     XXXYYYYYYYYY
xineq_force4(Avl,_Avh,Bvl,_Bvh,Bvl,Bwh):- xdec_lh(Avl,Bwh).
xineq_force4(Avl,_Avh,_Bvl,Bvh,Avl,Bvh).


%Warning this calls itself recursively to deal with the tricky problems
%inherent in correct integer division
:-mode(xtimes_force(++,++,++,++,++,++,-,-,-,-,-,-)).
xtimes_force(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh):-
      %fail early to prevent recursion
      xm(Avl,Avh,Bvl,Bvh,Ctl,Cth), 
      xmin(Cvh,Cth,Csh), xmax(Cvl,Ctl,Csl), xgeq(Csh,Csl), 
      xdf(Cvl,Cvh,Bvl,Bvh,Atl,Ath),
      xmin(Ath,Avh,Ash), xmax(Avl,Atl,Asl), xgeq(Ash,Asl),
      xround(Asl,Ash,Csl,Csh,Arl,Arh),
      xdf(Cvl,Cvh,Avl,Avh,Btl,Bth),
      xmin(Bth,Bvh,Bsh), xmax(Bvl,Btl,Bsl), xgeq(Bsh,Bsl),
      xround(Bsl,Bsh,Csl,Csh,Brl,Brh),
      ([Avl,Avh,Bvl,Bvh,Cvl,Cvh]\=[Arl,Arh,Brl,Brh,Csl,Csh] ->
	  xtimes_force(Arl,Arh,Brl,Brh,Csl,Csh,Awl,Awh,Bwl,Bwh,Cwl,Cwh)
         ;[Arl,Arh,Brl,Brh,Csl,Csh]=[Awl,Awh,Bwl,Bwh,Cwl,Cwh]
      ).

:-mode(xtimes_noforce(++,++,++,++,++,++,-,-,-,-,-,-)).
xtimes_noforce(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh):-
      %fail early to prevent recursion
      xm(Avl,Avh,Bvl,Bvh,Ctl,Cth), 
      xmin(Cvh,Cth,Csh), xmax(Cvl,Ctl,Csl), xgeq(Csh,Csl), 
      xd(Cvl,Cvh,Bvl,Bvh,Atl,Ath),
      xmin(Ath,Avh,Ash), xmax(Avl,Atl,Asl), xgeq(Ash,Asl),
      xround(Asl,Ash,Csl,Csh,Arl,Arh),
      xd(Cvl,Cvh,Avl,Avh,Btl,Bth),
      xmin(Bth,Bvh,Bsh), xmax(Bvl,Btl,Bsl), xgeq(Bsh,Bsl),
      xround(Bsl,Bsh,Csl,Csh,Brl,Brh),
      ([Avl,Avh,Bvl,Bvh,Cvl,Cvh]\=[Arl,Arh,Brl,Brh,Csl,Csh] ->
	  xtimes_noforce(Arl,Arh,Brl,Brh,Csl,Csh,Awl,Awh,Bwl,Bwh,Cwl,Cwh)
         ;[Arl,Arh,Brl,Brh,Csl,Csh]=[Awl,Awh,Bwl,Bwh,Cwl,Cwh]
      ).

%C := A*B (irrespective of the many possible signs of things)
:-mode(xm(++,++,++,++,-,-)).
xm(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xmult(Avl,Bvl,Tll), xmult(Avl,Bvh,Tlh), 
	xmult(Avh,Bvl,Thl), xmult(Avh,Bvh,Thh),
	xmax(Tll,Tlh,T1), xmax(Thl,Thh,T2), xmax(T1,T2,Cwh),
	xmin(Tll,Tlh,U1), xmin(Thl,Thh,U2), xmin(U1,U2,Cwl).

%Do multiplication of bounds
:-mode(xmult(++,++,-)).
xmult(_,0,0):- !.
xmult(0,_,0):- !.
xmult(pinf,pinf,pinf):- !.
xmult(pinf,minf,minf):- !.
xmult(pinf,I,pinf):- I>0, !. %integer(I)
xmult(pinf,I,minf):- I<0, !. %integer(I)
xmult(minf,pinf,minf):- !.
xmult(minf,minf,pinf):- !.
xmult(minf,I,minf):- I>0, !. %integer(I)
xmult(minf,I,pinf):- I<0, !. %integer(I)
xmult(I,pinf,pinf):- I>0, !. %integer(I)
xmult(I,pinf,minf):- I<0, !. %integer(I)
xmult(I,minf,minf):- I>0, !. %integer(I)
xmult(I,minf,pinf):- I<0, !. %integer(I)
xmult(I,J,K):- !, integer(I), integer(J), !, %Redundant initial ! is to fix
	                                     %compiler bug in eclipse
	mach_mult(I,J,K).

%C := A/B
:-mode(xd(++,++,++,++,-,-)).
%Warning red cuts
%Both contain a zero - everything possible
xd(Avl,Avh,Bvl,Bvh,minf,pinf):-
	xgeq0(Avh), xleq0(Avl), xgeq0(Bvh), xleq0(Bvl), !.
%A has a zero but B does not
xd(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xgeq0(Avh), xleq0(Avl), !, xd1(Avl,Avh,Bvl,Bvh,Cwl,Cwh).
%B has a zero but A does not
xd(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xgeq0(Bvh), xleq0(Bvl), !, xd2(Avl,Avh,Bvl,Bvh,Cwl,Cwh).
%neither have zeros
xd(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh).

:-mode(xdf(++,++,++,++,-,-)).
%Warning red cuts
%Both contain a zero - everything possible
xdf(Avl,Avh,Bvl,Bvh,minf,pinf):-
	xgeq0(Avh), xleq0(Avl), xgeq0(Bvh), xleq0(Bvl), !.
%A has a zero but B does not
xdf(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xgeq0(Avh), xleq0(Avl), !, xd1(Avl,Avh,Bvl,Bvh,Cwl,Cwh).
%B has a zero but A does not
xdf(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xgeq0(Bvh), xleq0(Bvl), !, xd2f(Avl,Avh,Bvl,Bvh,Cwl,Cwh).
%neither have zeros
xdf(Avl,Avh,Bvl,Bvh,Cwl,Cwh):-
	xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh).

%A contains a zero but B does not
:-mode(xd1(++,++,++,++,-,-)).
xd1(Avl,Avh,Bvl,_Bvh,Cwl,Cwh):- xgt0(Bvl), !, 
	xdivoi_np(Avl,Bvl,Cwl), xdivoi_pp(Avh,Bvl,Cwh).
xd1(Avl,Avh,_Bvl,Bvh,Cwl,Cwh):- xlt0(Bvh), !, 
	xdivoi_nn(Avl,Bvh,Cwh), xdivoi_pn(Avh,Bvh,Cwl).

:-mode(xd2(++,++,++,++,-,-)).
%B has a zero but A does not
%The result is really two disjoint intervals we return a single 
%inclusive interval
xd2(Avl,Avh,0,Bvh,Cwl,Cwh):- !, xgt0(Bvh), xd3(Avl,Avh,1,Bvh,Cwl,Cwh).
xd2(Avl,Avh,Bvl,0,Cwl,Cwh):- !, xlt0(Bvl), xd3(Avl,Avh,Bvl,-1,Cwl,Cwh).
%Take the union of the possibilities
xd2(Avl,Avh,Bvl,Bvh,Cwl,Cwh):- xgt0(Bvh), xlt0(Bvl), !, 
	xneg(Avl,Amh), xneg(Avh,Aml), 
	xmax(Amh,Avh,Cwh), xmin(Aml,Avl,Cwl).

:-mode(xd2f(++,++,++,++,-,-)).
%B has a zero but A does not
%The result is two disjoint intervals 
%After carefully checking for special cases whre there is really only one
%a genuine choice-point is created for two cases
xd2f(Avl,Avh,0,Bvh,Cwl,Cwh):- !, xd3(Avl,Avh,1,Bvh,Cwl,Cwh).
xd2f(Avl,Avh,Bvl,0,Cwl,Cwh):- !, xd3(Avl,Avh,Bvl,-1,Cwl,Cwh).
xd2f(Avl,Avh,_Bvl,Bvh,Cwl,Cwh):- xd3(Avl,Avh,1,Bvh,Cwl,Cwh).
xd2f(Avl,Avh,Bvl,_Bvh,Cwl,Cwh):- xd3(Avl,Avh,Bvl,-1,Cwl,Cwh).

:-mode(xd3(++,++,++,++,-,-)).
%Neither A nor B contain 0s
xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh):- xgeq0(Avl), xgt0(Bvl), !, 
	xdivio_pp(Avl,Bvh,Cwl), xdivoi_pp(Avh,Bvl,Cwh).
xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh):- xgeq0(Avl), xlt0(Bvh), !, 
	xdivio_pn(Avl,Bvl,Cwh), xdivoi_pn(Avh,Bvh,Cwl).
xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh):- xleq0(Avh), xgt0(Bvl), !, 
	xdivio_np(Avh,Bvh,Cwh), xdivoi_np(Avl,Bvl,Cwl).
xd3(Avl,Avh,Bvl,Bvh,Cwl,Cwh):- xleq0(Avh), xlt0(Bvh), !, 
	xdivio_nn(Avh,Bvl,Cwl), xdivoi_nn(Avl,Bvh,Cwh).

%Divide rounding toward 0 (no possibiity of division by 0)
%Divide an outer part by an inner part
%Inners must not be 0
:-mode(xdivoi_pp(++,++,-)).
xdivoi_pp(pinf,I,pinf):- !,     debug_check(xgeq0(I)).
xdivoi_pp(I,pinf,0):- !,        debug_check((integer(I), I>=0)).
xdivoi_pp(I,J,K):- 
	K is I // J, debug_check((integer(I), I>=0, integer(J), J>0)).

:-mode(xdivoi_pn(++,++,-)).
xdivoi_pn(pinf,I,minf):- !,     debug_check(xleq0(I)).
xdivoi_pn(I,minf,0):- !,        debug_check((integer(I), I>=0)).
xdivoi_pn(I,J,K):- 
	K is I // J, debug_check((integer(I), I>=0, integer(J), J<0)).

:-mode(xdivoi_np(++,++,-)).
xdivoi_np(minf,I,minf):- !, debug_check(xgeq0(I)). 
xdivoi_np(I,pinf,0):- !, debug_check((integer(I), I=<0)).
xdivoi_np(I,J,K):- 
	K is I // J, debug_check((integer(I), I=<0, integer(J), J>0)).


:-mode(xdivoi_nn(++,++,-)).
xdivoi_nn(minf,I,pinf):- !, debug_check(xleq0(I)).
xdivoi_nn(I,minf,0):- !, debug_check((integer(I), I=<0)).
xdivoi_nn(I,J,K):- 
	K is I // J, debug_check((integer(I), I=<0, integer(J), J<0)).

%Divide rounding away from 0 (no possibiity of division by 0)
%Divide an inner part by an outer part 
%Inners and outers can be pinf or minf (but not 0)
%Warning the arithmetic expressions below are very brittle.  The order of
%evaluation has to be done very carefully to avoid overflow
:-mode(xdivio_pp(++,++,-)).
%Very strange but true
xdivio_pp(I,pinf,1):-    !, debug_check(xgeq0(I)). 
xdivio_pp(pinf,1,pinf):- !.
xdivio_pp(pinf,I,K):-    !, K is (xmaxint//I)+1, 
	debug_check((integer(I), I>1)).
xdivio_pp(I,J,K):-       !, K is ((I-1) // J) + 1,
	debug_check((integer(I), I>0, integer(J), J>0)).

:-mode(xdivio_pn(++,++,-)).
xdivio_pn(I,minf,-1):-   !, debug_check(xgeq0(I)). %Very strange but true
xdivio_pn(pinf,-1,minf):-!.
xdivio_pn(pinf,I,K):-    !, K is (xmaxint // I)-1, 
	debug_check((integer(I), I< -1)).
xdivio_pn(I,J,K):-       !, K is ((I-1) // J) - 1,
	debug_check((integer(I), I>0, integer(J), J<0)).

:-mode(xdivio_np(++,++,-)).
xdivio_np(I,pinf,-1):-   !, debug_check(xleq0(I)).
xdivio_np(minf,1,minf):- !.
xdivio_np(minf,I,K):-    !, K is (xminint // I)-1,
	debug_check((integer(I), I>1)).
xdivio_np(I,J,K):-       !, K is ((I+1) // J) - 1,
	debug_check((integer(I), I<0, integer(J), J>0)).

:-mode(xdivio_nn(++,++,-)).
xdivio_nn(I,minf,1):-     !, debug_check(xleq0(I)).
xdivio_nn(minf,-1,pinf):- !.
xdivio_nn(minf,I,K):-     !, K is (xminint // I)+1,
	debug_check((integer(I), I< -1)).
xdivio_nn(I,J,K):-        !, K is ((I+1) // J) + 1,
	debug_check((integer(I), I<0, integer(J), J<0)).

%Round the interval L,H in until the bounds have multiples in the interval
%Cl,Ch
:-mode(xround(++,++,++,++,-,-)).
xround(L,H,Cl,Ch,Ln,Hn):-
	xroundup(L,H,Cl,Ch,Ln),
	xrounddown(Ln,H,Cl,Ch,Hn).

%Find smallest N >= L and =<H so that N has multiple in range [Cl..Ch]
:-mode(xroundup(++,++,++,++,-)).
xroundup(L,_H,Cl,Ch,L):- xleq0(Cl), xgeq0(Ch), !. %L*0 = 0
xroundup(0,H,Cl,Ch,N):- !, xgeq(H,1), xroundup(1,H,Cl,Ch,N).
xroundup(L,H,Cl,Ch,N):- xlt0(L), xgeq0(H), !, xroundup(L,-1,Cl,Ch,N).
%By now neither L,H nor Cl,Ch contain a 0
xroundup(L,H,Cl,Ch,N):- xgt0(L), xgt0(Cl), !, 
	xru(L,H,Cl,Ch,N).
xroundup(L,H,Cl,Ch,N):- xgt0(L), xlt0(Ch), !, 
        xneg(Ch,MCh), xneg(Cl,MCl),
        xru(L,H,MCh,MCl,N).
xroundup(L,H,Cl,Ch,N):- xlt0(H), xgt0(Cl), !, 
	xneg(H,MH), xneg(L,ML),
        xrd(MH,ML,Cl,Ch,MN), xneg(MN,N).
xroundup(L,H,Cl,Ch,N):- xlt0(H), xlt0(Ch), !,
        xneg(H,MH), xneg(L,ML), xneg(Cl,MCl), xneg(Ch,MCh),
        xrd(MH,ML,MCh,MCl,MN), xneg(MN,N).

%Decrement H until it has a multiple in the range [Cl..Ch]
:-mode(xrounddown(++,++,++,++,-)).
xrounddown(L,H,Cl,Ch,N):- 
	xneg(L,Lm), xneg(H,Hm), xroundup(Hm,Lm,Cl,Ch,Nm), xneg(Nm,N).

%At this point all parameters are >0
%Deal with some initial special cases
:-mode(xru(++,++,++,++,-)).
xru(L,_H,_Cl,pinf,L):- !.
xru(L,pinf,Cl,Ch,N):- !, xru(L,Ch,Cl,Ch,N).
xru(L,H,Cl,Ch,N):- xleq(L,Ch), !, %Because Ch<pinf the check below holds
	debug_check((integer(L), L>0, integer(Cl), Cl>0, integer(Ch), Ch>0)),
	debug_check((integer(H), L=<H)),
	D is Ch-Cl+1, 
	(D>=L 
         -> N=L
         ;  (Min is min(H,Ch), L=<Min, xu(L,Min,Cl,Ch,N))
        ).

%This calls itself recursively and increments L until either fails or 
%N is found
%It can be made significantly more efficient with a little intellectual labour
:-mode(xu(++,++,++,++,-)).
xu(L,Min,Cl,Ch,N):- 
	debug_check((integer(Min), integer(L), integer(Cl), integer(Ch))),
	debug_check((L=<Min,Cl=<Ch)),
	T0 is (Ch // L), T is T0*L, debug_check((T=<Ch)),
        (T>=Cl 
           -> N=L
              %Ware: do the comparison first so that +1 doesnt overflow
           ;  (L<Min,M is L+1,xu(M,Min,Cl,Ch,N))
        ),
        debug_check((integer(N),N>=L,N=<Min,U0 is Ch//N,U is U0*N,U>=Cl,U=<Ch,
                    (N=L->true;(U0*(N-1)<Cl)))
        ).



%At this point all parameters are >0
:-mode(xrd(++,++,++,++,-)).
xrd(_L,H,_Cl,pinf,H):- !.
xrd(L,pinf,Cl,Ch,N):- !, xrd(L,Ch,Cl,Ch,N).
xrd(L,H,Cl,Ch,N):- xleq(L,Ch), !, %Because Ch<pinf the check below holds
        debug_check((integer(L), L>0, integer(Cl), Cl>0, integer(Ch), Ch>0)),
        debug_check((integer(H), L=<H)),
	D is Ch-Cl+1, Min is min(H,Ch),
	(D>=Min 
         -> N=Min 
         ;  (D>=L
	      ->xrd1(Min,D,Cl,Ch,N)
	      ; xrd2(L,Min,Cl,Ch,N)
	    )
        ),
	debug_check(
	 (integer(N),N>=L,N=<H,T0 is Ch//N,T is T0*N,T>=Cl,T=<Ch,
          (N=H->true;(T0*(H+1)>Ch))
         )
        ).

:-mode(xrd1(++,++,++,++,-)).
xrd1(H,H,_Cl,_Ch,H):- !.
xrd1(H,D,Cl,Ch,N):- 
         T0 is (Ch // H), T is T0*H, debug_check((T=<Ch)),
	 (T>=Cl
          -> N=H
          ;  (M is H-1, xrd1(M,D,Cl,Ch,N))
         ).

:-mode(xrd2(++,++,++,++,-)).
xrd2(L,H,Cl,Ch,N):- H>=L,
         T0 is (Ch // H), T is T0*H, debug_check((T=<Ch)),
	 (T>=Cl
          -> N=H
          ;  (M is H-1, xrd2(L,M,Cl,Ch,N))
         ).



%B := A^2
:-mode(xsqm(++,++,-,-)).
xsqm(Avl,Avh,Bwl,Bwh):- xleq0(Avh), !, xmult(Avl,Avl,Bwh), xmult(Avh,Avh,Bwl).
xsqm(Avl,Avh,Bwl,Bwh):- xgeq0(Avl), !, xmult(Avl,Avl,Bwl), xmult(Avh,Avh,Bwh).
xsqm(Avl,Avh,0,Bwh):- !, 
	xmult(Avl,Avl,T1), xmult(Avh,Avh,T2), xmax(T1,T2,Bwh).

%A := sqrt(B)
:-mode(xsqrt(++,++,++,++,-,-)).
xsqrt(Bvl,Bvh,Avl,Avh,Awl,Awh):- 
	xsq(Bvl,Bvh,Avl,Avh,Atl,Ath),
	xmax(Avl,Atl,Awl), xmin(Avh,Ath,Awh).

:-mode(xsq(++,++,++,++,-,-)).
xsq(0,Bvh,_Avl,_Avh,Awl,Awh):- !, xsqrti(Bvh,Awh), xneg(Awh,Awl).
xsq(Bvl,Bvh,Avl,Avh,Awl,Awh):- 
	xsqrto(Bvl,Atl), xsqrti(Bvh,Ath),
	xneg(Ath,Aul), xneg(Atl,Auh),
	xsqrt0(Atl,Ath,Aul,Auh,Avl,Avh,Awl,Awh).

%First see if there is only one solution really
:-mode(xsqrt0(++,++,++,++,++,++,-,-)).
xsqrt0(Atl,Ath,_Aul,Auh,Avl,_Avh,X,Y):- xgt(Avl,Auh), !, X=Atl, Y=Ath.
xsqrt0(Atl,_Ath,Aul,Auh,_Avl,Avh,X,Y):- xgt(Atl,Avh), !, X=Aul, Y=Auh.
%Case where later we can force - split across two disjoint intervals
xsqrt0(_Atl,Ath,Aul,_Auh,_Avl,_Avh,Aul,Ath).


%A := sqrt(B) - force to split disjoint intervals
:-mode(xsqrt_force(++,++,++,++,-,-)).
xsqrt_force(Bvl,Bvh,Avl,Avh,Awl,Awh):-
        xsqf(Bvl,Bvh,Avl,Avh,Atl,Ath),
        xmax(Avl,Atl,Awl), xmin(Avh,Ath,Awh).

:-mode(xsqf(++,++,++,++,-,-)).
xsqf(0,Bvh,Avl,Avh,Axl,Axh):- !, xsqrti(Bvh,Awh), xneg(Awh,Awl),
	xmin(Avh,Awh,Axh), xmax(Avl,Awl,Axl), xgeq(Axh,Axl).
xsqf(Bvl,Bvh,Avl,Avh,Awl,Awh):- 
	xsqrto(Bvl,Atl), xsqrti(Bvh,Ath),
	xneg(Ath,Aul), xneg(Atl,Auh),
	xsqrt1(Atl,Ath,Aul,Auh,Avl,Avh,Awl,Awh).

%First see if there is only one solution really 
%avoid choice point unless neccesary
:-mode(xsqrt1(++,++,++,++,++,++,-,-)).
xsqrt1(Atl,Ath,_Aul,Auh,Avl,_Avh,X,Y):- xgt(Avl,Auh), !, X=Atl, Y=Ath.
xsqrt1(Atl,_Ath,Aul,Auh,_Avl,Avh,X,Y):- xgt(Atl,Avh), !, X=Aul, Y=Auh.
%Split across two disjoint intervals - this is a real choice point
xsqrt1(Atl,Ath,_Aul,_Auh,_Avl,_Avh,Atl,Ath).
xsqrt1(_Atl,_Ath,Aul,Auh,_Avl,_Avh,Aul,Auh).


%Take a sqrt rounding out (lower bounds)
:-mode(xsqrto(++,-)).
xsqrto(pinf,N):- !, N is xmaxsqint+1.
xsqrto(0,0):- !.
xsqrto(I,J):- integer(I), I>=0, !, 
	%First guess using floating sqrt then trim to right value using sqrto
	T is fix(sqrt(I)), S is T*T, sqrto(I,T,S,J).

:-mode(sqrto(++,++,++,-)).
sqrto(I,T,S,N):- xmaxsqint(T), S<I, !, N is xmaxsqint+1.
sqrto(I,T,S,J):- S<I, !, Tn is T+1, Sn is Tn*Tn, sqrto(I,Tn,Sn,J).
sqrto(I,T,S,J):- S>=I, !, Tn is T-1, Sn is Tn*Tn,
	(Sn<I -> J=T ; sqrto(I,Tn,Sn,J)).

%Take a sqrt rounding in (upper bounds)
:-mode(xsqrti(++,-)).
xsqrti(pinf,pinf).
xsqrti(I,J):- integer(I), I>=0, !, 
	%First guess using floating sqrt then trim to right value using sqrti
	T is fix(sqrt(I)), S is T*T, sqrti(I,T,S,J).

:-mode(sqrti(++,++,++,-)).
sqrti(I,T,S,T):- xmaxsqint(T), S=<I, !.
sqrti(I,T,S,J):- S=<I, !, Tn is T+1, Sn is Tn*Tn, 
	(Sn>I -> J=T ; sqrti(I,Tn,Sn,J)).
sqrti(I,T,S,J):- S>I, !, Tn is T-1, Sn is Tn*Tn, sqrti(I,Tn,Sn,J).

