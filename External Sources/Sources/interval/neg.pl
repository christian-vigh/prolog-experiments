%Interface routines to be used by simple version of constructuve negation

neg_user(igt_delay(A,B),ileq(A,B)).
neg_user(igeq_delay(A,B),igt(B,A)).
neg_user(iineq_delay(_,A,B),A=B).

neg_meta(A,B):- 
	debug_ii(start,neg_eq_ii(A,B),N),
	ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	neg_ineq_force(Bvl,Bvh,Avl,Avh,Awl,Awh),
	iset(A,Awl,Awh,N),
	debug_i(done,N).

%Set second interval smaller as result of being unequalto first
%This version is used to implement safe negation (assymetric inequality
%of head with one case of solution)
%neg_ineq_force
:-mode(neg_ineq_force(++,++,++,++,-,-)).
neg_ineq_force(Av,Av,Bvl,Bvh,Bwl,Bwh):- integer(Av), !, 
	neg_ineq_force1(Av,Bvl,Bvh,Bwl,Bwh).
neg_ineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xleq(Avl,Bvl), xlt(Avh,Bvh), xgeq(Avh,Bvl), !, 
	neg_ineq_force2(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
neg_ineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xlt(Bvl,Avl), xlt(Avh,Bvh), !,
	neg_ineq_force3(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
neg_ineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):- 
	xlt(Bvl,Avl), xleq(Bvh,Avh), xleq(Avl,Bvh), !, 
	neg_ineq_force4(Avl,Avh,Bvl,Bvh,Bwl,Bwh).
%B is contained in A 
neg_ineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh):-
	xleq(Bvh,Avh), xgeq(Bvl,Avl), !, fail.
%Default case A and B are disjoint
neg_ineq_force(_Avl,_Avh,Bvl,Bvh,Bvl,Bvh). 

:-mode(neg_ineq_force1(++,++,++,-,-)).
neg_ineq_force1(Av,Av,Bvh,Bwl,Bvh):- !, xinc_hl(Av,Bwl).
neg_ineq_force1(Av,Bvl,Av,Bvl,Bwh):- !, xdec_lh(Av,Bwh).
neg_ineq_force1(Av,Bvl,Bvh,Bwl,Bwh):- xgt(Av,Bvl), xgt(Bvh,Av), !, 
%       this generates a real choice-point the cases above are to prevent
%       it being created when not necessary
	(Bwl=Bvl,xdec_lh(Av,Bwh);(xinc_hl(Av,Bwl),Bwh=Bvh)).
%Av lies outside B
neg_ineq_force1(_Av,Bvl,Bvh,Bvl,Bvh).

:-mode(neg_ineq_force2(++,++,++,++,-,-)).
% A          |           |
% B               |         |
%                         YYY
neg_ineq_force2(_Avl,Avh,_Bvl,Bvh,Bwl,Bvh):- xinc_hl(Avh,Bwl).

:-mode(neg_ineq_force3(++,++,++,++,-,-)).
% A          |      |
% B     |                |
%       XXXXX        ZZZZZ
neg_ineq_force3(Avl,_Avh,Bvl,_Bvh,Bvl,Bwh):- xdec_lh(Avl,Bwh).
neg_ineq_force3(_Avl,Avh,_Bvl,Bvh,Bwl,Bvh):- xinc_hl(Avh,Bwl).

:-mode(neg_ineq_force4(++,++,++,++,-,-)).
% A      |             |
% B   |          |
%     XXX
neg_ineq_force4(Avl,_Avh,Bvl,_Bvh,Bvl,Bwh):- xdec_lh(Avl,Bwh).



