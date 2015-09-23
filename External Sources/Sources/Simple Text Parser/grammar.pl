/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * In this file, the grammar supported by the parser is defined.
 **/

s(s(Np,Vp,Period))-->np(Np),vp(Vp),period(Period).
np(np(N)) --> n(N).
vp(vp(V,Np,Pp)) --> v(V),np(Np),pp(Pp).
np(np(Det,N)) --> det(Det),n(N).
pp(pp(P,N)) --> p(P),n(N).
s(s(Sa,N,Vp0,C,Np0,Vp1,Semicolon,Pp,Ap,Subj,Vp2,Np1,Period)) --> 
	sa(Sa),n(N),vp(Vp0),c(C),np(Np0),vp(Vp1),semicolon(Semicolon),
	pp(Pp),ap(Ap),subj(Subj),vp(Vp2),np(Np1),period(Period).
sa(sa(P,Pn,N)) --> p(P),pn(Pn),n(N).
vp(vp(V,N)) --> v(V),n(N).
np(np(N,Pn)) --> n(N),pn(Pn).
vp(vp(V,Np)) --> v(V),np(Np).
np(np(Det,Adj,N)) --> det(Det),adj(Adj),n(N).
pp(pp(P,Adj,N)) --> p(P),adj(Adj),n(N).
ap(ap(Adv0,V0,Adv1,V1,Np)) --> adv(Adv0),v(V0),adv(Adv1),v(V1),np(Np).
vp(vp(V0,Adv,V1)) --> v(V0),adv(Adv),v(V1).
s(s(C0,N,Vp0,C1,Ap0,Pn,Vp1,Ap1,Period)) --> c(C0),n(N),vp(Vp0),c(C1),
	ap(Ap0),pn(Pn),vp(Vp1),ap(Ap1),period(Period).
vp(vp(V,Pnp,Np)) --> v(V),pnp(Pnp),np(Np).
pnp(pnp(Pn0,Pn1)) --> pn(Pn0),pn(Pn1).
np(np(Adj,N)) --> adj(Adj),n(N).
ap(ap(P,Pn,N)) --> p(P),pn(Pn),n(N).
vp(vp(V0,V1,V2)) --> v(V0),v(V1),v(V2).
ap(ap(P,Np)) --> p(P),np(Np).
np(np(N,Ap)) --> n(N),ap(Ap).
ap(ap(P,Adj)) --> p(P),adj(Adj).
s(s(N,Vp,Ap,Period)) --> n(N),vp(Vp),ap(Ap),period(Period).
np(np(N,Pp)) --> n(N),pp(Pp).
ap(ap(P,Adj,N)) --> p(P),adj(Adj),n(N).
s(s(P0,N0,Vp0,P1,Pn0,Pn1,Vp1,Ap0,Comma,N1,Vp2,Ap1,Ap2,Period)) -->	
	p(P0),n(N0),vp(Vp0),p(P1),pn(Pn0),pn(Pn1),vp(Vp1),ap(Ap0),
	comma(Comma),n(N1),vp(Vp2),ap(Ap1),ap(Ap2),period(Period).
vp(vp(V,Ap)) --> v(V),ap(Ap).
ap(ap(Part,Np)) --> part(Part),np(Np).
vp(vp(V0,V1)) --> v(V0),v(V1).
vp(vp(V,Part)) --> v(V),part(Part).
ap(ap(Vp,Np)) --> vp(Vp),np(Np).
vp(vp(Part,V)) --> part(Part),v(V).
s(s(Ap,Comma,Np,Vp,Period)) --> ap(Ap),comma(Comma),np(Np),vp(Vp),
	period(Period).
ap(ap(Adv,Np,Vp0,C,Vp1)) --> adv(Adv),np(Np),vp(Vp0),c(C),vp(Vp1).
vp(vp(Np,V)) --> np(Np),v(V).
np(np(Pn,Adj,N)) --> pn(Pn),adj(Adj),n(N).
s(s(C,Subj,Ap0,Comma,Np,Vp,Ap1,Period)) --> c(C),subj(Subj),ap(Ap0),
	comma(Comma),np(Np),vp(Vp),ap(Ap1),period(Period).
ap(ap(Np,Vp)) --> np(Np),vp(Vp).
vp(vp(V0,Adv0,Adv1,V1,Adv2)) --> v(V0),adv(Adv0),adv(Adv1),
	v(V1),adv(Adv2).
vp(vp(V0,V1,Part,V2)) --> v(V0),v(V1),part(Part),v(V2).
ap(ap(P,Np,V,Adv,Adj)) --> p(P),np(Np),v(V),adv(Adv),adj(Adj).
s(s(Pn,V,Subj0,Vp0,Subj1,N0,Vp1,C,N1,Vp2,Period)) --> pn(Pn),v(V),
	subj(Subj0),vp(Vp0),subj(Subj1),n(N0),vp(Vp1),c(C),n(N1),vp(Vp2),
	period(Period).
vp(vp(V,N,Pp)) --> v(V),n(N),pp(Pp).
