%This is an integer interval constraint package for Sepia 3.1
%
%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%Definition of high level routines to be called by the user

:- export iis/2, itimes/3, iadd/3, igt/2, ilt/2, igeq/2, ileq/2, iineq/2, 
	iinteger/1, isolve/1, isolve/2, isolve/3, isquare/2, isplit/1.

:-import(sepia_kernel). %needed for the delay condition initial/1

:-dynamic(iforce_choices/0). 
:-dynamic(isolving/0). 
%If iforce_choices is true then multiplication and other constraints may
%split the arguments to constraints into disjoint intervals thus creating
%choice points and (possibly unnecessary searches)
%isolve first attempts a solution without this on then turns it on and tries
%again (after that it tries splitting individual varaibles see isolve for 
%details)
%If fsolving is on then we know that later force_choices will be turned on

%How delays work
%Quite a sophisticated system of delays is used for the constraints.
%The main routines (such as itimes,iadd, ...) have no delays assocaiated.
%However, they call versions of the routines itmes_delay, iadd_delay that delay
%until any of their arguments is modified by a meta_bind call.
%This delay is done using the initial/1 delay imported from sepia_kernel.
%When woken this rotuine calls the original again.

%After a local calculation of the constraint to obtain the new intervals 
%the new intervals are proogated by calling iset/2 (see xinterval).
%However, as soon as the first of these is done other constraints may be 
%woken and so by the time these are done the arguments to the constraint may 
%be quite different.  This is checked for in most cases using diff/3 and if 
%there  is a difference the local constraint is executed again (until there 
%are no further changes).
%Warning: there is a very subtle piece of logic to do with all this.
%When calculating the new values to be used in the call to iset these must be 
%as exact as possible even though iset does an intersection of the old and new
%values.  That is the inner routines (such as xadd etc.) must return an
%interval which is as exact as they can make it and in particular is already
%intersected with the target interval.  Now when iset is called the interval 
%may already have changed (becasue of propogated constraints that have already 
%been woken) and iset will in fact do a new intersection.  However, the diff
%comparisons are done with the newly computed value and any change must 
%cause diff to be true and a recomputation of the constraint to be done.
%At least one optimization I tried ran afoul of this (and was it a bitch to
%find a decent test case to winkle it out).  So think really carefully before
%altering this covention.


%
%
%                I S O L V E
%
%
%Try and solve Goal, first by trying it with iforce_choices off then
%turning it on and trying again then finally try splitting the variables
%in the explicit list
%Different options are availbale for splitting the variables - 
%    round robin - default
%    sequential - split first until it is  single value then second ..
%    smallest - choose smallest varaible and split then try again ...
%isolve
%Care is tken to ensure that the asserts of control flags iforce_choices, 
%isolving are correctly retracted in the event of an error.  This is done
%using block calls and error code.  The asserts must be inside the block
%call to avoid a one step window where errors can slip out.

:-tool(isolve/1,isolve2/2).
isolve2(Goal,Module):- isolve(Goal,Goal,Module).

:-tool(isolve/2,isolve/3).
isolve(Vars,Goal,Module):-
       subcall(call(Goal,Module),DG),
       (DG\=[] -> (isolve_at_end(DG,Vars,Module));true).

isolve_at_end(DG,Vars,Module):- 
       call_list(DG,Module),
       %Serach for variables at last possible moment so that binding
       %to such things as complex terms works properly
       strip(Vars,Vars_final),
       i_split(Vars_final).

%Execute a list of goals - convert delayed versions to actual calls
%(see comments at start of arithmetic constraints about delayed versions)
call_list([],_Module):- !.
call_list([itimes_delay(_,A,B,C)|L],Module):- !, 
	itimes_force(A,B,C), call_list(L,Module).
call_list([iadd_delay(A,B,C)|L],Module):- !, 
	iadd(A,B,C), call_list(L,Module).
call_list([iineq_delay(_,A,B)|L],Module):- !, 
	iineq_force(A,B), call_list(L,Module).
call_list([isq_delay(_,A,B)|L],Module):- !, 
	isq_force(A,B), call_list(L,Module).
call_list([igt_delay(A,B)|L],Module):- !, igt(A,B), call_list(L,Module).
call_list([igeq_delay(A,B)|L],Module):- !, 
	igeq(A,B), call_list(L,Module).
call_list([G|L],Module):- call(G,Module), call_list(L,Module).

%Split a list of variables until they are integers (not intervals)
i_split(rr(L)):- !, rr_ispl(L,[]).
i_split(se(L)):- !, se_ispl(L).
i_split(sm(L)):- !, sm_ispl(L).
i_split(L):- rr_ispl(L,[]). %Default is round robin

%Do a round robin splitting - take off first list split and put
%on second list till first empty and then swap
%For some problems other strategies such as choosing the the smallest
%intervals to split will work better
:-mode(rr_ispl(+)).
rr_ispl([],[]):-!.
rr_ispl([],L):- rr_ispl(L,[]).
%If this is an interval which can be split then do so (some infinite intervals
%and ordinary variabels cannot be split)
rr_ispl([V|L],M):- xspok(V), !, isp(V), rr_ispl(L,[V|M]). 
%Ignore if it hasnot been constrained
rr_ispl([V|L],M):- var(V), !, rr_ispl(L,M).
%Ignore if already and integer
rr_ispl([V|L],M):- integer(V), !, rr_ispl(L,M).

%Split variables in sequence - that is kepp splitting the first until it is
%an integer then split the secodna dn so on.
:- mode(se_ispl(+)).
se_ispl([]):- !.
se_ispl([V|L]):- integer(V), !, se_ispl(L).
se_ispl([V|L]):- var(V), !, se_ispl(L).
se_ispl([V|L]):- isp(V), se_ispl([V|L]).

%Find the variable in the list with the smallest range and split it
%then after relaxation is complete try again
:-mode(sm_ispl(+)).
sm_ispl([]):- !.
sm_ispl([V|L]):- integer(V), !, sm_ispl(L).
sm_ispl(L):- smallest(L,S,M), isp(S), sm_isp([S|M]).

smallest([V|L],S,M):- range(V,VR), sm(V,VR,L,S,M).

:-mode(sm(?,++,+,-,-)).
sm(S,_,[],S,[]).
sm(V,VR,[LV|L],S,M):- integer(LV), !, sm(V,VR,L,S,M).
sm(V,VR,[LV|L],S,M):- range(LV,LVR), smc(V,VR,LV,LVR,L,S,M).

:-mode(smc(?,++,?,++,+,-,-)).
smc(V,VR,LV,LVR,L,S,[V|M]):- LVR<VR, sm(LV,LVR,L,S,M).
smc(V,VR,LV,LVR,L,S,[LV|M]):- LVR>=VR, sm(V,VR,L,S,M).


%Split an individual interval (roughly) in half
isp(V):- 
	debug_ii(split,split(V),N),
	ival(V,L,H), xisp(L,H,Ln,Hn), 
	iset(V,Ln,Hn,N).

%
%
%                  I S P L I T
%
%
%Split a variable and keep splitting until it is an integer
isplit(V):- integer(V), !.
isplit(V):- isp(V), isplit(V).

%
%
%                   I I S
%
%
%An arithmetic expression evaluator 
%It follows the sepia convention that a variable encountered at the time
%of evaluation is a number (not and arithmetic expression)
%I personally think this is an unfortunate convention but when in Rome...
%The following opertors are implemented:
%    / (integr division)
%    * + - (unary and binary)
%Function calls are expanded using the same conventions as Sepia.
:- op(700,xfx,iis).
:- tool(iis/2,iis/3).
iis(Ro,Ri,_Module):- (var(Ri);integer(Ri)), !,  Ro=Ri, iinteger(Ri).
iis(R,A+B,Module):- !, iadd(R0,R1,R), iis(R0,A,Module), iis(R1,B,Module).
iis(R,A-B,Module):- !, iadd(R,R1,R0), iis(R0,A,Module), iis(R1,B,Module).
iis(R,A*B,Module):- !, itimes(R0,R1,R), iis(R0,A,Module), iis(R1,B,Module).
%Rearange division to be a form of multiplication
iis(R,A/B,Module):- !, itimes(R,R1,R0), iis(R0,A,Module), iis(R1,B,Module).
iis(R,-B,Module):-  !, iadd(R,R0,0), iis(R0,B,Module).
iis(R,A,Module):-   A=..[F|P], append(P,[R],Q), B=..[F|Q], iinteger(R),
	call(B,Module).
/*
iis(Ro,Ri,_Module):- (var(Ri);integer(Ri)), !,  Ro=Ri, iinteger(Ri).
iis(R,A+B,Module):- !, iis(R0,A,Module), iis(R1,B,Module), iadd(R0,R1,R).
iis(R,A-B,Module):- !, iis(R0,A,Module), iis(R1,B,Module), iadd(R,R1,R0).
iis(R,A*B,Module):- !, iis(R0,A,Module), iis(R1,B,Module), itimes(R0,R1,R).
%Rearange division to be a form of multiplication
iis(R,A/B,Module):- !, iis(R0,A,Module), iis(R1,B,Module), itimes(R,R1,R0).
iis(R,-B,Module):-  !, iis(R0,B,Module), iadd(R,R0,0).
iis(R,A,Module):-   A=..[F|P], append(P,[R],Q), B=..[F|Q], iinteger(R),
	call(B,Module).

%We want to do recursive calls late (after constraints in place) so that
%some programs (like inverse factorial) work OK.  However in the usual case
%it is much better to first evaluate subterms then evaluate the constraint
%So this tricky bit with delaying on R accomplishes this.
delay delay_call(R,_B,_Module) if initial(R).
delay_call(_R,B,Module):-call(B,Module).
*/

%
%
%                    I I N T E G E R
%
%
%Constrain the argument to be  an integer.  Mainly of use to other routines
%rather than directly by the user
iinteger(A):- iinteger_check(A).

%
%
%                    I G E Q
%
%
% A is greater than or equal to B
igeq(A,B):- debug_ii(start,geq(A,B),N), igeqx(A,B,N).

igeqx(A,B,N):- A==B, !, iinteger(A), debug_i(done,N).
igeqx(A,B,N):- 
	    ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	    xmax(Avl,Bvl,Cwl), iset(A,Cwl,Avh,N),
	    xmin(Avh,Bvh,Cwh), iset(B,Bvl,Cwh,N),
	    (xgeq(Cwl,Cwh) -> 
		debug_i(done,N); 
		(diff(Cwl,Avh,A);diff(Bvl,Cwh,B))
	            ->(debug_i(retry,N),igeq(A,B))
		     ;(debug_i(delay,N),igeq_delay(A,B)
                )
            ).

delay igeq_delay(A,B) if initial([A,B]).
igeq_delay(A,B):- debug_ii(wake,geq(A,B),N), igeqx(A,B,N).

%
%
%                   I G T
%
%
%A greater than B
igt(A,B):- debug_ii(start,gt(A,B),N), igtx(A,B,N).

igtx(A,B,_N):- A==B, !, fail.
igtx(A,B,N):- 
	    ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	    xinc_ll(Bvl,Bwl), xmax(Avl,Bwl,Cwl), iset(A,Cwl,Avh,N),
	    xdec_hh(Avh,Awh), xmin(Awh,Bvh,Cwh), iset(B,Bvl,Cwh,N),
	    (xgt(Cwl,Cwh) 
                -> debug_i(done,N); 
		((diff(Cwl,Avh,A);diff(Bvl,Cwh,B))
                    -> (debug_i(retry,N),igt(A,B))
		     ; (debug_i(delay,N),igt_delay(A,B))
                )
            ).

delay igt_delay(A,B) if initial([A,B]).
igt_delay(A,B):- debug_ii(wake,gt(A,B),N), igtx(A,B,N).

%
%
%                        I L E Q
%
%
%A is less than or equal to B
ileq(A,B):- igeq(B,A).

%
%
%                        I L T
%
%
%A is less than B
ilt(A,B):- igt(B,A).

%
%
%                        I I N E Q
%
%
%A not equal to B

iineq(A,B):- debug_ii(start,ineq(A,B),N), iineqx(no_force,A,B,N).

iineq_force(A,B):- debug_ii(start,ineq(A,B),N), iineqx(force,A,B,N).

iineqx(_,A,B,_N):- A==B, !, fail.
iineqx(force,A,B,N):- !,
	ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	xineq_force(Avl,Avh,Bvl,Bvh,Bwl,Bwh), iset(B,Bwl,Bwh,N),
	xineq_force(Bvl,Bvh,Avl,Avh,Awl,Awh), iset(A,Awl,Awh,N), 
	((diff(Awl,Awh,A);diff(Bwl,Bwh,B))->
            (debug_i(retry,N),iineqx(force,A,B,N))
            ;((xgt(Awl,Bwh);xgt(Bwl,Awh)) -> 
	        debug_i(done,N)
                ;(debug_i(delay,N),iineq_delay(force,A,B))
            )
        ).
iineqx(no_force,A,B,N):- 
	ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	xineq(Avl,Avh,Bvl,Bvh,Bwl,Bwh), iset(B,Bwl,Bwh,N),
	xineq(Bvl,Bvh,Avl,Avh,Awl,Awh), iset(A,Awl,Awh,N), 
	((diff(Awl,Awh,A);diff(Bwl,Bwh,B))->
            (debug_i(retry,N),iineqx(no_force,A,B,N))
            ;((xgt(Awl,Bwh);xgt(Bwl,Awh)) -> 
	        debug_i(done,N)
                ;(debug_i(delay,N),iineq_delay(no_force,A,B))
            )
        ).
delay iineq_delay(_,A,B) if initial([A,B]).
iineq_delay(F,A,B):- debug_ii(wake,ineq(A,B),N), iineqx(F,A,B,N).


%
%
%                         I A D D
%
%
%sum of A and B equals C (any arguemnt may be assigned to)
iadd(A,B,C):- debug_ii(start,iadd(A,B,C),N), iaddx(A,B,C,N).

iaddx(A,B,C,N):- A==C, !, iinteger(A), B=0, debug_i(done,N).
iaddx(A,B,C,N):- B==C, !, iinteger(B), A=0, debug_i(done,N).
iaddx(A,B,C,N):- A==B, !, itimes(2,A,C), debug_i(done,N).
iaddx(A,B,C,N):- A==0, !, B=C, iinteger(B), debug_i(done,N).
iaddx(A,B,C,N):- B==0, !, A=C, iinteger(A), debug_i(done,N).
iaddx(A,B,C,N):- ival(A,Avl,Avh), ival(B,Bvl,Bvh), ival(C,Cvl,Cvh),
	      xsub_lh(Cvl,Bvh,Atl), xsub_hl(Cvh,Bvl,Ath), 
	      xmax(Atl,Avl,Awl), xmin(Ath,Avh,Awh), iset(A,Awl,Awh,N),
	      xsub_lh(Cvl,Avh,Btl), xsub_hl(Cvh,Avl,Bth), 
	      xmax(Btl,Bvl,Bwl), xmin(Bth,Bvh,Bwh), iset(B,Bwl,Bwh,N),
	      xadd_ll(Avl,Bvl,Ctl), xadd_hh(Avh,Bvh,Cth), 
	      xmax(Ctl,Cvl,Cwl), xmin(Cth,Cvh,Cwh), iset(C,Cwl,Cwh,N),
	      %if isets above have propogated changes may be different 
	      %already so call iadd/3 again
	      ((diff(Awl,Awh,A);diff(Bwl,Bwh,B);diff(Cwl,Cwh,C))->
		  (debug_i(retry,N),iadd(A,B,C));
	          %If all non-vars (must be integers) then no need to 
	          %continue delay
	          ((nonvar(A), nonvar(B), nonvar(C)) -> debug_i(done,N)
		      ;(debug_i(delay,N),iadd_delay(A,B,C))
                  )
	      ).


delay iadd_delay(A,B,C) if initial([A,B,C]).
iadd_delay(A,B,C):- debug_ii(wake,iadd(A,B,C),N), iaddx(A,B,C,N).

%product of A and B equals C
%This code checks for iforce_choices flag and keeps a record
%of whether the consequent splitting has been done (forced) or not (not_forced)
itimes(A,B,C):- itimes(not_forced,A,B,C).
itimes_force(A,B,C):- itimes(force,A,B,C).

itimes(F,A,B,C):- debug_ii(start,itimes(F,A,B,C),N), itimesx(F,A,B,C,N).

%Generate the 9 disjoint convex regions
%See original "Logical Arithmetic" paper for the theory behind this
itimes_cases(0,0,0).
itimes_cases(0,B,0):- igt(B,0).
itimes_cases(0,B,0):- ilt(B,0).
itimes_cases(A,0,0):- igt(A,0).
itimes_cases(A,0,0):- ilt(A,0).
itimes_cases(A,B,C):- igt(A,0), igt(B,0), igt(C,0).
itimes_cases(A,B,C):- igt(A,0), ilt(B,0), ilt(C,0).
itimes_cases(A,B,C):- ilt(A,0), igt(B,0), ilt(C,0).
itimes_cases(A,B,C):- ilt(A,0), ilt(B,0), igt(C,0).

%First get rid of all the special cases that can be simplified
%Part of this is essential as there mustnt be duplicated variables in the goal
itimesx(_F,A,B,C,N):- A==0, !, C=0, debug_i(done,N), iinteger(B).
itimesx(_F,A,B,C,N):- B==0, !, C=0, debug_i(done,N), iinteger(A).
itimesx(_F,A,B,C,N):- A==C, !, debug_i(case,N), 
	((iinteger(B),A=0);(iinteger(A), B=1, (ilt(A,0);igt(A,0)))).
itimesx(_F,A,B,C,N):- B==C, !, debug_i(case,N), 
	((iinteger(A),B=0);(iinteger(B), A=1, (ilt(B,0);igt(B,0)))).
itimesx(_F,A,B,C,N):- A==1, !, B=C, debug_i(done,N), iinteger(B).
itimesx(_F,A,B,C,N):- B==1, !, A=C, debug_i(done,N), iinteger(A).
itimesx(_F,A,B,C,N):- A==B, !, debug_i(done,N), isquare(A,C).
itimesx(force,A,B,C,N):- !, 
	 debug_i(cases,N), itimes_cases(A,B,C), itimes(forced,A,B,C).
itimesx(F,A,B,C,N):- %The general case
	      ival(A,Avl,Avh), ival(B,Bvl,Bvh), ival(C,Cvl,Cvh),
	      xtimes(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh),
	      iset(A,Awl,Awh,N), iset(B,Bwl,Bwh,N), iset(C,Cwl,Cwh,N),
	      ((diff(Awl,Awh,A);diff(Bwl,Bwh,B);diff(Cwl,Cwh,C))->
		      (debug_i(retry,N),itimes(F,A,B,C));
		      %If all non-vars (must be integers) then no need to 
	              %continue delay
	              (nonvar(A), nonvar(B), nonvar(C) -> debug_i(done,N)
			  ;(debug_i(delay,N),itimes_delay(F,A,B,C))
                      )
	      ).

delay itimes_delay(_F,A,B,C) if initial([A,B,C]).
itimes_delay(F,A,B,C):- debug_ii(wake,itimes(F,A,B,C),N), itimesx(F,A,B,C,N).

:-mode(xtimes(++,++,++,++,++,++,-,-,-,-,-,-)).
xtimes(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh):-
	iforce_choices, !, 
	xtimes_force(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh).
xtimes(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh):-
	xtimes_noforce(Avl,Avh,Bvl,Bvh,Cvl,Cvh,Awl,Awh,Bwl,Bwh,Cwl,Cwh).

%
%
%                 I S Q U A R E
%
%
%square of A equals  B
%This is not only handy but also necessary to handle the special case in 
%itimes when the first two arguments are the same variable (theory requires 
%that all constraints have distinct varaibles)
isquare(A,B):- igeq(B,0), isq(A,B).

isq(A,B):- debug_ii(start,isq(A,B),N), isq(no_force,A,B,N).
isq_force(A,B):- debug_ii(start,isq(A,B),N), isq(force,A,B,N).

isq(_,A,B,N):- A==0, !, B=0, debug_i(done,N).
isq(_,A,B,N):- B==0, !, A=0, debug_i(done,N).
isq(_,A,B,N):- A==B, !, debug_i(cases,N), (A=0;A=1).
%General case
isq(force,A,B,N):- !,
	ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	xsqrt_force(Bvl,Bvh,Avl,Avh,Awl,Awh), iset(A,Awl,Awh,N),
	xsqm(Awl,Awh,Btl,Bth), xmax(Btl,Bvl,Bwl), xmin(Bth,Bvh,Bwh),
	iset(B,Bwl,Bwh,N),
	((diff(Awl,Awh,A);diff(Bwl,Bwh,B))->(debug_i(retry,N),isq_force(A,B));
	    (nonvar(A), nonvar(B) 
               -> debug_i(done,N)
                ; (debug_i(delay,N),isq_delay(force,A,B))
            )
        ).
isq(no_force,A,B,N):-
	ival(A,Avl,Avh), ival(B,Bvl,Bvh),
	xsqrt(Bvl,Bvh,Avl,Avh,Awl,Awh), iset(A,Awl,Awh,N),
	xsqm(Awl,Awh,Btl,Bth), xmax(Btl,Bvl,Bwl), xmin(Bth,Bvh,Bwh),
	iset(B,Bwl,Bwh,N),
	((diff(Awl,Awh,A);diff(Bwl,Bwh,B))->(debug_i(retry,N),isq(A,B));
	    (nonvar(A), nonvar(B) 
               -> debug_i(done,N)
                ; (debug_i(delay,N),isq_delay(no_force,A,B))
            )
        ).

delay isq_delay(_,A,B) if initial([A,B]).
isq_delay(F,A,B):- debug_ii(wake,isq(A,B),N), isq(F,A,B,N).

%Checkif binding changed
:-mode(diff(++,++,?)).
diff(L,H,A):- ival(A,Lc,Hc), [Lc,Hc]\=[L,H].

%Compute the number of integers in an interval - infinite ranges are equal
%to xmaxint as are ranges too long to be represented in an integer
:-mode(range(?,-)).
range(V,R):- ival(V,L,H), xr(L,H,R).

