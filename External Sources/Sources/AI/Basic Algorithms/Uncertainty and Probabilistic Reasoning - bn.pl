/*- bn.pl -------------------------------------------------*/
/*---------------------------------------------------------*/
/* Computational Intelligence: a logical approach.         */
/* Prolog Code.                                            */
/* BELIEF NETWORK INTERPRETER                              */
/* Copyright (c) 1998, Poole, Mackworth, Goebel            */
/*                     and Oxford University Press.        */
/*---------------------------------------------------------*/
/* A belief network is represented with the relations
  variables(Xs) Xs is the list of random variables.
   Xs is ordered: parents of node are before the node.
  parents(X,Ps) Ps list of parents of variable X.
   Ps is ordered consistently with Xs
  values(X,Vs) Vs is the list of values of X
  pr(X,As,D) X is a variable, As is a list of Pi=Vi where
   Pi is a parent of X, and Vi is a value for variable Pi
  The elements of As are ordered consistently with Ps.     */
/*---------------------------------------------------------*/
/*---------------------------------------------------------*/
/* p(Var,Obs,Dist) is true if Dist represents the
   probability distribution of P(Var|Obs)
   where Obs is a list of Vari=Vali. Var is not observed.  */
/*---------------------------------------------------------*/
p(Var,Obs,VDist) :-
   relevant(Var,Obs,RelVars),
   to_sum_out(RelVars,Var,Obs,SO),
   joint(RelVars,Obs,Joint),
   sum_out_each(SO,Joint,Dist),
   collectt(Dist,DT0),
   normalize(DT0,0,_,VDist).

/*---------------------------------------------------------*/
/* relevant(Var,Obs,RelVars) Relvars is the relevant
   variables given query Var and observations Obs.
   This is the most conservative.                          */
/*---------------------------------------------------------*/
relevant(_,_,Vs) :-
   variables(Vs).  

/*---------------------------------------------------------*/
/* to_sum_out(Vs,Var,Obs,SO), 
    Given all variables Vs, query variable Var
  and observations Obs, S0 specifies the elimination
  ordering. Here, naively, the elimination ordering
  is the same as variable ordering                         */
/*---------------------------------------------------------*/
to_sum_out(Vs,Var,Obs,SO) :-
   remove(Var,Vs,RVs),
   remove_each_obs(Obs,RVs,SO).

/*---------------------------------------------------------*/
/* remove_each_obs(Obs,RVs,SO) removes each of the
  observation variables from RVs resulting in SO.          */
/*---------------------------------------------------------*/
remove_each_obs([],SO,SO) :- !.
remove_each_obs([X=_|Os],Vs0,SO) :-
   remove_if_present(X,Vs0,Vs1),
   remove_each_obs(Os,Vs1,SO).

/*---------------------------------------------------------*/
/* A joint probability distribution is represented
as a list of distribution trees, of the form
         dtree(Vars,DTree) 
where Vars is a list of Variables (ordered
consistently with the ordering of variables), and
DTree is tree representation for the function from
values of variables into numbers such that if
Vars=[] then DTree is a number. Otherwise
Vars=[Var|RVars], and DTree is a list with one
element for each value of Var, and each element
is a tree representation for RVars. The ordering
of the elements in DTree is given by the ordering
of Vals given by values(Var,Vals).                         */

/*---------------------------------------------------------*/
/* joint(Vs,Obs,Joint) Vs is a list of variables,
   Obs is an observation list returns a list of
   dtrees that takes the observations into account.
   There is a dtree for each non-observed variable.        */
/*---------------------------------------------------------*/
joint([],_,[]) :- !.
joint([X|Xs],Obs,[dtree(DVars,DTree)|JXs]) :-
   parents(X,PX),
   make_dvars(PX,X,Obs,DVars),
   DVars \== [], !,
   make_dtree(PX,X,Obs,[],DTree),
   joint(Xs,Obs,JXs).
joint([_|Xs],Obs,JXs) :-
   /* we remove any dtree with no variables */
   joint(Xs,Obs,JXs).

/*---------------------------------------------------------*/
/* make_dvars(PX,X,Obs,DVars)  
   where X is a variable and PX are the parents of
   X and Obs is observation list returns
   DVars = {X} U PX - observed variables
   This relies on PX ordered before X                      */
/*---------------------------------------------------------*/
make_dvars([],X,Obs,[]) :-
   member(X=_,Obs), !.
make_dvars([],X,_,[X]).
make_dvars([V|R],X,Obs,DVs) :-
   member(V=_,Obs), !,
   make_dvars(R,X,Obs,DVs).
make_dvars([V|R],X,Obs,[V|DVs]) :-
   /* not member(V=_,Obs), */
   make_dvars(R,X,Obs,DVs).

/*---------------------------------------------------------*/
/* make_dtree(RP,X,Obs,Con,Dtree) constructs a factor
   corresponding to p(X|PX). RP is list of remaining
   parents of X, Obs is the observations, Con is a
   context of assignments to previous (in the
   variable ordering) parents of X - in reverse order
   to the variable assignment, returns DTree as the
   dtree corresponding to values of RP.                    */
/*---------------------------------------------------------*/
make_dtree([],X,Obs,Con,DX) :-
   member(X=OVal,Obs), !,
   reverse(Con,RCon),
   pr(X,RCon,DXPr),
   values(X,Vals),
   select_corresp_elt(Vals,OVal,DXPr,DX).
make_dtree([],X,_,Con,DX) :-
   reverse(Con,RCon),
   pr(X,RCon,DX).
make_dtree([P|RP],X,Obs,Con,DX) :-
   member(P=Val,Obs),!,
   make_dtree(RP,X,Obs,[P=Val|Con],DX).
make_dtree([P|RP],X,Obs,Con,DX) :-
   values(P,Vals),
   make_dtree_for_vals(Vals,P,RP,X,Obs,Con,DX).

/*---------------------------------------------------------*/
/* make_dtree_for_vals(Vals,P,RP,X,Obs,Con,DX).
   makes a DTree for each value in Vals, and
   collected them into DX.  Other variables are as
   for make_dtree.                                         */
/*---------------------------------------------------------*/
make_dtree_for_vals([],_,_,_,_,_,[]) :- !.
make_dtree_for_vals([Val|Vals],P,RP,X,Obs,Con,[ST|DX]):-
   make_dtree(RP,X,Obs,[P=Val|Con],ST),
   make_dtree_for_vals(Vals,P,RP,X,Obs,Con,DX).

/*---------------------------------------------------------*/
/* select_corresp_elt(Vals,Val,List,Elt) is true
   if Elt is at the same position in List as Val is
   in list Vals. Assumes Vals, Val, List are bound.        */
/*---------------------------------------------------------*/
select_corresp_elt([Val|_],Val,[Elt|_],Elt) :-
   !.
select_corresp_elt([_|Vals],Val,[_|Rest],Elt) :-
   select_corresp_elt(Vals,Val,Rest,Elt).

/*---------------------------------------------------------*/
/* sum_out_each(SO,Joint0,Joint1) is true if
   Joint1 is a distribution Joint0 with each
   variable in SO summed out                               */
/*---------------------------------------------------------*/
sum_out_each([],J,J) :- !.
sum_out_each([X|Xs],J0,J2) :-
   sum_out(X,J0,J1),
   sum_out_each(Xs,J1,J2).

/*---------------------------------------------------------*/
/* sum_out_each(V,J0,J1) is true if
   Joint1 is a distribution Joint0 with
   variable V summed out.                                  */
/*---------------------------------------------------------*/
sum_out(X,J0,[dtree(CVars1,CTree)|NoX]) :-
   partition(J0,X,NoX,SomeX),
   variables(AllVars),
   find_tree_vars(SomeX,AllVars,CVars),
   remove(X,CVars,CVars1),
   CVars1 \== [], !,
   create_tree(CVars1,CVars1,SomeX,X,[],CTree).
sum_out(X,J0,NoX) :-
   /* remove any dtrees that have no variables */
   partition(J0,X,NoX,_).

/*---------------------------------------------------------*/
/* partition(J0,X,NoX,SomeX) partitions J0 into
   those dtrees that contain variable X (SomeX) and
   those that do not contain X (NoX)                       */
/*---------------------------------------------------------*/
partition([],_,[],[]) :- !.
partition([dtree(Vs,Di)|R],X,NoX,[dtree(Vs,Di)|SomeX]) :-
   member(X,Vs),
   !,
   partition(R,X,NoX,SomeX).
partition([dtree(Vs,Di)|R],X,[dtree(Vs,Di)|NoX],SomeX) :-
   partition(R,X,NoX,SomeX).

/*---------------------------------------------------------*/
/* find_tree_vars(SomeX,AllVars,CVars) is true
   if CVars is the set of variables that appear in
   some dtree in SomeX, ordered according to AllVars       */
/*---------------------------------------------------------*/
find_tree_vars([],_,[]) :- !.
find_tree_vars([dtree(Vs,_)|RDs],All,Res) :-
    find_tree_vars(RDs,All,Cvars0),
    ordered_union(Vs,Cvars0,Res,All).

/*---------------------------------------------------------*/
/* create_tree(CVars,Vars,SomeX,X,Context,CTree)
   CTree is the tree corresponding to variables CVars.
   The values of the leaves of the tree are obtained
   by multiplying the corresponding values in SomeX.       */
/*---------------------------------------------------------*/
create_tree([],Vars,SomeX,X,Context,Num) :- 
   reverse(Context,CVals),
   values(X,Vals),
   sum_vals(Vals,X,Vars,CVals,SomeX,0,Num), !. /* ??? */
create_tree([Var|CVars],Vars,SomeX,X,Context,CTree) :-
   values(Var,Vals),
   create_tree_vals(Vals,CVars,Vars,SomeX,X,Context,CTree).

/*---------------------------------------------------------*/
/* create_tree_vals(Vals,CVars,Vars,SomeX,X,Context,CTree).
   creates a tree for each value in Vals.                  */
/*---------------------------------------------------------*/
create_tree_vals([],_,_,_,_,_,[]) :- !.
create_tree_vals([Val|Vals],CVars,Vars,
                     SomeX,X,Context,[SubTr|CTree]) :-
   create_tree(CVars,Vars,SomeX,X,[Val|Context],SubTr),
   create_tree_vals(Vals,CVars,Vars,SomeX,X,Context,CTree).

/*---------------------------------------------------------*/
/* sum_vals(Vals,X,Vars,CVals,SomeX,Acc,Sum).
   sums out X in the context Vars=CVals
   Vals is the remaining set of values to be added 
   SomeX is the factors that need to be multiplied         */
/*---------------------------------------------------------*/
sum_vals([],_,_,_,_,S,S) :- !.
sum_vals([Val|Vals],X,Vars,CVals,SomeX,S0,Sum) :-
   mult_vals(SomeX,Val,X,Vars,CVals,1,Prod),
   S1 is S0+Prod,
   sum_vals(Vals,X,Vars,CVals,SomeX,S1,Sum).

/*---------------------------------------------------------*/
/* mult_vals(SomeX,Val,X,Vars,CVals,Acc,Prod),
   computes product of SomeX factors given X=Val,Vars=CVals*/
/*---------------------------------------------------------*/
mult_vals([],_,_,_,_,P,P) :- !.
mult_vals([Tree|SomeX],Val,X,Vars,CVals,P0,Prod) :-
   lookup(X,Val,Vars,CVals,Tree,ContextVal),
   P1 is P0*ContextVal,
   mult_vals(SomeX,Val,X,Vars,CVals,P1,Prod).

/*---------------------------------------------------------*/
/* lookup(Var0,Val0,Vars,Vals,dtree(DVars,DTree),Prob)
   DVars is a subset of Vars U {Var}. Returns
   the value Prob by looking up "Var0=Val0 & Vars=Vals"
   in DTree.  It assumes that the elements of Vars
   and TreeVars are ordered consistently.                  */
/*---------------------------------------------------------*/
lookup(_,_,[],[],dtree([],P),P).
lookup(Var0,Val0,[Var|RVars],[Val|RVals],
           dtree([Var|TVars],DTree),Prob) :-
   !,
   values(Var,Vals),
   select_corresp_elt(Vals,Val,DTree,Subtree),
   lookup(Var0,Val0,RVars,RVals,dtree(TVars,Subtree),Prob).
lookup(Var0,Val0,RVars,RVals,dtree([Var0|TVars],DTree),Prob):-
   !,
   values(Var0,Vals),
   select_corresp_elt(Vals,Val0,DTree,Subtree),
   lookup(Var0,Val0,RVars,RVals,dtree(TVars,Subtree),Prob).
lookup(Var0,Val0,[_|RVars],[_|RVals],DT,Prob) :-
   lookup(Var0,Val0,RVars,RVals,DT,Prob).

/*---------------------------------------------------------*/
/* collectt(Dist,DT) multiplies all of the factors together
   forming a DTRee. This assumes that all of the factors
   contain just the query variable                         */
/*---------------------------------------------------------*/
collectt([dtree(_,DT)],DT) :- !.
collectt([dtree(_,DT0)|R],DT2) :-
   collectt(R ,DT1),
   multiply_corresp_elts(DT0,DT1,DT2).

/*---------------------------------------------------------*/
/* multiply_corresp_elts(DT0,DT1,DT2) DT2 is the dot
   product of DT0 and DT1                                  */
/*---------------------------------------------------------*/
multiply_corresp_elts([],[],[]).
multiply_corresp_elts([E0|L0],[E1|L1],[E2|L2]) :-
   E2 is E0*E1,
   multiply_corresp_elts(L0,L1,L2).

/*---------------------------------------------------------*/
/* normalize(List,CumVal,Sum,NList) makes NList
   the same a list, but where elements sum to 1.
   Sum is the sum of all of the list, and CumVal
   is the accumulated sum to this point.                   */
/*---------------------------------------------------------*/
normalize([],S,S,[]).
normalize([A|L],CV,Sum,[AN|LN]) :-
   CV1 is CV + A,
   normalize(L,CV1,Sum,LN),
   AN is A/Sum.

/*---------------------------------------------------------*/
/* ordered_union(L0,L1,R,RL) is true if R = L0 U L1, where RL
   is a reference list that provides the ordering of elements.
   L0, L1, RL must all be bound.                           */
/*---------------------------------------------------------*/
ordered_union([],L,L,_) :- !.
ordered_union(L,[],L,_) :- !.
ordered_union([E|L0],[E|L1],[E|R],[E|RL]) :- 
   !,
   ordered_union(L0,L1,R,RL).
ordered_union([E|L0],L1,[E|R],[E|RL]) :- 
   !,
   ordered_union(L0,L1,R,RL).
ordered_union(L0,[E|L1],[E|R],[E|RL]) :- 
   !,
   ordered_union(L0,L1,R,RL).
ordered_union(L0,L1,R,[_|RL]) :- 
   !,
   ordered_union(L0,L1,R,RL).

/*---------------------------------------------------------*/
/* STANDARD DEFINITIONS                                    */
/*---------------------------------------------------------*/
/* reverse(L,R) is true if R contains same elements 
   as list L, in reverse order                             */
/*---------------------------------------------------------*/
reverse(L,R) :-
   rev(L,[],R).
rev([],R,R).
rev([H|T],Acc,R) :-
   rev(T,[H|Acc],R).

/*---------------------------------------------------------*/
/* remove(E,L,R) true if R is the list L with 
   one occurrence of E removed                             */
/*---------------------------------------------------------*/
remove(E,[E|L],L).
remove(E,[A|L],[A|R]) :-
   remove(E,L,R).

/*---------------------------------------------------------*/
/* remove_if_present(E,L,R) true if R is the list
   L with one occurrence of E removed                      */
/*---------------------------------------------------------*/
remove_if_present(_,[],[]).
remove_if_present(E,[E|L],L) :- !.
remove_if_present(E,[A|L],[A|R]) :-
   remove_if_present(E,L,R).

/*- End of bn.pl ------------------------------------------*/
