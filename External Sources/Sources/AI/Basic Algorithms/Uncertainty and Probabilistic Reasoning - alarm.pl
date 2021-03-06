/*---------------------------------------------------------*/
/*  Belief network for the Alarm example                   */
/*---------------------------------------------------------*/

/*---------------------------------------------------------*/
/* list of all variables, ordered so the parents of a node 
   are before the node.                                    */
/*---------------------------------------------------------*/
variables([b, e, a, j, m]).

/*---------------------------------------------------------*/
/* Structure of the graph                                  */
/*---------------------------------------------------------*/
parents(j,[a]).
parents(m,[a]).
parents(a,[b,e]).
parents(e,[]).
parents(b,[]).

/*---------------------------------------------------------*/
/* values for variables                                    */
/*---------------------------------------------------------*/
values(a,[t,f]).
values(b,[t,f]).
values(e,[t,f]).
values(j,[t,f]).
values(m,[t,f]).

/*---------------------------------------------------------*/
/* conditional probabilities                               */
/*---------------------------------------------------------*/
pr(j,[a=t],[0.90,0.10]).
pr(j,[a=f],[0.05,0.95]).

pr(m,[a=t],[0.70,0.30]).
pr(m,[a=f],[0.01,0.99]).

pr(a,[b=t,e=t],[0.95,0.05]).
pr(a,[b=t,e=f],[0.94,0.06]).
pr(a,[b=f,e=t],[0.29,0.71]).
pr(a,[b=f,e=f],[0.001,0.999]).

pr(b,[],[0.001,0.999]).
pr(e,[],[0.002,0.998]).

/*---------------------------------------------------------*/
