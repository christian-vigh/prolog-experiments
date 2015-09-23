/*--------------------------------------------------------------*/
/*  A data sample describing decisions on loan applications     */
/*  from bank clients. "emp" means employed and "buy" describes */
/*  the loan purpose: computer (comp) or car.                   */
/*--------------------------------------------------------------*/

:-dynamic(example/3).

example(1,  approve, [emp=yes, buy=comp, sex=f, married=no ]).
example(2,  reject,  [emp=no,  buy=comp, sex=f, married=yes]).
example(3,  approve, [emp=yes, buy=comp, sex=m, married=no ]).
example(4,  approve, [emp=yes, buy=car,  sex=f, married=yes]).
example(5,  reject,  [emp=yes, buy=car,  sex=f, married=no ]).
example(6,  approve, [emp=yes, buy=comp, sex=f, married=yes]).
example(7,  approve, [emp=yes, buy=comp, sex=f, married=no ]).
example(8,  approve, [emp=yes, buy=comp, sex=m, married=no ]).
example(9,  approve, [emp=yes, buy=comp, sex=m, married=yes]).
example(10, approve, [emp=yes, buy=comp, sex=m, married=yes]).
example(11, reject,  [emp=no,  buy=comp, sex=m, married=yes]).
example(12, reject,  [emp=no,  buy=car,  sex=f, married=yes]).


/*--------------------------------------------------------------*/
/* Belief network (runs with bn.pl)                             */
/*--------------------------------------------------------------*/

/*--------------------------------------------------------------*/
/* List of variables                                            */
/*--------------------------------------------------------------*/
variables([class, emp, buy, sex, married]).

/*--------------------------------------------------------------*/
/* Structure of the graph                                       */
/*--------------------------------------------------------------*/
parents(emp,[class]).
parents(buy,[class]).
parents(sex,[class]).
parents(married,[class]).
parents(class,[]).

/*--------------------------------------------------------------*/
/* Values for variables                                         */
/*--------------------------------------------------------------*/
values(emp,[yes,no]).
values(buy,[comp,car]).
values(sex,[m,f]).
values(married,[yes,no]).
values(class,[approve,reject]).

/*--------------------------------------------------------------*/
/* conditional probabilities                                    */
/*--------------------------------------------------------------*/
pr(emp,[class=approve],[1.000,0.000]).
pr(emp,[class=reject],[0.250,0.750]).

pr(buy,[class=approve],[0.875,0.125]).
pr(buy,[class=reject],[0.500,0.500]).

pr(sex,[class=approve],[0.500,0.500]).
pr(sex,[class=reject],[0.250,0.750]).

pr(married,[class=approve],[0.500,0.500]).
pr(married,[class=reject],[0.750,0.250]).

pr(class,[],[0.667,0.333]).


/*--------------------------------------------------------------*/
/* Suggested Experiments with learning algorithms:              */
/*--------------------------------------------------------------*/
/* 1. Represent nominal attributes as structural (for vs.pl)    */
/*--------------------------------------------------------------*/
son(yes,?).
son(no,?).
son(comp,?).
son(car,?).
son(f,?).
son(m,?).
/*--------------------------------------------------------------*/
/* 2. vs (inductive bias):  Exchange 4 and 5.                   */
/*--------------------------------------------------------------*/
/* 3. id3 (effect of noise): switch classes of 12 and 6.        */
/*--------------------------------------------------------------*/
/* For classification (assume that the class is unknown):       */
/*   example(13, reject,  [emp=yes,buy=car,sex=m,married=no]).  */
/*--------------------------------------------------------------*/
