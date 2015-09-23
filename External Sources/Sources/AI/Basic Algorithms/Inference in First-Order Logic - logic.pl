%------------------------------------------------------------------------
% Examples of Propositional and First-Order Logic sentences
%------------------------------------------------------------------------
% To be used with (see examples below): 
%  - clausify.pl to transalte in CNF
%  - sat.pl to check for validity/satisfiability/entailment
%  - resolve.pl for clausal resolution
%------------------------------------------------------------------------
:- op(850,fx,~).    % negation
:- op(900,xfy,#).   % disjunction
:- op(900,xfy,&).   % conjunction
:- op(950,xfy,->).  % implication
%-------------------------------------------------------------------------
% Exercise 7.9 from AIMA: Stuart Russell, Peter Norvig. Artificial 
% Intelligence: % A Modern Approach, Second Edition,  Prentice Hall, 2003.
% If the unicorn is mythical, then it is immortal, but if
% it is not mythical, then it is a mortal mammal. If the 
% unicorn is either immortal or a mammal, then it is horned. 
% The unicorn is magical if it is horned.
%  (a) Can we prove that the unicorn is mythical?
%  (b) Can we prove that the unicorn is magical?
%  (c) Can we prove that the unicorn is horned?
%-------------------------------------------------------------------------

unicorn((mythical -> immortal) & 
	(~mythical -> mammal) & 
	(immortal # mammal -> horned) &
	(horned -> magical)).

/* Examples of using the above sentence:
----------------------------------------

1. Translate into CNF (consult clausify.pl)
-------------------------------------------

?- unicorn(S),translate(S,C).

S = (mythical->immortal)& (~mythical->mammal)& (immortal#mammal->horned)& (horned->magical)
C = [cl([immortal], [mythical]), cl([mythical, mammal], []), cl([horned], [immortal]), cl([horned], [mammal]), cl([magical], [horned])] 

2. Find models - truth value assignment to the predicates (consult clausify.pl and sat.pl).
   T = predicates that are true, F = predicates that are false.
------------------------------------------------------------------------------------------

?- unicorn(S),translate(S,C),sat(C,T,F).

S = (mythical->immortal)& (~mythical->mammal)& (immortal#mammal->horned)& (horned->magical)
C = [cl([immortal], [mythical]), cl([mythical, mammal], []), cl([horned], [immortal]), cl([horned], [mammal]), cl([magical], [horned])]
T = [horned, magical, mammal]
F = [immortal, mythical] ;

S = (mythical->immortal)& (~mythical->mammal)& (immortal#mammal->horned)& (horned->magical)
C = [cl([immortal], [mythical]), cl([mythical, mammal], []), cl([horned], [immortal]), cl([horned], [mammal]), cl([magical], [horned])]
T = [horned, immortal, magical, mammal]
F = [mythical]

3. Proving what follows from S using the deduction theorem (consult clausify.pl and sat.pl)
-------------------------------------------------------------------------------------------

?- unicorn(S),translate(S & ~mythical,C),sat(C,T,F).  % S |= mythical <=> S & ~mythical |= false 

S = (mythical->immortal)& (~mythical->mammal)& (immortal#mammal->horned)& (horned->magical)
C = [cl([immortal], [mythical]), cl([mythical, mammal], []), cl([horned], [immortal]), cl([horned], [mammal]), cl([magical], [horned]), cl([], [mythical])]
T = [horned, magical, mammal]
F = [immortal, mythical] 

Yes (S |\= mythical, S does not entail mythical)

?- unicorn(S),translate(S & ~horned,C),sat(C,T,F).  % S |= horned <=> S & ~horned |= false 

No (S |= horned, S entails horned)

4. Inference by resolution (consult clausify.pl and resolve.pl)
---------------------------------------------------------------

?- unicorn(S),translate(S,C),resolve(C).
Derived: cl([horned], [mythical])
Derived: cl([immortal, mammal], [])
Derived: cl([horned, mythical], [])
Derived: cl([horned, mammal], [])
Derived: cl([magical], [immortal])
Derived: cl([magical], [mythical])
Derived: cl([magical, mammal], [])
Derived: cl([magical], [mammal])
Derived: cl([magical, mythical], [])
Derived: cl([horned, immortal], [])
Derived: cl([magical, immortal], [])
Derived: cl([horned], [])               => S |= horned
Derived: cl([magical, horned], [])
Derived: cl([magical], [])              => S |= magical

S = (mythical->immortal)& (~mythical->mammal)& (immortal#mammal->horned)& (horned->magical)
C = [cl([immortal], [mythical]), cl([mythical, mammal], []), cl([horned], [immortal]), cl([horned], [mammal]), cl([magical], [horned])] 

?- unicorn(S),translate(S & ~horned,C),resolve(C).  % NOTE the empty clauses at the end
Derived: cl([horned], [mythical])
Derived: cl([immortal, mammal], [])
Derived: cl([horned, mythical], [])
Derived: cl([horned, mammal], [])
Derived: cl([magical], [immortal])
Derived: cl([magical], [mythical])
Derived: cl([magical, mammal], [])
Derived: cl([], [immortal])
Derived: cl([], [mythical])
Derived: cl([mammal], [])
Derived: cl([magical], [mammal])
Derived: cl([magical, mythical], [])
Derived: cl([], [mammal])
Derived: cl([mythical], [])
Derived: cl([horned, immortal], [])
Derived: cl([magical, immortal], [])
Derived: cl([immortal], [])
Derived: cl([horned], [])
Derived: cl([magical, horned], [])
Derived: cl([magical], [])
Derived: cl([], [])             empty clause derived ==> (S & ~horned) is unsatisfiable

*/

%-------------------------------------------------------------------------
%  Other examples 
%-------------------------------------------------------------------------
% Three people, Amy, Bob, and Cal, are each either a liar 
% or a truth-teller. Assume that liars always lie, and 
% truth-tellers always tell the truth.
%  - Amy says, "Cal and I are truthful."
%  - Bob says, "Cal is a liar."
%  - Cal says, "Bob speaks the truth or Amy lies."
% What can you conclude about the truthfulness of each?
%-------------------------------------------------------------------------

liars1((amy -> cal & amy) &
       (bob -> ~cal) &
       (cal -> bob # ~amy)).

%-------------------------------------------------------------------------
% Three people, Amy, Bob, and Cal, are each either a liar 
% or a truth-teller. Assume that liars always lie, and 
% truth-tellers always tell the truth.
%  - Amy says, "Cal is not honest."
%  - Bob says, "Amy and Cal never lie."
%  - Cal says, "Bob is correct."
% What can you conclude about the truthfulness of each?
%-------------------------------------------------------------------------

liars2((amy -> ~cal) &
       (bob -> amy & cal) &
       (cal -> amy & cal)).

%-------------------------------------------------------------------------
% Propositional representation of part of the wumpus world (AIMA, Fig.7.2)
% Rooms (11), (12), (21), (22)
%-------------------------------------------------------------------------

wumpus_pl(

% Knowledge about rooms (11), (12), (21), (22)

  (wumpus11 -> stench21 & stench12) &
  (wumpus12 -> stench11 & stench22) &
  (wumpus21 -> stench11 & stench22) &
  (wumpus22 -> stench21 & stench12) &

  (pit11 -> breeze21 & breeze12) &
  (pit12 -> breeze11 & breeze22) &
  (pit21 -> breeze11 & breeze22) &
  (pit22 -> breeze21 & breeze12) &

% Agent perception at room (11)

  ~wumpus11 &
  ~pit11    &
  ~stench11 &
  ~breeze11 &

% Agent perception at room (12)

  ~stench12 &
  breeze12  &

% Agent perception at room (21)

  stench21 &
  ~breeze21

  ).

%---------------------------------------------------------------
% First-Order representation of the wumpus world (AIMA, Fig.7.2)
%---------------------------------------------------------------

wumpus_fol(

% Knowledge about wumpus world

  (wumpus(X,Y) & succ(X,X1) & succ(Y,Y1) -> stench(X,Y1) & stench(X1,Y)) &
  (pit(X,Y) & succ(X,X1) & succ(Y,Y1) -> breeze(X,Y1) & breeze(X1,Y))    &

  succ(1,2) &
  succ(2,1) &
  succ(2,3) &
  succ(3,2) &
  succ(3,4) &
  succ(4,3) &

% Agent perception at room (11)

  ~wumpus(1,1) &
  ~pit(1,1)    &
  ~stench(1,1) &
  ~breeze(1,1) &

% Agent perception at room (12)

  ~stench(1,2) &
  breeze(1,2)  &

% Agent perception at room (21)

  stench(2,1) &
  ~breeze(2,1)

  ).

%-------------------------------------
% Who loves John? (skolemization)
%-------------------------------------

john(

  all(X,exists(Y,(man(X) -> woman(Y)))) &
  loves(X,Y) &
  man(john) &
  ~loves(john,X)

 ).

%-------------------------------------
% The lost driver
%-------------------------------------

driver(

  lost(driver) &
  (use_phone(X) -> get_out(X)) &
  (use_map(X) -> get_out(X)) &
  (lost(X) -> use_map(X) # use_phone(X))

  ).

