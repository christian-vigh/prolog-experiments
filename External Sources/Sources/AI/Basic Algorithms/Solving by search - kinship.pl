/*  Two family trees and their representation in Prolog (from Quinlan-90):



         christopher = penelope            andrew = christine

              _______|_________           ________|_________
             |                 |         |                  |

victoria = james            margaret = arthur            jennifer = charles

                              _______|_________
                             |                 |

                           colin           charlotte

*/


/* The basic facts */

husband(christopher,penelope).
husband(andrew,christine).
husband(james,victoria).
husband(arthur,margaret).
husband(charles,jennifer).

parent(christopher,james).
parent(christopher,margaret).
parent(penelope,james).
parent(penelope,margaret).
parent(andrew,arthur).
parent(andrew,jennifer).
parent(christine,arthur).
parent(christine,jennifer).
parent(margaret,colin).
parent(margaret,charlotte).
parent(arthur,colin).
parent(arthur,charlotte).


/* Using Prolog rules to define more relationships */

grandparent(X,Y) :- parent(X,Z),parent(Z,Y).

man(X) :- husband(X,_).

father(X,Y) :- parent(X,Y),man(X).

mother(X,Y) :- parent(X,Y), \+ man(X).  % "\+" means logical "NOT".

grandfather(X,Y) :- father(X,Z), father(Z,Y).
grandfather(X,Y) :- father(X,Z), mother(Z,Y).


/* Prolog search mechanism for the goal "grandfather(X,Y)" AND-OR-NOT search tree 

                                   grandfather(X,Y)
                                          |
                                         OR
                     _____________________|_____________________
                    |                                           |
             grandfather(X,Y)                            grandfather(X,Y)
                    |                                           |
                   AND                                         AND
          __________|___________                      __________|___________
          |                     |                    |                      |
     father(X,Z)           father(Z,Y)          father(X,Z)             mother(Z,Y)
          |                     |                    |                      |
         AND                   AND                  AND                    AND
     _____|_____           _____|_____          _____|_____            _____|_____ 
    |           |         |           |        |           |          |           |
parent(X,Z)   man(X)  parent(Z,Y)   man(Z)  parent(X,Z)   man(Z)   parent(Z,Y)   NOT
                |                     |                    |                      |
            husband(X,_)         husband(Z,_)          husband(Z,_)             man(Z)
                                                                                  |
                                                                              husband(Z,_)


Complete the tree with facts and see how variables are instantiated ...

*/


/* Alternative (shorter) definition of grandfather (draw the AND-OR tree): 

grandfather(X,Y) :- grandparent(X,Y),man(X).

*/


/* Exercises: 

1. Define more relationships: grandmother, woman, wife, daughter, son, brother, 
   sister, uncle, aunt, nephew, niece. Use the same database of facts and add more rules.

2. Draw the AND-OR Prolog trees for the new relationships.

3. What happens if you add more than one definition of a single relationship?

4. Are more facts needed to define all possible relationships? Why?

*/


