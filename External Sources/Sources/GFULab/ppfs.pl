/*******************************************************************
  Module: PPFS.PL

    Author: Juan C. Ruiz Anton
            Universitat Jaume I, Castelló, Spain
            January 2004
               
  Code for displaying feature structures
	   
 *******************************************************************/ 

 :- module(ppfs,[pp_f_structure/4]).


 %----------------------------------------------------------
 % PP_F_STRUCTURE( +attributes, +tab list, FD )
 %----------------------------------------------------------

 pp_f_structure(L,Atts,A,B) :- 
             level_msg(L,LevelMsg),
             nl,
             write(LevelMsg),
             nl,nl, 
             tab(A), openbox, pp_f_struct(Atts,[A],B), tabular(c,[A]).

	    
 pp_f_struct(_,_,[]) :- !.
 pp_f_struct(Ats,P,[A|B]) :- 
     pp_feature(Ats,P,A), 
     pp_f_struct(Ats,P,B).

 % writes the vertical lines ('|') for drawing boxes
 
 tabular(_,[]) :- !.
 tabular(c,[X]) :- !,tab(X), put(43), put(45),nl. % closes box
 tabular(m,[X]) :- !,tab(X), put(166).
 tabular(T,[A|B]) :- tab(A), put(166), tabular(T,B).

 % for features

 pp_feature(_,_,sem=_) :- !.
 pp_feature(_,_,_A=Var) :- var(Var),!.

 pp_feature(Ats,Z,A=R) :- 
          is_list(R),
          !,
          tabular(m,Z), tab(1), write(A), tab(1), 
          openbox, 
          string_length(A,L),
          L1 is L+2,
          append(Z,[L1],Z1),
          pp_f_struct(Ats,Z1,R),
          tabular(c,Z1).
	  
 pp_feature(L,Z,A='Plus') :- 
          visib(A,L), !,
          tabular(m,Z),	
          tab(1), 
          write(+A), 
          tab(1),nl.
 
 pp_feature(L,Z,A='Minus') :- 
          visib(A,L),!,
          tabular(m,Z),	
          tab(1), 
          write(-A), 
          tab(1),nl.
 
 pp_feature(L,Z,A=B) :-	
          visib(A,L),!,
          tabular(m,Z),	
          tab(1), 
          write(A=B), 
          tab(1),nl.

 pp_feature(_,_,_).

 openbox :- put(43), put(45) ,nl.


  %%%
  %%% visib(+R,+L)
  %%% <- The feature R is visible if it is declared as such in the list L, 
  %%%    or if there is no declaration of visible features

 visib(_A,[]) :- !.
 visib(A,L) :- dmember(A,L).

 
 level_msg(synt,'SYNTACTIC REPRESENTATION:').
 level_msg(sem,'SEMANTIC REPRESENTATION:').

