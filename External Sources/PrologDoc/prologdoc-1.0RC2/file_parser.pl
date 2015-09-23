/* -*- Mode: Prolog -*-

  This file is part of PrologDoc (http://prologdoc.sourceforge.net/).

  Copyright (C) 1999 by Elisheva Bonchek (le7bonch AT cs.huji.ac.il) and Sara Cohen (sarac AT ie.technion.ac.il, http://iew3.technion.ac.il/~sarac) 
  Copyright (C) 2004 by Bram Adams (bram.adams AT ugent.be)
  Copyright (C) 2004 by Salvador Fandino (sfandino@yahoo.com)


  PrologDoc is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  PrologDoc is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PrologDoc; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/***
  @descr This module can be used to analyse a file documented
         by the <font color=red><i> PrologDoc </i></font> 
	 documentation syntax. The file is analyzed to find the 
         following information:</p>
               <ul> 
	          <li> The files that it consults
	          <li> The libraries it consults
	          <li> Side-effects of loading in the file (i.e.,
		       actions that are performed upon loading)
		  <li> General information about the file 
	          <li> Predicates defined in the file, along with
	               information specified about them
		  <li> If the file is a module -- the module name
	               and the predicates shown are only those exported
	       </ul><p>
	 This information can be used to generate automatic documentation
         about the file.
  @author Sara Cohen
  @author Elisheva Bonchek
  @date   19/7/99
*/

:-module(file_parser,[analyze/7]).

:-use_module(library(lists)).
:-use_module(library(system)).
:-use_module(file_translater).

/**
  @form analyze(FileName, FileConsults, LibConsults,
	        SideEffects,Details, General, ModuleName)
  @constraints 
        @ground FileName
        @unrestricted FileConsults
        @unrestricted LibConsults
        @unrestricted SideEffects
        @unrestricted Details
        @unrestricted General
        @unrestricted ModuleName
   @descr True if</p> 
        <ul>
	    <li> FileConsults are the files consulted by FileName
	    <li> LibConsults are the libraries consulted by FileName
	    <li> SideEffects are the side-effects upon loading FileName
	    <li> Details are the information about the predicates defined in
	         FileName (only those exported if FileName is a module
	    <li> General is general information about FileName
	    <li> ModuleName is the name of the module defined by FileName
        </ul><p>
*/
analyze(FileName, FileConsults, LibConsults, SideEffects,Details, General, ModuleName) :-
	read_file(FileName, Predicates, SideEffects, Consulted, Comments, General, Module),
	split_consult(Consulted, FileConsults, LibConsults),
	sort(Predicates, PredSort),
	sort(Comments, ComSort),
	pred_union(PredSort, ComSort, Union),
	(   Module = (ModuleName, Export)
	->  sort(Export,ExpSort),
	    pred_intersection(ExpSort,Union,Details)
	;   Details = Union,
	    ModuleName = '' ).



% Name of a temporary file used for processing
temp('tmp').

/*
  read_file(FileName,Predicates,SideEffects,Consulted,
            Comments,General,Module)
  Finds for the file FileName the Predicates defined in it,
  the SideEffects upon loading it, the files Consulted in it,
  the Comments describing the perdicates in it, General information
  about it and the Module defined by it (if one exists)
*/
read_file(FileName,Predicates,SideEffects,Consulted,
	  Comments,General,Module):-
	temp(Temp),
	translate(FileName,Temp),
	see(Temp),
	process_term1(AllPredicates, SideEffects, Consulted, Comments, General, Module, []),
	make_set(AllPredicates,Predicates),
	seen,
	!,
	delete_file(Temp).

/*
  process_term1(Predicates,SideEffects,Consulted, Comments,General,Module, Q)
  process_term2(Term,Predicates,SideEffects,Consulted, Comments,General,Module, Q)
  Predicates used to read terms from the file and process the information to build the profile
*/

process_term1(AllPredicates, SideEffects, Consulted, Comments, General, Module, [Term|Q]) :-
	!,
	process_term2(Term, AllPredicates, SideEffects, Consulted, Comments, General, Module, Q).

process_term1(AllPredicates, SideEffects, Consulted, Comments, General, Module, []) :-
	read_term(Term, [module(read_here)]),
	process_term2(Term, AllPredicates,SideEffects, Consulted,Comments,General,Module, []).

% end of file
process_term2(end_of_file,[],[],[],[],[],[], _) :- !.

% predicate comment
process_term2(xx(Comment), Predicates, SideEffects, Consulted, [(Pred, Values)|Comments], General, Module, Q) :-
	!,
	parse_comment(Comment,Values),
	Predicates = [(Pred, _)|_],
	process_term1(Predicates, SideEffects, Consulted, Comments, General, Module, Q).

% general comment
process_term2(xxx(Comment), Predicates, SideEffects, Consulted, Comments, Values, Module, Q) :-
	!,
	parse_comment(Comment, Values1),
	append(Values1, General, Values),
	process_term1(Predicates, SideEffects, Consulted, Comments, General, Module, Q).

% expands ":- (A, B)." as "(:- A), (:- B)."
process_term2(':-'((A, B)), Predicates, SideEffects, Consult, Comments, General, Module, Q) :-
	!,
	process_term2(':-'(A), Predicates, SideEffects, Consult, Comments, General, Module, [':-'(B)|Q]).

% module declaration
process_term2(':-'(module(ModuleName, ExportPred)), Predicates, SideEffects, Consult, Comments, General, (ModuleName, SplitExportPred), Q) :-
	!,
	split_names_arities(ExportPred,SplitExportPred),
	process_term1(Predicates, SideEffects, Consult, Comments, General, _, Q).

% consult list (empty)
process_term2(':-'([]), Predicates, SideEffects, Consult, Comments, General, Module, Q) :-
	!,
	process_term1(Predicates, SideEffects, Consult, Comments, General, Module, Q).

% consult list
process_term2(':-'([C|T]), Predicates, SideEffects, [C|Consult], Comments, General, Module, Q) :-
	!,
	process_term1(Predicates, SideEffects, Consult, Comments, General, Module, [':-'(T)|Q]).

% consult(file) or use_module(file)
process_term2(':-'(Dir), Predicates, SideEffects, [C|Consult], Comments, General, Module, Q) :-
	(   Dir = consult(C)
	;   Dir = use_module(C) ),
	!,
	process_term1(Predicates, SideEffects, Consult, Comments, General, Module, Q).

% side effects
process_term2(':-'(SideEffect), Predicates, [SideEffect|SideEffects], Consult, Comments, General, Module, Q) :-
	!,
	process_term1(Predicates, SideEffects, Consult, Comments, General, Module, Q).

% predicate definition
process_term2(':-'(Head, _), [((Name, Arity),[])|Predicates], SideEffects, Consulted, Comments, General, Module, Q) :-
	!,
	functor(Head, Name, Arity),
	process_term1(Predicates, SideEffects, Consulted, Comments, General, Module, Q).

% predicate definition (without tail)
process_term2(Term, [((Name, Arity),[])|Predicates], SideEffects, Consulted, Comments, General, Module, Q) :-
	% Term \= ':-'(_),	red cuts in previous clauses make these unnecessary
	% Term \= ':-'(_,_),
	functor(Term, Name, Arity),
	process_term1(Predicates, SideEffects, Consulted, Comments, General, Module, Q).


/*
  split_consult(Consults, Files, Libs)
     if Files are the files listed in Consults and Libs
     are the libraries listed in Consults
*/
split_consult([], [], []).
split_consult([H|T], Files, Libs) :-
	(   H =.. [Path, File|_]
	->  (   Path = library
	    ->	Libs = [File|Libs1]
	    ;	Libs = [H|Libs1] ),
	    split_consult(T, Files, Libs1)
	;   Files = [H|Files1],
	    split_consult(T, Files1, Libs) ).


/*
  pred_intersection(List1,List2,List3)
     if List3 lists the Predicates in List1 and in List2
*/
pred_intersection([],_,[]) :-
	!.
pred_intersection(_,[],[]) :-
	!.
pred_intersection([Pred|List1],[(Pred,Comments)|List2],
	     [(Pred,Comments)|List3]):-!,
	pred_intersection(List1,List2,List3).
pred_intersection([Pred1|List1],[(Pred2,Comments)|List2],List3):-
	Pred1 @< Pred2,!,
	pred_intersection(List1,[(Pred2,Comments)|List2],List3).
pred_intersection([Pred1|List1],[(Pred2,_Comments)|List2],List3):-
	Pred1 @>= Pred2,!,
	pred_intersection([Pred1|List1],List2,List3).

/*
  pred_union(List1,List2,List3)
     if List3 lists the Predicates in List1 or in List2
*/
pred_union([], List, List) :-
	!.
pred_union(List, [], List) :-
	!.
pred_union([(Pred,[])|List1], [(Pred,Comments)|List2],
	   [(Pred,Comments)|List3]) :-
	!,
	pred_union(List1,List2,List3).
pred_union([(Pred1,[])|List1],[(Pred2,Comments)|List2],
	   [(Pred1,[])|List3]) :-
	Pred1 @< Pred2,
	!,
	pred_union(List1,[(Pred2,Comments)|List2],List3).
pred_union([(Pred1,[])|List1],[(Pred2,Comments)|List2],
	   [(Pred2,Comments)|List3]) :-
	Pred1 @>= Pred2,
	!,
	pred_union([(Pred1,[])|List1],List2,List3).

/* 
   split_names_arities(List1,List2)
      if List2 is derived from List1 by spliting up the
         predicate_names/arities to (predicate_names,arities)
*/
split_names_arities([],[]).
split_names_arities([(Name/Arity)|NAs],[(Name,Arity)|NAsPairs]):-
	split_names_arities(NAs,NAsPairs).

/*
  make_set(List1,List2) 
    if List2 is derived from List1 by removing duplicates
*/
make_set(L1,L2):-make_set(L1,L2,[]).
make_set([],[],_Seen).
make_set([H|T],[H|T1],Seen):-
	\+ member(H,Seen),!,
	make_set(T,T1,[H|Seen]).
make_set([_|T],T1,Seen):-
	make_set(T,T1,Seen).

