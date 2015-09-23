 /*=========================================================================
   File  : FRONTEND.PL

    A simple front-end for user interaction
    
    Author: Juan C. Ruiz Anton
            Universitat Jaume I, Castelló, Spain
            January 2004    
 ==========================================================================*/

 :- module(frontend,[
      main/0,
      restart/0
     ]).

 :- use_module(msg,[msg/1, msg/2]).
 :- use_module(gfutils,[writelist/1]).
 :- use_module(ppfs,[pp_f_structure/4]).
 :- use_module(gfu,[compile_file/2, parse/2, dmember/2]).
 :- use_module(tokenizer,[tokenize/3]).

 :- dynamic text/3, link/2, left/6.

 %==========================================================================

 
 main :-
    title_screen, 
    flag(axiom,_,'S'),
    flag(det,_,1),	    % Deterministic parsing, by default
    flag(trac,_,0),
    flag(cpl,_,1),	    % Turns on the completeness condition
    flag(lnk,_,1),	    % Compiled with links (by default)
    run.

 title_screen :-
    write('GFU-LAB (version 1.0)'),nl,
    write('Copyright (c) Juan C. Ruiz Anton 1994'),nl,
    write('Universitat Jaume I (Castelló, Spain)'),nl,nl,   
    write('For help : enter \'h\' or \'?\''),nl.

run :-
	repeat,
	nl, write('GFU> '),
	get_what(Opt),
	( Opt==exit 
	-> halt,!
	;  procr(Opt),fail).
             
             
 %% get_what(-Opt)
 %%  Waits for a user's command (Opt)

 get_what(Opt) :-
	read_line_to_codes(user,STR),
	tokenize(STR,[],Tokens),
	interpret_tokens(Tokens,Opt).
 
 
 %%------------------------------------------------------
 %% interpret_tokens(++T,-C)
 %%  <- Interprets the token list T as a GFU command C
 %%------------------------------------------------------

 interpret_tokens([],_) :- !,fail.

 interpret_tokens(['*'|Words],input(Words)) :- !.

 interpret_tokens([T,Num|_],test(Num)) :- 
        integer(Num),!,
        ( T=test ; T=t).

 interpret_tokens([T,all],testall) :- 
        ( T=test ; T=t),!.
 
 interpret_tokens(['?'],help) :- !.

 interpret_tokens([h],help) :- !.
        
 interpret_tokens([H,T],help(T)) :- 
       (H=help ; H=h),!.

 interpret_tokens([E],exit) :- 
       (E=exit ; E=quit ; E=bye),!.

 interpret_tokens([info],info) :- !.
 
 interpret_tokens([l|Rest],load(Type,File)) :-!, 
       filename(Rest,File,Type).

 interpret_tokens([ver|_],version) :-!.

 interpret_tokens([s|Rest],svdb(File)) :-!, 
       filename(Rest,File,_).

 interpret_tokens([links|_],chlink(1)) :- !.
 interpret_tokens([no,links|_],chlink(0)) :- !.

 interpret_tokens([det|_],chdet(1)) :- !.
 interpret_tokens([no,det|_],chdet(0) ) :- !.

 interpret_tokens([trace|_],debug) :- !.
 interpret_tokens([no,trace|_],nodebug) :- !.

 interpret_tokens([compl|_],compl) :- !.
 interpret_tokens([no,compl|_],nocompl) :- !.

 interpret_tokens([list|_],show(list)) :- !.

 interpret_tokens([synt|L],show(synt=L)) :- !.

 interpret_tokens([sem],show(sem=[])) :- !.

 interpret_tokens([axiom,'=',Ax|_],new_axiom(Ax)) :- !.

 interpret_tokens(_,error).
 
 
 %% filename(+L,-T,-E)
 %%   <- Given a list of atoms L, joins its elements together
 %%      forming a filename F, and returning its extension type E (gra, lex, txt)

 filename([],'',no) :-!.
 filename(['.',Ext|_],E,Ext) :-  filetype(Ext),atom_concat('.',Ext,E),!.
 filename([A|B],L,Type) :- filename(B,S,Type), atom_concat(A,S,L).
 
 filetype(gra).
 filetype(lex).
 filetype(txt).
                                      
 restart :- run.

 %---------------------------------------------------------------------
 % procr: 

 procr(help) :-	!,
        nl,
        write('  How to...?'),nl,
        write('  -------------'),nl,nl,
        write('   load grammars and lexicons  .............. h load'),nl,
        write('   Parse a given phrase ..................... h parse'),nl,
        write('   Save grammars ............................ h save'),nl,
        write('   Trace the way the parser works ........... h trace'),nl,
        write('   See the results of the analysis .......... h see'),nl,
        write('   Quit ..................................... quit'),nl.

 procr(help(load)) :- !,
        nl,
        write('   l FILENAME ............ Compiles the specified file'),nl,
        write('   [no] links ............ Compilation without/with link clauses'),nl,
        write('   info .................. Displays some parsing parameters'),nl.

 procr(help(parse)) :- !,
        nl,
        write('   [no] det ............. Switches [off] the deterministic parsing'),nl,
        write('   [no] trace ........... Switches [off] the trace'),nl,
        write('   axiom = N ............ Defines N as the new parsing axiom'),nl,
        write('   * STRING ............. Parses the sentence STRING'),nl,
        write('   test NUM.............. Parses the sentence numbered NUM '),nl.

 procr(help(save)) :- !,	
        write('   s FILENAME ........ Saves the database in prolog format'),nl.

 procr(help(trace)) :- !,
        nl,
        write('   [no] trace ........... Switches [off/in] the trace'),nl,
        write('   [no] compl ........... Switches [off/in] the completeness condition'),nl.
	
 procr(help(see)) :- !,
        nl,
        write('   synt [:R] ............ Displays the syntactic FD, simplified'),nl,
        write('                          for the features specified in R'),nl,
        write('   sem  ................. Displays the semantic part of the DF'), nl,
        write('                          (if implemented)'),nl,
        write('   list ................. Displays the whole DF as a feature list'),nl,
        write('   dump <FILE> .......... Writes the syntactic and semantic DF in'),nl,
	write('                          the specified file'),nl.

 procr(exit) :- halt.

 procr(error) :- msg(25),!.

 procr(input(C)) :- !,
      flag(axiom,Ax,Ax),	% Locates the current axiom
      flag(c2,_,0),		% sets the parses counter to zero
      parse(Ax,C).

 procr( test(C) ) :-    
      flag(axiom,AxD,AxD),!,               
      (  gfu:text(C,Ax1,Words)
	   -> (Ax1=top -> Ax=AxD ; Ax=Ax1),
	      msg(28,Words), 
	      flag(c2,_,0),
	      parse(Ax,Words)
	   ;  msg(29) 
       ).

 procr(testall) :- !,
	flag(c3,_,1),
        testall.

 procr(show(list)) :- 
	findall(M,recorded(matrix,M,_),Parses),!,
	forall( member(P,Parses), 
                (write('==>'),write(P),nl,nl)).
	
 procr(show(sem=L)) :- 
	findall(M,recorded(matrix,M,_),Parses),!,
	forall( member(P,Parses), 
                (write('==>'),dmember(sem=S,P),!,pp_f_structure(sem,L,7,S) ) ).

 procr(show(synt=L)) :-
	findall(M,recorded(matrix,M,_),Parses),!,
	forall( member(P,Parses), 
                (write('==>'), pp_f_structure(synt,L,7,P) ) ).

 procr(show(_)) :- msg(30),!.

 procr( dumptofile(File) ) :-
 	recorded( matrix, M, _),!,
	telling(Old),
	tell(File),
	pp_f_structure(synt,[],7,M),
	dmember(sem=S,M),
	pp_f_structure(sem,[],7,S),
	told,
	tell(Old),
	msg(31).

 procr(load(Type,X)) :- !,
       exists_file(X)
	  -> compile_file(X,Type) 
	  ;  msg(26). 

 procr(new_axiom(Ax)) :- !,
     flag(axiom,_,Ax).

 procr(info) :- 
     ( flag(language,L,L) -> true; L=no),
     flag(det,D,D),
     flag(axiom,A,A),
     flag(lnk,LK,LK),
     findall(X1,gfu:left(X1,_,_,_,_,_),L1), length(L1,PS),% counts rules
     findall(X2,gfu:link(X2,_),L2), length(L2,LKS),	  % counts Links
     findall(X3,gfu:word(X3,_,_),L3), length(L3,Ws),	  % counts lexical entries
     display_info(L,D,A,LK,PS,LKS,Ws),!.
   
 procr(svdb(File)) :-
        % Should check first that File does exist	
        telling(OLD),
        tell(File), 
        listing([gfu:contr,gfu:colloc,gfu:func,gfu:'$HEAD']),
        nl,listing([gfu:left,gfu:mult,gfu:link,gfu:deflt,gfu:proc,gfu:word]),!,nl,
        told,
        tell(OLD),
        msg(19).

 procr(debug) :-
	flag(trac,_,1),!.

 procr(nodebug) :-
	flag(trac,_,0),!.

 procr(compl) :-
	flag(cpl,_,1),!.

 procr(nocompl) :- 
	flag(cpl,_,0),!.

 procr(chdet(Num)) :-
       flag(det,_,Num),
       write('OK'),nl.

 procr(chlink(Num)) :-
      flag(lnk,_,Num),
      write('OK'),nl.


 procr(version) :- 
        nl, 
        write(' GFU-LAB / version 1.0'),nl,
        write('  Author: J. Carlos Ruiz-Anton'),nl,
        write('  Universitat Jaume I, Castelló, Spain'),nl.

 procr(_).

 testall :-
      repeat,
      flag(c3,M,M+1),
      ( gfu:text(M,_,_)
          -> write(M),tab(1),
             procr(test(M)),nl,
             ( recorded( matrix, Mx, _)
                 -> pp_f_structure(synt,[],7,Mx),
                    dmember(sem=S,Mx),
                    pp_f_structure(sem,[],7,S)
                  ; fail)
           ; !).
  

 display_info(_,_,_,_,PS,_,_) :-
	PS=0,!,
	msg(32).
		
 display_info(L,D,A,LK,PS,LKS,W) :- !,
        writeln('GRAMMAR INFORMATION'),
        write('    Language........... '),write(L),nl,
        write('    Axiom ............. '),write(A),nl,
        ( D=1 -> Det='deterministic' ; Det='nondeterministic'),
        write('    Analysis .......... '),write(Det),write(', '),
        ( LK=1 -> M='optimized'; M='non-optimized'),
        write(M),nl,
        write('    PS rules .......... '),write(PS),nl,
        write('    LINKs ............. '),write(LKS),nl,
        write('    Lexical entries ... '),write(W),nl.


