/*******************************************************************
  Module: GFU.PL
          Parser and compiler of GFU language

    Author: Juan C. Ruiz Anton
            Universitat Jaume I, Castelló, Spain
            January 2004
            	   
 *******************************************************************/ 

 :- module(gfu,[compile_file/2, parse/2, dmember/2]).

 :- use_module(msg,[msg/1]).
 :- use_module(readutils,[readlist/2]).
 :- use_module(gfutils,[writelist/1,pick/3,atom_string_list/2,list_to_disj/2,litteral/1,prefix/3,lastnv/2,lst/1]).
 :- use_module(comp_swi,[eraseall/1,new_counter/2]).

 :- dynamic func/1, proc/3, deflt/2, '$HEAD'/1, text/3, isa/2, mult/5, left/6, link/2, colloc/2, contr/2, class/3, template/3, word/3.


 %--------------------------------------------------------------------
 % compile_file(+F)
 % Compiles a grammar/lexicon file onto dynamic prolog terms 
 %--------------------------------------------------------------------

compile_file(FileName,Ext) :-
	file_name_extension(_Name,Ext,FileName),
        msg_filetype(Ext),
	% compilation of the lexicon file fails if functions are not declared
        (   Ext==lex, \+func(_)
        ->  msg(33), 
            fail
        ;   true
        ),
        sweep_out(Ext),
        eraseall(file),
        recorda(file, Ext, _D),
        (   open(FileName, read, E, [])
        ->  read_loop(E),
            close(E),
            nl,
            (   Ext=gra
            ->  complete_comp
            ;   true
            )
        ;   msg(21), fail
        ).


 msg_filetype(gra) :- !,write('=> Compiling the Grammar :'),nl.
 msg_filetype(lex) :- !,write('=> Compiling the Lexicon :'),nl.
 msg_filetype(txt) :- !,write('=> Loading text :'),nl.
 msg_filetype(Other) :- write('Wrong type type:'), write(Other),nl,!, fail.


 %-------------------------------------------------------------------------
 %  sweep_out( +Type)
 %   <- erases all rules of the specified Type (gra, lex and txt)
 %      before loading another file back.
 %-------------------------------------------------------------------------

 sweep_out(gra) :-
	retractall(func(_)),
	retractall('$HEAD'(_)),
	retractall(mult(_,_,_,_,_)),
	retractall(left(_,_,_,_,_,_)),
	retractall(link(_,_)),
	retractall(deflt(_,_)),
	retractall(proc(_,_,_)),
	retractall(isa(_,_)),
	flag(c2,_,0).   % set the counter of defaults c2 to zero

 sweep_out(lex) :- 
	retractall(colloc(_,_)),
	retractall(contr(_,_)),
	retractall(class(_,_,_)),
	retractall(template(_,_,_)),
        retractall(word(_,_,_)).

 sweep_out(txt) :- 
	retractall(text(_,_,_)).


 %% A loop for reading lines in the open stream

 read_loop(Str) :-
     repeat,
     readlist(Str,Term),
     % write('==>'),write(Term),nl,  % ojo
     (Term=[eof] -> !
                 ; compile_term(Term), 
		   fail ).


/*-----------------------------------------------------------------------
   COMPLETE_COMP generates the LINK clauses and the terminative clauses
   for every category in the asserted rules. 

   In this working version of GFU, compilation time of the LINK table 
   is usually unacceptable for many grammars, and it is preferrable 
   to leave it out in testing grammars.

   When compiling fully-tested grammars, especially to make a stand-alone 
   prolog application, all the links should be compiled.
  ----------------------------------------------------------------------*/
  
 complete_comp:-
       assertz( left(X,X,REP,REP,L,L) ),
       ( flag(lnk,1,1)
           -> msg(22), 
              gener_links 
	    ; assert(link(_,_))
       ).


 %------------------------------------------------------------------------ 
 % GENER_LINKS/0 generates recursive links such as 
 % det-np and np-s ==> det-s 
 %------------------------------------------------------------------------

 gener_links :- clause(link(A,B),true),
                clause(link(B,C),true),
                B \= C,	
                assertc( link(A,C)), 
                fail.
 
 gener_links :- assertz( link(X,X) ).	


 /*---------------------------------------------------------------
  compile_term( +T)
    <- Creates a dynamic term from a token list T, and asserts it
       into the internal database.
 -----------------------------------------------------------------*/

 compile_term(T) :- 
      parse_term(T,T1),
      % write(T1),nl, % ojo
      prolog_term(T1),
      !.

 compile_term(X) :- msg(20), writelist(X),nl.

 
 /*---------------------------------------------------------------
    PARSE_TERM( +L, -T)
      == T is a prolog term corresponding to the tokenlist L.
  ---------------------------------------------------------------*/

 parse_term( [C,-,>|L], contr(C,LS)) :- !,
        atom_string_list(L,LS).

 parse_term([Loc, <,-|L], colloc(List,Loc)) :- !,
        atom_string_list(L,List).

 parse_term([E,<|L], '$SUB'(E,L)) :- !.

 parse_term([C,=,=,>| Rest], for(C,Eqs)) :-!,  
        parse_equations(Eqs,Rest,[]).	  

 parse_term(['FG',= | Fs ],func(Fs)) :- !.

 parse_term(['LANGUAGE',=,L],lang(L)) :- !.

 parse_term(['START',=,L],axiom(L)) :- !.
 
 parse_term([@,M,= | Rest],macro(M,Eqs)) :- !,
         parse_equations(Eqs,Rest,[]).
	 
 parse_term([C, -,-,> | B ], rule(C,Body)) :- !,
                parse_body(Body,B,[]).

 % for words (lexemes and roots)
 parse_term([P,=,C,'(',Type,')',':'|Rest], word(P,C,Type,Eqs)) :- !,
                parse_equations(Eqs,Rest,[]).
                
 parse_term([P,=,C,':'|Rest], word(P,C,C,Eqs)) :- !,
                parse_equations(Eqs,Rest,[]).

 parse_term([P,=,C], word(P,C,C,[]) ) :- !.
 
 parse_term([L], word(L,L,'NIL',[]) ) :- recorded(file,lex,_),!. 

 % For supertypes (in the lexicon file)

 parse_term(['CLASS',T,'(',ST,')',':'|Rest], class(T,ST,Eqs)) :- !,
                parse_equations(Eqs,Rest,[]).

 parse_term(['CLASS',T,':'|Rest], class(T,'NIL',Eqs)) :- !,
                parse_equations(Eqs,Rest,[]).

 % For test sentences

 parse_term([#,N,'(',Ax,')'|Rest],text(N,Ax,T)) :- !,  
        atom_string_list(Rest,T).

 parse_term([#,N|Rest],text(N,top,T)) :- !,   
        atom_string_list(Rest,T).


 %------------------------------------------------------------------
 % parse_equations( -L, +Te, -Ts)
 %  == L is the list of equations that is at the beginning
 %     of the token list Te. The remaining tokens are in list Ts.
 %------------------------------------------------------------------
      
 parse_equations([Eq|Z],L1,L) :-
                parse_eq(Eq,L1,L2),
                parse_more_equations(Z,L2,L).

 parse_more_equations(Y,L1,L) :- parse_equations(Y,L1,L),!.
 parse_more_equations([],X,X).

 %%%
 %%%  parse_eq( -T, +L1, -L2)
 %%%     T is a prolog term for an equation at the beginning of
 %%%     the token list T. L1 and L2 are difefrence lists of tokens.

 parse_eq(not(E),['NOT'|L1],L ) :-!,parse_eq(E,L1,L).

 parse_eq(macro(M),[@,M|L],L ) :- !.
    
 parse_eq(Ds,['('|L1],L ) :- !,
    parse_disj(Ds,L1,[')'|L]).

 parse_eq(if(Cond,Then,Else),['IF'|L1],L) :- !,
    parse_equations(Cond,L1,['THEN'|L2]),
    parse_equations(Then,L2,L3), 
    parse_else(Else,L3,L).
	 
 parse_eq( Eq,L1,L ) :-	  
        path(P1,L1,L2),
        operator(OP,L2,L3),
        path_value(P2,L3,L),
        Eq =.. [OP,P1,P2].

 %%%
 %%% parse_disj
 %%%   <- Recognizes a sequence of disjunctive elements
 
 parse_disj( (D;Ds),L1,L ) :- 
     parse_equations(D,L1,['|'|L2]),
     parse_disj(Ds,L2,L),!.
 parse_disj(X,L1,L) :- parse_equations(X,L1,L).

 % parse_else
 
 parse_else( Else,['ELSE'|L1],L ) :- !, parse_equations(Else,L1,L).
 parse_else( true,L,L ).

 %%%
 %%% path( -R, +T1, -T2)
 %%%   <- R is the representation of a path.
 %%%      For example, U, U/head=sujeto, etc.
       
 path( M/P, L1,L ) :-
        metavariable(M,L1,['/'|L2]),
        attr_path(P,L2,L),!.

 path('U',['U'|L],L).
 path('D',['D'|L],L).

 %%% metavariable

 metavariable('U',['U'|L],L).
 metavariable('D',['D'|L],L).

 %%%
 %%%  attr_path
 %%%   == Parses an attribute path (the separator is a slash)

 attr_path(A/As,L1,L) :-
     attrib(A,L1,['/'|L2]),!,
     attr_path(As,L2,L).
		
 attr_path(X,L1,L) :- attrib(X,L1,L).

 %%%
 %%%  attrib
 %%%   <- Describes the attribute types:
 %%%      (i)   expressions of fucntional uncertainty
 %%%      (ii)  functions
 %%%      (iii) symbols

 attrib(X,L1,L) :- fi_expr(X,L1,L),!.
 attrib(ev(X),['{'|L1],L) :- path(X,L1,['}'|L]),!.
 attrib(X,[X|L],L) :- litteral(X), \+ prolog_atom(X).

 %%%
 
 fi_expr( set(F), ['FG'|L],L ) :- get_all_funcs(F).
 fi_expr( set(X), ['('|L1],L ) :-  funcset(X,L1,L).
 fi_expr( fi(X),['*',X|L], L) :- litteral(X). 

 %%%
 %%%  operator
 %%%   <- declares the functors of the operators 
 %%%      for unification, constraint and membership

 operator(cn,[=,c|L],L) :- !.
 operator(eq,[ '=' |L],L).
 operator('in',[in|L],L).
 operator('rel',[&,=|L],L).

 %%%
 %%% Finds all the defined functions; if they are not found, 
 %%% produces an error message.

  
 get_all_funcs(F) :- bagof(Fu,func(Fu),F),!.
 get_all_funcs(_) :- msg(1),fail.

  
 funcset([A],[A,')'|L], L) :- litteral(A), !.
 funcset([F1|Fs], [F1,'|'|L1],L) :-
      litteral(F1),
      funcset(Fs,L1,L).

 %%%
 %%% subcat
 %%%   <- Parses subcategarization expressions, in the form  
 %%%      <F1 ... Fn>, where Fi is a subcategorizable function. 
 %%%      Each of these functions may be preceded by the optionality
 %%%      operator '&'.
 
 subcat([],['>'|L],L) :- !.
 subcat([opt(F1)|Fs],['&',F1|L1],L) :- !,
          litteral(F1),
          subcat(Fs,L1,L).	     
 subcat([F1|Fs],[F1|L1],L ) :-
          litteral(F1),
          subcat(Fs,L1,L).	      
	  
 %%%
 %%%  prolog_atom(+A)
 %%%    <-- A is a primitive of prolog.
 %%%        This predicate is used to prevent the sequence [M,A],  
 %%%        --where M is a metavariable-- to eb considered as a path.

 prolog_atom(in). 
 prolog_atom('/').
 prolog_atom('|').
 prolog_atom(')').

 %%%
 %%% path_value
 %%%   <- Describes the types of possible equation values :
 %%%         (i)   a path;
 %%%         (ii)  a metavariable
 %%%         (iii) a head with subcategorization
 %%%         (iv)  a fucntion call
 %%%         (v)   a category symbol
  
 path_value(X,L1,L) :- path(X,L1,L),!.
 path_value(X,L1,L) :- metavariable(X,L1,L),!. 
 path_value('$SUB'(F), ['<'|L1],L) :- !,
          subcat(F,L1,L).
 path_value('$INDEX',['{','INDEX','}'|L],L) :- !.  % Treats INDEX values
 path_value('Plus',['+'|L],L) :- !. 
 path_value('Minus',['-'|L],L) :- !.
 path_value(X,[X|L],L) :- atomic(X).


 %%%
 %%% parse_body( -H, +T1, -T2)
 %%%    <- H is a list of daughters (in prolog form) parsed in
 %%%       the token list T1. T2 is the difference list of T1.
 
 parse_body([D|DTRS],L1,L) :-
                parse_dtr(D,L1,L2),
                parse_dtrs(DTRS,L2,L).

 parse_dtrs([],[],[]) :- !.
 parse_dtrs(X,L1,L) :-  parse_body(X,L1,L).
 
 %%%
 %%%  parse_dtr
 %%%   <- Recognizes a daughter, with the form:
 %%%       &H  : optional;
 %%%       *H  : multiple;
 %%%       (H1 / ... / Hn ) disjunction;
 %%%       H-Eqs o H (with no explicit equation)
 
 parse_dtr( opt(X), ['&'|L1],L ) :- !, parse_dtr(X,L1,L).

 parse_dtr( mult(X), ['*'|L1],L) :- !, parse_dtr(X,L1,L).

 parse_dtr( Ds, ['('|L1],L) :- !, parse_disj_dtr(Ds,L1,[')'|L]).

 parse_dtr(C=Eq, [C,':'|L1],L) :- !,parse_equations(Eq,L1,L).

 parse_dtr(C=eq('U','D'),[C|L],L) :- litteral(C).

  %%%
  %%% parse_disj_dtr
  %%%   <- Recognizes a sequence of disjunctive daughters, in the format
  %%%      D1 | D2 | ... | Dn

 parse_disj_dtr( (D;Ds), L1,L ) :- 
     parse_dtr(D,L1,['|'|L2]),!,
     parse_disj_dtr(Ds,L2,L).
 parse_disj_dtr(X,L1,L2) :- parse_dtr(X,L1,L2).


 /******************************************************************
     A LFG to BUP Compiler
 *******************************************************************/

 %%%
 %%% PROLOG_TERM(+T)

 prolog_term( macro(N,Eqs) ) :- !, 
        ( recorded(file,lex,_)
	    -> Tp=lex, F=template
            ;  Tp=ps, F=proc ),
        eval( feq(Tp,Eqs,U,D),Body),
        Head =.. [F,N,U,D],
        assertz((Head:-Body)).

 prolog_term( word(LEX,CAT,Type,EQS) ) :-
        eval( feq(lex,EQS,REP,REP),BODY),!,
        call(BODY),
        d_info(LEX),
        inherit_from_type(Type,REP,REP1),
        assertz(word(LEX,CAT,REP1)). 

 prolog_term( class(C,ST,EQS) ) :-
        eval( feq(lex,EQS,REP,REP),BODY),!,
        call(BODY),
        assert( class(C,ST,REP)).  

 prolog_term( rule(C,DTRS) ) :- !,
        compile_PS(C-DTRS),
        d_info(C-DTRS).

 prolog_term( func([H|F]) ) :- !,
	assert('$HEAD'(H)),
	hy_assert( fnc, F).

 prolog_term( lang(L) ) :- !, 
	flag(language,_,L).

 prolog_term( axiom(L) ) :- !, 
        flag(axiom,_,L).

 prolog_term( '$SUB'(T,Ts) ) :- !, 
	hy_assert(st(T),Ts).

 prolog_term( for(C,Eqs) ) :- !, 
        eval( feq(fcr,Eqs,U,_D),Body),
	flag(c2,M,M+1),
        assertz( ( deflt(C-M,U) :- (Body ->!;true)) ).

 % contr, colloc, text
 prolog_term(X) :- assertz(X).
 

 %%%
 %%% inherit_from_type( +T, +R1, -R2)
 %%%  <- All default rules defined by the type T are added to 
 %%%     the feature list R1, and returns R2.
 
 inherit_from_type(T,Rep0,Rep) :-
         clause( class(T,ST,Fs), true),!,
         do_defaults(Fs,Rep0,Rep1),
         inherit_from_type(ST,Rep1,Rep).

 inherit_from_type(_,X,X).

  do_defaults(V,R,R) :- var(V), !.
  % do_defaults([V],R,R) :- var(V),!.
  do_defaults([F/V1|Fs],R0,[F/V3|R]) :-
         pick(F/V2,R0,R1),!,
         defaults_in_value(V1,V2,V3),
         do_defaults(Fs,R1,R).
  do_defaults([F|Fs],R0,[F|R]) :- do_defaults(Fs,R0,R).

  defaults_in_value(V1,V,V) :-    
        ( var(V1) ; var(V) ),!,
        V1=V.

  defaults_in_value(V1,V2,V3) :- 
        lst(V1),!,
        do_defaults(V1,V2,V3).
  defaults_in_value(_,X,X).

 %----------------------------------------------------------------------

 assertlink(F,B) :- 
	flag(lnk,1,1),!,
        arg(1,B,ARG),
        nonvar(ARG),
        ARG=link(C,_),
        assertc( link(F,C)). 
 assertlink(_,_).
	  
 %----------------------------------------------------------------------
 %   HY_ASSERT( +SuperType, +Set)
 %    Defines ISA relations for all the elements of the Set
 %    with regard to the SuperType
 
 hy_assert(_,[]) :- !.
 hy_assert(st(X), [A|B] ):- assert( isa(A,X)), hy_assert(st(X),B).
 hy_assert(fnc, [A|B] ):- assert( func(A) ), hy_assert(fnc,B).

 %----------------------------------------------------------------------

 assertc( link(A,A) ) :- !.
 assertc(F) :- clause( F,true), !.
 assertc((H:-B)) :- clause(H,B),!.
 assertc(F) :- assertz( F).

 %----------------------------------------------------------------------

 compile_PS(X) :- 
         exp_term(X, (H:-B1)),
         eval(B1,B),!, 
         H = left(F,_,_,_,_,_),	
         assertlink(F,B), 
	 % write((H:-B)),nl,
         assert( (H:-B) ).

  %%%
  %%% EXP_TERM
  
  %%% For rules of the type A --> (B) C...  
 
  exp_term( A-[opt(B)|C], T) :-
        !,
        compile_PS(A-[B|C]),   
        exp_term( A-C, T).

  %%% For rules of the type A --> *B  
 
  exp_term( A-[mult(B)], T) :-
        !,
        exp_term( A-[B,mult(B)], T).

  %%% For rules of the type A --> *B C ... 
 
 exp_term( A - [mult(B)|C], T) :-
        !,
        compile_PS(A-C),
        exp_term(A-[B,mult(B)|C], T).

  %%% For rules of the type A --> { B | C } C ...  

 exp_term( A-[(ALT1;ALT2)|C], T) :-
        !,
        compile_PS( A-[ALT1|C] ),
        exp_term( A-[ALT2|C],T ).

  % For rules of the type  A --> {B|C} 
 
 exp_term( A-[(ALT1;ALT2)], T) :-
        !,
        compile_PS( A-[ALT1]),
        exp_term( A-ALT2,T ).
	
  % For rules of the type A --> B C ... 

 exp_term( A-[B=E1|C],
           ( left(B,GOAL,REP,REPF,L0,L) :- 
               ( link(A,GOAL), 
                 feq(ps,E1,M,REP), BU, 
                 add_defaults(A,M), 
                 left(A,GOAL,M,REPF,Lx,L)) )) :- !,
        compile_rest_dtr(M,C,BU,L0,Lx).
 
 %  For unary rules  A --> B 
  
 exp_term( A-[B=E1],
           ( left(B,GOAL,REP,REPF,L0,L) :- ( 
               link(A,GOAL),
               feq(ps,E1,M,REP),
               add_defaults(A,M),
               left(A,GOAL,M,REPF,L0,L)))) :- !.

 %--------------------------------------------------------------------------
 %  compile_rest_dtr(M,Tk,Tp,L1,L2)
 %    <-- compiles the inner section of the body of a PS rule, 
 %        being L1 and L2 difference lists of tokens,
 %        M the variable of the mother, 

 % For sequences of nodes	
   
 compile_rest_dtr( _M, [], true, L, L ) :- !.
	     
 compile_rest_dtr( M, [A|B], (X,Y), L1,Lx ) :-
        !,
        compile_rest_dtr(M,A,X,L1,L2),
        compile_rest_dtr(M,B,Y,L2,Lx).

 % For optional nodes 

 compile_rest_dtr( M, opt(A), ( ROP ; L1=L2), L1,L2 ) :- 
        compile_rest_dtr(M,A,ROP,L1,L2),!. 
       
 % For disjunctions of nodes 

 compile_rest_dtr( M, (ALT1;ALT2), (R1;R2),  L1,Lx ) :-	  
        compile_rest_dtr(M, ALT1, R1, L1,Lx),
        compile_rest_dtr(M, ALT2, R2, L1,Lx),!.

 % For multiple nodes

 compile_rest_dtr( M, mult(A=EF), mult(Num,A,M,L1,Lx), L1,Lx ) :-   
        compil_Kleene(Num,A,EF,M,L1,Lx),!.

 % Standard nodes

 compile_rest_dtr( M, A=EF, (goal(A,G,L1,L2), 
                 unify(REP,G),
                 feq(ps,EF,M,REP)), L1,L2 ).


 %---------------------------------------------------------------------------
 % COMPILE_KLEENE/6
 %   <-  Every time a daughter with *H form appears in a rule, 
 %       the compiler asserts two rules with functor 'mult'(H), 
 %       and the following definition:
 %  
 %          mult(H) --> goal(H),
 %                apply the corresponding feature-structure
 %                mult(H).
 %          mult(H) --> []
 

 compil_Kleene(Num,N,EF,M,L1,Lx) :-
       eval( ( goal(N,G,L1,L2),	
                   unify(REP,G),
                   feq(ps,EF,M,REP), 
                   mult(Num,N,M,L2,Lx) ), Body),
       assertc( ( mult(N,M,L1,Lx) :- Body) ),
       assertc_Kleene( N, Num, ( mult(Num,N,M,L1,Lx) :- Body) ),
       assertc( mult(Num,N,_,X,X) ).	   


 %-----------------------------------------------------------------------------
 % Code for partial evaluation, from Pereira y Shieber, p. 172-175

 eval( (L,Rest), Exp) :- !,
      eval(L,L_Exp),
      eval(Rest,R_Exp),
      conjoin(L_Exp,R_Exp,Exp).

 eval( (A -> B ; C), (NA -> NB ; NC) ) :-
      !,
      eval(A,NA),
      eval(B,NB),
      eval(C,NC).

 eval( (A->B), (NA->NB;true) ) :-
      !,
      eval(A,NA),
      eval(B,NB).

 eval( (L;Rest), (ExpL;ExpR)) :- !,
      eval(L,ExpL),
      eval(Rest,ExpR).

 eval(L,E) :- aux(L,B),!, eval(B,E).
 eval(L,L).

 %---------------------------------------------------------------------------

 conjoin((A,B),C,ABC) :- !,
     conjoin(B,C,BC),
     conjoin(A,BC,ABC).

 conjoin(true,A,A) :- !.
 conjoin(A,true,A) :- !.

 conjoin(A,C,(A,C)).

 /*********************************************
  **            Auxiliaries                   **
  *********************************************/

 aux(template(A,B,C), Body) :- clause( template(A,B,C), Body).

 aux( denotes(Var,Var),true) :- var(Var).
 aux( denotes(Atom,Atom),true) :- atomic(Atom).
 aux( denotes(top(U), top(U)),true). 
 aux( denotes(c(X,_),Y),true) :-  var(Y) -> Y= c(X,_) ; X=Y.
 aux( denotes(u(X,Y), u(X,Y)),true).
 aux( denotes( r(X,Y), r(X,Y)),true).
 aux( denotes(X,X),true) :- X=_P-{_}.
 aux( denotes(L,L),true) :- lst(L).
 aux( denotes(Dag/Path,Value), val(Dag,Path,Value,_)).
 
 aux( funcs(F),true) :- !,get_all_funcs(F). 

 %%% FEQ/4 processes constraints

 aux( feq(_,[],_,_), true).
 aux( feq(Type,[A|B],Ru,Rd), ( feq(Type,A,Ru,Rd), feq(Type,B,Ru,Rd))).

 aux( feq(Type, (A;B),Ru,Rd), (feq(Type,A,Ru,Rd); feq(Type,B,Ru,Rd))).

 %% U=D

 aux( feq( _, eq('U','D'), X, X), true ).

 % For functional uncertainty. 
 % Not the best implementation! :-( 
 %% ex. U/*sc/subj=D/focus
     
 aux( feq( _Type, eq(MV/(fi(FI)/AF),M/Path),U,D), 
      ( path(M,M1,U-D), path(MV,CL,U-D), evalattr(AF,FF,U-D), 
        val(M1,Path, Mov,_),
        long_dist_dep(CL,FI,FF,Mov) )).	

%% ex. U/*sc/subj=D

 aux( feq( _Type, eq(MV/(fi(FI)/AF),M),U,D), 
      ( path(M,M1,U-D), path(MV,CL,U-D), evalattr(AF,FF,U-D), 
        long_dist_dep(CL,FI,FF,M1) )). 

 %% D/index={INDEX}

 aux( feq( _Type, eq(M/Path,'$INDEX'),U,D), 
     ( path(M,M1,U-D), evalattr(Path,Path1,U-D), flag(c0,ID,ID+1),
       eq(M1/Path1,ID))).

 %% U/subcat = <arg1>

 aux( feq( _Type, eq('U'/subcat,'$SUB'(X)),U,_D), 
     ( funcs(F),frame(U,F-F1,X),not_frame(U,F1) ) ).

 % Negation NOT D/case=?

 aux( feq( _Type,not(eq(M/C,'?')),U,D), 
     ( path(M,M1,U-D),path(C,_C1,U-D),not( consistent(C,_,M1)))) :-!. 

/* aux( feq(Type, not(eq(M/C,'?')),U,D), 
     ( path(M,M1,U-D), evalattr(C,C1,U-D), eq(M1/C1,u))) :-!. */

%% Negation NOT D/case=ac

 aux( feq( _Type, not(eq(M/C,V)),U,D), 
     ( path(M,M1,U-D),path(C,C1,U-D),path(V,W,U-D),eq(M1/C1,u(_,W) ))).

 %% equations for unification
 %% U/case=?

 aux( feq( _Type, eq(M/Path,'?'),U,D), 
     ( path(M,M1,U-D), evalattr(Path,Path1,U-D), eq(M1/Path1,_))) :-!. 

 %% U/case = D/case

 aux( feq( _Type, eq(M1/Path,M2/R),U,D), 
     ( path(M1,M3,U-D), evalattr(Path,Path1,U-D),
       path(M2,M4,U-D), evalattr(R,Int,U-D),  
       eq(M3/Path1,M4/Int))). 

 %% U/case=dat

 aux( feq( _Type, eq(M/Path,R),U,D), 
     ( path(M,M1,U-D), path(R,Int,U-D), evalattr(Path,Path1,U-D), 
       eq(M1/Path1,Int))). 

 %% D/sem in U/sem/restr (???)

 aux( feq( _Type, in(M/At1,N/At2),U,D), 
     ( path(M,M1,U-D), path(N,N1,U-D), 
       val(M1,At1,B1,_), val(N1, At2, {[B1|_]}, _))).

 %% D in U/adj

 aux( feq( _Type, in(M,N/At),U,D), 
     ( path(M,M1,U-D), path(N,N1,U-D), val(N1, At, {[M1|_]}, _))).

 %% constraints of existence
 %% NOT D/case =c ac

 aux( feq( _Type, not(cn(M/C,V)),U,D),
     ( path(M,M1,U-D),path(C,C1,U-D),path(V,W,U-D), \+ consistent(C1,W,M1) )).

 %% D/case = ac ; also tp be used in general principles (fcr)

 aux( feq(Type, cn(MV/C,V),U,D), 
     ( path(MV,M1,U-D),path(C,C1,U-D),path(V,W,U-D), consistent(C1,W,M1) )) :-
                (MV='D' ; Type=fcr).

 %% U/case =c ac (constraints for U in lexical and phrasal rules)

  aux( feq(Type, cn('U'/C,V),U,D),          %   'U' era M
      ( path(C,C1,U-D),path(V,W,U-D),eq(U/C1,c(W,_)) )) :-
        (Type=ps ; Type=lex).

  % relaxed unification: D/sem &= comest

  aux( feq( _Type, rel(M/C,V),U,D), 
      ( path(M,M1,U-D),path(C,C1,U-D),path(V,W,U-D),eq(M1/C1,r(_,W)) )).

 % Macros
 % We could distinguished here between leaving partial evaluation
 % of the templaets for the acse of words (Type=lex), or calling a
 % procedure for the grammatical case (Type = ps or fcr).
 
 aux( feq(lex,macro(X),U,D), template(X,U,D) ).
 aux( feq(Type,macro(X),U,D), proc(X,U,D) ) :- not(Type=lex). 

 % IF a THEN b

 aux( feq(Type, if(A,B,C),Ru,Rd), 
      (	 feq(Type,A,Ru,Rd) -> feq(Type,B,Ru,Rd); feq(Type,C,Ru,Rd))).

 aux( feq( _Type,true,_,_), true).
 
 aux( path(X,X,_), true) :- var(X).
 aux( path(T,TL,_), true) :- clause(isa(T,_),true), typelist(T,_,TL).
 aux( path('D',D,_U-D), true).
 aux( path('U',U,U-_D), true).
 aux( path(X,X,_), true) :- atomic(X).
 aux( path(M/P,M1/P,MetaVars), path(M,M1,MetaVars)).

 aux( eq(X,Y), ( denotes(X,Z), denotes(Y,Z))).

 /*---------------------------------------------------------------------
    evalattr( +C1, -C2, +Mv )
     <- For a path of attributes C1, it evaluates the functions
       that might appear. Otherwise, it remains unchanged. Mv gets 
       the values of the metavariables.
	
        Examples:
	
        evalattr( case, X, Y)  : 
                X = case  
                Y=true
		
        evalattr( conc/num, X, Y )
                X=conc/num
                Y=true
		
        evalattr( ev(D/theta), X, Y)
                X=Val
                Y=( member(theta/Val,D), Val\=u )
		
  ---------------------------------------------------------------------*/

 aux( evalattr(V,V,_MV), true) :- var(V),!.  
 aux( evalattr(X,X,_MV), true) :- atomic(X),!.  

 aux( evalattr(fi(L),fi(L),_MV), true) :- !.
 aux( evalattr(set(L),D,_MV), true) :- !, list_to_disj(L,D).
 
 aux( evalattr(ev(V1/X),T,MV),				 
      (path(V1,V2,MV),val(V2,X,T,_), T \= u ) ) :- !.

 aux( evalattr(V1/Vs, X1/Xs, MV), 
      (evalattr(V1,X1,MV), evalattr(Vs,Xs,MV)) ).   
 
 %%% conviene comprobar que la clausula de EVALATTR --para tratar
 %%% caminos A/B-- funciona adecuadamente. La idea es que se pueda tratar
 %%% cosas como U/subj/{D/quant}=D/val 
     
 aux( frame(_U,F-F,[]), true).
 aux( frame(U,F1-F,[A|B]), ( frame(U,F1-F2,A), frame(U,F2-F,B) ) ).
% aux( frame(U,F1-F2,opt(A)), ( val(U,A,_,_), delete(A,F1,F2) )). Antes
 aux( frame(U,F1-F2,opt(A)), ( val(U,A,[status/opt|_],_), delete(A,F1,F2) )).
 aux( frame(U,F1-F2,A), ( val(U,A,[status/obl|_],_), delete(A,F1,F2) )).
       
 aux( not_frame(_U,[]), true).
 aux( not_frame(U,[A|B]), ( val(U,A,u,_), not_frame(U,B))).

 aux( add_defaults(CAT,DAG), apply_FCR(L,DAG) ) :-  
              findall(CAT-N,clause(gfu:deflt(CAT-N,_),_),L).
 aux( add_defaults(_CAT,_DAG), true).

 aux( apply_FCR([],_), true).
 aux( apply_FCR([FCR|Rest],DAG), ( deflt(FCR,DAG), apply_FCR(Rest,DAG) ) ). 
			     
 aux( delete(X,[X|Y],Y), true).
 aux( delete(X,[A|B],[A|C]), delete(X,B,C)). 
 
 % TYPELIST

 typelist(top,L, top(L) ) :- !.

 typelist(X,_,X) :- var(X),!.

 typelist(X,L0,L) :-				
        clause(isa(X,ST),true),!,
        maketerm(X,L0,TL),
        typelist(ST,TL,L).

 typelist(_X,_L0,_L) :- msg(23),fail. % msg 23 not declared!

 % MAKETERM

 maketerm(Term,_,Term) :-  not( clause(isa(_,Term),true) ),!.

 maketerm(Ft,SubTerm,Term) :- 
        functor(Term,Ft,1),
        arg(1,Term,SubTerm).


 %%%----------------------------------------------------------------------------
 %%%   d_info( +O)
 %%%     <- Displays the compiled object O, for user information

 d_info(M-_) :- !, write(M),tab(1).
 d_info(X) :- write(X), tab(1).

 
 %-----------------------------------------------------------------------------
 %  assertc_Kleene( +Cat, -Num, +Cl)
 %    <- Before asserting a clause Cl, is existence is checked out. 
 %       Otherwise, the counter is increased, in order to ensure the
 %       compilation of other versions of *Cat that could occur in 
 %       different rules.

 assertc_Kleene(_C,_Num,(H:-B)) :- clause(H,B),!.
 assertc_Kleene(C,Num,Cl) :- new_counter(C,Num), assert(Cl).


/*=================================================================

   Section: BUP.PL

      Uses the bottom-up parser by Matsumoto, Tanaka, Hirakawa, 
      Miyoshi and Yasukawa (1983)

 =================================================================*/


 %-------------------------------------------------------------------------
 % elapsed( +Ent, +Number of words)
   
 elapsed(TMP,Ps) :-  
      nl,
      parse_msg(1), 
      write(TMP),
      parse_msg(2),
      Psec is TMP/Ps,
      write(' (') ,write(Psec),
      parse_msg(10).

 %-------------------------------------------------------------------------
		  
 parse(G,Ts) :-	
      tidy_data,
      length(Ts,Words),
      get_time(T1),
      preparse(Ts,T),
      goal(G,DAG,T,[]),
      simplify(DAG,DAG1), 
      get_time(T2),
      TMP is (T2-T1)/100,
      (flag(det,1,1) -> !; true),   % deterministic cut
      recordz(matrix,DAG1,_),
      elapsed(TMP,Words),nl,
      flag(c2,M,M+1).		    % incrases the parses counter 
			     
	       
  parse(_,_) :- 
      parse_msg(3), nl.


 %-------------------------------------------------------------------------
 % Preparse

  preparse(A,C) :-
      decontr(A,B),
      decolloc(B,C),
      lookup(C).


 %-------------------------------------------------------------------------

  decontr([],[]):-!.
  decontr([A|X],[B,C|Y]) :-
      contr(A,[B,C]),
      decontr(X,Y).
  decontr([X|S1],[X|S2]) :- decontr(S1,S2).


 %-------------------------------------------------------------------------

  decolloc([],[]) :- !.
  decolloc([A,B,C|S1],[D|S2]) :-
     colloc([A,B,C],D),!,
     decolloc(S1,S2).
  decolloc([A,B|S1],[C|S2]) :-
     colloc([A,B],C),!,
     decolloc(S1,S2).
  decolloc([X|S1],[X|S2]) :- decolloc(S1,S2).


 %-------------------------------------------------------------------------
 % lookup(+WordList)
 % A procedure to collect all the occurrences of the words in WordList. 

 lookup([]):-!.

 lookup([A|B]) :- lookup1(A),lookup(B).

  
 lookup1(A) :-
	word(A,C,S),
	recordz( oc, lex(A,C,S), _Ref),
	fail.

 lookup1(A) :- 
      recorded(oc, lex(A,_,_), _Ref) 
         -> true
	 ;  (parse_msg(1,A), !, fail).


 %--------------------------------------------------------------------
 % GOAL
 %   Extended version (Gazdar y Mellish pp. 416)
 %--------------------------------------------------------------------

 goal(_,_,[],_) :- !, fail.

 goal(GOAL,REP,L0,L) :-			     
       recorded( wfg, goal(GOAL,_,L0,_), _ ),!,
       recorded( wfg, goal(GOAL,REP,L0,L), _ ) ;       
       recorded( fg, goal(GOAL,L0), _ ),!, fail.

 goal(GOAL,REP,[P1|Ps],P) :-
      recorded(oc,lex(P1,C,REPLEX),_),
      link(C,GOAL),
      trace1(5,GOAL,[P1|Ps]),	       
      left(C,GOAL,REPLEX,REP,Ps,P),
      recordz( wfg, goal(GOAL,REP,[P1|Ps],P), _ ),
      trace2(GOAL,[P1|Ps],P,REP).
      
 goal(G,_REP,L0,_L) :-
      ( recorded(wfg,goal(G,_,L0,_),_),
        parse_msg(3,G) ;
        ( trace1(8,G,L0),
          recordz( fg, goal(G,L0),_))),!,fail.


tidy_data :- 
	eraseall( wfg ),     % deletes the well-formed structure table
	eraseall( fg ),      % deleets the failed structures table 
	eraseall(oc),        % deletes the word table
	eraseall(matrix),    % deletes the results of the last parse
	flag(c0,_,1).	% sets the symbol counter to one 


/*=================================================================

      BUP Auxiliary predicates 

 =================================================================*/

 %------------------------------------------------------------------------
 % Long-distance Treatment 
 %  LONG_DIST_DEP/4 ( ?DF, +list of intermediate functions, 
 %                    +list of final functions, +value).
 %		       
 %   ex:  { u: *comp .. FG } would compile to 
 %        long_dist_dep(X,[comp],[subj,obj,obj2,...],V)

 long_dist_dep(V,_,_FF,_Val) :- 
        var(V),parse_msg(7),!,fail.

  % Tries to attach in the local tree

 long_dist_dep(Ef,_,FF,Val) :- attach(Ef,FF,Val).

  % If this fail, looks for the intermediate function (if any), 
  % to be applied recursively over its sub-Ef

  long_dist_dep(Ef,Fs,FF,Val) :-
        atomic(Fs),!,
        val(Ef,Fs,SubEf,_),
        long_dist_dep(SubEf,Fs,FF,Val).
 

 %------------------------------------------------------------------------
 %   ATTACH( +EF, +FF, +V)
 %     <- Applies the list of final functions FF recursively on the local
 %        domain of the functional structure EF, hasta encontrar
 %        una que unifique con el Valor V
 %------------------------------------------------------------------------
 
 attach(Ef,FF,Val) :- atomic(FF), !, unify(Ef,[FF/Val|_]).
 attach(Ef,(FF1;FFs),Val) :- 
        attach(Ef,FF1,Val) ;
        attach(Ef,FFs,Val).


 % UTILITIES

 %-----------------------------------------------------------------------
 % TRACER  
 %  A very redimentary debugger

  trace1(N_msg,GOAL,L) :-
      flag(trac,1,1),!,
      parse_msg(N_msg),	
      write(GOAL), 
      write(' (n/q) = '), 
      writelist(L), 
      trace_loop(nil).

  trace1(_,_,_).

 %-------------------------------------------------------------------------

  trace2(GOAL,L1,L2,REP) :-
      flag(trac,1,1),!,
      prefix(L,L1,L2),
      parse_msg(6), 
      write(GOAL), 
      write(' (n/q/v) = '), 
      writelist(L), 
      trace_loop(REP).

 trace2(_,_,_,_).

 %-------------------------------------------------------------------------

  trace3(X) :-  
	flag(trac,1,1),
	parse_msg(2,X).
 

 %--------------------------------------------------------------------------
 % trace_loop( +Obj)
 % <- Runs a user interaction loop. The user can inspect the object Obj,
 %    either in prolog form or in dependency form.
 %--------------------------------------------------------------------------

 trace_loop(OBJ) :-
    get_single_char(ASC), % in ARITY keyb(ASC,_P),
    command_key(ASC,C),nl,
    exec_trace(C,OBJ),!.
	  
 trace_loop(_).


 command_key(110,traceoff).
 command_key(113,traceoff).
 command_key(118,seefd).
 command_key(10,c).    % 10 = carriage return in UNIX

 %-------------------------------------------------------------------------

 exec_trace(traceoff,_) :- !, flag(trac,_,0).

 % To see the FD
 exec_trace(seefd,nil) :- !, nl, parse_msg(4), nl, fail.
 exec_trace(seefd,OBJ) :- !, nl, write(OBJ), nl, fail.
 exec_trace(c,_).
 

/*=========================================================================
    Section UNIFDAG

    Utilities de:
         unification
         simplification of FDs
         pretty-printing
	 
 =========================================================================*/

    
 %------------------------------------------------------------------------
 % UNIFY (according to Gazdar y Mellish)  

 unify(Dag,Dag) :- !.
 
 unify(X,c(X,t)) :- !. 
 unify(c(X,t),X) :- !. 

 %% treatment of relaxed constraints:

 unify(A,r(X,B)) :- !,
      var(X)
       -> (unify(A,B) ; X=A)
       ;  unify(A,X). 

 unify(r(X,B),A) :- !,
       var(X)
        -> (unify(B,A), X=B ; true)
        ;  unify(X,A).

 %%
 %%  Negation
 %%  Note that this definition of negation will cause trouble if there
 %%  are variable values in teh features (its implications are still
 %%  doubtful)
  
 unify( X, u(Y,X) ) :- var(Y),!,fail. 
 unify( u(Y,X), X ) :- var(Y),!,fail.

 unify(X, u(X,_)) :- !.
 unify( u(X,_), X) :- !. 
 
 unify({X},{Y}) :- !,append_i(X,Y). 
 
 unify([Path/Value|Dags1], Dag) :-
        val(Dag,Path,Value,Dags2),
        unify(Dags1,Dags2).

 %-----------------------------------------------------------------------
 %  VAL/4 (?FD, +Path, ?Value, -Tail)
 %   Determines the value V of the path in the feature description FD

 val(_, X,_,_) :- var(X), !, fail.

 % The next clause handles set-features. When a feature C=V is to be
 % unified with a set X, it is unified recursively against all the members 
 % of the set.

 val({X},C,V,_T) :- 
        lst(X),	
        unify_all(X,C,V).

 val(Ef1,Feature/Path,Value,Rest) :- !,
        val(Ef1,Feature,Ef2,Rest),
        val(Ef2,Path,Value,_).

 % This treatment of disjunction could be improved by applying 
 % partial evaluation of VAL. But this is rather complex, since 
 % the first argument of VAL may en a variable, and the second
 % argument may be a variable (albeit bound) in compile time.

 val(Ef1,(A1;A2),Value,Rest) :- !,
        ( val(Ef1,A1,Value,Rest) ;
          val(Ef1,A2,Value,Rest) ).

 val([Feature/Value1|Rest],Feature,Value2,Rest) :-
        !, 
        unify(Value1,Value2).
	
 val([Dag|Rest1],Rasgo,Value,[Dag|Rest2]) :-
        val(Rest1,Rasgo,Value,Rest2).


 % UNIFY_ALL(M,A,V) unifies the feature A=V against all the feature-matrices
 % in the list M (called by VAL)

 unify_all(X,_,_) :- var(X),!.
 unify_all([F|Fs],C,V) :-
        val(F,C,V,_),
        unify_all(Fs,C,V).


 %---------------------------------------------------------------------------
 %  CONSISTENT(+Path, +Value, +Feature-Structure) checks that the 
 %  Feature Structure includes a value for the specified Path. Note this 
 %  is not a constructive predicate, and can be used only in mode (+,+,+)

 consistent(R/Path,Val,EF) :- !,
        dmember(R/Vi,EF),
        consistent(Path,Val,Vi). 

 consistent(X,V,L) :- member(X/V,L),!.
 consistent(X,V,_) :- trace3(X/V),!,fail.


 % APPEND for incomplete lists (i.e. ended in a variable)
 
 append_i(X,X) :- !.
 append_i([R1|A],[R2|B]) :- 
   append_i([R2|C],A),
   append_i(B,[R1|C]).
 
 % a determinist member/2
 
 dmember(_,Var) :- var(Var), !, fail.
 dmember(X,[X|_]) :-!.
 dmember(X,[_|Y]) :- dmember(X,Y).


 %-----------------------------------------------------------------------
 %  SIMPLIFY( +FD,-List) 
 %    <- Transforms the feature description FD in a feature list. 
 %       Deletes unusual values such as c(v,t), u(v,n) and the 
 %       anonymous tail of the lists.

  
 simplify(Var,[]) :- var(Var),!.
 
 % deletes 'u'-valued features

 simplify([_A/u|Rest0],Rest) :-  
        !, simplify(Rest0,Rest).

 % deletes feature 'status'

 simplify([status/_T|Rest0],Rest) :-       
        !, simplify(Rest0,Rest).

 % deletes negative features
    
 simplify([A/u(X,_)|Rest0],[A=X1|Rest]) :-   
        !,
        simplify_val(X,X1),
        simplify(Rest0,Rest).
		       
 simplify([A/c(X,F)|Rest0], [A=X1|Rest]) :-
        !,F==t,
        simplify_val(X,X1),
        simplify(Rest0,Rest).


 %% Simplifies relaxed features. These features have the form r(FD,C) 
 %% where FD is a feature description, and C is the control structure.
 %% If C (the canonical structure, v.gr. that the object of 'drink' is LIQUID)
 %% is different from the real FD, then a metaphorical interpretation is assumed.

 simplify([A/r(X,C)|Rest0],[A=X1|Rest]) :- 
        X=C,!,
        simplify_val(X,X1),
        simplify(Rest0,Rest).

 simplify([A/r(_X,C)|Rest0],[A=X1,'RELAX'= A|Rest]) :- !,
        simplify_val(C,X1),
        simplify(Rest0,Rest).

 simplify([A/r(X,C)|Rest0],[A=X1|Rest]) :- !,
        X=C,!,
        simplify_val(X,X1),
        simplify(Rest0,Rest).
      
 % for SET features
 simplify([A/{SET}|Rest0],Rest) :-
        !,
        simplify_set(A,SET,SET1),
        simplify(Rest0,Rest1),
        append(SET1,Rest1,Rest).

 simplify([A/L|Rest0],[A=L1|Rest]) :-	  
        func(A),
        '$HEAD'(H),
        dmember(H/_,L),!,
        simplify(L,L1),
        simplify(Rest0,Rest). 

 simplify([A/L|Rest0],Rest ) :-	   
        func(A),	     % If the attribute A is an obligatory function
	flag(cpl,1,1),!,     % If the completeness principle is on
        (   dmember(status/obl,L) 
	     -> parse_msg(11), fail
	      ; true ),
        simplify(Rest0,Rest). 

 % for other dependents 
  
 simplify([A/L|Rest0],[A=L1|Rest]) :-
        lst(L),!,
        simplify(L,L1),
        simplify(Rest0,Rest). 
	
 % for the rest
                  
 simplify([A/V|Rest0],[A=V1|Rest]) :-
        simplify_val(V,V1),
        simplify(Rest0,Rest).	      

 simplify_set(_Atr,Vr,[]) :- var(Vr),!.	      
 simplify_set(Atr,[A|B],[Atr=A1|C]):-
        simplify(A,A1),
        simplify_set(Atr,B,C).


 %%% Simplifies the values of the features, in particular, hierarchical features.

 simplify_val( top(TL), V ) :- lastnv(TL,V),!.
 simplify_val(V,V).

      
 % Messages of the parser

 
 parse_msg(1) :- write('Sentence parsed in ').
 parse_msg(2) :- write(' sec. ').
 parse_msg(3) :- 
        flag(axiom,Ax,Ax),
 	flag(c2,Ps,Ps),
        (   Ps=0 
	       -> write('I cannot analyze this expression as '), write(Ax)
                ; nl,write('  *** parses : '), write(Ps)
	),
        put(7).

 parse_msg(4) :- write('Wait until the rule be completed').
 parse_msg(5) :- write('      * Trying '). 
 parse_msg(6) :- write('* Completed ').	

 parse_msg(7) :-  
	flag(trac,1,1)
           -> write(' ** Wrong functional incertainty rule ** '),nl
	   ;  true.	
	   
 parse_msg(8) :- write('      ** Failed ').	
 parse_msg(9) :- write('      ** FD inconsistent ').
 parse_msg(10) :- write(' words./sec.)').

 parse_msg(11) :- 
	flag(trac,1,1)
           -> write('      ** incomplete FD ** '),nl
	   ;  true.
 

 parse_msg(1,X) :- write('I don\'t know the word '), write(X),nl.
 parse_msg(2,X) :- write('   ** Consistency failure '), write(X),nl.   
 parse_msg(3,X) :-
	flag(trac,1,1)
	   -> write('** Retrieved '), write(X), nl
	   ;  true.



