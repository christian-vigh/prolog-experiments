 /*=========================================================================
   File  : TOKENIZER.PL
   
    Author: Juan C. Ruiz Anton
            Universitat Jaume I, Castelló, Spain
            January 2004
               
 ==========================================================================*/

 :- module(tokenizer,[ tokenize/3 ]).

 %-----------------------------------------------------------------------
 % readln( +STR, -LF, +Tk)
 %    Produces a list of atoms (Tk) out from a string STR (in the form of 
 %    a list of ASCII numbers referred to characters. LF is the resulting
 %    character list (expectedly empty).

 tokenize([],[],[]) :- !.

 tokenize([10|X],X,[]) :- !.

 tokenize([13|X],X,[]) :- !.

 tokenize([X|L0],L1,Ts) :- negligible(X), !, tokenize(L0,L1,Ts).

 tokenize([X|L0],L,[W|Ts]) :- letter(X), !,
        readword(L0,L,[X|D],D,W,Ts).

 tokenize([X|L0],L,[N|Ts]) :- digit(X), !,
        V is X-48,
        readint(L0,L,V,N,Ts).

 tokenize([X|L0],L,[S1|Ts]) :-
        name(S1,[X]),
        tokenize(L0,L,Ts).


 %% negligible(+C)
 negligible(X) :- X=<32.	% blank and previous ASCII characters
 negligible(44).		% commas 
 negligible(45).		% hyphen 


 %% letter(+C): Succeeds if C is a letter
 letter(C) :- C>="a", C=<"z", !.
 letter(C) :- C>="A", C=<"Z", !.
 letter(39).				% apostroph (')
 letter(94).				% caret (^)

 % ASCII superior: letras con diacriticos en PC 

 letter(C) :- C>=128, C=<154,!.
 letter(C) :- C>=160, C=<167,!.
 letter(C) :- C>=224, C=<235,!.    % Greek characters

 %% digit(+P): succeeds if character P is a digit
 digit(C) :- C>="0", C=<"9".
 
  
 % readword
 readword([X|L0],L,LC,[X|D],W,Ts) :- alpha(X), !,
        readword(L0,L,LC,D,W,Ts).

 readword(L0,L,LC,[],W1,Ts) :-
        name(W1,LC),
        tokenize(L0,L,Ts).

 % readint
 readint([X|L0],L,V,N,Ts) :- digit(X), !,
        V1 is V*10+X-48,
        readint(L0,L,V1,N,Ts).

 readint(L,M,N,N,Ts) :-
        tokenize(L,M,Ts).
 
                           
 %% alpha(+C1): Succeeds if C is an alphanumeric character

 alpha(C) :- letter(C), !.
 alpha(C) :- digit(C), !.
 alpha(95).	% the underscore
