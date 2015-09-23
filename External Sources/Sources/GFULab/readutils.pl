 :- module(readutils,[readlist/2]).

 %%%
 %%% readlist(+Stream,-List)
 %%% Reads a phrase (ended in a dot) and returns a token list
 %%% (Clocksin & Mellish, p. 87)

 readlist(S,Ws) :- 
        get0(S,C),
        readword(S,C,W,C1), 
        restsent(S,W,N,C1,W2),
        cons(N,W2,Ws).
	
 %% restsent

 restsent(_,eof,eof,_,[]) :- !.
 restsent(_,'.',true,_,[]) :- !.
 restsent(S,W,W,C,Ws) :- 
        readword(S,C,W1,C1),
        restsent(S,W1,N,C1,W2),
        cons(N,W2,Ws).

 readword(_,-1,eof,0) :- !.	      % end of file (Linux)

 readword(S,37,true,C1) :- !,	      % for comments
        to_eol(S),		 
        get0(S,C1).

 readword(S,C,W,C1) :-
        singlechar(C),!,
        name(W,[C]),
        get0(S,C1).

 readword(S,C,W,C2) :-
        in_word(C),!,
        get0(S,C1),
        restword(S,C1,Cs,C2),
        name(W,[C|Cs]).

 readword(S,_C,W,C2) :-
        get0(S,C1),
        readword(S,C1,W,C2).

 restword(S,C,[C|Cs],C2) :-
        in_word(C),!,
        get0(S,C1),
        restword(S,C1,Cs,C2).
 
 restword(_,C,[],C).
 
 singlechar(C) :- C>32, C<39.	       % !"#$%&
 singlechar(C) :- C>39, C<48.	       % ()+-,-./ 
 singlechar(C) :- C>57, C<65.	       % ;:<=>?@ 
 singlechar(C) :- C>90, C<95.	       % [\]
 singlechar(C) :- C>122, C<127.	       % {|}~ 
 singlechar(168).		       % ¨
 singlechar(C) :- C>235, C<254.	       % mathematic signs

 in_word(C) :- C>96,C<123.	       % a...z 
 in_word(C) :- C>64,C<91.	       % A...Z 
 in_word(C) :- C>47,C<58.	       % 0...9 
 in_word(C) :- C>=192, C=<214,!.       % letters with diacritics 
 in_word(C) :- C>=217, C=<254,!.       % ditto
 in_word(39).			       % apostroph (')
 in_word(94).			       % caret (^)
 in_word(95).			       % underscore (_)
 
  
 to_eol(S) :-
        get0(S,C),
        ((C==10;C== -1) -> ! ; to_eol(S)).	% 10 is unix end-of-line code


 cons(true,X,X):-!.
 cons(A,B,[A|B]).	       





    
