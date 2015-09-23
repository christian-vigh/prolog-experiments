/**
 * Simple PROLOG text parser
 *
 * Dimitri PISSARENKO, University of Derby in Austria
 * May 15, 2002
 *
 * Definition of the read_in predicate for converting strings
 * to PROLOG lists that can be used in connection with NLP facilities
 * of LPA PROLOG.
 *
 * This predicate was taken from:
 * David Mitchell, Inital Code for Assignment 6,
 * http://www.cs.sfu.ca/~mitchell/teaching/384/a6files/microml, March 2001
 *
 **/

/* Read in a Sentence */
r(S) :- read_in(S).
read_in([W|Ws]):- get0(C), readword(C,W,C1), restsent(W,C1,Ws).
/* note that you cannot call this with W,Ws partially instantiated */

/* Given a word and the folling character, read rest of sentence */
restsent(W,_,[]):- lastword(W), !.
restsent(_,C,[W1|Ws]):- readword(C,W1,C1),restsent(W1,C1,Ws).
 
/* Given the first character, read the rest of a word */ 
readword(C,W,C1):- single_character(C),!,name(W,[C]),get0(C1).
readword(C,W,C2):- 
	   in_word(C,NewC), !,
	   get0(C1),
	   restword(C1,Cs,C2),
	   name(W,[NewC|Cs]).
readword(_,W,C2):- get0(C1),readword(C1,W,C2).
restword(C,[NewC|Cs],C2):-
	 in_word(C,NewC), !,
	 get0(C1),
	 restword(C1,Cs,C2).
restword(C,[],C).

/* these characters end sentences */
lastword('.').
lastword('!').
lastword('?').

/* functions of ascii characters                       */
/*  we define each as being:                           */
/*     - a character which may occur in a word.        */
/*     - a single-char word.                           */
/*     - something we ingnore.                         */
/* below 32 we ignore */
%%%%in_word(39,39).        /* ' */   
/* 48-57 are numerals */
in_word(C,C):- C>47, C<58. /* 0..9 */
%%%%in_word(34,34).        /* " */
/* 97-122 are lower case characters */
in_word(C,C):- C>96, C<123.  /* a..z */
/* 65-90 are upper case characters */
in_word(C,L):- C>64, C<91, L is C+32.      /* A..Z */
%% single_character(32).  /*   */
single_character(33).  /* ! */
single_character(34).  /* " */
single_character(35).  /* # */
single_character(36).  /* $ */
single_character(37).  /* % */
single_character(38).  /* & */
single_character(39).  /* ' */
single_character(40).  /* ( */
single_character(41).  /* ) */
single_character(42).  /* * */
single_character(43).  /* + */
single_character(44).  /* , */
single_character(45).  /* - */    %%in_word(45,45).        /* - */
single_character(46).  /* . */
single_character(47).  /* / */
single_character(58).  /* : */
single_character(59).  /* ; */
single_character(60).  /* < */
single_character(61).  /* = */
single_character(62).  /* > */
single_character(63).  /* ? */
single_character(64).  /* @ */
single_character(91).  /* [ */
single_character(92).  /* \ */
single_character(93).  /* ] */
single_character(94).  /* ^ */
single_character(95).  /* _ */
single_character(96).  /* ` */
single_character(123).  /* { */
single_character(124).  /* | */
single_character(125).  /* } */
single_character(126).  /* ~ */
