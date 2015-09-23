/****h* Modules/ctype
 ===============================================================================
 *
 * NAME
 *	ctype - Character-classification routines.
 *
 * FILE
 *	Modules/ctype.pro
 *
 * CONTENTS
 *	Character-classification routines, such as the C library ctype.
 *
 * AUTHOR
 *	Christian Vigh, August 2005.
 *
 ===============================================================================
 ******/


:- module(ctype).

:-	export(isalnum/1).
:- 	export(isalpha/1).
:- 	export(isdigit/1).
:-	export(isxdigit/1).
:-	export(isodigit/1).
:- 	export(isbdigit/1).
:-	export(isascii/1).
:- 	export(iscntrl/1).
:-	export(islower/1).
:- 	export(isupper/1).
:- 	export(isprint/1).
:- 	export(isspace/1).

:- end_module(ctype).
 

:- body(ctype).


/****f* ctype/isalnum
 *
 * PREDICATE
 *	isalnum/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is alphanumeric (letter or digit).
 *
 * SOURCE
 */
isalnum(C) :-
	( isalpha(C) ; isdigit(C) ).
/******/

	
	
/****f* ctype/isalpha
 *
 * PREDICATE
 *	isalpha/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is alphabetic.
 *
 * SOURCE
 */
isalpha(C) :-
	( 
		( C @>= 'a', C @=< 'z' ) ;
		( C @>= 'A', C @=< 'Z' )
	 ).
/******/



/****f* ctype/isdigit
 *
 * PREDICATE
 *	isdigit/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a decimal digit.
 *
 * SOURCE
 */
isdigit(C) :-
	C @>= '0', C @=< '9'.
/******/



/****f* ctype/isxdigit
 *
 * PREDICATE
 *	isxdigit/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is an hexadecimal digit.
 *
 * SOURCE
 */
isxdigit(C) :-
	( isdigit(C) ; 
		( C @>= 'A', C @=< 'F' ) ;
		( C @>= 'a', C @=< 'f' )
	 ).
/******/


	 

/****f* ctype/isodigit
 *
 * PREDICATE
 *	isodigit/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is an octal digit.
 *
 * SOURCE
 */
isodigit(C) :-
	C @>= '0', C @=< '7'.
/******/
	
	
	

/****f* ctype/isbdigit
 *
 * PREDICATE
 *	isbdigit/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a binary digit.
 *
 * SOURCE
 */
isbdigit('1').
isbdigit('0').
/******/



/****f* ctype/isascii
 *
 * PREDICATE
 *	isascii/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is an ascii value (< 128).
 *
 * SOURCE
 */
isascii(C) :-
	atom_codes(C, X),
	X >= 0, X < 128.
/******/




/****f* ctype/iscntrl
 *
 * PREDICATE
 *	iscntrl/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a control character (0 <= C <= 1F, or C = 7F).
 *
 * SOURCE
 */
iscntrl(C) :-
	atom_codes(C, X),
	(( X >= 0, X =< 31 ) ; X == 127).
/******/




/****f* ctype/islower, isupper
 *
 * PREDICATE
 *	islower/1, isupper/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a lowercase/uppercase character.
 *
 * SOURCE
 */
islower(C) :-
	C @>= 'a', C @=< 'z'.
isupper(C) :-
	C @>= 'A', C @=< 'Z'.
/******/



/****f* ctype/isprint
 *
 * PREDICATE
 *	isprint/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a printable value (>32 and <128).
 *
 * SOURCE
 */
isprint(C) :-
	atom_codes(C, X),
	X >= 32, X < 128.
/******/



/****f* ctype/isspace
 *
 * PREDICATE
 *	isspace/1
 *
 * DESCRIPTION
 *	Succeeds if the specified character is a space (> 0x09 and < 0x0D, or 0x20).
 *
 * SOURCE
 */
isspace(C) :-
	atom_codes(C, X),
	(X >= 9, X =< 13) ; X == 32.
/******/


:- end_body(ctype).
