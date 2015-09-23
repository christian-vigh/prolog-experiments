/*
  This file is part of PrologDoc (http://prologdoc.sourceforge.net/).

  Copyright (C) 1999 by Elisheva Bonchek (le7bonch AT cs.huji.ac.il) and Sara Cohen (sarac AT ie.technion.ac.il, http://iew3.technion.ac.il/~sarac) 
  Copyright (C) 2004 by Bram Adams (bram.adams AT ugent.be)


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
   @descr In this file the ascii values of various
          useful characters are defined
   @author Elisheva Bonchek
   @author Sara Cohen
   @date 25/7/99
*/

:-module(ascii_symbols,[space/1,tab_sign/1,newline/1,star/1,slash/1,
	                back_slash/1,at_sign/1,left_parenthesis/1,
			right_parenthesis/1,x/1,quote/1,period/1,
			right_bracket/1,left_bracket/1,comma/1,
			line/1,question/1,minus/1,plus/1,white_space/1]).

% Define the ascii numbers of usefile characters

/** 
  @form space(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a space (' ')
*/
space(32).               % ' '

/** 
  @form tab_sign(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a tab sign (\t)
*/
tab_sign(9).             % \t

/** 
  @form newline(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a newline (\n)
*/
newline(10).             % \n

/** 
  @form star(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a  star (*)
*/
star(42).                % *

/** 
  @form slash(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a slash (\)
*/
slash(47).               % \->wrong?BA

/** 
  @form back_slash(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a back slash (/)
*/
back_slash(92).          % /->wrong?BA

/** 
  @form at_sign(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of an at sign (*)
*/
at_sign(64).             % @

/** 
  @form left_parenthesis(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a left parenthesis ('(')
*/
left_parenthesis(40).    % (

/** 
  @form right_parenthesis(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a right parenthesis (')')
*/
right_parenthesis(41).   % )

/** 
  @form x(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of an x (x)
*/
x(120).                  % x

/** 
  @form quote(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a quotation mark (\")
*/
quote(34).               % "

/** 
  @form period(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a period (.)
*/
period(46).              % .

/** 
  @form right_bracket(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a right bracket (])
*/
right_bracket(93).       % ]

/** 
  @form left_bracket(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a left bracket ([)
*/
left_bracket(91).        % [

/** 
  @form comma(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a comma (,)
*/
comma(44).               % ,

/** 
  @form line(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a vertical line (|)
*/
line(124).               % |

/** 
  @form question(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a question mark (?)
*/
question(63).            % ?

/** 
  @form minus(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a minus sign (-)
*/
minus(45).               % -

/** 
  @form plus(Number)
  @constraints
     @unrestricted Number
  @descr True if Number is the ascii number of a plus sign (+)
*/
plus(43).                % +

/**
  @form white_space(X)
  @constraints 
       @unrestricted X
  @descr True if X is a space, tab, or newline ascii value.
*/
white_space(X):-space(X);tab_sign(X);newline(X). % any white space
