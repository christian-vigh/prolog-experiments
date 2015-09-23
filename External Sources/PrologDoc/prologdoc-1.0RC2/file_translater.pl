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
  @descr This module can be used to translate a file documented
         by the <font color=red><i> PrologDoc </i></font> 
	 documentation syntax to a file where the PrologDoc documentation
         is hidden within predicates. The new file can then easily be analyzed
         to build a file profile. <br>
         The assumption of this module is that no user defined predicates
         are called xx or xxx. Thus, predicate comments will be transformed
         to xx predicates and general comments will be transormed to xxx
         predicates. <br>
	 Note that we assume that users do not use the \\@ symbol within
         documentation descriptions. 
         If the symbol is desired it should be written as \\\\@.
  @author Sara Cohen
  @author Elisheva Bonchek
  @date   19/7/99
*/

:-module(file_translater,[translate/2,read_file_list/2,parse_comment/2]).

:-use_module(ascii_symbols).

/** 
  @form read_file_list(FileName,Contents)
  @constraints 
          @ground       FileName
          @unrestricted Contents
  @descr True if Contents is the list of characters in the file FileName
*/
read_file_list(FileName,Contents):-
	see(FileName),
	get0(Letter),
	read_all(Letter,Contents),
	seen.

/*
   read_all(Letter,Letters)
      adds Letter to Letters until the end of the file is reached
*/
read_all(-1, []):- !.
read_all(Letter, [Letter|Letters]):-
	get0(NewLetter),
	read_all(NewLetter,Letters).

/*
   transform_comments(Text,InComment,Result)
       transforms Text to Result by hiding documentation 
         comments within predicates
       InComment is the boolean value of being within a comment in the
         reading of Text
*/

% end of text
transform_comments([],_InComment,[]).

% start of a general comment 
transform_comments([Slash,Star,Star,Star|Text],
	           _InComment,[X,X,X,LeftParen,Quote|NewText]):-
	slash(Slash),star(Star),!,
	left_parenthesis(LeftParen),x(X),quote(Quote),
	transform_comments(Text,true,NewText).

% start of a predicate description comment
transform_comments([Slash,Star,Star|Text],
	           _InComment,[X,X,LeftParen,Quote|NewText]):-
	slash(Slash),star(Star),!,
	left_parenthesis(LeftParen),x(X),quote(Quote),
	transform_comments(Text,true,NewText).

% end of a documentation comment
transform_comments([Star,Slash|Text],true,[Quote,RightParen,Period|NewText]):-
	star(Star),slash(Slash),!,
	right_parenthesis(RightParen),period(Period),quote(Quote),
	transform_comments(Text,false,NewText).

% any character
transform_comments([Letter|Text],InComment,[Letter|NewText]):-
	transform_comments(Text,InComment,NewText).

/**
   @form translate(InFile,OutFile)
   @constraints 
         @ground InFile
         @ground OutFile
   @descr Translates the file InFile by hiding documentation within
          predicates to produce the file OutFile
*/
translate(ReadFile,WriteFile):-
	read_file_list(ReadFile,ContentList),
	transform_comments(ContentList,false,Transformed),
	name(Content,Transformed),
	tell(WriteFile),
	write(Content),
	told.

/**
  @form parse_comment(Comment,ParameterValueList)
  @constraints
        @ground Comment
        @unrestricted ParameterValueList
  @descr True if ParameterValueList is a list of the pairs of 
         parameter names and values that are defined in the comment
         Comment. 
*/

% empty comment
parse_comment([],[]):-!.

% 'constraint' parameter name
parse_comment(Comment,[(constraints,Value)|Parameters]):-
	get_parameter_name(Comment,NameList,Rem1),
	name(constraints,NameList),!,
	get_constraint_list(Rem1,Value,Rem2),
	parse_comment(Rem2,Parameters).

% any other pair of parameter name and value
parse_comment(Comment,[(Name,Value)|Parameters]):-
	get_parameter_name(Comment,NameList,Rem1),
	get_parameter_value(Rem1,ValueList,Rem2),
	name(Name,NameList),
	name(Value,ValueList),
	parse_comment(Rem2,Parameters).

% The format parameter names	
format_parameter(X):- X == ground; X == unground; X == unrestricted.

/*
    get_constraint_list(Comment,ParameterValueList,Remainder)
       gets the format constraint parameters and values, leaving
       the rest of the comment in Remainder
*/
get_constraint_list(Comment,[(Name,Value)|Parameters],Remainder):-
	get_parameter_name(Comment,NameList,Rem1),
	name(Name,NameList),
	format_parameter(Name),!,
	get_parameter_value(Rem1,ValueList,Rem2),
	name(Value,ValueList),
	get_constraint_list(Rem2,Parameters,Remainder).

get_constraint_list(Comment,[],Comment).

/* 
    get_parameter_name(Comment,Name,Remainder)
        Name is the next parameter name in Comment and Remainder 
        is the rest of Comment
*/
get_parameter_name(Comment,Name,Remainder):-
	get_parameter_name(Comment,Name,Remainder,false).

% start of name, see an @
get_parameter_name([AtSign|Comment],Name,Remainder,false):-
	at_sign(AtSign),!,
	get_parameter_name(Comment,Name,Remainder,true).

% not start of name - see an \@
get_parameter_name([BackSlash,AtSign|Comment],Name,Remainder,false):-
	at_sign(AtSign),back_slash(BackSlash),!,
	get_parameter_name(Comment,Name,Remainder,false).

% end of name - see a ' '
get_parameter_name([Space|Comment],[],Comment,true):-
	white_space(Space).

% middle of name
get_parameter_name([Char|Comment],[Char|Name],Remainder,true):-
	get_parameter_name(Comment,Name,Remainder,true).

% before name
get_parameter_name([_Char|Comment],Name,Remainder,false):-
	get_parameter_name(Comment,Name,Remainder,false).

/* 
    get_parameter_value(Comment,Value,Remainder)
        Value is the next parameter value in Comment and Remainder 
        is the rest of Comment
*/

% end of comment
get_parameter_value([],[],[]).

% end of value - see an @
get_parameter_value([AtSign|Comment],[],[AtSign|Comment]):-
	at_sign(AtSign).

% not end of value - see an \@
get_parameter_value([BackSlash,AtSign|Comment],[AtSign|Value],Remainder):-
	at_sign(AtSign),back_slash(BackSlash),!,
	get_parameter_value(Comment,Value,Remainder).

% exchange \n with ' '
get_parameter_value([NewLine|Comment],[Space|Value],Remainder):-
	newline(NewLine),!,space(Space),
	get_parameter_value(Comment,Value,Remainder).

% any character
get_parameter_value([Char|Comment],[Char|Value],Remainder):-
	get_parameter_value(Comment,Value,Remainder).


