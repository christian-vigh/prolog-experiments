/* xml.pl : Contains xml_parse/[2,3] a bi-directional XML parser written in
 * Prolog.
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 *
 * Current Release: 1.5
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml.pl,v $
 * Revision 1.2  2002-05-25 23:17:54+01  john
 * Set Current Release to 1.5.
 *
 * Revision 1.1  2002-01-31 21:04:45+00  john
 * Updated Copyright statements.
 *
 * Revision 1.0  2001-10-17 20:46:24+01  john
 * Initial revision
 *
 *
 *
 */
/*
:- module( xml,
	[
       xml2pl/2,
	pl2xml/2,
	xml_parse/2,
	xml_parse/3,
	xml_subterm/2,
	xml_pp/1
	] ).
*/

/* added by Neng-Fa Zhou */
xml2pl(Input,Output):-
    check_xml_file(Input,Input1),
    readFile(Input1,Codes),
    xml_parse(Codes, Document),
    tell(Output),
    xml_pp(Document),
    told.

xml2pl(Input):-
    check_xml_file(Input,Input1),
    atom_codes(Input1,Str1),
    (append(MainStr,".xml",Str1)->true;MainStr=Str1),
    append(MainStr,".pl",OutputStr),
    atom_codes(Output,OutputStr),
    xml2pl(Input1,Output).

$xml2pl([Input,Output]):-!,
    xml2pl(Input,Document),
    tell(Output),
    xml_pp(Document),
    told.
$xml2pl(_):-
    write(user,'Usage: pl2xml InFile OutFile'),nl(user).

%%%%
pl2xml(Document,Codes):-
     xml_parse(Codes, Document).

$pl2xml([Input,Output]):-!,
    (exists(Input)->
     see(Input),
     read(Document),
     pl2xml(Document,Codes),
     tell(Output),
     write_codes(Codes),
     told;
     handle_exception(file_not_exist,Input)).
$pl2xml(_):-
    write(user,'Usage: pl2xml InFile OutFile'),nl(user).

write_codes([]).
write_codes([Code|Codes]):-
    char_code(Char,Code),
    write(Char),
    write_codes(Codes).

check_xml_file(File,File1):-
    atom(File),
    plus_ext(File,xml,File2), % in B-Prolog
    exists(File2),!,
    File1=File2.
check_xml_file(File,File1):-
    atom(File),
    exists(File),!,
    File1=File.
check_xml_file(File,_File1):-
    handle_exception(file_not_found,chr2ar(File)).

/* xml_parse( {+Controls}, +?Chars, ?+Document ) parses Chars to/from a data
 * structure of the form xml(<atts>, <content>). <atts> is a list of
 * <atom>=<string> attributes from the (possibly implicit) XML signature of the
 * document. <content> is a (possibly empty) list comprising occurrences of :
 *
 *	pcdata(<string>)					:	Text
 *	comment(<string>)					:	An xml comment;
 *  element(<tag>,<atts>,<content>)		:	<tag>..</tag> encloses <content>
 *										:   <tag /> if empty
 *	instructions(<atom>, <string>)		:	Processing <? <atom> <params> ?>"
 * 	cdata( <string> )					:	<![CDATA[ <string> ]]>
 *	doctype(<atom>, <doctype id>)		:	DTD <!DOCTYPE .. >
 *
 * The conversions are not completely symmetrical, in that weaker XML is
 * accepted than can be generated. Specifically, in-bound (Chars -> Document)
 * does not  require strictly well-formed XML. Document is instantiated to the
 * term malformed(Attributes, Content) if Chars does not represent well-formed
 * XML. The Content of a malformed/2 structure can contain:
 *
 *	unparsed( <string> )				:	Text which has not been parsed
 *	out_of_context( <tag> )				:	<tag> is not closed
 *
 * in addition to the standard term types.
 *
 * Out-bound (Document -> Chars) parsing _does_ require that Document defines
 * strictly well-formed XML. If an error is detected a 'domain' exception is
 * raised.
 *
 * The domain exception will attempt to identify the particular sub-term in
 * error and the message will show a list of its ancestor elements in the form
 * <tag>{(id)}* where <id> is the value of any attribute _named_ id.
 *
 * At this release, the Controls applying to in-bound (Chars -> Document)
 * parsing are:
 *
 *	extended_characters(<bool>)			:	Use the extended character
 *										:	entities for XHTML (default true)
 *
 *	format(<bool>)						:	Strip layouts when no character data
 *										:	appears between elements.
 *										:	(default true)
 *
 *	[<bool> is one of 'true' or 'false']
 *
 * For out-bound (Document -> Chars) parsing, the only available option is:
 *
 *	format(<Bool>)						:	Indent the element content
 *										:	(default true)
 *
 * Different DCGs for input and output are used because input parsing is
 * more flexible than output parsing. Errors in input are recorded as part
 * of the data structure. Output parsing throws an exception if the document
 * is not well-formed, diagnosis tries to identify the specific culprit term.
 */
xml_parse( Chars, Document ) :-
	xml_parse( [], Chars, Document ).

xml_parse( Controls, Chars, Document ) :-
	( ground( Chars ) ->
		xml_to_document( Controls, Chars, Document )
	; otherwise ->
		document_to_xml( Controls, Document, Chars )
	).

document_to_xml( Controls, Document, Chars ) :-
	( member( format(false), Controls ) ->
		Format = false
	; otherwise ->
		Format = true
	),
	( ground( Document ),
	  document_generation(Format, Document, Chars0, [] ) ->
			Chars = Chars0
	; otherwise ->
		xml_fault( Document, [], Culprit, Path, Message ),
		throw(
			application_error('XML Parse: ~s in ~q~nCulprit: ~q~nPath: ~s', 
				[Message,Document,Culprit,Path] )
			)
	).

/* xml_subterm( +XMLTerm, ?Subterm ) unifies Subterm with a sub-term of Term.
 * Note that XMLTerm is a sub-term of itself. 
 */
xml_subterm( Term, Term ).
xml_subterm( xml(_Attributes, Content), Term ) :-
	xml_subterm( Content, Term ).	
xml_subterm( [H|T], Term ) :-
	( xml_subterm( H, Term )
	; xml_subterm( T, Term )
	).
xml_subterm( element(_Name,_Attributes,Content), Term ) :-
	xml_subterm( Content, Term ).
xml_subterm( namespace(_URI,_Prefix,Content), Term ) :-
	xml_subterm( Content, Term ).

/* xml is intended to be a rather modular module: it should be easy to
 * build a program that can output XML, but not read it, or vice versa.
 * Similarly, you may be happy to dispense with diagnosis once you are
 * sure that your code will only try to make valid calls to xml_parse/2.
 *
 * It is intended that the code should be very portable too. Clearly,
 * some small changes will be needed between platforms, but these should
 * be limited to xml_utilities. xml_utilities contains most of the shared
 * code and most of the potentially non-portable code.
 */

/* :- include('xml_acquisition.pl'). */
/* xml_acquisition.pl : XML -> Document translation.
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml_acquisition.pl,v $
 * Revision 1.2  2002-05-25 13:18:48+01  john
 * Eliminated calls to parse/3 to improve portability.
 *
 * Revision 1.1  2002-01-31 21:04:45+00  john
 * Updated Copyright statements.
 *
 * Revision 1.0  2001-10-17 20:46:23+01  john
 * Initial revision
 *
 *
 *
 */

%:- ensure_loaded( xml_utilities ).

/* xml_to_document( +Controls, +XML, ?Document ) translates the list of
 * character codes XML into the Prolog term Document. Controls is a list
 * of terms controlling the treatment of layout characters and character
 * entities.
 */
xml_to_document( Controls, XML, Document ) :-
	initial_context( Controls, Context ),
	( xml_declaration( Attributes0, XML, XML1 ) ->
		Attributes = Attributes0
	; otherwise ->
		XML1 = XML,
		Attributes = []
	),
	xml_to_document( XML1, Context, Terms, [], WellFormed ),
	xml_to_document1( WellFormed, Attributes, Terms, Document ).

xml_to_document1( true,  Attributes, Terms, xml(Attributes, Terms) ).
xml_to_document1( false, Attributes, Terms, malformed(Attributes, Terms) ).

% unparsed( +Unparsed, +Context, ?Terms, ?Residue, ?WellFormed )
unparsed( Unparsed, _Context, [unparsed(Unparsed)], [], false ).

xml_declaration( Attributes ) -->
	spaces,
	"<?",
	nmtoken( xml ),
	xml_declaration_attributes( Attributes ),
	spaces,
	"?>".

xml_to_document( [], Context, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
xml_to_document( [Char|Chars], Context, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		entity_reference( Chars, Context, Terms, Residue, WF )
	; Char =< " ",
	  \+ space_preserve( Context ) ->		
		layouts( Chars, Context, [Char|T], T, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata([Char|Chars1])|Terms1],
		acquire_pcdata( Chars, Context, Chars1, Terms1, Residue, WF )
	).

layouts( [], Context, _Plus, _Minus, Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
layouts( [Char|Chars], Context, Plus, Minus, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		Chars1 = [],
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		entity_reference( Chars, Context, Terms, Residue, WF )
	; Char =< " " ->
		Minus = [Char|Minus1],
		layouts( Chars, Context, Plus, Minus1, Terms, Residue, WF )
	; void_context( Context ) ->
		unparsed( [Char|Chars], Context, Terms, Residue, WF )
	; otherwise ->
		Terms = [pcdata(Plus)|Terms1],
		Minus = [Char|Chars1],
		context_update( space_preserve, Context, true, Context1 ),
		acquire_pcdata( Chars, Context1, Chars1, Terms1, Residue, WF )
	).

acquire_pcdata( [], Context, [], Terms, [], WF ) :-
	close_context( Context, Terms, WF ).
acquire_pcdata( [Char|Chars], Context, Chars1, Terms, Residue, WF ) :-
	( Char =:= "<" ->
		Chars1 = [],
		xml_markup_structure( Chars, Context, Terms, Residue, WF )
	; Char =:= "&" ->
		reference_in_pcdata( Chars, Context, Chars1, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Chars, Context, Chars2, Terms, Residue, WF )
	).

xml_markup_structure( [], Context, Terms, Residue, WF ) :-
	unparsed( "<", Context, Terms, Residue, WF ).
xml_markup_structure( Chars, Context, Terms, Residue, WF ) :-
	Chars = [Char|Chars1],
	( Char =:= "/" ->
		closing_tag( Context, Chars1, Terms, Residue, WF )
	; Char =:= "?" ->
		pi_acquisition( Chars1, Context, Terms, Residue, WF )
	; Char =:= "!" ->
		declaration_acquisition( Chars1, Context, Terms, Residue, WF )
	; open_tag(Tag,Context,Attributes,Type, Chars, Chars2 ) ->
		push_tag( Tag, Chars2, Context, Attributes, Type, Terms, Residue, WF )
	; otherwise ->
		unparsed( [0'<|Chars], Context, Terms, Residue, WF )
	).

push_tag( Tag, Chars, Context, Attributes, Type, Terms, Residue, WF ) :-
	new_element(Tag, Chars, Context, Attributes, Type, Term, Rest, WF0),
	push_tag1( WF0, Context, Term, Rest, Terms, Residue, WF ).

push_tag1( true, Context, Term, Chars, [Term|Terms], Residue, WF ) :-
	xml_to_document( Chars, Context, Terms, Residue, WF ).
push_tag1( false, _Context, Term, Chars, [Term], Chars, false ).

new_element( TagChars, Chars, Context, Attributes0, Type, Term, Residue, WF ) :-
	input_attributes( Attributes0, Context, Context1, Attributes ),
	current_namespace( Context, CurrentNamespace ),
	( append( NSChars, [0':|TagChars1], TagChars ),
	  specific_namespace( NSChars, Context1, SpecificNamespace ) ->
		Namespace0 = SpecificNamespace
	; otherwise ->
		NSChars = "",
		TagChars1 = TagChars,
		default_namespace( Context1, Namespace0 )
	),
	( Namespace0 == CurrentNamespace ->
		Term = element(Tag, Attributes, Contents),
		Context2 = Context1
	; otherwise ->
		Term = namespace( Namespace0, NSChars,
					element(Tag, Attributes, Contents)
					),
		context_update( current_namespace, Context1, Namespace0, Context2 )
	),
	atom_codes( Tag, TagChars1 ),
	close_tag( Type, Chars, Context2, Contents, Residue, WF ).

close_tag( empty, Residue, _Context, [], Residue, true ).
close_tag( push(Tag), Chars, Context0, Contents, Residue, WF ) :-
	context_update( element, Context0, Tag, Context1 ),
	xml_to_document( Chars, Context1, Contents, Residue, WF ).

pi_acquisition( Chars, Context, Terms, Residue, WellFormed ) :-
	( inline_instruction(Target, Processing, Chars, Rest ),
	  Target \== xml ->
		Terms = [instructions(Target, Processing)|Terms1],
		xml_to_document( Rest, Context, Terms1, Residue, WellFormed )
	; otherwise ->
		unparsed( [0'<,0'?|Chars], Context, Terms, Residue, WellFormed )
	).

declaration_acquisition( Chars, Context, Terms, Residue, WF ) :-
	( declaration_type( Chars, Type, Chars1 ),
	  declaration_parse( Type, Context, Term, Context1, Chars1, Rest ) ->
		Terms = [Term|Terms1],
		xml_to_document( Rest, Context1, Terms1, Residue, WF )
	; otherwise ->
		unparsed( [0'<,0'!|Chars], Context, Terms, Residue, WF )
	).

open_tag( Tag, Namespaces, Attributes, Termination ) -->
	nmtoken_chars( Tag ),
	attributes( Attributes, [], Namespaces ),
	spaces,
	open_tag_terminator( Tag, Termination ).

open_tag_terminator( Tag, push(Tag) ) -->
	">".
open_tag_terminator( _Tag, empty ) -->
	"/>".

declaration_parse( comment, Namespaces, comment(Comment), Namespaces ) -->
	comment(Comment).
declaration_parse( cdata, Namespaces, cdata(CData), Namespaces ) -->
	cdata( CData ).
declaration_parse( doctype, Namespaces0, doctype(Name, Names), Namespaces ) -->
	doctype( Name, Names, Namespaces0, Namespaces ),
	spaces,
	">".

inline_instruction( Target, Processing, Plus, Minus  ) :-
	nmtoken(Target, Plus, Mid0 ),
	spaces( Mid0, Mid1 ),
	append( Processing, [0'?,0'>|Minus], Mid1 ),
	!.

entity_reference_name( Reference ) -->
	nmtoken_chars( Reference ),
	";".

declaration_type( [Char1,Char2|Chars1], Class, Rest ) :-
	Chars = [Char1,Char2|Chars1],
	( declaration_type1( Char1, Char2, Chars1, Class0, Residue ) ->
		Class = Class0,
		Rest = Residue
	; otherwise ->
		Class = generic,
		Rest = Chars
	).

declaration_type1( 0'-, 0'-, Chars, comment, Chars ).
declaration_type1( 0'[, 0'C, Chars, cdata, Residue ) :-
	append( "DATA[", Residue, Chars ).
declaration_type1( 0'D, 0'O, Chars, doctype, Residue ) :-
	append( "CTYPE", Residue, Chars ).

closing_tag( Context, Chars, Terms, Residue, WellFormed ) :-
	( closing_tag_name( Tag, Chars, Rest ),
	  current_tag( Context, Tag ) ->
		Terms = [],
		Residue = Rest,
		WellFormed = true
	; otherwise ->
		unparsed( [0'<,0'/|Chars], Context, Terms, Residue, WellFormed )
	).

closing_tag_name( Tag ) -->
	nmtoken_chars( Tag ),
	spaces,
	">".

entity_reference( Chars, Context, Terms, Residue, WF ) :-
	( standard_character_entity( Char, Chars, Rest ) ->
		Terms = [pcdata([Char|Chars1])|Terms1],
		acquire_pcdata( Rest, Context, Chars1, Terms1, Residue, WF )
	; entity_reference_name( Reference, Chars, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		xml_to_document( Full, Context, Terms, Residue, WF )
	; otherwise ->
		unparsed( [0'&|Chars], Context, Terms, Residue, WF )
	).

reference_in_pcdata( Chars0, Context, Chars1, Terms, Residue, WF ) :-
	( standard_character_entity(Char, Chars0, Rest ) ->
		Chars1 = [Char|Chars2],
		acquire_pcdata( Rest, Context, Chars2, Terms, Residue, WF )
	; entity_reference_name(Reference, Chars0, Rest ),
	  defined_entity( Reference, Context, String ) ->
		append( String, Rest, Full ),
		acquire_pcdata( Full, Context, Chars1, Terms, Residue, WF )
	; otherwise ->
		Chars1 = [],
		unparsed( [0'&|Chars0], Context, Terms, Residue, WF )
	).

input_attributes( [], Context, Context, [] ).
input_attributes( [Attr|Attributes0], Context0, Context, Attributes ) :-
	Attr = (NameChars=Value),
	( NameChars == "xmlns" ->
		Attributes = Attributes1,
		atom_codes( URI, Value ),
		context_update( default_namespace, Context0, URI, Context1 )
	; Attr == ("xml:space"="preserve") ->
		Attributes = ['xml:space'="preserve"|Attributes1],
		context_update( space_preserve, Context0, true, Context1 )
	; append( "xmlns:", Unqualified, NameChars ) ->
		Attributes = [Name=Value|Attributes1],
		atom_codes( Name, NameChars ),
		atom_codes( URI, Value ),
		context_update( ns_prefix(Unqualified), Context0, URI, Context1 )
	; otherwise ->
		Attributes = [Name=Value|Attributes1],
		atom_codes( Name, NameChars ),
		Context1 = Context0
	),
	input_attributes( Attributes0, Context1, Context, Attributes1 ).

attributes( [Name=Value|Attributes], Seen, Namespaces ) -->
	spaces,
	nmtoken_chars( Name ),
	{\+ member(Name, Seen)},
	spaces,
	"=",
	spaces,
	attribute_value( Value, Namespaces ),
	attributes( Attributes, [Name|Seen], Namespaces ).
attributes( [], _Seen, _Namespaces ) --> "".

xml_declaration_attributes( [] ) --> "".
xml_declaration_attributes( [Name=Value|Attributes] ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	"=",
	spaces,
	xml_string( Value ),
	{xml_declaration_attribute_valid(Name, Value)},
	xml_declaration_attributes( Attributes ),
	spaces.

doctype( Name, External, Namespaces0, Namespaces1 ) -->
	spaces,
	nmtoken( Name ),
	spaces,
	doctype_id( External ),
	spaces,
	doctype1( Namespaces0, Namespaces1 ).

doctype1( Namespaces0, Namespaces1 ) -->
	"[",
	!,
	dtd( Namespaces0, Namespaces1 ),
	"]".
doctype1( Namespaces, Namespaces ) --> "".

doctype_id( system(URL) ) -->
	"SYSTEM",
	spaces,
	uri( URL ).
doctype_id( public(URN,URL) ) -->
	"PUBLIC",
	spaces,
	uri( URN ),
	spaces,
	uri( URL ).
doctype_id( local ) --> "".

dtd( Namespaces0, Namespaces1 ) -->
	spaces,
	"<!ENTITY",
	spaces,
	nmtoken_chars( Name ),
	spaces,
	quote( Quote ),
	entity_value( Quote, Namespaces0, String ),
	spaces,
	">",
	{\+ character_entity( Name, _StandardChar ), 
	 % Don't allow &lt; &quote; etc. to be updated
	 context_update( entity(Name), Namespaces0, String, Namespaces2 )},
	dtd( Namespaces2, Namespaces1 ).
dtd( Namespaces0, Namespaces1 ) -->
	spaces,
	"<!--", comment(_Comment),
	dtd( Namespaces0, Namespaces1 ).
dtd( Namespaces, Namespaces ) --> spaces.

nmtokens( [Name|Names] ) -->
	spaces,
	nmtoken( Name ),
	nmtokens( Names ).
nmtokens( [] ) --> [].

entity_value( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_entity( Namespaces, Quote, String, Plus, Minus )
	; otherwise ->
		String = [Char|String1],
		entity_value( Quote, Namespaces, String1, Plus, Minus )
	).

attribute_value( String, Namespaces ) -->
	quote( Quote ),
	attribute_leading_layouts( Quote, Namespaces, String ).

attribute_leading_layouts( _Quote, _Namespace, [], [], [] ).
attribute_leading_layouts( Quote, Namespaces, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_layout( Namespaces, Quote, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		String = [Char|String1],
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_leading_layouts( Quote, Namespaces, String, Plus, Minus )
	).

attribute_layouts( _Quote, _Namespaces, _Layout, [], [], [] ).
attribute_layouts( Quote, Namespaces, Layout, String, [Char|Plus], Minus ) :-
	( Char == Quote ->
		String = [],
		Minus = Plus
	; Char =:= "&" ->
		reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus )
	; Char > 32, Char \== 160 ->
		( Layout == true ->
			String = [0' ,Char|String1]
		; otherwise ->
			String = [Char|String1]
		),
		attribute_layouts( Quote, Namespaces, false, String1, Plus, Minus )
	; otherwise ->
		attribute_layouts( Quote, Namespaces, true, String, Plus, Minus )
	).

reference_in_layout( NS, Quote, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		String = [Char|String1],
		attribute_layouts( Quote, NS, false,  String1, Mid, Minus )
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, NS, Text ) ->
		append( Text, Suffix, Mid ),
		attribute_leading_layouts( Quote, NS, String, Mid, Minus )
	; otherwise -> % Just & is okay in a value
		String = [0'&|String1],
		attribute_layouts( Quote, NS, false, String1, Plus, Minus )
	).

reference_in_value( Namespaces, Quote, Layout, String, Plus, Minus ) :-
	( standard_character_entity( Char, Plus, Mid ) ->
		( Layout == true ->
			String = [0' ,Char|String1]
		; otherwise ->
			String = [Char|String1]
		),
		Layout1 = false
	; entity_reference_name( Name, Plus, Suffix ),
	  defined_entity( Name, Namespaces, Text ) ->
		String = String1,
		append( Text, Suffix, Mid ),
		Layout1 = Layout
	; otherwise -> % Just & is okay in a value
		Mid = Plus,
		String = [0'&|String1],
		Layout1 = false
	),
	attribute_layouts( Quote, Namespaces, Layout1, String1, Mid, Minus ).

/* References are resolved backwards in Entity defintions so that
 * circularity is avoided.
 */
reference_in_entity( Namespaces, Quote, String, Plus, Minus ) :-
	( standard_character_entity( _SomeChar, Plus, _Rest ) ->
		String = [0'&|String1], % Character entities are unparsed
		Mid = Plus
	; entity_reference_name( Name, Plus, Suffix ), 
	  defined_entity( Name, Namespaces, Text ) -> 
		String = String1,
		append( Text, Suffix, Mid )
	),
	entity_value( Quote, Namespaces, String1, Mid, Minus ).

standard_character_entity( Char ) -->
	"#x", hex_character_reference( Char ), ";".
standard_character_entity( Char ) -->
	"#", digit( Digit ), digits( Digits ), ";",
	{number_chars( Char, [Digit|Digits])}.
standard_character_entity( C ) -->
	chars( String ),
	";",
	!,
	{character_entity(String, C)}.

uri( URI ) -->
	quote( Quote ),
	uri1( Quote, URI ).

uri1( Quote, [] ) -->
	quote( Quote ),
	!.
uri1( Quote, [Char|Chars] ) -->
	[Char],
	uri1( Quote, Chars ).

comment( Chars, Plus, Minus ) :-
	append( Chars, [0'-,0'-,0'>|Minus], Plus ),
	!.

cdata( Chars, Plus, Minus ) :-
	append( Chars, [0'],0'],0'>|Minus], Plus ),
	!.
% Syntax Components

hex_character_reference( Code ) -->
	hex_character_reference1( 0, Code ).

hex_character_reference1( Current, Code ) -->
	hex_digit_char( Value ),
	!,
	{New is (Current << 4) + Value},
	hex_character_reference1( New, Code ).
hex_character_reference1( Code, Code ) --> "".

hex_digit_char( 0 ) --> "0".
hex_digit_char( 1 ) --> "1".
hex_digit_char( 2 ) --> "2".
hex_digit_char( 3 ) --> "3".
hex_digit_char( 4 ) --> "4".
hex_digit_char( 5 ) --> "5".
hex_digit_char( 6 ) --> "6".
hex_digit_char( 7 ) --> "7".
hex_digit_char( 8 ) --> "8".
hex_digit_char( 9 ) --> "9".
hex_digit_char( 10 ) --> "A".
hex_digit_char( 11 ) --> "B".
hex_digit_char( 12 ) --> "C".
hex_digit_char( 13 ) --> "D".
hex_digit_char( 14 ) --> "E".
hex_digit_char( 15 ) --> "F".
hex_digit_char( 10 ) --> "a".
hex_digit_char( 11 ) --> "b".
hex_digit_char( 12 ) --> "c".
hex_digit_char( 13 ) --> "d".
hex_digit_char( 14 ) --> "e".
hex_digit_char( 15 ) --> "f".

quote( 0'" ) --> %"
	"""".
quote( 0'' ) -->
	"'".

spaces( [], [] ).
spaces( [Char|Chars0], Chars1 ) :-
	( Char =< 32 ->
		spaces( Chars0, Chars1 )
	; otherwise ->
		Chars1 = [Char|Chars0]
	).

nmtoken( Name ) -->
	nmtoken_chars( Chars ),
	{atom_codes(Name, Chars)}.

nmtoken_chars( [Char|Chars] ) -->
	[Char],
	{alphabet( Char )},
	nmtoken_chars_tail( Chars ).

nmtoken_chars_tail( [Char|Chars] ) -->
	[Char],
	{nmtoken_char(Char)},
	!,
	nmtoken_chars_tail( Chars ).
nmtoken_chars_tail([]) --> "".

nmtoken_char( 0'a ).
nmtoken_char( 0'b ).
nmtoken_char( 0'c ).
nmtoken_char( 0'd ).
nmtoken_char( 0'e ).
nmtoken_char( 0'f ).
nmtoken_char( 0'g ).
nmtoken_char( 0'h ).
nmtoken_char( 0'i ).
nmtoken_char( 0'j ).
nmtoken_char( 0'k ).
nmtoken_char( 0'l ).
nmtoken_char( 0'm ).
nmtoken_char( 0'n ).
nmtoken_char( 0'o ).
nmtoken_char( 0'p ).
nmtoken_char( 0'q ).
nmtoken_char( 0'r ).
nmtoken_char( 0's ).
nmtoken_char( 0't ).
nmtoken_char( 0'u ).
nmtoken_char( 0'v ).
nmtoken_char( 0'w ).
nmtoken_char( 0'x ).
nmtoken_char( 0'y ).
nmtoken_char( 0'z ).
nmtoken_char( 0'A ).
nmtoken_char( 0'B ).
nmtoken_char( 0'C ).
nmtoken_char( 0'D ).
nmtoken_char( 0'E ).
nmtoken_char( 0'F ).
nmtoken_char( 0'G ).
nmtoken_char( 0'H ).
nmtoken_char( 0'I ).
nmtoken_char( 0'J ).
nmtoken_char( 0'K ).
nmtoken_char( 0'L ).
nmtoken_char( 0'M ).
nmtoken_char( 0'N ).
nmtoken_char( 0'O ).
nmtoken_char( 0'P ).
nmtoken_char( 0'Q ).
nmtoken_char( 0'R ).
nmtoken_char( 0'S ).
nmtoken_char( 0'T ).
nmtoken_char( 0'U ).
nmtoken_char( 0'V ).
nmtoken_char( 0'W ).
nmtoken_char( 0'X ).
nmtoken_char( 0'Y ).
nmtoken_char( 0'Z ).
nmtoken_char( 0'0 ).
nmtoken_char( 0'1 ).
nmtoken_char( 0'2 ).
nmtoken_char( 0'3 ).
nmtoken_char( 0'4 ).
nmtoken_char( 0'5 ).
nmtoken_char( 0'6 ).
nmtoken_char( 0'7 ).
nmtoken_char( 0'8 ).
nmtoken_char( 0'9 ).
nmtoken_char( 0'. ).
nmtoken_char( 0'- ).
nmtoken_char( 0'_ ).
nmtoken_char( 0': ).

xml_string( String ) -->
	quote( Quote ),
	xml_string1( Quote, String ).

xml_string1( Quote, [] ) -->
	quote( Quote ),
	!.
xml_string1( Quote, [Char|Chars] ) -->
	[Char],
	xml_string1( Quote, Chars ).

alphabet( 0'a ).
alphabet( 0'b ).
alphabet( 0'c ).
alphabet( 0'd ).
alphabet( 0'e ).
alphabet( 0'f ).
alphabet( 0'g ).
alphabet( 0'h ).
alphabet( 0'i ).
alphabet( 0'j ).
alphabet( 0'k ).
alphabet( 0'l ).
alphabet( 0'm ).
alphabet( 0'n ).
alphabet( 0'o ).
alphabet( 0'p ).
alphabet( 0'q ).
alphabet( 0'r ).
alphabet( 0's ).
alphabet( 0't ).
alphabet( 0'u ).
alphabet( 0'v ).
alphabet( 0'w ).
alphabet( 0'x ).
alphabet( 0'y ).
alphabet( 0'z ).
alphabet( 0'A ).
alphabet( 0'B ).
alphabet( 0'C ).
alphabet( 0'D ).
alphabet( 0'E ).
alphabet( 0'F ).
alphabet( 0'G ).
alphabet( 0'H ).
alphabet( 0'I ).
alphabet( 0'J ).
alphabet( 0'K ).
alphabet( 0'L ).
alphabet( 0'M ).
alphabet( 0'N ).
alphabet( 0'O ).
alphabet( 0'P ).
alphabet( 0'Q ).
alphabet( 0'R ).
alphabet( 0'S ).
alphabet( 0'T ).
alphabet( 0'U ).
alphabet( 0'V ).
alphabet( 0'W ).
alphabet( 0'X ).
alphabet( 0'Y ).
alphabet( 0'Z ).

digit( C ) --> [C], {digit_table( C )}.

digit_table( 0'0 ).
digit_table( 0'1 ).
digit_table( 0'2 ).
digit_table( 0'3 ).
digit_table( 0'4 ).
digit_table( 0'5 ).
digit_table( 0'6 ).
digit_table( 0'7 ).
digit_table( 0'8 ).
digit_table( 0'9 ).

digits( [Digit|Digits] ) -->
	digit( Digit ),
	digits( Digits ).
digits( [] ) --> [].

chars( Chars, Plus, Minus ) :-
	append( Chars, Minus, Plus ).

character_entity( "quot", 0'" ). %"
character_entity( "amp", 0'&  ).
character_entity( "lt", 0'< ).
character_entity( "gt", 0'> ).
character_entity( "apos", 0'' ).

/* :- include('src\xml_diagnosis.pl'). */
/* xml_diagnosis.pl : XML exception diagnosis.
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml_diagnosis.pl,v $
 * Revision 1.3  2002-05-10 10:22:10+01  john
 * Diagnosing invalid character data.
 *
 * Revision 1.2  2002-04-21 19:39:03+01  john
 * Correcting format of explanatory strings.
 *
 * Revision 1.1  2002-01-31 21:04:45+00  john
 * Updated Copyright statements.
 *
 * Revision 1.0  2001-10-17 20:46:23+01  john
 * Initial revision
 *
 *
 *
 */


/* xml_fault( +Term, +Indentation, ?SubTerm, ?Path, ?Message ) identifies SubTerm
 * as a sub-term of Term which cannot be serialized after Indentation.
 * Message is an atom naming the type of error; Path is a string encoding a
 * list of SubTerm's ancestor elements in the form <tag>{(id)}* where <tag> is the
 * element tag and <id> is the value of any attribute _named_ id.
 */
xml_fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
	var( Term ).
xml_fault( xml(Attributes,_Content), _Indent, Term, [], Message ) :-
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Term, Message ).
xml_fault( xml(_Attributes,Content), Indent, Culprit, Path, Message ) :-
	xml_content_fault( Content, Indent, Culprit, Path, Message ).
xml_fault( Term, _Indent, Term, [], "Illegal Term" ).

xml_content_fault( Term, _Indent, Term, [], "Illegal Variable" ) :-
	var( Term ).
xml_content_fault( pcdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_content_fault( cdata(Chars), _Indent, Chars, [], "Invalid Character Data" ) :-
	\+ is_chars( Chars ).
xml_content_fault( [H|_T], Indent, Culprit, Path, Message ) :-
	xml_content_fault( H, Indent, Culprit, Path, Message ).
xml_content_fault( [_H|T], Indent, Culprit, Path, Message ) :-
	xml_content_fault( T, Indent, Culprit, Path, Message ).
xml_content_fault( namespace(_URI,_Prefix,Element), Indent, Culprit, Path, Message ) :-
	element_fault( Element, [0' |Indent], Culprit, Path, Message ).
xml_content_fault( Element, Indent, Culprit, Path, Message ) :-
	element_fault( Element, [0' |Indent], Culprit, Path, Message ).
xml_content_fault( Term, Indent, Term, [], "Illegal Term" ) :-
	\+ generation(Term, "", false, Indent, _Format, _Plus, _Minus ).

element_fault( element(Tag, _Attributes, _Contents), _Indent, Tag, [], "Tag must be an atom" ) :-
	\+ atom( Tag ).
element_fault( element(Tag, Attributes, _Contents), _Indent, Tag, [], "Attributes must be instantiated" ) :-
	var( Attributes ).
element_fault( element(Tag, Attributes, _Contents), _Indent, Faulty, Path, Message ) :-
	fault_path( Tag, Attributes, Path, [] ),
	member( Attribute, Attributes ),
	attribute_fault( Attribute, Faulty, Message ).
element_fault( element(Tag, Attributes, Contents), Indent, Culprit, Path, Message ) :-
	fault_path( Tag, Attributes, Path, Path1 ),
	xml_content_fault( Contents, Indent, Culprit, Path1, Message ).

attribute_fault( Attribute, Attribute, "Illegal Variable" ) :-
	var( Attribute ).
attribute_fault( Name=Value, Name=Value, "Attribute Name must be atom" ) :-
	\+ atom(Name).
attribute_fault( Name=Value, Name=Value, "Attribute Value must be chars" ) :-
	\+ is_chars( Value ).
attribute_fault( Attribute, Attribute, "Malformed Attribute" ) :-
	\+ Attribute = (_Name=_Value).

is_chars( Chars ) :-
	is_list( Chars ),
	\+ (member( Char, Chars ), \+ (integer(Char), Char >=0, Char =< 255)).

fault_path( Tag, Attributes ) -->
	{atom_codes( Tag, Chars )},
	chars( Chars ),
	fault_id( Attributes ),
	" ".

fault_id( Attributes ) -->
	{member( id=Chars, Attributes ), is_chars( Chars )},
	!,
	"(", chars(Chars), ")".
fault_id( _Attributes ) --> "".

/* :- include('xml_generation.pl'). */
/* xml_generation.pl : Document -> XML translation
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml_generation.pl,v $
 * Revision 1.3  2002-05-25 13:19:25+01  john
 * Eliminated calls to parse/3 to improve portability.
 *
 * Revision 1.2  2002-01-31 21:04:45+00  john
 * Updated Copyright statements.
 *
 * Revision 1.1  2001-11-04 17:28:02+00  john
 * Adding < as a 'must be entity' ... where did it go to? I'll never know.
 *
 * Revision 1.0  2001-10-17 20:46:23+01  john
 * Initial revision
 *
 *
 *
 */

/* document_generation( +Format, +Document ) is a DCG generating Document
 * as a list of character codes. Format is true|false defining whether layouts,
 * to provide indentation, should be added between the element content of
 * the resultant "string". Note that formatting is disabled for elements that
 * are interspersed with pcdata/1 terms, such as XHTML's 'inline' elements.
 * Also, Format is over-ridden, for an individual element, by an explicit
 * 'xml:space'="preserve" attribute.
 */
document_generation( Format, xml(Attributes, Document) ) -->
	document_generation_body( Attributes, Format, Document ).

document_generation_body( [], Format, Document ) -->
	generation( Document, "", Format, [], _Format1 ).
document_generation_body( Attributes, Format, Document ) -->
	{	Attributes = [_|_],
		xml_declaration_attributes_valid( Attributes )
	},
	"<?xml",
	generated_attributes( Attributes, Format, Format0 ),
	"?>",
	indent( true, [] ),
	generation( Document, "", Format0, [], _Format1 ).

generation( [], _Prefix, Format, _Indent, Format ) --> [].
generation( [Term|Terms], Prefix, Format0, Indent, Format ) -->
	generation( Term, Prefix, Format0, Indent, Format1 ),
	generation( Terms, Prefix, Format1, Indent, Format ).
generation( doctype(Name, External), _Prefix, Format, [], Format ) -->
	"<!DOCTYPE ",
	generated_name( Name ),
	generated_external_id( External ),
	">".
generation( instructions(Target,Process), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<?", generated_name(Target), " ", chars( Process ) ,"?>".
generation( pcdata(Chars), _Prefix, _Format, _Indent, false ) -->
	pcdata_generation( Chars ).
generation( comment( Comment ), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<!--", chars( Comment ), "-->".
generation( namespace(URI, Prefix, element(Name, Atts, Content)),
		_Prefix0, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<", generated_prefixed_name( Prefix, Name ),
	generated_prefixed_attributes( Prefix, URI, Atts, Format, Format1 ), 
	generated_content( Content, Format1, Indent, Prefix, Name ).
generation( element(Name, Atts, Content), Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<", generated_prefixed_name( Prefix, Name ),
	generated_attributes( Atts, Format, Format1 ), 
	generated_content( Content, Format1, Indent, Prefix, Name ).
generation( cdata(CData), _Prefix, Format, Indent, Format ) -->
	indent( Format, Indent ),
	"<![CDATA[", chars(CData), "]]>".

generated_attributes( [], Format, Format  ) --> [].
generated_attributes( [Name=Value|Attributes], Format0, Format  ) -->
	{(	Name == 'xml:space',
		Value="preserve" ->
			Format1 = false
	  ; otherwise ->
			Format1 = Format0
	  )},
	" ",
	generated_name( Name ),
	"=""",
	quoted_string( Value ),
	"""",
	generated_attributes( Attributes, Format1, Format  ).

generated_prefixed_name( [], Name ) -->
	generated_name( Name ).
generated_prefixed_name( Prefix, Name ) -->
	{Prefix = [_|_]},
	chars( Prefix ), ":",
	generated_name( Name ).

generated_content( [], _Format, _Indent, _Prefix, _Namespace ) -->
	" />". % Leave an extra space for XHTML output.
generated_content( [H|T], Format, Indent, Prefix, Namespace ) -->
	">",
	generation( H, Prefix, Format, [0' |Indent], Format1 ),
	generation( T, Prefix, Format1, [0' |Indent], Format2 ),
	indent( Format2, Indent ),
	"</", generated_prefixed_name( Prefix, Namespace ), ">".

generated_prefixed_attributes( [_|_Prefix], _URI, Atts, Format0, Format ) -->
	generated_attributes( Atts, Format0, Format  ).
generated_prefixed_attributes( [], URI, Atts, Format0, Format  ) -->
	{atom_codes( URI, Namespace ),
	 findall( Attr, (member(Attr, Atts), \+ Attr=(xmlns=_Val)), Atts1 )
	},
	generated_attributes( [xmlns=Namespace|Atts1], Format0, Format  ).

generated_name( Name, Plus, Minus ) :-
	atom_codes( Name, Chars ),
	append( Chars, Minus, Plus ).

generated_external_id( local ) --> "".
generated_external_id( system(URL) ) -->
	" SYSTEM """,
	chars( URL ),
	"""".
generated_external_id( public(URN,URL) ) -->
	" PUBLIC """,
	chars( URN ),
	""" """,
	chars( URL ),
	"""".

/* quoted_string( +Chars ) is a DCG representing Chars, a list of character
 * codes, as a legal XML attribute string. Any leading or trailing layout
 * characters are removed. &, " and < characters are replaced by &amp;, &quot;
 * and &lt; respectively.
 */
quoted_string( Raw, Plus, Minus ) :-
	quoted_string1( Raw, NoLeadingLayouts ),
	quoted_string2( NoLeadingLayouts, Layout, Layout, Plus, Minus ).

quoted_string1( [], [] ).
quoted_string1( [Char|Chars], NoLeadingLayouts ) :-
	( Char > 32 ->
		NoLeadingLayouts = [Char|Chars]
	; otherwise ->
		quoted_string1( Chars, NoLeadingLayouts )
	).

quoted_string2( [], _LayoutPlus, _LayoutMinus, List, List ).
quoted_string2( [Char|Chars], LayoutPlus, LayoutMinus, Plus, Minus ) :-
	( must_be_entity( Char ) ->
		Plus = LayoutPlus,
		pcdata_7bit( Char, LayoutMinus, Plus1 ),
		LayoutPlus1 = LayoutMinus1
	; Char > 127 ->
		Plus = LayoutPlus,
		pcdata_8bits_plus( Char, LayoutMinus, Plus1 ),
		LayoutPlus1 = LayoutMinus1
	; Char > 32 ->
		Plus = LayoutPlus,
		LayoutMinus = [Char|Plus1],
		LayoutPlus1 = LayoutMinus1
	; otherwise ->
		Plus = Plus1,
		LayoutMinus = [Char|LayoutMinus1],
		LayoutPlus1 = LayoutPlus
	),
	quoted_string2( Chars, LayoutPlus1, LayoutMinus1, Plus1, Minus ).

indent( false, _Indent ) --> [].
indent( true, Indent ) -->
	"
",	chars( Indent ).

/* pcdata_generation( +Chars ) is a DCG representing Chars, a list of character
 * codes as legal XML "Parsed character data" (PCDATA) string. Any codes
 * which cannot be represented by a 7-bit character are replaced by their
 * decimal numeric character entity e.g. code 160 (non-breaking space) is
 * represented as &#160;.
 */
pcdata_generation( [], Plus, Plus ).
pcdata_generation( [Char|Chars], Plus, Minus ) :-
	( Char =< 127 ->
		pcdata_7bit( Char, Plus, Mid )
	; otherwise ->
		pcdata_8bits_plus( Char, Plus, Mid )
	),
	pcdata_generation( Chars, Mid, Minus ).

/* pcdata_7bit(+Char) represents the ascii character set in its
 * simplest format, using the character entities &amp; &quot; &lt; and &gt;
 * which are common to both XML and HTML. The numeric entity &#39; is used in
 * place of &apos;, because browsers don't recognize it in HTML.
 */
pcdata_7bit( 9 ) --> [9].
pcdata_7bit( 10 ) --> [10].
pcdata_7bit( 11 ) --> "".
pcdata_7bit( 12 ) --> "".
pcdata_7bit( 13 ) --> "".
pcdata_7bit( 14 ) --> "".
pcdata_7bit( 15 ) --> "".
pcdata_7bit( 16 ) --> "".
pcdata_7bit( 17 ) --> "".
pcdata_7bit( 18 ) --> "".
pcdata_7bit( 19 ) --> "".
pcdata_7bit( 20 ) --> "".
pcdata_7bit( 21 ) --> "".
pcdata_7bit( 22 ) --> "".
pcdata_7bit( 23 ) --> "".
pcdata_7bit( 24 ) --> "".
pcdata_7bit( 25 ) --> "".
pcdata_7bit( 26 ) --> "".
pcdata_7bit( 27 ) --> "".
pcdata_7bit( 28 ) --> "".
pcdata_7bit( 29 ) --> "".
pcdata_7bit( 30 ) --> "".
pcdata_7bit( 31 ) --> "".
pcdata_7bit( 32 ) --> " ".
pcdata_7bit( 33 ) --> "!".
pcdata_7bit( 34 ) --> "&quot;".
pcdata_7bit( 35 ) --> "#".
pcdata_7bit( 36 ) --> "$".
pcdata_7bit( 37 ) --> "%".
pcdata_7bit( 38 ) --> "&amp;".
pcdata_7bit( 39 ) --> "&#39;".
pcdata_7bit( 40 ) --> "(".
pcdata_7bit( 41 ) --> ")".
pcdata_7bit( 42 ) --> "*".
pcdata_7bit( 43 ) --> "+".
pcdata_7bit( 44 ) --> ",".
pcdata_7bit( 45 ) --> "-".
pcdata_7bit( 46 ) --> ".".
pcdata_7bit( 47 ) --> "/".
pcdata_7bit( 48 ) --> "0".
pcdata_7bit( 49 ) --> "1".
pcdata_7bit( 50 ) --> "2".
pcdata_7bit( 51 ) --> "3".
pcdata_7bit( 52 ) --> "4".
pcdata_7bit( 53 ) --> "5".
pcdata_7bit( 54 ) --> "6".
pcdata_7bit( 55 ) --> "7".
pcdata_7bit( 56 ) --> "8".
pcdata_7bit( 57 ) --> "9".
pcdata_7bit( 58 ) --> ":".
pcdata_7bit( 59 ) --> ";".
pcdata_7bit( 60 ) --> "&lt;".
pcdata_7bit( 61 ) --> "=".
pcdata_7bit( 62 ) --> "&gt;".
pcdata_7bit( 63 ) --> "?".
pcdata_7bit( 64 ) --> "@".
pcdata_7bit( 65 ) --> "A".
pcdata_7bit( 66 ) --> "B".
pcdata_7bit( 67 ) --> "C".
pcdata_7bit( 68 ) --> "D".
pcdata_7bit( 69 ) --> "E".
pcdata_7bit( 70 ) --> "F".
pcdata_7bit( 71 ) --> "G".
pcdata_7bit( 72 ) --> "H".
pcdata_7bit( 73 ) --> "I".
pcdata_7bit( 74 ) --> "J".
pcdata_7bit( 75 ) --> "K".
pcdata_7bit( 76 ) --> "L".
pcdata_7bit( 77 ) --> "M".
pcdata_7bit( 78 ) --> "N".
pcdata_7bit( 79 ) --> "O".
pcdata_7bit( 80 ) --> "P".
pcdata_7bit( 81 ) --> "Q".
pcdata_7bit( 82 ) --> "R".
pcdata_7bit( 83 ) --> "S".
pcdata_7bit( 84 ) --> "T".
pcdata_7bit( 85 ) --> "U".
pcdata_7bit( 86 ) --> "V".
pcdata_7bit( 87 ) --> "W".
pcdata_7bit( 88 ) --> "X".
pcdata_7bit( 89 ) --> "Y".
pcdata_7bit( 90 ) --> "Z".
pcdata_7bit( 91 ) --> "[".
pcdata_7bit( 92 ) --> [92].
pcdata_7bit( 93 ) --> "]".
pcdata_7bit( 94 ) --> "^".
pcdata_7bit( 95 ) --> "_".
pcdata_7bit( 96 ) --> "&#96;".
pcdata_7bit( 97 ) --> "a".
pcdata_7bit( 98 ) --> "b".
pcdata_7bit( 99 ) --> "c".
pcdata_7bit( 100 ) --> "d".
pcdata_7bit( 101 ) --> "e".
pcdata_7bit( 102 ) --> "f".
pcdata_7bit( 103 ) --> "g".
pcdata_7bit( 104 ) --> "h".
pcdata_7bit( 105 ) --> "i".
pcdata_7bit( 106 ) --> "j".
pcdata_7bit( 107 ) --> "k".
pcdata_7bit( 108 ) --> "l".
pcdata_7bit( 109 ) --> "m".
pcdata_7bit( 110 ) --> "n".
pcdata_7bit( 111 ) --> "o".
pcdata_7bit( 112 ) --> "p".
pcdata_7bit( 113 ) --> "q".
pcdata_7bit( 114 ) --> "r".
pcdata_7bit( 115 ) --> "s".
pcdata_7bit( 116 ) --> "t".
pcdata_7bit( 117 ) --> "u".
pcdata_7bit( 118 ) --> "v".
pcdata_7bit( 119 ) --> "w".
pcdata_7bit( 120 ) --> "x".
pcdata_7bit( 121 ) --> "y".
pcdata_7bit( 122 ) --> "z".
pcdata_7bit( 123 ) --> "{".
pcdata_7bit( 124 ) --> "|".
pcdata_7bit( 125 ) --> "}".
pcdata_7bit( 126 ) --> "~".
pcdata_7bit( 127 ) --> "&#127;".

pcdata_8bits_plus( Code ) -->
	{number_codes(Code, Chars)},
	"&#", chars( Chars ), ";".


/* must_be_entity( ?Code ) where Code is the character value
 * of the form &quot;, &amp; or &lt;
 */
must_be_entity( 0'< ).		% less-than sign
must_be_entity( 0'" ).		% " quotation mark
must_be_entity( 0'& ).		% ampersand
must_be_entity( 0'> ).		% greater-than sign

/* :- include('xml_pp.pl'). */
/* xml_pp: "pretty print" an XML Document on the current output stream.
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml_pp.pl,v $
 * Revision 1.1  2002-01-31 21:13:10+00  john
 * Updated Copyright statements.
 *
 * Revision 1.0  2001-10-17 20:46:23+01  john
 * Initial revision
 *
 *
 *
 */


%:- ensure_loaded( xml_utilities ).

/* xml_pp( +XMLDocument ) "pretty prints" XMLDocument on the current
 * output stream.
 */
xml_pp( xml(Attributes, Document) ) :-
	write( 'xml( ' ), pp_attributes( Attributes, "" ), put( 0', ), nl,
	xml_pp_list( Document, "	" ),
	format( ' ).~n', [] ).
xml_pp( malformed(Attributes, Document) ) :-
	write( 'malformed( ' ), pp_attributes( Attributes, "" ), put( 0', ), nl,
	xml_pp_list( Document, "	" ),
	format( ' ).~n', [] ).

xml_pp_indented( [], Indent ) :-
	format( '~s[]', [Indent] ).
xml_pp_indented( List, Indent ) :-
	List = [_|_],
	format( '~s', [Indent] ),
	xml_pp_list( List, Indent ).
xml_pp_indented( comment(Text), Indent ) :-
	format( '~scomment(', [Indent] ), pp_string(Text), put( 0') ).
xml_pp_indented( namespace(URI,Prefix,Element), Indent ) :-
	format( '~snamespace( ~q, "~s",~n', [Indent,URI,Prefix] ),
	xml_pp_indented( Element, [0'	|Indent] ),
	format( '~n~s)', [[0'	|Indent]] ).
xml_pp_indented( element(Tag,Attributes,Contents), Indent ) :-
	format( '~selement( ~q,~n', [Indent,Tag] ),
	pp_attributes( Attributes, [0'	|Indent] ), put(0',), nl,
	xml_pp_list( Contents, [0'	|Indent] ), write( ' )' ).
xml_pp_indented( instructions(Target, Processing), Indent ) :-
	format( '~sinstructions( ~q, ', [Indent,Target] ),
	pp_string(Processing), put( 0') ).
xml_pp_indented( doctype(Name, DoctypeId), Indent ) :-
	format( '~sdoctype( ~q, ', [Indent,Name] ),
	xml_pp_indented( DoctypeId, [0'	|Indent] ),
	write( ' )' ).
xml_pp_indented( cdata(CData), Indent ) :-
	format( '~scdata(', [Indent] ), pp_string(CData), put( 0') ).
xml_pp_indented( pcdata(PCData), Indent ) :-
	format( '~spcdata(', [Indent] ), pp_string(PCData), put( 0') ).
xml_pp_indented( public(URN,URL), _Indent ) :-
	format( 'public( "~s", "~s" )', [URN,URL] ).
xml_pp_indented( system(URL), _Indent ) :-
	format( 'system( "~s" )', [URL] ).
xml_pp_indented( local, _Indent ) :-
	write( local ).
xml_pp_indented( out_of_context(Tag), Indent ) :-
	format( '~s/* SYNTAX ERROR */ out_of_context( ~q )', [Indent,Tag] ).
xml_pp_indented( unparsed(String), Indent ) :-
	format( '~s/* SYNTAX ERROR */ unparsed( ', [Indent] ),
	pp_string(String), put( 0') ).

xml_pp_list( [], Indent ) :-
	format( '~s[]', [Indent] ).
xml_pp_list( [H|T], Indent ) :-
	format( '~s[~n', [Indent] ),
	xml_pp_indented( H, Indent ),
	xml_pp_list1( T, Indent ),
	format( '~s]', [Indent] ).

xml_pp_list1( [], _Indent ) :-
	nl.
xml_pp_list1( [H|T], Indent ) :-
	put( 0', ), nl,
	xml_pp_indented( H, Indent ),
	xml_pp_list1( T, Indent ).

pp_attributes( [], Indent ) :-
	format( '~s[]', [Indent] ).
pp_attributes( [Attribute|Attributes], Indent ) :-
	format( '~s[', [Indent] ),
	pp_attributes1( Attributes, Attribute ),
	put( 0'] ).

pp_attributes1( [], Name=Value ) :-
	format( '~q=', [Name] ), pp_string( Value ).
pp_attributes1( [H|T], Name=Value ) :-
	format( '~q=', [Name] ), pp_string( Value ), write( ', ' ),
	pp_attributes1( T, H ).

/* :- include('xml_utilities.pl'). */
/* XML Utilities
 *
 * Copyright (C) 2001, 2002 Binding Time Limited
 * 
 * TERMS AND CONDITIONS:
 *
 * This program is offered free of charge, as unsupported source code. You may
 * use it, copy it, distribute it, modify it or sell it without restriction. 
 * 
 * We hope that it will be useful to you, but it is provided "as is" without
 * any warranty express or implied, including but not limited to the warranty
 * of non-infringement and the implied warranties of merchantability and fitness
 * for a particular purpose.
 * 
 * Binding Time Limited will not be liable for any damages suffered by you as
 * a result of using the Program. In no event will Binding Time Limited be
 * liable for any special, indirect or consequential damages or lost profits
 * even if Binding Time Limited has been advised of the possibility of their
 * occurrence. Binding Time Limited will not be liable for any third party
 * claims against you.
 *
 * History:
 * $Log: xml_utilities.pl,v $
 * Revision 1.2  2002-01-31 21:02:20+00  john
 * Changed to 'canonical' (lowercase) character encoding names.
 *
 * Revision 1.1  2001-11-04 17:26:22+00  john
 * Added uppercase variants of the supported character encoding names.
 *
 * Revision 1.0  2001-10-17 20:46:23+01  john
 * Initial revision
 *
 *
 *
 */

% Entity and Namespace map operations: these maps are usually quite small, so a
% linear list lookup is okay. They could be substituted by a logarithmic data
% structure - in extremis.

/* empty_map( ?Map ) is true if Map is a null map.
 */
empty_map( [] ).

/* map_member( +Key, +Map, ?Data ) is true if Map is a ordered map structure
 * which records the pair Key-Data. Key must be ground.
 */
map_member( Key0, [Key1-Data1|Rest], Data0 ) :-
	( Key0 == Key1 ->
		Data0 = Data1
	; Key0 @> Key1 ->
		map_member( Key0, Rest, Data0 )
	).

/* map_store( +Map0, +Key, +Data, ?Map1 ) is true if Map0 is an ordered map
 * structure, Key must be ground, and Map1 is identical to Map0 except that
 * the pair Key-Data is recorded by Map1.
 */
map_store( [], Key, Data, [Key-Data] ).
map_store( [Key0-Data0|Map0], Key, Data, Map ) :-
	( Key == Key0 ->
		Map = [Key-Data|Map0]
	; Key @< Key0 ->
		Map = [Key-Data,Key0-Data0|Map0]
	; otherwise -> % >
		Map = [Key0-Data0|Map1],
		map_store( Map0, Key, Data, Map1 )
	).

/* context(?Element, ?PreserveSpace, ?CurrentNS, ?DefaultNS, ?Entities, ?Namespaces )
 * is an ADT hiding the "state" arguments for XML Acquisition
 */
initial_context(
		Controls,
		context(void,PreserveSpace,'','',Entities,Empty)
		) :-
	empty_map( Empty ),
	( member( extended_characters(false), Controls ) ->
		Entities = Empty
	; otherwise ->
		extended_character_entities(Entities)
	),
	( member( format(false), Controls ) ->
		PreserveSpace = true
	; otherwise ->
		PreserveSpace = false
	).

context_update( current_namespace, Context0, URI, Context1 ) :-
	Context0 = context(Element,Preserve,_Current,Default,Entities,Namespaces),
	Context1 = context(Element,Preserve,URI,Default,Entities,Namespaces).
context_update( element, Context0, Tag, Context1 ) :-
	Context0 = context(_Element,Preserve,Current,Default,Entities,Namespaces),
	Context1 = context(tag(Tag),Preserve,Current,Default,Entities,Namespaces).
context_update( default_namespace, Context0, URI, Context1 ):-
	Context0 = context(Element,Preserve,Current,_Default,Entities,Namespaces),
	Context1 = context(Element,Preserve,Current,URI,Entities,Namespaces).
context_update( space_preserve, Context0, Boolean, Context1 ):-
	Context0 = context(Element,_Preserve,Current,Default,Entities,Namespaces),
	Context1 = context(Element,Boolean,Current,Default,Entities,Namespaces).
context_update( ns_prefix(Prefix), Context0, URI, Context1 ) :-
	Context0 = context(Element,Preserve,Current,Default,Entities,Namespaces0),
	Context1 = context(Element,Preserve,Current,Default,Entities,Namespaces1),
	map_store( Namespaces0, Prefix, URI, Namespaces1 ).
context_update( entity(Name), Context0, String, Context1 ) :-
	Context0 = context(Element,Preserve,Current,Default,Entities0,Namespaces),
	Context1 = context(Element,Preserve,Current,Default,Entities1,Namespaces),
	map_store( Entities0, Name, String, Entities1 ).

current_tag( Context, Tag ) :-
	Context = context(tag(Tag),_Preserve,_Current,_Default,_Entities,_Namespaces).

current_namespace( Context, Current ) :-
	Context = context(_Element,_Preserve,Current,_Default,_Entities,_Namespaces).

default_namespace( Context, Default ) :-
	Context = context(_Element,_Preserve,_Current,Default,_Entities,_Namespaces).

space_preserve( Context ) :-
	Context = context(_Element,true,_Current,_Default,_Entities,_Namespaces).

specific_namespace( Prefix, Context, URI ) :-
	Context = context(_Element,_Preserve,_Current,_Default,_Entities,Namespaces),
	map_member( Prefix, Namespaces, URI ).

defined_entity( Reference, Context, String ) :-
	Context = context(_Element,_Preserve,_Current,_Default,Entities,_Namespaces),
	map_member( Reference, Entities, String ).
	
close_context( Context, Terms, WellFormed ) :-
	Context = context(Element,_Preserve,_Current,_Default,_Entities,_Namespaces),
	close_context1( Element, Terms, WellFormed ).

close_context1( void, [], true ).
close_context1( tag(TagChars), [out_of_context(Tag)], false ) :-
	atom_chars( Tag, TagChars ).

void_context(
	context(void,_Preserve,_Current,_Default,_Entities,_Namespaces)
	).

/* pp_string( +String ) prints String onto the current output stream.
 * If String contains only 7-bit chars it is printed in shorthand quoted
 * format, otherwise it is written as a list.
 * If your Prolog uses " to delimit a special string type, just use write/1.
 */
pp_string( Chars ) :-
	( member( Char, Chars ),
	  Char > 255 ->
		write( Chars )
	; otherwise ->
		put_quote,
		pp_string1( Chars ),
		put_quote
	).

pp_string1( [] ).
pp_string1( [Char|Chars] ) :-
	( Char =:= """"  -> put_quote
	; true
	),
	put( Char ),
	pp_string1( Chars ).

put_quote :-
	put( 0'" ). % "

xml_declaration_attributes_valid( [] ).
xml_declaration_attributes_valid( [Name=Value|Attributes] ) :-
	xml_declaration_attribute_valid( Name, Value ),
	xml_declaration_attributes_valid( Attributes ).

xml_declaration_attribute_valid( Name, Value ) :-
	lowercase( Value, Lowercase ),
	canonical_xml_declaration_attribute( Name, Lowercase ).

canonical_xml_declaration_attribute( version, "1.0" ).
canonical_xml_declaration_attribute( standalone, "yes" ).
canonical_xml_declaration_attribute( standalone, "no" ).
canonical_xml_declaration_attribute( encoding, "utf-8" ).
canonical_xml_declaration_attribute( encoding, "utf-16" ).
canonical_xml_declaration_attribute( encoding, "ascii" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-1" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-2" ).
canonical_xml_declaration_attribute( encoding, "iso-8859-15" ).
canonical_xml_declaration_attribute( encoding, "windows-1252" ).

/* lowercase( +MixedCase, ?Lowercase ) holds when Lowercase and MixedCase are
 * lists of character codes, and Lowercase is identical to MixedCase with
 * every uppercase character replaced by its lowercase equivalent.
 */
lowercase( [], [] ).
lowercase( [Char|Chars], [Lower|LowerCase] ) :-
	( Char >= "A", Char =< "Z" ->
		Lower is Char + "a" - "A"
	; otherwise ->
		Lower = Char
	),
	lowercase( Chars, LowerCase ).

extended_character_entities( [
	"Aacute"-[193],		% latin capital letter A with acute,
	"aacute"-[225],		% latin small letter a with acute,
	"Acirc"-[194],		% latin capital letter A with circumflex,
	"acirc"-[226],		% latin small letter a with circumflex,
	"acute"-[180],		% acute accent = spacing acute,
	"AElig"-[198],		% latin capital letter AE
	"aelig"-[230],		% latin small letter ae
	"Agrave"-[192],		% latin capital letter A with grave
	"agrave"-[224],		% latin small letter a with grave
	"alefsym"-[8501],	% alef symbol = first transfinite cardinal,
	"Alpha"-[913],		% greek capital letter alpha, U+0391
	"alpha"-[945],		% greek small letter alpha,
	"and"-[8743],		% logical and = wedge, U+2227 ISOtech
	"ang"-[8736],		% angle, U+2220 ISOamso
	"Aring"-[197],		% latin capital letter A with ring above
	"aring"-[229],		% latin small letter a with ring above
	"asymp"-[8776],		% almost equal to = asymptotic to,
	"Atilde"-[195],		% latin capital letter A with tilde,
	"atilde"-[227],		% latin small letter a with tilde,
	"Auml"-[196],		% latin capital letter A with diaeresis,
	"auml"-[228],		% latin small letter a with diaeresis,
	"bdquo"-[8222],		% double low-9 quotation mark, U+201E NEW
	"Beta"-[914],		% greek capital letter beta, U+0392
	"beta"-[946],		% greek small letter beta, U+03B2 ISOgrk3
	"brvbar"-[166],		% broken bar = broken vertical bar,
	"bull"-[8226],		% bullet = black small circle,
	"cap"-[8745],		% intersection = cap, U+2229 ISOtech
	"Ccedil"-[199],		% latin capital letter C with cedilla,
	"ccedil"-[231],		% latin small letter c with cedilla,
	"cedil"-[184],		% cedilla = spacing cedilla, U+00B8 ISOdia>
	"cent"-[162],		% cent sign, U+00A2 ISOnum>
	"Chi"-[935],		% greek capital letter chi, U+03A7
	"chi"-[967],		% greek small letter chi, U+03C7 ISOgrk3
	"circ"-[710],		% modifier letter circumflex accent,
	"clubs"-[9827],		% black club suit = shamrock,
	"cong"-[8773],		% approximately equal to, U+2245 ISOtech
	"copy"-[169],		% copyright sign, U+00A9 ISOnum>
	"crarr"-[8629],		% downwards arrow with corner leftwards
	"cup"-[8746],		% union = cup, U+222A ISOtech
	"curren"-[164],		% currency sign, U+00A4 ISOnum>
	"dagger"-[8224],	% dagger, U+2020 ISOpub
	"Dagger"-[8225],	% double dagger, U+2021 ISOpub
	"darr"-[8595],		% downwards arrow, U+2193 ISOnum
	"dArr"-[8659],		% downwards double arrow, U+21D3 ISOamsa
	"deg"-[176],		% degree sign, U+00B0 ISOnum>
	"Delta"-[916],		% greek capital letter delta,
	"delta"-[948],		% greek small letter delta,
	"diams"-[9830],		% black diamond suit, U+2666 ISOpub
	"divide"-[247],		% division sign, U+00F7 ISOnum>
	"Eacute"-[201],		% latin capital letter E with acute,
	"eacute"-[233],		% latin small letter e with acute,
	"Ecirc"-[202],		% latin capital letter E with circumflex,
	"ecirc"-[234],		% latin small letter e with circumflex,
	"Egrave"-[200],		% latin capital letter E with grave,
	"egrave"-[232],		% latin small letter e with grave,
	"empty"-[8709],		% empty set = null set = diameter,
	"emsp"-[8195],		% em space, U+2003 ISOpub
	"ensp"-[8194],		% en space, U+2002 ISOpub
	"Epsilon"-[917],	% greek capital letter epsilon, U+0395
	"epsilon"-[949],	% greek small letter epsilon,
	"equiv"-[8801],		% identical to, U+2261 ISOtech
	"Eta"-[919],		% greek capital letter eta, U+0397
	"eta"-[951],		% greek small letter eta, U+03B7 ISOgrk3
	"ETH"-[208],		% latin capital letter ETH, U+00D0 ISOlat1>
	"eth"-[240],		% latin small letter eth, U+00F0 ISOlat1>
	"Euml"-[203],		% latin capital letter E with diaeresis,
	"euml"-[235],		% latin small letter e with diaeresis,
	"euro"-[8364],		% euro sign, U+20AC NEW
	"exist"-[8707],		% there exists, U+2203 ISOtech
	"fnof"-[402],		% latin small f with hook = function
	"forall"-[8704],	% for all, U+2200 ISOtech
	"frac12"-[189],		% vulgar fraction one half
	"frac14"-[188],		% vulgar fraction one quarter
	"frac34"-[190],		% vulgar fraction three quarters
	"frasl"-[8260],		% fraction slash, U+2044 NEW
	"Gamma"-[915],		% greek capital letter gamma,
	"gamma"-[947],		% greek small letter gamma,
	"ge"-[8805],		% greater-than or equal to,
	"harr"-[8596],		% left right arrow, U+2194 ISOamsa
	"hArr"-[8660],		% left right double arrow,
	"hearts"-[9829],	% black heart suit = valentine,
	"hellip"-[8230],	% horizontal ellipsis = three dot leader,
	"Iacute"-[205],		% latin capital letter I with acute,
	"iacute"-[237],		% latin small letter i with acute,
	"Icirc"-[206],		% latin capital letter I with circumflex,
	"icirc"-[238],		% latin small letter i with circumflex,
	"iexcl"-[161],		% inverted exclamation mark, U+00A1 ISOnum>
	"Igrave"-[204],		% latin capital letter I with grave,
	"igrave"-[236],		% latin small letter i with grave,
	"image"-[8465],		% blackletter capital I = imaginary part,
	"infin"-[8734],		% infinity, U+221E ISOtech
	"int"-[8747],		% integral, U+222B ISOtech
	"Iota"-[921],		% greek capital letter iota, U+0399
	"iota"-[953],		% greek small letter iota, U+03B9 ISOgrk3
	"iquest"-[191],		% inverted question mark
	"isin"-[8712],		% element of, U+2208 ISOtech
	"Iuml"-[207],		% latin capital letter I with diaeresis,
	"iuml"-[239],		% latin small letter i with diaeresis,
	"Kappa"-[922],		% greek capital letter kappa, U+039A
	"kappa"-[954],		% greek small letter kappa,
	"Lambda"-[923],		% greek capital letter lambda,
	"lambda"-[955],		% greek small letter lambda,
	"lang"-[9001],		% left-pointing angle bracket = bra,
	"laquo"-[171],		% left-pointing double angle quotation mark
	"larr"-[8592],		% leftwards arrow, U+2190 ISOnum
	"lArr"-[8656],		% leftwards double arrow, U+21D0 ISOtech
	"lceil"-[8968],		% left ceiling = apl upstile,
	"ldquo"-[8220],		% left double quotation mark,
	"le"-[8804],		% less-than or equal to, U+2264 ISOtech
	"lfloor"-[8970],	% left floor = apl downstile,
	"lowast"-[8727],	% asterisk operator, U+2217 ISOtech
	"loz"-[9674],		% lozenge, U+25CA ISOpub
	"lrm"-[8206],		% left-to-right mark, U+200E NEW RFC 2070
	"lsaquo"-[8249],	% single left-pointing angle quotation mark,
	"lsquo"-[8216],		% left single quotation mark,
	"macr"-[175],		% macron = spacing macron = overline
	"mdash"-[8212],		% em dash, U+2014 ISOpub
	"micro"-[181],		% micro sign, U+00B5 ISOnum>
	"middot"-[183],		% middle dot = Georgian comma
	"minus"-[8722],		% minus sign, U+2212 ISOtech
	"Mu"-[924],			% greek capital letter mu, U+039C
	"mu"-[956],			% greek small letter mu, U+03BC ISOgrk3
	"nabla"-[8711],		% nabla = backward difference,
	"nbsp"-[160],		% no-break space = non-breaking space,
	"ndash"-[8211],		% en dash, U+2013 ISOpub
	"ne"-[8800],		% not equal to, U+2260 ISOtech
	"ni"-[8715],		% contains as member, U+220B ISOtech
	"not"-[172],		% not sign, U+00AC ISOnum>
	"notin"-[8713],		% not an element of, U+2209 ISOtech
	"nsub"-[8836],		% not a subset of, U+2284 ISOamsn
	"Ntilde"-[209],		% latin capital letter N with tilde,
	"ntilde"-[241],		% latin small letter n with tilde,
	"Nu"-[925],			% greek capital letter nu, U+039D
	"nu"-[957],			% greek small letter nu, U+03BD ISOgrk3
	"Oacute"-[211],		% latin capital letter O with acute,
	"oacute"-[243],		% latin small letter o with acute,
	"Ocirc"-[212],		% latin capital letter O with circumflex,
	"ocirc"-[244],		% latin small letter o with circumflex,
	"OElig"-[338],		% latin capital ligature OE,
	"oelig"-[339],		% latin small ligature oe, U+0153 ISOlat2
	"Ograve"-[210],		% latin capital letter O with grave,
	"ograve"-[242],		% latin small letter o with grave,
	"oline"-[8254],		% overline = spacing overscore,
	"Omega"-[937],		% greek capital letter omega,
	"omega"-[969],		% greek small letter omega,
	"Omicron"-[927],	% greek capital letter omicron, U+039F
	"omicron"-[959],	% greek small letter omicron, U+03BF NEW
	"oplus"-[8853],		% circled plus = direct sum,
	"or"-[8744],		% logical or = vee, U+2228 ISOtech
	"ordf"-[170],		% feminine ordinal indicator, U+00AA ISOnum>
	"ordm"-[186],		% masculine ordinal indicator,
	"Oslash"-[216],		% latin capital letter O with stroke
	"oslash"-[248],		% latin small letter o with stroke,
	"Otilde"-[213],		% latin capital letter O with tilde,
	"otilde"-[245],		% latin small letter o with tilde,
	"otimes"-[8855],	% circled times = vector product,
	"Ouml"-[214],		% latin capital letter O with diaeresis,
	"ouml"-[246],		% latin small letter o with diaeresis,
	"para"-[182],		% pilcrow sign = paragraph sign,
	"part"-[8706],		% partial differential, U+2202 ISOtech
	"permil"-[8240],	% per mille sign, U+2030 ISOtech
	"perp"-[8869],		% up tack = orthogonal to = perpendicular,
	"Phi"-[934],		% greek capital letter phi,
	"phi"-[966],		% greek small letter phi, U+03C6 ISOgrk3
	"Pi"-[928],			% greek capital letter pi, U+03A0 ISOgrk3
	"pi"-[960],			% greek small letter pi, U+03C0 ISOgrk3
	"piv"-[982],		% greek pi symbol, U+03D6 ISOgrk3
	"plusmn"-[177],		% plus-minus sign = plus-or-minus sign,
	"pound"-[163],		% pound sign, U+00A3 ISOnum>
	"prime"-[8242],		% prime = minutes = feet, U+2032 ISOtech
	"Prime"-[8243],		% double prime = seconds = inches,
	"prod"-[8719],		% n-ary product = product sign,
	"prop"-[8733],		% proportional to, U+221D ISOtech
	"Psi"-[936],		% greek capital letter psi,
	"psi"-[968],		% greek small letter psi, U+03C8 ISOgrk3
	"radic"-[8730],		% square root = radical sign,
	"rang"-[9002],		% right-pointing angle bracket = ket,
	"raquo"-[187],		% right-pointing double angle quotation mark
	"rarr"-[8594],		% rightwards arrow, U+2192 ISOnum
	"rArr"-[8658],		% rightwards double arrow,
	"rceil"-[8969],		% right ceiling, U+2309 ISOamsc
	"rdquo"-[8221],		% right double quotation mark,
	"real"-[8476],		% blackletter capital R = real part symbol,
	"reg"-[174],		% registered sign = registered trade mark sign,
	"rfloor"-[8971],	% right floor, U+230B ISOamsc
	"Rho"-[929],		% greek capital letter rho, U+03A1
	"rho"-[961],		% greek small letter rho, U+03C1 ISOgrk3
	"rlm"-[8207],		% right-to-left mark, U+200F NEW RFC 2070
	"rsaquo"-[8250],	% single right-pointing angle quotation mark,
	"rsquo"-[8217],		% right single quotation mark,
	"sbquo"-[8218],		% single low-9 quotation mark, U+201A NEW
	"Scaron"-[352],		% latin capital letter S with caron,
	"scaron"-[353],		% latin small letter s with caron,
	"sdot"-[8901],		% dot operator, U+22C5 ISOamsb
	"sect"-[167],		% section sign, U+00A7 ISOnum>
	"shy"-[173],		% soft hyphen = discretionary hyphen,
	"Sigma"-[931],		% greek capital letter sigma,
	"sigma"-[963],		% greek small letter sigma,
	"sigmaf"-[962],		% greek small letter final sigma,
	"sim"-[8764],		% tilde operator = varies with = similar to,
	"spades"-[9824],	% black spade suit, U+2660 ISOpub
	"sub"-[8834],		% subset of, U+2282 ISOtech
	"sube"-[8838],		% subset of or equal to, U+2286 ISOtech
	"sum"-[8721],		% n-ary sumation, U+2211 ISOamsb
	"sup"-[8835],		% superset of, U+2283 ISOtech
	"sup1"-[185],		% superscript one = superscript digit one,
	"sup2"-[178],		% superscript two = superscript digit two
	"sup3"-[179],		% superscript three = superscript digit three
	"supe"-[8839],		% superset of or equal to,
	"szlig"-[223],		% latin small letter sharp s = ess-zed,
	"Tau"-[932],		% greek capital letter tau, U+03A4
	"tau"-[964],		% greek small letter tau, U+03C4 ISOgrk3
	"there4"-[8756],	% therefore, U+2234 ISOtech
	"Theta"-[920],		% greek capital letter theta,
	"theta"-[952],		% greek small letter theta,
	"thetasym"-[977],	% greek small letter theta symbol,
	"thinsp"-[8201],	% thin space, U+2009 ISOpub
	"THORN"-[222],		% latin capital letter THORN,
	"thorn"-[254],		% latin small letter thorn with,
	"tilde"-[732],		% small tilde, U+02DC ISOdia
	"times"-[215],		% multiplication sign, U+00D7 ISOnum>
	"trade"-[8482],		% trade mark sign, U+2122 ISOnum
	"Uacute"-[218],		% latin capital letter U with acute,
	"uacute"-[250],		% latin small letter u with acute,
	"uarr"-[8593],		% upwards arrow, U+2191 ISOnum
	"uArr"-[8657],		% upwards double arrow, U+21D1 ISOamsa
	"Ucirc"-[219],		% latin capital letter U with circumflex,
	"ucirc"-[251],		% latin small letter u with circumflex,
	"Ugrave"-[217],		% latin capital letter U with grave,
	"ugrave"-[249],		% latin small letter u with grave,
	"uml"-[168],		% diaeresis = spacing diaeresis,
	"upsih"-[978],		% greek upsilon with hook symbol,
	"Upsilon"-[933],	% greek capital letter upsilon,
	"upsilon"-[965],	% greek small letter upsilon,
	"Uuml"-[220],		% latin capital letter U with diaeresis,
	"uuml"-[252],		% latin small letter u with diaeresis,
	"weierp"-[8472],	% script capital P = power set
	"Xi"-[926],			% greek capital letter xi, U+039E ISOgrk3
	"xi"-[958],			% greek small letter xi, U+03BE ISOgrk3
	"Yacute"-[221],		% latin capital letter Y with acute,
	"yacute"-[253],		% latin small letter y with acute,
	"yen"-[165],		% yen sign = yuan sign, U+00A5 ISOnum>
	"yuml"-[255],		% latin small letter y with diaeresis,
	"Yuml"-[376],		% latin capital letter Y with diaeresis,
	"Zeta"-[918],		% greek capital letter zeta, U+0396
	"zeta"-[950],		% greek small letter zeta, U+03B6 ISOgrk3
	"zwj"-[8205],		% zero width joiner, U+200D NEW RFC 2070
	"zwnj"-[8204]		% zero width non-joiner,
	] ).

% The following code is for Quintus Prolog primarily. Some of these
% predicates are built-in to SWI, LPA etc.

/* member( ?Element, ?List ) holds when Element is a member of List.
 */
/*
member( H, [H|_] ).
member( H, [_|T] ):-
    member( H, T ).
*/
/* select( ?Element, ?List0, ?List1 ) is true if List1 is equal to List0
 * with Element removed.
 */
select( H, [H|T], T ).
select( Element, [H|T0], [H|T1] ):-
    select( Element, T0, T1 ).

/* is_list( +List ) holds when List is a list.
 */
is_list( List ) :-
	nonvar( List ),
	is_list1( List ).

is_list1( [] ).
is_list1( [_|_] ).

/* atom_codes/2, number_codes/2 and throw/1 are ISO predicates.
 */
/*
atom_codes( Atom, Codes ) :-
	atom_chars( Atom, Codes ).

number_codes( Number, Codes ) :-
	number_chars( Number, Codes ).

throw( Exception ) :-
	raise_exception( Exception ).
*/
otherwise.


