%   File   : HELPER.PL
%   Author : R.A.O'Keefe
%   Updated: 16 December 1983
%   Purpose: Print extracts from Help files.
%   Needs  : try_hard_to_see/4 from Util:TrySee.Pl

%   give_help(Area, Topic)
%	-- looks for an assertion help_file(Area, FileName, Delimiter)
%	-- which you must supply.  It then tries hard to open the file
%	-- with default extensions "", "HLP", and "PL" and with device
%	-- defaults "DSK", "MEC", "UTIL", and "PLL".  If the file can't
%	-- be found, or if there is no help_file assertion, it gives an
%	-- apology.  Otherwise it searches the file for the sequence
%	-- Delimiter Topic ".", e.g. #help.


:- public
	give_help/0,				% ->
	give_help/1,				% Area ->
	give_help/2.				% Area x Topic ->

:- mode
	give_help,				%  list of areas.
	give_help(+),				%  list of topics in an area.
	give_help(+, +),			%  help on a specific topic.
	    find_help(+, +),			%  find and type a topic of list
		read_after_delimiter(+),	%  find "#" or return end_of_file
		find_help(+, +, +),	    	%  check a list of topics
		    topic_is_among(+, +),	%  member on commas instead of dots
		    type_until_delimiter(+).	%  display body of a Note.



give_help :-
	write('Help is available in the following areas:'), nl,
	help_file(Area, _, _),
	tab(4), writeq(Area), nl,
	fail.
give_help :-
	write('Call give_help(Area) for a list of topics in an Area.'), nl,
	write('Call give_help('), write('Area'),
	write(',Topic) for help about a specific topic.'), nl.


give_help(Area) :-
	write('The topics in '), writeq(Area),
	write(' for which help is available are:'), nl,
	give_help(Area, Topic),
	write('Call give_help('), writeq(Area),
	write(',Topic) for help about a specific topic.'), nl.


give_help(Area, Topic) :-
	(   help_file(Area, HelpName, Delimiter),
		call(Delim is Delimiter)
	;   atom(Area),
		Delim is "#", HelpName = Area
	),
	(   try_hard_to_see(HelpName, [mec,util,pll], [hlp,pl], HelpFile), !,
		seeing(Old),
		see(HelpFile),
		find_help(Delim, Topic),
		seen,
		see(Old)
	;    write('** No help is available on '), writeq(Topic),
		write(' in '), writeq(Area), nl
	),  !.


	find_help(Delimiter, Topic) :-
		var(Topic), !,
		read_after_delimiter(Delimiter, Topics),
		(   Topics = end_of_file
		;   tab(4), write(Topics), nl, fail
		).
	find_help(Delimiter, Topic) :-
		read_after_delimiter(Delimiter, Topics),
		find_help(Topics, Topic, Delimiter).

		find_help(end_of_file, Topic, _) :- !,
			seeing(HelpFile),
			write('** No help is available on '), writeq(Topic),
			write(' in file '), writeq(HelpFile), nl.
		find_help(Topics, Topic, Delimiter) :-
			topic_is_among(Topic, Topics), !,
			type_until_delimiter(Delimiter).


		topic_is_among(Topic, (Topics1,Topics2)) :- !,
			(   topic_is_among(Topic, Topics1)
			;   topic_is_among(Topic, Topics2)
			).
		topic_is_among(Topic, (Topics1;Topics2)) :- !,
			(   topic_is_among(Topic, Topics1)
			;   topic_is_among(Topic, Topics2)
			).
		topic_is_among(Topic, Topic).


		read_after_delimiter(Delimiter, Topics) :-
			repeat,
				get0(Character),
				(   Character = 26, !, Topics = end_of_file
				;   Character = Delimiter, read(Topics)
				).


		type_until_delimiter(Delimiter) :-
			get0(C),
			C =\= 26, C =\= Delimiter, 
			put(C), !,
			type_until_delimiter(Delimiter).
		type_until_delimiter(Delimiter).
	

