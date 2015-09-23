% Invoke an editor then return to Prolog.
% examples
%	?- redo(foo).
%	?- redo([foo,bar]).
% The predicates you name do not have to exist.
% [BUG]	If you redo a predicate and delete it all from the temp file,
%	it will still exist afterwards.

% ****** NEEDS ANOTHER FILE ******
% *                              *
% *      append.pl               *
% *                              *
% ******                    ******

% Change the clauses below to the names of your favourite editor
% and your favourite temporary file name.
% Note: Double quotes, and no spaces or punctuation.

editor("ue").
tempfile("prolog.tmp").


redo(Predicate) :-
	nonvar(Predicate),
	create_output_file,
	listall(Predicate),
	edit_the_file.

create_output_file :-
	tempfile(File_chars),
	name(File,File_chars),
        tell(File).

listall([Predicate|More_predicates]) :-	% Listall has to be able to cope with
	listing(Predicate),		% an argument which is a predicate
	listall(More_predicates).	% name or a list of predicate names.
					% Hence the odd clause about
listall([]).				% Predicate not being [] (below)

listall(Predicate) :-
	Predicate \== [],
        listing(Predicate).

edit_the_file :-
        told,
	editor(Editor_chars),
	tempfile(File_chars),
	append(Editor_chars,[32|File_chars],Command),	% 32 is <space>
        shell(Command),
	name(File,File_chars),
        reconsult(File),
	append("rm ",File_chars,Remove),
	shell(Remove).
