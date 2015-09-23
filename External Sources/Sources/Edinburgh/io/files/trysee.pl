%   File   : TRYSEE.PL
%   Author : R.A.O'Keefe
%   Updated: 16 December 1983
%   Purpose: Search through several directories/extensions to find a file
%   Needs  : no other file

%   try_hard_to_see(FileName, DeviceDefaults, ExtensionDefaults)
%	-- tries all the Extension and Device defaults (varying the
%	-- extensions first) until it succeeds in 'see'ing the file,
%	-- and fails if the file cannot be found.
%   try_hard_to_see(FileName, Devs, Exts, FileFound)
%	-- is like try_hard_to_see/3, but doesn't open the file, it
%	-- just binds FileFound to it.  If no file can be found, it
%	-- just fails.  The other version prints a message.

%	Complete rewrite for Nip/Unix
%	Ken Johnson 14-8-87

try_hard_to_see(File_name, Directories, Extensions) :-
	try_hard_to_see(File_name, Directories, Extensions, File_found),
	!,
	see(File_found).

try_hard_to_see(File_name, Directories, Extensions, File_found) :-
	possible_file(File_name, Directories, Extensions, File_found),
	exists(File_found).

% possible_file(Title,Dirs,Extns,-Candidate)
% generates on back tracking all combinations of
% Dir + Title + Ext
% with restrictions:
% If you specify / in the file name it will not try any Dirs
% If you specify . it won't try any Exts

possible_file(Title, Dirs, _, Candidate) :-
	name_includes(0'., Title),
	!,
	(
		Candidate = Title
	;
	     \+ name_includes(0'/, Title),
		member(D, Dirs),
		fconcat(D, Title, 0'/, Candidate)
	).


possible_file(Title, _, Exts, Candidate) :-
	name_includes(0'/, Title),
	!,
	(
		Candidate = Title
	;
		member(E, Exts),
		fconcat(Title, E, 0'., Candidate)
	).

possible_file(Title, Dirs, Exts, Candidate) :-
	(					% Note that putting this
		D_title = Title			% "member" call above the
	;					% other causes the system
		member(D,Dirs),			% to search extensions first
		fconcat(D,Title,0'/,D_title)	% and directories second
	),
	(
		Candidate = D_title		% Try Directory + Title
	;
		member(E,Exts),			% Try Dir + Title + Ext
		fconcat(D_title,E,0'.,Candidate)
	).


name_includes(Char,Name) :-
	name(Name,String),
	member(Char,String).

% fconcat/4	for concatenating file names
%		A and B are atoms, Ch is an ascii code
%		The concatenation ensures that the code Ch occurs once
%		between A and B,  e.g.
%		fconcat( 'bar', '/foo', 0'/, 'bar/foo').
%		fconcat( 'bar/', 'foo', 0'/, 'bar/foo').
%		fconcat( 'bar', 'foo',  0'/, 'bar/foo').

fconcat(A,B,Ch,Out) :-
	name(A,As),
	name(B,Bs),
	(
		Bs = [Ch|_],
		append(As,Bs,Os)
	;
		append(_,[Ch],As),		% Call mode append(-,+,+)
		append(As,Bs,Os)		% Call mode append(+,+,-)
	;
		append(As,[Ch|Bs],Os)
	),
	!,
	name(Out,Os).

fconcat(A,B,Out) :-
	name(A,As),
	name(B,Bs),
	append(As,Bs,Os),
	name(Out,Os).

append([],X,X).

append([H|T],U,[H|V]) :-
	append(T,U,V).

member(X,[X|_]).

member(X,[_|T]) :-
	member(X,T).

