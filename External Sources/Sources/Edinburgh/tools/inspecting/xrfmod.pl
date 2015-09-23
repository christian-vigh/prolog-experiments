%   File   : XRFMOD.PL
%   Author : Richard A. O'Keefe
%   Updated: 19 September 1984
%   Purpose: Update the declarations in Prolog source files.

/*  This file replaces a TECO program.
    The idea is that the cross-referencer can create import-export
    declarations for Prolog source files, and we can use this to
    maintain such declarations in the source files automatically.
    If a source file contains
		%here%
    we replace that string by the declarations, in the layout
		%declarations%\n
		the actual declarations\n
	        %end%
    If the source file contains
		%declarations% ... %end%
    we replace that text by the new block.  If it contains neither
    %here% nor %declarations%, or if it contains %declarations% but
    that is not followed by %end%, an error message will be printed
    and the file will not be changed.

    What update_declarations(File) does is this.  First it creates a
    new file File.TMP (after discarding the extension if any of File).
    Then it copies characters from File to File.TMP looking for %here%
    or %declarations%.  If it finds %declarations% it will skip until
    it finds %end%.  If anything goes wrong it will print its error
    message and delete File.TMP.  If all goes well, it will write out
    the new declarations, copy the rest of File to File.TMP, and then
    it will do some complicated file juggling.

    First it deletes File.BAK, if there is such a file.
    Then it renames File to File.BAK.
    Then it renames File.TMP to File
    The point of all this juggling is to try to ensure that you won't
    lose your original file.  Something like this ought to be built in.

    I'm sorry about the amazing mess this file is in.  To some extent,
    that's what file hacking does to you, but the main problem is that
    I wrote this in a tearing hurry and didn't stop to design it.
*/

:- public
	update_declarations/3.

/* import
	do_publics/1		from 'XRFOUT.PL',
	do_imports/1		from 'XRFOUT.PL'.
*/
:- mode
	abandon_update(+),
	copy_skipped(+),
	copy_to_percent(-),
	copy_to_percent(+, -),
	finish_update(+, +, +, +),
	new_extension(+, +, -),
	skip_to_percent(+),
	try_percent(+),
	try_percent(+, +, +, -),
	update_declarations(+, +, +).


update_declarations(File, Exports, Imports) :-
	seeing(OldSee),
	telling(OldTell),
	nofileerrors,
	(   new_extension(File, "TMP", TmpFile),
	    see(File),
	    tell(TmpFile),
	    fileerrors,
	    !,
	    copy_to_percent(Which),
	    (   Which = 0, !,	%  neither 'here' nor 'declarations'
		    abandon_update('%here% nor %declarations%')
	    ;   Which = 1, !,	%  $here$ found
		    finish_update(File, TmpFile, Exports, Imports)
	    ;   Which = 2, 	%  $declaratiop
		    skip_to_percent(32), !,
		    finish_update(File, TmpFile, Exports, Imports)
	    ;    abandon_update('%end% after %declarations%')
	    )
	;   fileerrors,
	    close(File),		% ok even if File wasn't open
	    tell(user),
	    write('! problem opening '), write(File),
	    write(' or its .TMP copy'), nl
	),
	see(OldSee),
	tell(OldTell).


%   finish_update writes out the new declarations between new
%   %declarations% and %end% brackets.  It then copies the rest of
%   the original file into the temporary copy, and closes it.
%   Finally, it juggles the files around as described above.  Note
%   that this is for Dec-10 Prolog running on Bottoms-10, when you
%   do rename/2 the file has to be open, and rename/2 will close it.
%   I have no idea whether this will run under C Prolog or not, and
%   I strongly suspect that it won't.

finish_update(File, TmpFile, Exports, Imports) :-
	write('%declarations%'), nl, nl,
	do_publics(Exports),
	do_imports(Imports),
	write('%end%'),  % NO nl
	repeat,
	    get0(C),
	    ( C = 26 ; put(C), fail ),
	!,
	told,			%  close the .TMP file
	new_extension(File, "BAK", BakFile),
	nofileerrors,
	(   see(BakFile), rename(BakFile, [])
	;   true		%  Delete the backup file
	),  !,			%  if it already exists.
	fileerrors,
	see(File),    rename(File, BakFile),
	see(TmpFile), rename(TmpFile, File).


abandon_update(Error) :-
	telling(TmpFile),
	rename(TmpFile, []),		% delete the .TMP file
	seeing(File),
	seen,				% close the original
	tell(user),
	write('! Warning: '), write(File), write(' has no '),
	write(Error), write(' -- not changed.'), nl.


copy_to_percent(Which) :-
	get0(C),
	copy_to_percent(C, Which).


copy_to_percent(26, 0) :- !.		% didn't find a percent at all.
copy_to_percent(0'%, Which) :- !,
	get0(C),
	(   C = 0'h, !, try_percent("ere%", "h%", 1, Which)
	;   C = 0'd, !, try_percent("eclarations%", "d%", 2, Which)
	;   put(0'%), copy_to_percent(Which)
	).
copy_to_percent(C, Which) :-
	put(C),
	get0(D),
	copy_to_percent(D, Which).


try_percent([], _, Which, Which).
try_percent([Char|Chars], Skipped, WillBe, Which) :-
	get0(C),
	(   C = Char, !, try_percent(Chars, [Char|Skipped], WillBe, Which)
	;   copy_skipped([C|Skipped]), copy_to_percent(Which)
	).


copy_skipped([]).
copy_skipped([Char|Chars]) :-
	copy_skipped(Chars),
	put(Char).


%   skip_to_percent skips characters looking for %end%.
%   It succeeds if it finds it, fails if it hits end of file.

skip_to_percent(26) :- !, fail.
skip_to_percent(0'%) :- !,
	try_percent("end%").
skip_to_percent(_) :-
	get0(C),
	skip_to_percent(C).


try_percent([]).
try_percent([Char|Chars]) :-
	get0(C),
	(   C = Char, !, try_percent(Chars)
	;   skip_to_percent(C)
	).



%   new_extension('device:filnam.ext', "NEW", 'device:filnam.NEW')
%   new_extension('device:filnam.',    "NEW", 'device:filnam.NEW')
%   new_extension('device:filnam',     "NEW", 'device:filnam.NEW')

new_extension(File, Extension, NewFile) :-
	name(File, Name),
	(   append(Prefix, [46|_], Name)
	;   Prefix = Name
	),
	append(Prefix, [46|Extension], NewName), !,
	name(NewFile, NewName).

