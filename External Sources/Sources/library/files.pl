%   File   : FILES.PL
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 21 August 1984
%   Purpose: Routines for playing with files.

:- public
	append/1,
	check_exists/1,
	file_exists/1,
	open/1,
	open/2,
	close/2,
	delete/1.

:- mode
	append(+),
	check_exists(+),
	file_exists(+),
	open(+),
	open(?, +),
	close(+, +),
	delete(+).




			% Check to see if a file exists and produce
			%  an error message if it doesn't.

check_exists(File) :-
	file_exists(File),
	!.
check_exists(File) :-
	telling(OldTell), tell(user),
	nl, write('! File: '), write(File),
	write(' cannot be opened for input.'), nl,
	tell(OldTell),
	!, fail.



			% Succeed if a file exists, otherwise fail

file_exists(File) :-
	atom(File),
	seeing(OldSee),
	(   nofileerrors, see(File), !, fileerrors, seen, see(OldSee)
	;   fileerrors, fail
	).



			% Open a file, checking that it exists

open(File) :-
	check_exists(File),
	see(File).



			% Open a file and return current file
			% This is seeing/2 in C Prolog.

open(Old, File) :-
	seeing(Old),
	open(File).



			% Close file and see old file again

close(File, Old) :-
	close(File),
	see(Old).



			% Delete a file (note that rename requires that
			%  the file be open, in Dec-10 Prolog)

delete(File) :-
	open(Old, File),
	rename(File, []),
	see(Old).



/*  append(File)
    is supposed to open the file in "append" mode, that is for output,
    with the new output going at the end of the old contents instead of
    replacing them entirely as tell/1 would.  However, Bottoms-10 does
    not provide this facility the way UNIX does, so the simplest way of
    implementing the operation is to rename the old file File.EXT to
    File.BAK, to copy the contents of File.BAK into a new version of
    File.EXT, and to leave this new file open.

    As far as Prolog is concerned, you can use this predicate exactly
    as you use append/1 in C Prolog, that is, you can use it to open
    the file instead of tell and thereafter use tell to switch to it.
    (The other pattern which C Prolog permits, which is using append/1
    all the time instead of tell/1, will NOT work.)  However, as far
    as the operating system is concernd they are not equivalent, as
    the File.BAK will be left lying around which we don't really want,
    and in some cases involving path names Bottoms-10 won't get the
    rename right.  Also, any existing File.BAK will be deleted.
*/
append(File) :-
	seeing(Old),
	see(File),
	name(File, Chars),
	(   append(Prefix, [0'.|Suffix], Chars)
	;   Prefix = Chars
	),  !,
	append(Prefix, ".BAK", BackupChars),
	name(Backup, BackupChars),
	nofileerrors,
	(   see(Backup), rename(Backup, [])
	;   true		%  Delete the backup file
	),  !,			%  if it already exists.
	fileerrors,
	see(File),
	rename(File, Backup),
	see(Backup),
	tell(File),
	repeat,
	    get0(Ch),
	    ( Ch = 26 ; put(Ch), fail ),
	!,
	seen,
	see(Old).	




