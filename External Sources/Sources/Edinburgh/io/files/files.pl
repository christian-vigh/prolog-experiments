%   File   : FILES.PL
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 21 August 1984
%   Purpose: Routines for playing with files.
%  Converted for NIP   Ken Johnson 23-6-87
%  NIP does not support opening files in Append mode

			% Check to see if a file exists and produce
			%  an error message if it doesn't.

check_exists(File) :-
	file_exists(File),
	!.

check_exists(File) :-
	message_to_user(['! File ',
			File,
			' cannot be opened for input.']),
	!,
	fail.

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



			%  This looks unlikely but it works

delete(File) :-
	rename(File, []).

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
	;   true                %  Delete the backup file
	),fileerrors,
	see(File),
	rename(File, Backup),
	see(Backup),
	tell(File),
	repeat,get0(Ch),
	    ( Ch = 26 ; put(Ch), fail ),
	    !,
	    seen,
	    see(Old).


%   File   : /usr/lib/prolog/open
%   Author : R. A. O'Keefe
%   Updated: 17 October 1984
%   Purpose: open files with error messages and failure.

/*  The problem with nofileerrors mode is that when a see or tell or
    whatever fails, it does so quietly, without an intimation that
    this has happened.  The trouble with fileerrors mode is that if
    see or tell fails, it aborts, and the program cannot recover.

    Dec-10 Prolog I/O, which C Prolog copies with only minor changes,
    is generally admitted to be an unsatisfactory makeshift.  It has
    not been improved because the feeling is that it needs to be
    replaced (revolution not evolution) and in the mean-time one can
    actually program around most of its problems.  You have to be
    subtle, though.

    This file defines three commands:

	open_file(File, Mode, Action)

		tries to open File in (read, write) Mode.
		If it can't, it will print an error message, and
		call Action.  The error message will always go to
		the terminal.

	open_file(File, Mode)

		is shorthand for open(File, Mode, fail)

	open_file(File)

		is shorthand for open(File, read, fail)

*/

open_file(File) :-
	open_file(File, read, fail).


open_file(File, Mode) :-
	open_file(File, Mode, fail).


open_file(File, _, Action) :-
	\+ atom(File),
	!,
	message_to_user(['! Bad file name ',
			  File,
			  ' in call to open_file.'
			]),
	call(Action).

open_file(_, Mode, Action) :-
	\+ (atom(Mode), (Mode == read ; Mode == write)),
	!,
	message_to_user(['! Bad mode ',
			 Mode,
			 ' in call to open_file.']),
	call(Action).

open_file(File, Mode, Action) :-
	nofileerrors,
	(   open_file_1(Mode, File), !, fileerrors
	;   fileerrors,
	    message_to_user(['! Can''t open file',
				File,
				' in ',
				Mode,
				' mode.']),
	    call(Action)
	).

open_file_1(read, File) :-
	see(File).

open_file_1(write, File) :-
	tell(File).


message_to_user(Stuff) :-
	telling(Where),
	tell(user),
	message_to_user_1(Stuff),
	told,
	tell(Where).

message_to_user_1([]) :-
	nl,
	!.

message_to_user_1([H|T]) :-
	write(H),
	message_to_user_1(T).

