%   File   : BACKUP.PL
%   Author : R.A.O'Keefe
%   Updated: 19 February 1984
%   Purpose: Rename a file according to common "back up" convention.
%   Beware : this is Tops-10-specific.

%   This file MUST be compiled with TrySee.Pl, since it uses the routines
%	parse_file(-Device, -Name, -Extension, +String, +[])
%	pack_file_title(+Device, +Name, +Extension, -String)
%	normalise_file_component(+String, +Length, -Truncated)
%   which are defined in that file.  Herein is defined one predicate:
%	backup(FileName, BackUpExtn)
%   which checks whether a file named by FileName exists, and if so
%   tries to back it up.  The BackUpExtn is the extension to be merged
%   with the extension of the file name, e.g. if it is "Q" and the file
%   was "Foo.Pl" the backup will be "Foo.Ql".  Spaces at the end are
%   significant.  For convenience, backup/1 is also defined.  Note that
%   both these routines will succeed even if the file didn't exist.


:- public
	backup/1,				%  backup(F) -> F.BAK
	backup/2.				%  backup(F,Ext) for e.g. Q convention

:- mode
	backup(+),
	    backup(+, +),
		backup_name(+, +, -),
		    merge_extensions(+, +, -).


backup(FileName) :-
	backup(FileName, "BAK").


backup(FileName, BackUpExtn) :-
	atom(BackUpExtn),
	name(BackUpExtn, BackUpString), !,
	backup(FileName, BackUpString).
backup(FileName, BackUpExtn) :-
	\+ atom(FileName), !,
	ttynl, display('**error: '),
	display(backup(FileName,'_')), ttynl,
	fail.
backup(FileName, BackUpExtn) :-
	seeing(OldFile),
	nofileerrors,
	see(FileName), !,
	fileerrors,
	seeing(NewFile),
	backup_name(NewFile, BackUpExtn, BackFile),
	rename(NewFile, BackFile),
	see(OldFile).
backup(FileName, BackUpExtn) :-
	fileerrors.


	backup_name(OldFile, BackExtn, NewFile) :-
		name(OldFile, OldName),
		parse_file(Device, Name, Extension, OldName, []),
		normalise_file_component(BackExtn, 3, BackupExtension),
		merge_extensions(Extension, BackupExtension, NewExtension),
		pack_file_title(Device, Name, NewExtension, NewName),
		name(NewFile, NewName).
	

		merge_extensions([_|OldT], [NewH|NewT], [NewH|AnsT]) :- !,
			merge_extensions(OldT, NewT, AnsT).
		merge_extensions([], New, New).

