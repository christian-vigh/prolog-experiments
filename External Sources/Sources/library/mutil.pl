%   File   : MUTIL
%   Author : Lawrence Byrd
%   Updated: 17 May 1983
%   Purpose: Load a minimal Utilities Package


%% See MUTIL.MIC which calls this and then sets up a core image %%

% The logical name "util:" is assumed to point to the right area, if you
% are not using TOPS10 version 7.01, or don't understand logical names,
% then just edit them all out.

% The following files, found in UTIL, have been ommited for MUTIL to
% make it smaller:
%			LONG.PL
%			TIDY.PL
%			READIN.PL
%			BAGUTL.PL
%			MULTIL.PL
%
% Ie mainly the rational arithmetic package, plus a couple of less useful
%    bits.


:- [
		'util:util.ops',	% General operator declarations
		'util:arith.ops'	% Arithmetic operator declarations
   ].


:- compile([
		'util:files.pl',	% Manipulate files
		'util:writef.pl',	% Formatted write (writef)
		'util:trace.pl',	% Tracing routines
		'util:listut.pl',	% List routines
		'util:setutl.pl',	% Set routines
		'util:applic.pl',	% Application routines
		'util:flagro.pl',	% Flag handling
		'util:struct.pl',	% Structure crunching
		'util:metutl.pl',	% More structure crunching
		'util:gensym.pl'	% Generate symbols
	    ]).

:- [
		'util:edit.pl',		% Jump to FINE and back
		'util:invoca.pl',	% Invocation routines
		'util:imisce.pl'	% Miscellaneous
   ].


%% Temporary addition (added 18 February 82)

core_image :- plsys(core_image).



