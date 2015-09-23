%   File   : LIB.PL
%   Author : R.A.O'Keefe
%   Updated: 18 February 1984
%   Purpose: A Bottoms-10 version of the VAX "lib" predicate.

%   NOTE: this file MUST NOT BE COMPILED.
:-  true ; true.
%   There, I TOLD you not to compile this file!

%   The following directories could have been specified as
%	libdirectory(mec).
%	libdirectory(util).
%   except that lib(foo) would then try mec:foo as well as mec:foo.pl.

libdirectory('mec:?.pl').
libdirectory('util:?.pl').

:- compile('util:lib2.pl').

