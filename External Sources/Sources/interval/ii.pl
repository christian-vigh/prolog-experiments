%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%This version creates optimized code with no debug checks

:-nodbgcomp.
:-module(interval).
:-[macros].
:-[utilities].
:-[nodebug].
:-[mach_dep].
:-[intdef].
:-[xinterval].
:-[simple].
:-[neg].
:-[interval].
:- module(ii).
:- set_flag(toplevel_module,ii).
:-import interval.
:-[arith].
:-[copyright].
