%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%This version includes debugging features (it enables Sepia's debugging and 
%includes special code for debugging the interval arithmetic stuff)
:-dbgcomp.
:-module(interval).
:-[macros].
:-[utilities].
:-[mach_dep].
:-[intdef].
:-[xinterval].
:-[simple].
:-[neg].
:-[debug].
:-[interval].
:- module(ii).
:- set_flag(toplevel_module,ii).
:-import interval.
:-[arith].
:-[copyright].
