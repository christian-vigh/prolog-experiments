%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%These macros elide all calls to the debugging/statistics code
%This improves efficiency in these low level routines
trans(debug_ii(_,_,_),true).
trans(debug_set(_,_,_,_),true).
trans(debug_i(_,_),true).
trans(debug_check(_),true).

:-define_local_macro(debug_ii/3,trans/2,[]).
:-define_local_macro(debug_set/4,trans/2,[]).
:-define_local_macro(debug_i/2,trans/2,[]).
:-define_local_macro(debug_check/1,trans/2,[]).
