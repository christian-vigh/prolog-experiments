%Copyright 1992
%John G. Cleary
%University of Calgary
%cleary@cpsc.ucalgary.ca

%These are miscellaneous macros that transform the source code in the
%system

%Insert code that is only needed for for eclipse system (versions 3.2 and
%higher

trans_sepia(eclipse(_),true).
trans_eclipse(eclipse(G),G).

:-
	get_flag(version,V),
	(V@>='3.1'
         -> define_local_macro(eclipse/1,trans_eclipse/2,[])
         ;  define_local_macro(eclipse/1,trans_sepia/2,[])
        ).

