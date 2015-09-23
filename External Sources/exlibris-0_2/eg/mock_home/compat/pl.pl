% Tested with sicstus(3:9:0), swi(4:0:11)+swi(5:0:5), and yap(4:3:21).
%
:-
	( current_predicate( pl/1 ) ->
	  true
	  ;
	  current_prolog_flag( version, VersionFlag ),
	  (atom_concat('SICStus ',VerPfxAtom,VersionFlag) ->
	    atom_concat(VerAtom,_Rem,VerPfxAtom),
	    atom_concat(VerOnly,' ',VerAtom),
	    atom_concat(MjrPfx,MnrFixAtom,VerOnly),
	    atom_concat(MajorAtom,'.',MjrPfx),
	    atom_concat(MnrPfx,FixAtom,MnrFixAtom),
	    atom_concat(MinorAtom,'.',MnrPfx),
	    atom_codes(MajorAtom,MajorCs),
	    number_codes(Major,MajorCs),
	    atom_codes(MinorAtom,MinorCs),
	    number_codes(Minor,MinorCs),
	    atom_codes(FixAtom,FixCs),
	    number_codes(Fix,FixCs),
	    !,
	    assert( pl(sicstus(Major:Minor:Fix)) )
	    ;
	    (atom_concat('Yap-',VerPfxAtom,VersionFlag) ->
	      atom_concat(MjrPfx,MnrFixAtom,VerPfxAtom),
	      atom_concat(MajorAtom,'.',MjrPfx),
	      atom_concat(MnrPfx,FixAtom,MnrFixAtom),
	      atom_concat(MinorAtom,'.',MnrPfx),
	      atom_codes(MajorAtom,MajorCs),
	      number_codes(Major,MajorCs),
	      atom_codes(MinorAtom,MinorCs),
	      number_codes(Minor,MinorCs),
	      atom_codes(FixAtom,FixCs),
	      number_codes(Fix,FixCs),
	      !,
	      assert( pl(yap(Major:Minor:Fix)) )
	      ;
	      (number(VersionFlag) -> % Swi
	        Fix   is VersionFlag mod 100,
	        Major is VersionFlag // 10000,
	        Minor is VersionFlag - (Major*10000) - Fix,
	        !,
	        assert( pl(swi(Major:Minor:Fix)) )
	        ;
	        write( user_error, 'Unrecognised prolog in trying to assert if_pl/1.' ),
	        nl( user_error )
	      )
	    )
	  )
	).

% cannot support Ciao yet since :- doesnt mean the same thing.
% we should use
% :- initialization(init).
% 
% init:-
    % write(a), 
    % nl.
% 
	  % % ciao should be first because it : is not an xfy operator
	  % % and current_op is not a built-in predicate
	  % ( VersionFlag=ciao(JdotN,Fix) ->
	    % atom_codes( JdotN, JdotNCs ),
	    % ((JdotNCs = [MjC1,0'.|MnCs],MjCs=[MjC1]);
	      % (JdotNCs = [MjC1,MjC2,0'.|MnCs],MjCs=[MjC1,MjC2])
	    % ),
	    % atom_codes( Major, MjCs ),
	    % atom_codes( Minor, MnCs ),
	    % !,
	    % bb_put( ciao, yes )
	    % % assert( (pl(ciao(Major:Minor:Fix))) )
