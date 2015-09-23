% Following threelines inserted by ExLibris 0:2.
:- ensure_loaded( 'lib/add_library_directory' ).
:- add_library_directory( abs_ald(lib) ).

% ExLibris 0:2, June 2002.
% A program for simple configuration management of
% Prolog source code. The basic idea is that project files
% loading code from ``home'' libraries are prepared for
% exportation by copying all relevant files. It supports
% some non standard directives for system conditioned
% execution/loading, and for tentative, runtime, dependencies.
% See documentation in dirstribution directory doc/ .
% It currently runs on SICStus 3.9.0 (not earlier) 
% and Swi-Prolog 5.0.6 on machines supporting Unix style directory naming.
% Example usage:
% exlibris( [source('eg/simple/one_file'),dest('exlibris_example'),copy(selective),homelibs('eg/mock_home')] ).
% unix% cd exlibris_example
% unix% prolog            in {sicstus(3:9:0),swi(5:0:6),yap(4:3:21)}
% ?- [one_file].
% ?- one_file( [a,b,c] ).
% 
% The above exlibris/1 call works fine on SICStus but will give
% some warnings on Swi since it most likely, unless you have 
% a lists.pl file in a home library, will not be
% able to find 'lists' library. Since project 'eg/simple' is written
% to run on any of SICStus, Swi or Yap one should specify
% the relevant directories in the syslibs option.
% Usually you can skip Yap because its library structure
% has recently been made compatible to Sicstus one.
% Similarly Swi libraries is a subset of the SICStus ones,
% although system is something very different. 
% Thus, something like the following should be used with Swi:
% exlibris( [source('eg/simple/one_file'),dest('exlibris_example'),copy(selective),homelibs('eg/mock_home'),syslibs(['/usr/local/lib/sicstus-3.9.0/library','/usr/local/lib/pl-5.0.6/library'])] ).
% should do it.
% library in the call
% 
% To see how exporting for different systems effects the copying of source
% files try, and compare to the above, the following :
% 
% exlibris( [source('eg/simple/one_file'),dest('exlibris_example_swi'),copy(selective),homelibs('eg/mock_home'),pls(swi(_))] ).
% exlibris( [source('eg/simple/one_file'),dest('exlibris_example_sicstus'),copy(selective),homelibs('eg/mock_home'),pls(sicstus(_))] ).
% exlibris( [source('eg/simple/one_file'),dest('exlibris_example_yap'),copy(selective),homelibs('eg/mock_home'),pls(yap(_))] ).
% 
% ExLibris is used to build its distributions with:
% exlibris( [source('exlibris'),dest('~/web/exlibris/exlibris-0_2'),copy(recursive)] ).
% exlibris( [source('exlibris'),dest('/homes/nicos/public_html/exlibris/exlibris-0_2'),copy(recursive)] ).
%
%
% For a sicstus(3:9:0) built one can use:
% exlibris( [source('exlibris'),dest('~/web/exlibris/exlibris-0_1-sics_3_9_0'),copy(recursive), pls(sicstus(3:9:0))] ). 
% 
% Terminology:
%  follow 	= read the terms of a source file to find what does it load.
%  pl-term 	= a term of the form pl-name(pl-version). pl-id in {ciao,sicstus,swi,yap},
%						pl-version is like Mj-Mn-Fix, all being numbers.
%  -pair  	= a T1-T2 term.
%
% Variable Naming.
% s at the end mean a list of.
% L at the end mean a Library.
% Rt  = Root
% Prv = Provisional, eg when unsure if atomic of list.
% Abs = Absolute version, eg for a filename to its Absolute version.
% Sys = System
% Hm  = Home
% Loc = Local 
% Pl  = Prolog, ie a prolog-term something like sicstus(3:9:0).
% Pr  = Pair
%


:- ensure_loaded( library('compat/if_pl') ).				% if_pl
												% if_pl_sys
:- ensure_loaded( library('compat/pl_match') ).			% pls_match/3.
:- ensure_loaded( library(requires) ).					% /2.
:- requires( [flatten/2,atom_prefix/2,member/2,append/3,delete/3,memberchk/2,remove_duplicates/2] ).
:- requires( absolute_file_name/3 ).
:- requires( file_exists/1 ).
:- requires( replace/4 ).
:- ensure_loaded( library('option/select_either') ).		% /3.
:- ensure_loaded( library('option/select_preferred') ).	% /3.
:- ensure_loaded( library('file/fname_dir_name') ).		% /3.
:- ensure_loaded( library('file/directory_files_regular') ).% /3.
:- ensure_loaded( library('exec/sys_info_on') ).			% /1.
:- ensure_loaded( library('file/dir_will_exist') ).		% /1.
:- ensure_loaded( library('file/dirpath_will_exist') ).	% /1.
:- ensure_loaded( library('io/file_to_list_of_lines') ).	% /2.
:- ensure_loaded( library('io/list_of_lines_to_file') ).	% /2.
:- ensure_loaded( library('io/list_of_lines_with_portray_to_file') ).	% /2.
:- ensure_loaded( library('file/append_to_dir') ).		% /3.
:- ensure_loaded( library(werr) ).						% /1.
:- ensure_loaded( library('list/delete_leading') ).		% /2.
:- ensure_loaded( library('term/to_list') ).				% /2.
:- ensure_loaded( library('file/is_a_directory') ).		% /1.
:- ensure_loaded( library('file/is_a_regular_file') ).		% /1.
:- ensure_loaded( library('clause/assert_unique') ).		% /1.
:- ensure_loaded( library('term/tuple_to_list') ).		% /2.
:- ensure_loaded( library('term/portray_clauses') ).		% /1.
:- ensure_loaded( library('write/write_list_with_line_numbers') ). % /3.
:- ensure_loaded( library('kv/kv_decompose') ).			% /3.
:- ensure_loaded( library('term/load_terms') ).			% /1.
:- ensure_loaded( library('list/longest_prefix') ). 		% /3.
:- ensure_loaded( library('list/contains') ). 			% /2.
:- ensure_loaded( library('mkindex') ).					% /2.
:- ensure_loaded( library('ensure/may_load') ).			% /1.
:- may_load( library('compat/swi/built_ins') ).
:- clean_requires_indices.

exlibris_version( 0:2 ).

% exlibris( Opts ) :-
% Opts a list that may contain the following one argument terms.
% Mandatory ones have a + infront of them, and * goes around default value
% of the non-mandatory ones.
%
%	+src		: 	file or directory or directory and files.
%				each file in a provided as an ``entry'' file
%	cp		:	whether the whole directory structure holding entry files
%				should be copied recursvively. Values, {recursive,*selective*}.
% 	+dest	:	destination directory, should not exist.
% 	homelibs	:	where are the ``home libraries'' ? *'~/pl/lib'*
% 	loclib	:	the relative path from Dest of the directory to 
%				copy home library dirs to. May or may not exist.  Def: *lib*. 
% 	syslibs 	:	the location of the system library. Defaults to the first answer
%				to for the query ?- library_directory( L ).
% 	recursive :	to be supported, ignore. {true,*false*}.
% 	pls		:	a pl-term or list of pl-terms for which we want to export.
% 	echo		:    Boolean, whether normal operation should be reported. (true).
% single_file	: 	Boolean, (false). true for bundling in a single file.
%
% First deal with the top level, "entry", files/directories, with export/9
% and then recurse making sure all required files are copied with traverse/9.
% Traversing starts with the entry files.
%
exlibris( Opts ) :-
	exlibris_defaults( Defs ),
	( select_either( source(Src), Opts, Defs ) -> 
		to_list( Src, MixSrcs ),
		% select_preferred( src_exts(Exts), Opts, Defs ),
		% assert_unique( exlibris_bb_src_exts(Exts) ),
		source_files( MixSrcs, [], Srcs ),
		exlibris_options( Opts, Defs, Dst, Cp, Rec, Sng, Pls, LocL, HmLs, SysLs ),
		( file_exists(Dst) ->
			werr( ['Destination, ',Dst,', already exists.'] )
			;
			dir_will_exist( Dst ),
			afn( LocL, AbsLocL, [file_type(directory),file_errors(fail),access(none),relative_to(Dst)] ), 
			( make_destination_lib_dir(Sng,Dst,LocL,AbsLocL) ->
				export( Srcs, SrcTrips ),
				lib_directory_pairs( SrcTrips, LocL, HmLs, SysLs, LDirPairs ),
				requires_associations( LDirPairs ),
				write_libs( HmLs, SysLs, LDirPairs ),
				( Sng == true ->
					from_sources_dst_file( Srcs, Dst, DstF ), 
					% write( dstf(DstF) ), nl,
					travsingle( SrcTrips, [], [], Pls, LocL, LDirPairs, DstF )
					;
					traverse( SrcTrips, [], Rec, Pls,  LocL, LDirPairs, Dst, LocL, Cp, [] ),
					add_lib_location( LDirPairs, AbsAdd ),
					append_to_dir( AbsLocL, 'add_library_directory.pl', AddDst ),
					copy( AbsAdd, '', AddDst, exp(false,any,false) ),
					mkindex( [style(std),dir(AbsLocL)] ),
					mkindex( [style(new),dir(AbsLocL)] )
				)
				;
				true % Let make_destination_lib_dir/3 deal with messages.
			)
		)
		;
		werr( ['No source file or directory given. Nothing to do.'] )
	).

% exlibris_defaults( -DefOpts ) :-
% DefOpts, list of default options.
%
exlibris_defaults( [
	homelibs(HomeLs),
	loclib(lib),
	syslibs([SysL]),
	% recursive(false),
	copy(selective),
	echo(true),
	single_file(false),
	pls(all),
	src_exts(['.pl'])
	] ) :-
		findall( Lib, library_directory(Lib), Libs ),
		remove_duplicates( Libs, NoDups ),
		if_pl( sicstus(_), default_libs_sicstus(NoDups,SysL,HomeLs) ),
		if_pl( swi(_), default_libs_swi(NoDups,SysL,HomeLs) ),
		if_pl( yap(_), default_libs_yap(NoDups,SysL,HomeLs) ),
		!.

% default_libs_*( +Libs, -SysL, -HomeLs ) :-
% Choose one of the Libs to be your SysL(ibrary) and
% the rest are assumed to be the HomeL(ibrarie)s.
% Swi assrta's 'lib' dir as a library directory, if
% such a dir exists.
default_libs_sicstus( AllLibs, SysL, HomeLs ) :-
	select( SysL, AllLibs, HomeLs ),
	atom_concat( _Pfx, 'library', SysL ),
	atom_codes( SysL, SysLCs ),
	contains( SysLCs, "sicstus-" ),
	!.
default_libs_swi( AllLibs, SysL, HomeLs ) :-
	select( SysL, AllLibs, HomeLs ),
	atom_concat( _Pfx, 'library', SysL ),
	atom_codes( SysL, SysLCs ),
	contains( SysLCs, "pl-" ),
	!.
default_libs_yap( AllLibs, SysL, HomeLs ) :-
	select( SysL, AllLibs, HomeLs ),
	atom_concat( _Pfx, 'Yap', SysL ),
	!.

% exlibris_options( +Opts, +Defs, -Dst, -Cp, -Rec, -Pls, -DstL, -HmLs, SysL ) :-
% Pick all neccessary options, preferably from Opts, but otherwise from Defs,
% and make each to respective standard form.
%
exlibris_options( Opts, Defs, AbsDst, Cp, Rec, Sng, Pls, DstL, HmLs, SysLs  ) :-
	select_preferred( dest(Dst), Opts, Defs ),
	select_preferred( syslibs(PrvSysLs), Opts, Defs ),
	to_list( PrvSysLs,SysLs ),
	assert_unique( exlibris_bb_system_libs(SysLs) ),
	select_preferred( homelibs(PrvHmLs), Opts, Defs ),
	select_preferred( loclib(DstL), Opts, Defs ),
	to_list( PrvHmLs, HmLs ),
	absolute_file_name( Dst, AbsDst ),
	% select_preferred( recursive(Rec), Opts, Defs ),
	Rec = true, % in the future add support for Rec=false
	select_preferred( copy(PrvCp), Opts, Defs ),
	select_preferred( pls(PrvPls), Opts, Defs ),
	select_preferred( echo(Echo), Opts, Defs ),
	select_preferred( single_file(Sng), Opts, Defs ),
	assert_unique( exlibris_bb_echo(Echo) ),
	( var(PrvPls) -> true ; pls_list( PrvPls, Pls ) ),
	( (PrvCp == selective ; PrvCp == recursive) ->
			Cp = PrvCp
			;
			werr( ['Unrecognised copy option: ', PrvCp, '.',' Using selective.'] ),
			Cp = selective
	).

% export( +Srcs, -Triplets ) :-
% For each Src file create a triplet. The absolute location of the 
% file is put as an argument to a src/1 term. Second is the 
% directory while the third is '' and signifies that it is not
% a library referenced file.
%
export( [], [] ).
export( [HSrc|TSrc], [src(HAbsSrc)-HAbsSrcDir-''|TPs] ) :-
	afn( HSrc, HAbsSrc, [file_type(source),access(exist)] ),
	fname_dir_name( HAbsSrc, HAbsSrcDir, _HAbsSrcBName ),
	export( TSrc, TPs ).

% traverse( +Trips, +Vis, +Rec, +Pls, +LPfx, +SrcLs, +AbsDst, +AbsDstL, +Cp, +RecDirs ).
% Trip = (Triplet =) File-Root-Pfx,
% Main loop of ExLibris, each File is copied (location clarified with Root)
% and all the files loaded from within it are added to the next iteration's
% triplets, IncAbsFiles. Vis is a list of already copied
% files that avoid duplications in the traversal.
% RecDirs is a list of directoties already copied recursively ie by using cp -r.
% The remaining arguments are exlibris options that control various
% aspects of the traversal:
% 	Rec,		currently not in use
% 	Pls, 	what are the prologs we exporting for ? list of pl-terms
%	LPfx,	the library prefix
%	SrcLs,	the source library, where lib files are coming from
%	AbsDst,	the complete destination of library directory
%	Cp,		either 'selective' or recursive
%
traverse( [], _Vis, _Rec, _Pls, _LPfx, _SrcLs, _Dst, _DstL, _Cp, _RecDirs ).
traverse( [H|T], Vis, Rec, Pls, LPfx, SrcLs, Dst, DstL, Cp, RecDirs ) :-
	H = PrvH-HRoot-HPfx,
	( PrvH = src(AbsH) ->
		SrcFg = src, Sel = Cp, EnsRecDirs = RecDirs
		;
		SrcFg = normal, Sel = selective, EnsRecDirs = [],
		AbsH = PrvH
	),
	% afn( H, AbsH, [file_type(source),access(exist)] ),
	fname_dir_name( AbsH, SrcDir, BName ),
	working_directory( Old, SrcDir ),
	source_references( BName, false, Pls, Files, Ls ),
	% ensure_file_is_present( AbsH, Vis, SrcFg, [HRoot-HRoot], Pls, Rec, Sel, HPfx, EnsRecDirs, Dst, DstL, NxVis, NxRDs ),
	ensure_file_is_present( AbsH, Vis, SrcFg, HRoot, Pls, Rec, Sel, HPfx, EnsRecDirs, Dst, DstL, NxVis, NxRDs ),
	absolute_references( Ls, NxVis, T, SrcLs, LPfx, IncAbsLs ),
	absolute_references( Files, NxVis, IncAbsLs, [AbsH-HRoot], HPfx, IncAbsFiles ),
	working_directory( _SrcDir, Old ),
	traverse( IncAbsFiles, NxVis, Rec, Pls, LPfx, SrcLs, Dst, DstL, Cp, NxRDs ).

% travsingle( +SrcTrips, +Vis, +Terms, +Pls, +LocL, +LDirPairs, +DstF ) :-
% As in traverse but when single file bundling is required (single_file(true)).
% DstF is the file.
%
travsingle( [], _Vis, Lines, _Pls, _LPfx, _SrcLs, DstF ) :-
	write_dstfile( DstF ),
	list_of_lines_with_portray_to_file( Lines, DstF ).
travsingle( [H|T], Vis, Terms, Pls, LPfx, SrcLs, DstF ) :-
	H = PrvH-HRoot-HPfx, 
	( PrvH = src(AbsH) ->
		true
		;
		AbsH = PrvH
	),
	fname_dir_name( AbsH, SrcDir, BName ),
	working_directory( Old, SrcDir ),
	source_references( BName, true, Pls, Files, Ls ),
	NxVis = [AbsH|Vis],
	absolute_references( Ls, NxVis, T, SrcLs, LPfx, IncAbsLs ),
	absolute_references( Files, NxVis, IncAbsLs, [AbsH-HRoot], HPfx, IncAbsFiles ),
	% add_read_in_from( BName, BTerms, Terms, NxTerms ),
	file_to_list_of_lines( BName, BNameLines ),
	file_to_export_lines( BName, BNameLines, exp(single,Pls,true), ExpLines ),
	atom_codes( BName, BNameCs ),
	append( "% code inserted from file, ", BNameCs, First ),
	append( Terms, ["",First,""|ExpLines], NxTerms ),
	working_directory( _SrcDir, Old ),
	travsingle( IncAbsFiles, NxVis, NxTerms, Pls, LPfx, SrcLs, DstF ).

% ensure_file_is_present( +File, +Vis, +Flag, +Xrefs, +Pls, +Rec, +Cp, +Pfx, 
%                            +RecDirs, +RtDstDir, +DstLib, -NxVis, -NxRDs ) :-
% Ensure that a File is already copied or copy it. 
% Vis is the list of copied files.
% Flag indicates if you deal a src file (==src) as oppose to a lib file.
% Xrefs a list of reference directories.
% Variables, Pls, Rec, Cp, are used to transform the copied file.
% Pfx, possible lib prefix to be appended onto the destination dir.
% RecDirs list of dirs already copied recursively (if file resides within one
% of them
% it will not be copied). RtDstDir, the root destination directory, to where
% the whole project is being copied onto. DstLib, the destination subdir,
% location is relative to RtDstDir.
% NxVis the update Vis. NxRDs the updated RecDirs.
%
ensure_file_is_present( H, Vis, Flag, Xrf, Pls, Rec, Cp, Pfx, RecDirs, RtDstDir, DstLib, NxVis, NxRDs ) :-
	Opts = [file_type(source),file_errors(fail),relative_to(Xrf),
			solutions(all),access(exist)],
	% member( Xrf-RtSrcDir, Xrefs ),
	afn( H, AbsH, Opts ),
	( file_in_system_lib(AbsH) ->
		NxVis = Vis
		;
		( file_not_to_be_copied(AbsH,Cp,RecDirs) ->
			NxVis = Vis,
			NxRDs = RecDirs
			;
			( memberchk(AbsH,Vis) ->
				werr( ['Internal consistency error copying visited with: ',AbsH,' ',Vis] ),
				NxVis = Vis,
				NxRDs = RecDirs
				;
				fname_dir_name( AbsH, AhDir, AhName ),
				base_roots_n_pfx_to_dirs( AhDir, Rec, Xrf, RtDstDir, Pfx, DstDir ),
				% afn( AhName, DstFile, [file_type(source),relative_to(DstDir)] ),
				append_to_dir( DstDir, AhName, PrvDstFile ),
				afn( PrvDstFile, DstFile, [file_type(source)] ),
				select_copy( Cp, AbsH, DstFile, RecDirs, exp(Rec,Pls,false), NxRDs ),
				correct_src_directives( DstFile, Flag, Rec, DstLib, Pls ),
				NxVis = [AbsH|Vis]
			)
		)	
  	).

% absolute_references( +Files, +Vis, +Block, +Xrefs, +InhPfx, -Refs ) :-
% For each File, find location relative to some Xref. If this doesnot 
% contradict Vis or Block add a Ref, in Block, holding the absolute
% location its directory and its inheritent prefix, InhPfx.
% 
absolute_references( [], _Vis, Refs, _Xrefs, _LPfx, Refs ).
absolute_references( [H|T], Vis, Block, Xrefs, InhPfx, Refs ) :-
	% write( h(H) ), nl,
	( absolute_reference( H, Vis, Block, Xrefs, InhPfx, NxB ) ->
		true 
		;
		xrefs_to_werr_term( [''-''|Xrefs], '...skipping.', Wrefs ),
		werr( ['Cannot locate file: ',H,', relative to: '|Wrefs] ),
		NxB = Block
	),
	absolute_references( T, Vis, NxB, Xrefs, InhPfx, Refs ).

% absolute_references( +File, +Vis, +Block, +Xrefs, +InhPfx, -Refs ) :-
% As in absolute_references/6 for a single File.
%
absolute_reference( H, Vis, Block, Xrefs, InhPfx, NxB ) :-
	Opts = [file_type(source),file_errors(fail),relative_to(Xrf),
			solutions(all),access(exist)],
	member( Xrf-RtSrcDir, Xrefs ),
	afn( H, AbsH, Opts ),
	( file_in_system_lib(AbsH) ->
		% donot follow system libraries or files already copies.
		NxB = Block
		;
		reference_to_follows( AbsH, Vis, Block, RtSrcDir, InhPfx, NxB )
  	).

% reference_to_follows( +File, +Vis, +Block, +RtSrcDir, +InhPfx, -NxB ) :-
% Update Block, a list of files to be traversed with all the
% references in File. Donot add visited, Vis, files. Use RtSrcDir 
% and InhPfx in the added structure.
% NxB the updated Block.
%
reference_to_follows( File, Vis, Block, RtSrcDir, InhPfx, NxB ) :-
	( (memberchk(File-_Rt-_Pfx,Block);memberchk(File,Vis)) ->
		NxB = Block
		;
		Trip = File-RtSrcDir-InhPfx,
		NxB = [Trip|Block]
	).

% base_roots_n_pfx_to_dirs( +SrcDir, +Rec, +RootSrcDir, +RootDstDir, +Pfx, -DstDir ) :-
% Depending to whether Rec==true or not decide what the DestDir should be for
% particular SrcDir, RootSrcDir, RootDstDir, and  Pfx. Although this predicate
% supports Rec==false, other parts of the code, donot do so as yet.
%
base_roots_n_pfx_to_dirs( SrcDir, Rec, RootSrcDir, RootDstDir, Pfx, DstDir ) :-
	( Rec = true ->
		atoms_longest_prefix( RootSrcDir, SrcDir, PfxDir ),
		atom_concat( PfxDir, SlDstLocDir, SrcDir ),
		( atom_concat( '/', DstLocDir, SlDstLocDir ) ->
			true
			;
			DstLocDir = SlDstLocDir
		),
		append_to_dir( RootDstDir, Pfx, RPDstDir ),
		append_to_dir( RPDstDir, DstLocDir, DstDir )
		;
		DstDir = RootDstDir
	).

% source_references( +Local, +Sng, +Pls, -Files, -Libs ) :-
% A source file loads files Files and libraries Libs.
% Pls is a list of pl-terms which are used to match 
% conditional load predicates, if_pl/2,3,4 . Sng indicates,
% single file operation.
%
source_references( Local, Sng, Pls, Files, Libs ) :-
	open( Local, read, Stream ),
	stream_source_references( Stream, Sng, Pls, Files, Libs ),
	close( Stream ).

% stream_source_references( +Stream, +Sng, +Pls, -Files, -Libs ) :-
% As source_references/4 but for the first argument, which is
% in this case a Stream.
% 
stream_source_references( Stream, Sng, Pls, Files, Libs ) :-
	( at_end_of_stream( Stream ) ->
		Files = [],
		Libs = []
		;
		catch( read(Stream,Term), All, read_in_handler(Sng,All,Succ) ),
		( Succ == false -> 
			NxF = Files, 
			NxL = Libs
			;
			source_term_references( Term, Pls, Files, Libs, NxF, NxL )
		),
		!,
		stream_source_references( Stream, Sng, Pls, NxF, NxL )
	).

% read_in_handler( +Sng, +Exc, -Succ ) :-
% Cover reading in errors. Sng, is true if operating single file.
% Exc is the exception and Succ will be false if not in single file.
read_in_handler( Sng, _Exc, Succ ) :-
	( Sng = true ->
		fail
		;
		Succ = false
	).

% source_term_references( +Term, +Pls, +InF, +InL, -TailF, -TailL ) :-
% The term Term which is taken to be a prolog sourde term, has some
% of these are TailF and TailL. Pls discriminates conditional load
% predicate if_pl/2,3,4.
%
source_term_references( Term, Pls, InF, InL, TailF, TailL ) :-
	( Term = (:- Drctv) ->
		directive_loads( Drctv, Pls, ToLoadRec, _DcTerm ),
		flatten( ToLoadRec, ToLoad ),
		to_load_separate( ToLoad, InF, InL, TailF, TailL )
		;
		InF = TailF,
		InL = TailL
	).

% directive_loads( +Goal, +Pls, -ToLoad, -DcTerm ) :-
% Directive Goal, loads list of files ToLoad, when
% considering any prolog systems identified by some pl-term in Pls.
% DcTerm has information for rebuilting Goal with changed arguments.
% Used, in single file bundling.
%
directive_loads( (A,B), Pls, ToLoad, not_load_directive((A,B)) ) :-
	!,
	directive_loads( A, Pls, TLA, _TTA ),
	directive_loads( B, Pls, TLB, _TTB ),
	append( TLA, TLB, ToLoad ).
	% toterm_combine( TTA, TTB, ToTerm ).
directive_loads( if_pl(IfPls,Call), Pls, ToLoad,  DcTerm ) :-
	!,
	Meta = directive_loads( Call, Pls, ToLoad, CallArg),
	if_pl_matches( IfPls, Pls, Meta, ToLoad, Match, _Tight ),
	( Match == true ->
		DcTerm = (if_pl,[IfPls,CallArg],2)
		;
		ToLoad = [],
		DcTerm = not_matching
	).
directive_loads( if_pl(IfPls,Then,Else), Pls, ToLoad, DcTerm ) :-
	!,
	Meta1 = directive_loads( Then, Pls, ToLoad1, TTrm1 ),
	Meta2 = directive_loads( Else, Pls, ToLoad2, TTrm2 ),
	if_pl_then_else( IfPls, Pls, Meta1, Meta2, ToLoad1, ToLoad2, _Tight ),
	( TTrm1 == not_matching ->
		( TTrm2 == not_matching ->
			DcTerm = not_matching
			;
			DcTerm = (if_pl,[not(IfPls),TTrm2],2),
			ToLoad = ToLoad1
		)
		;
		( TTrm2 == not_matching ->
			DcTerm = (if_pl,[IfPls,TTrm1],2),
			ToLoad = ToLoad1
			;
			DcTerm = (if_pl,[IfPls,TTrm1,TTrm2],[2,3]),
			ToLoad = [ToLoad1,ToLoad2]
		)
	).
directive_loads( may_load(Files), _Pls, ToLoad, not_matching ) :-
	!,
	to_list( Files, ToLoad ).
directive_loads( requires(Specs), ForPls, ToLoad, DcTerm ) :-
	exlibris_bb_reqs_new( New ),
	to_list( Specs, SpecList ),
	requires_specklist_to_files( SpecList, ForPls, New, ToLoad ),
	DcTerm = (requires,[Specs],1).
directive_loads( ensure_loaded(Files), _Pls, ToLoad, DcTerm ) :-
	!,
	( ensure_load_barred(Files) ->
		ToLoad = [], DcTerm = not_matching
		;
		to_list( Files, ToLoad ), 
		DcTerm = (ensure_loaded,Files,1)
	).
directive_loads( use_module(File), _Pls, [File], DcTerm ) :-
	!,
	DcTerm = (use_module,[File],1).
directive_loads( use_module(File,Imps), _Pls, [File], DcTerm ) :-
	!,
	DcTerm = (use_module,[File,Imps],1).
directive_loads( use_module(Mod,File,Imps), _Pls, [File], DcTerm ) :-
	!,
	DcTerm = (use_module,[Mod,File,Imps],2).
directive_loads( compile(Files), _Pls, ToLoad, DcTerm ) :-
	!,
	to_list( Files, ToLoad ),
	DcTerm = (compile,[Files],1).
directive_loads( consult(Files), _Pls, ToLoad, DcTerm ) :-
	!,
	to_list( Files, ToLoad ),
	DcTerm = (consult,[Files],1).
directive_loads( load_files(Files), _Pls, ToLoad, DcTerm ) :-
	!,
	to_list( Files, ToLoad ),
	DcTerm = (load_files,[Files],1).
directive_loads( load_files(Files,Opts), _Pls, ToLoad, DcTerm ) :-
	!,
	to_list( Files, ToLoad ),
	DcTerm = (load_files,[Files,Opts],1).
directive_loads( require(Specs), _Pls, ToLoad, DcTerm ) :-
	!,
	exlibris_bb_reqs_std( Std ),
	to_list( Specs, SpecsList ),
	require_specklist_to_files( SpecsList, Std, ToLoad ),
	DcTerm = (require,[Specs],1).
directive_loads( Files, _Pls, ToLoad, (consult,[Files],1) ) :-
	is_list( Files ),
	!,
	ToLoad = Files.
directive_loads( Any, _Pls, [], not_load_directive(Any) ).

% requires_specklist_to_files( +Specs, +ForPls, +Reqs, -Files ) :-
% For each Spec, add all files, into Files, that are pointed to by Reqs
% and match some Pl in ForPls. This is for the new style Reqs.
%
requires_specklist_to_files( [], _ForPls, _Reqs, [] ).
requires_specklist_to_files( [Name/Arity|T], ForPls, Reqs, ToLoad ) :-
	findall( (File,Mod), ( member((Name,Arity,RqPls,Mod,File),Reqs),
	                 pls_match(ForPls,RqPls,_MtcPls) ), Files ),
	( Files == [] ->
		werr( [['Cannot locate source file for predicate, ',
	             Name/Arity], ' in directive, requires/1.'] ),
		ToLoad = TailLoad
		;
		add_files_to_load( Files, ToLoad, TailLoad )
		% ToLoad = [library(File)|TailLoad]
	),
	requires_specklist_to_files( T, ForPls, Reqs, TailLoad ).

% requires_specklist_to_files( +Specs, +Reqs, -Files ) :-
% For each Spec, add all files, into Files, that are pointed to by Reqs.
% This is for the std style Reqs.
%
require_specklist_to_files( [], _Reqs, [] ).
require_specklist_to_files( [Name/Arity|T], Reqs, ToLoad ) :-
	( memberchk((Name,Arity,_Mod,File),Reqs) ->
		ToLoad = [library(File)|TailLoad]
		;
		werr( ['Cannot locate source file for predicate, ',
				Name/Arity, ' in directive, require/1.'] ),
		ToLoad = TailLoad
	),
	require_specklist_to_files( T, Reqs, TailLoad ).

% if_pl_matches( +IfPls, +Pls, +Meta, -This, -IfMatch, -Tight ) :-
% Match are all Pls pl-terms that match with IfPls.
% Meta is only called once if there was at least one match,
% and should instantiate This. If no Match is found This will == [].
% Tight == true iff Pls is a list of pl-terms that all 
% match with some IfPl.  IfMatch idicates if a match was found.
% 
if_pl_matches( PrvIfPls, Pls, Meta, This, IfMatch, Tight ) :-
	( all_pls( Pls ) ->
		Tight = false,
		% ( if_pls(IfPls,ExPl,fail) ->
			% This = [], IfMatch = false
			% ;
		%  	call(Meta),
		call( Meta ),
		IfMatch = true
		% )
		;
		to_list( PrvIfPls, IfPls ),
		findall( ExPl, 
			(	
				member(ExPl,Pls),
				\+ if_pl_sys(IfPls,ExPl,fail)
			),
				Match  ),
		delete_all( Match, Pls, NotMatch ),
		( NotMatch == [] -> Tight = true ; Tight = false ),
		( Match == [] ->
			IfMatch = false,
			This = []
			;
			IfMatch = true,
			call( Meta )
		)
	).

% if_pl_then( +IfPls, +Pls, +Meta, -This, -Tight ) :-
% Find the This which is produced by calling Meta on Call when some pl-term in Pls
% matches the conditions of conditional call identified by IfPls.
% This predicate calls to if_pls/5 which is an exteral predicate; definded
% in library('compat/if_pl'). Match is the list of matching Pls.
% Tight == true iff Pls is a list of pl-terms that all 
% match with IfName and VerOps.
% 
if_pl_then( IfPls, Pls, Meta, This, Tight ) :-
	if_pl_then( IfPls, Pls, Meta, _Match, This, Tight ).

% if_pl_then( +IfPls, +Pls, +Meta, -This, -Match, -Tight ) :-
% As in if_pl_then/6 but with Match being a list of the matching Pls.
% 
if_pl_then( IfPls, Pls, Meta, This, Match, Tight ) :-
	( all_pls(Pls) ->
		Tight = false,
		call( Meta )
		;
		findall( ExPl,
			(
				(var(Pls);(\+ var(Pls),member(ExPl,Pls))),
				if_pl_sys( IfPls, ExPl, true, fail )
			),
				Match  ),
		delete_all( Match, Pls, NotMatch ),
		( NotMatch == [] -> Tight = true ; Tight = false ),
		( Match == [] ->
			This = []
			;
			call( Meta )
		)
	).

% if_pl_then_else( +IfPls, +Pls, +Meta1, +Meta2, -This1, -This2, -Tight ) :-
% As in if_pl_then_loads but with Meta2 and This2 for else part.
% Tight could be in {then,else,both} signifying which bits matched to some Pl.
%
if_pl_then_else( IfPls, Pls, Meta1, Meta2, This1, This2, Tight ) :-
	( all_pls(Pls) ->
		call( Meta1 ), call( Meta2 ), Tight = false
		;
		% if_pl_matches( IfPls, Pls, Meta, PlMatch, Tight ),
		if_pl_then( IfPls, Pls, Meta1, This1, ThenPls, ThenTight ),
		( ThenTight == true ->
			Tight = then,
			This2 = []
			;
			call( Meta2 ),
			( ThenPls = [] ->
				Tight = else
				;
				Tight = both
			)
		)
	).

% select_copy( +Select, +SrcFile, +DstFile, +RecDirs, +ExpTerm, -NxRDs ) :-
% Copy file SrcFile to destination DstFile. The whole directory
% where File resides maybe copied if Select==all. Otherwise
% Select should be ==selective and only the file is Copied.
% File and DstDir should be absolute values. ExpTerm is = exp(Rec,Pls,Rm)
% Rec and Pls are exlibris/1 options and Rm is a boolean that depends
% on where select_copy/4 is called from. ExpTerm is used to clean 
% a project file for exportation.
% NxRDs is RecDirs updated.
%
select_copy( Select, SrcFile, DstFile, RecDirs, ExpTerm, NxRDs ) :-
	fname_dir_name( SrcFile, SrcFileDir, _SrcName ),
	select_copy_from( Select, SrcFile, SrcFileDir, DstFile, RecDirs, ExpTerm, NxRDs ).

% select_copy_from( +Select, +SrcFile, +SrcFileDir, +DstFile, +RecDirs, +ExpTerm, -NxRDs ) :-
% As select_copy/6, but the file's directory, SrcFileDir, is an extra argument.
%
select_copy_from( Select, SrcFile, SrcFileDir, DstFile, RecDirs, ExpTerm, NxRDs ) :-
	( Select == recursive ->
		append_to_dir( SrcFileDir, '*', Src ),
		copy( Src, ' -r ', DstFile, ExpTerm ),
		NxRDs = [SrcFileDir|RecDirs]
		;
		NxRDs = RecDirs,
		( file_exists( DstFile ) ->
			( is_a_regular_file(DstFile) ->
					diff_pl_source_files( SrcFile, DstFile, ExpTerm )
					;
					werr( ['File exists but no regular, ',DstFile,'. Not copied.'] )
				)
				;
				copy( SrcFile, '', DstFile, ExpTerm )
		)
	).

% copy( +Src, +Flag, +DstFile, +ExpTerm ) :-
% Copy Src on the directory holding DstFile using the unix argument +Flag.
% Once copied, clean the file according to ExpTerm 
%
copy( Src, Flag, Dst, ExpTerm ) :-
	fname_dir_name( Dst, DstDir, _DestBName ),
	dirpath_will_exist( DstDir ),
	atom_concat( 'cp ', Flag,  CpFlg ),
	atom_concat( CpFlg, Src,  CpPfx ),
	atom_concat( CpPfx, ' ', CpMdx ),
	( Flag = '' ->
		atom_concat( CpMdx, Dst, Cp )
		;
		atom_concat( CpMdx, DstDir, Cp )
	),
	exlibris_bb_echo( Echo ),
	sys_info_on( Cp, Echo, user ),
	file_to_list_of_lines( Dst, DstLines ),
	file_to_export_lines( Dst, DstLines, ExpTerm, ExpLines ),
	list_of_lines_with_portray_to_file( ExpLines, Dst ).

%%% specific auxiliary predicates 

% from_sources_dst_file( +Sources, +Dst, -DstF ) :-
% Construct DstF as the addition of the basename of the first
% Source on to Dst (directory). DstF is the single file where
% all files will be copied into.
%
from_sources_dst_file( [H|_T], Dst, DstF ) :-
	fname_dir_name( H, _Dir, BName ),
	append_to_dir( Dst, BName, DstF ).

% write_dstfile( +DstF ) :-
% Report (single) destination file DstF if echo is turned on.
%
write_dstfile( DstF ) :-
	( exlibris_bb_echo(true) ->
		write( 'Writing single file into, ' ),
		write( DstF ), write( ' .' ), nl
		;
		true
	).

% make_destination_lib_dir( +Dst, +DstL, -AbsDstL ) :-
% With reference to Dst create directory path DstLib, if it doesnt exist.
% The absolute path of DstL is AbsDstL. Predicate fails without creating
% any directories if AbsDstL is not within Dst.
%
make_destination_lib_dir( Sng, Dst, DstL, AbsDstL ) :-
	( Sng == true ->
		true
		;
		AbsOpts = [relative_to(Dst)],
		afn( DstL, AbsDstL, AbsOpts ),
		( atom_prefix( AbsDstL, Dst ) ->
			( file_exists(AbsDstL) ->
				( is_a_directory(AbsDstL) ->
					true
					;
					werr( ['Destination library should be a directory.'] ),
					fail
				)
				;
				dirpath_will_exist(AbsDstL)
			)
			;
			werr( [['Library destination, libdest(), should be dir relative to destination dir, dest().'],'Given options were translated to, ',AbsDstL,' and ',Dst] ),
			fail
		)
	).

% source_files( +FilesAndDirs, +Acc, -Files ) :-
% Files is the list of files that are either files in FilesAndDirs
% or a file within a directory in FilesAndDirs. Note subdirectories
% of directories in FilesAndDirs are not looked into. Acc is Files'
% accumulator.
% 
source_files( [], Files, Files ).
source_files( [H|T], Acc, Files  ) :-
	AbsOpts = [access(exist),file_errors(fail)],
	( afn(H,AbsH,[file_type(directory)|AbsOpts]) ->
		directory_files_regular( AbsH, HFiles )
		;
		( afn(H,AbsH,[file_type(source)|AbsOpts]) ->
			HFiles = [AbsH]
			;
			werr( ['Skipping non existant, or wrong type of, source file: ',AbsH] ),
			HFiles = []
		)
	),
	append( Acc, HFiles, NxAcc ),
	source_files( T, NxAcc, Files ).

% to_load_separate( +Files, +NonLibFiles, +LibFiles, -TailNonLibFiles, -TailLibFiles ) :-
% The absolute Each file, in Files, is either a libfile or not. Variable
% LibFiles gets partially instantiated to the list of libfiles with it
% tail being TailLibFiles, NonLibFiles is the partial list of those that
% aren't and having tail TailNonLibFiles.
%
to_load_separate( [], NonLibFiles, LibFiles, NonLibFiles, LibFiles ).
to_load_separate( [H|T], NonLibFiles, LibFiles, TailNonLibFiles, TailLibFiles ) :-
	( H = library(Lfile) ->
		NonLibFiles = MoreNonLibFiles,
		LibFiles  = [Lfile|MoreLibFiles]
		;
		NonLibFiles = [H|MoreNonLibFiles],
		LibFiles  = MoreLibFiles
	),
	to_load_separate( T, MoreNonLibFiles, MoreLibFiles, TailNonLibFiles, TailLibFiles ).

% correct_src_directives( +File, +Rec, +LibLocation, +Pls ) :-
% Transform/correct a entry level File. Rec is not recognised currently,
% but when ==false library files will lose their relative subdirectories
% in load directives. LibLocation is to be added as a library_directory/1.
% Pls is used to remove any loads that are irrelevant to all pl-terms in it. 
%
correct_src_directives( File, Flag, Rec, LibLocation, Pls ) :-
	file_to_list_of_lines( File, FileLines ),
	( Flag == src -> 
		Rm = true,
		export_preamble( LibLocation, Preamble )
		;
		Rm = false,
		Preamble = []
	),
	file_to_export_lines( File, FileLines, exp(Rec,Pls,Rm), CleanLines ),
	append( Preamble, CleanLines, AllExportLines ),
	list_of_lines_with_portray_to_file( AllExportLines, File ).

% export_preamble( +LibLocation, -TopLines ) :-
% Lines of text TopLines are the code for adding LibLocatioin as a
% library directory.
%
export_preamble( LibLocation, [A,B,C,""]) :-
	exlibris_version( Mj:Mn ), 
	number_codes( Mj, MjCs ),
	number_codes( Mn, MnCs ),
	flatten( ["% Following threelines inserted by ExLibris ",MjCs,":",MnCs,"."], A ),
	atom_codes( LibLocation, LibCs ),
	flatten( [":- ensure_loaded( '",LibCs,"/add_library_directory' )."], B ),
	flatten( [":- add_library_directory( abs_ald(",LibCs,") )."], C ).
	% B = ":- multifile library_directory/1.",
	% C = ":- prolog_load_context( directory, ThisDir ),",
	% atom_codes( LibLocation, LibLocationCs ),
	% flatten( ["   atom_concat( ThisDir, '", [0'/|LibLocationCs],"', AbsLDir ),"], D ),
	% E = "   assert( (library_directory(AbsLDir)) ).".

% lib_directory_pairs( +Srcs, +LocL, +HmLs, +SysLs, -LDirPairs ) :-
% LDirPairs is the -pair of identical LDirLocations of libraries
% to be used in exporting. LocL is considered relative to Srcs,
% where as HmLs and SysL are considered relative to here.
% 
lib_directory_pairs( Srcs, LocL, HmLs, SysLs, LDirPairs ) :-
	source_lib_directories( Srcs, LocL, [], LocLibPairs ),
	AfnOpts = [file_type(directory),access(exist),file_errors(fail)],
	exp_hm_lib_directories( HmLs, AfnOpts, HmPairs ),
	% afn( SysL, AbsSysL, AfnOpts ),
	% if_pl( swi, [(_,=)], expand_file_name(SysL,[ExpSysL]), ExpSysL=SysL ),
	exp_lib_directories( SysLs, AfnOpts, SysPairs ),
	flatten( [HmPairs,SysPairs,LocLibPairs], PrvLDirPairs ),
	remove_duplicates( PrvLDirPairs, LDirPairs ).
	% append( HmPairs, [ExpSysL-ExpSysL], PairsPost ),
	% append( LocLibPairs, PairsPost, LDirPairs ).

requires_associations( Dirs ) :-
	requires_associations_1( Dirs, std, NestStdReqs ),
	requires_associations_1( Dirs, new, NestNewReqs ),
	flatten( NestStdReqs, Std ),
	flatten( NestNewReqs, New ),
	assert_unique( exlibris_bb_reqs_std(Std) ),
	assert_unique( exlibris_bb_reqs_new(New) ).

requires_associations_1( [], _Wh, [] ).
requires_associations_1( [H-_Head|T], Wh, [HReqs|TReqs] ) :-
	( Wh == std ->
		Ipl = 'INDEX.pl'
		;
		Ipl = 'Index.pl'
	),
	append_to_dir( H, Ipl, Index ),
	% absolute_file_name( H, AbsH ),
	( file_exists(Index) ->
		load_terms( Index, Terms ),
		index_terms_to_requires( Terms, Wh, H, HReqs )
		;
		( exlibris_bb_echo(true) ->
			write( 'Library directory ' ), write( H ),
			write( ' has no ' ), write( Ipl ), 
			write( ' .' ), nl
			;
			true
		),
		HReqs = []
	),
	requires_associations_1( T, Wh, TReqs ).

add_files_to_load( [], TailLoad, TailLoad ).
add_files_to_load( [(H,Mod)|T], ToLoad, TailLoad ) :-
	( Mod = built_in ->
		More = ToLoad
		;
		ToLoad = [library(H)|More]
		% ToLoad = [lib(Lct,H)|More]
	),
	add_files_to_load( T, More, TailLoad ).

index_terms_to_requires( [], _Wh, _Dir, [] ).
index_terms_to_requires( [H|T], Wh, Dir, Reqs ) :-
	( Wh == std ->	
		( H = index(Name,Arity,Mod,File) ->
			append_to_dir( Dir, File, DirFile ),
			absolute_file_name( DirFile, Full ),
			Reqs = [(Name,Arity,Mod,File)|TReqs]
			;
			Reqs = TReqs
		)
		;
		( H = index(Name,Arity,Pls,Mod,File) ->
			% alternatively Full=library(File)
			append_to_dir( Dir, File, DirFile ),
			absolute_file_name( DirFile, Full ),
			Reqs = [(Name,Arity,Pls,Mod,File)|TReqs]
			;
			Reqs = TReqs
		)
	),
	index_terms_to_requires( T, Wh, Dir, TReqs ).

% source_lib_directories( +FileDirPairs, +SrcLibs, +AccSrcLibPairs, +SrcLibPairs ).
% SrcLibPairs is the -pair of identical SrcLibLocations. The location is the 
% absolute location of the dir part of the file dir -pair of the first element of 
% FileDirPairs. The location is found in reference to members of SrcLibs.
% AccSrcLibPairs, is an accumulator for SrcLibPairs.
%
% source_lib_directories( [], _LocLib, _SrcLibPairs, [] ).
% source_lib_directories( [_HFile-HDir-_Third|T], LocLib, Acc, SrcLibPairs ) :-
source_lib_directories( _Srcs, LocLib, _Acc, [LocLib-LocLib] ).
	% AfnOpts = [file_type(directory),access(exist),
				% file_errors(fail),relative_to(HDir)],
	% findall( HLib-HLib, afn(LocLib,HLib,AfnOpts ), NewPairs ),
	% append( Acc, NewPairs, NxAcc ),
	% source_lib_directories( T, LocLib, NxAcc, SrcLibPairs ).

% file_to_export_lines( +File, +SourceLines, exp(+Rec,+Pls,+Rm), -ExpLines ) :-
% ExpLines are the lines SourceLines as transformed for exportation.
% File is the File where SourceLines come from. Rec is currently of no
% importance. Pls is a list pl-terms and Rm==true means library_directory/1
% definitions and corresponding multifile are removed.
%
file_to_export_lines( Source, SourceLines, exp(Rec,Pls,Rm), ExpLines ) :-
	open( Source, read, Stream ),
	stream_to_export_lines( Stream, 0, SourceLines, Rec, Pls, Rm, ExpLines ),
	close( Stream ).

% stream_to_export_lines( +Stream, +CntRm, +AllLines, +Rec, +Pls, +Rm, -ExpLines ) :-
% As in file_to_export_lines but first argument is a stream and added the 
% second argument for counting how many lines removed thus far (-1). 
% Todo: also remove ':- dynamic library_directory/1.'
% Todo: support Rec=false by deleting directory prefixes from load directives.
%
stream_to_export_lines( Stream, CntRm, AllLines, Rec, Pls, Rm, ExpLines ) :-
	( at_end_of_stream(Stream)  ->
		AllLines = ExpLines
		;
		read_term_with_line( Stream, Term, Line ),
		( ( 	Term = (:- Goal),
			directive_updates_export_lines( Rec, Goal, Line, CntRm, AllLines, Pls, NxCnt, NxLines )
		  ) -> 
				true
				;
				( ( Rm==true,
				    donot_export_termline(Term) ) ->
				    % (Term=library_directory(_AnyLib)
				  	% ; Term= (:- multifile library_directory/1)) ) ->
						Nth is Line - CntRm,
						nth( Nth, AllLines, _LibTrm, NxLines ),
						NxCnt is CntRm + 1
						;
				 		NxLines = AllLines,
						NxCnt = CntRm
				)
		),
		stream_to_export_lines( Stream, NxCnt, NxLines, Rec, Pls, Rm, ExpLines )
	).

donot_export_termline( library_directory(_AnyLib) ).
donot_export_termline( (:- multifile library_directory/1) ).
donot_export_termline( (:- ensure_loaded(Files) ) ) :-
	ensure_load_barred( Files ).
donot_export_termline( (:- add_library_directory(_AnyDir) ) ).

% directive_updates_export_lines( +IfPl, +Line, +CntRm, +AllLines, +Pls, -NxCnt, -NxLines ) :-
% an if_pl/2,3,4 directive which appears in line Line of AllLines is
% removed or transformed depending on its relevance to a list of pl-terms Pls.
% NxLines is identical to AllLines apart for the removed/transformed one.
% If a line is removed then NxCnt becomes CntRm + 1, otherwise = CntRm.
%
directive_updates_export_lines( single, Dct, Line, CntRm, AllLines, Pls, NxCnt, NxLines ) :-
	% directive_loads( Dct, Pls, ToLoad, (Fnc,Args,Nths) ),
	directive_loads( Dct, Pls, ToLoad, DcTerm ),
	construct_directive( DcTerm, ToLoad, Repl ),
	( Repl\==vacuous ->
		Replace = portray_directive(Repl),
		replace_export_line( Line, CntRm, AllLines, Replace, NxCnt, NxLines )
		;
		replace_export_line( Line, CntRm, AllLines, '', NxCnt, NxLines )
	).
directive_updates_export_lines( _, if_pl(IfPls,Call), Line, CntRm, AllLines, Pls, NxCnt, NxLines ) :-
	Meta = (PlMatch = pl_match),
	if_pl_matches( IfPls, Pls, Meta, PlMatch, _IfMatch, Tight ),
	( PlMatch == pl_match ->
		( Tight == true ->
			Replace = portray_directive(Call),
			replace_export_line( Line, CntRm, AllLines, Replace, NxCnt, NxLines )
			;
			NxCnt = CntRm, 
			NxLines = AllLines
		)
		;
		replace_export_line( Line, CntRm, AllLines, '', NxCnt, NxLines )
	).

% directive_ifpl_updates_export_lines( if_pl(IfName,VersOps,Call), Line, CntRm, AllLines, Pls, NxCnt, NxLines ) :-
	% Meta = (PlMatch = pl_match),
	% if_pl_then( IfName, VersOps, Pls, Meta, _Match, PlMatch, Tight ),
	% ( PlMatch == pl_match ->
		% ( Tight = true -> % not Match
			% Replace = portray_directive(Call),
			% replace_export_line( Line, CntRm, AllLines, Replace, NxCnt, NxLines )
			% ;
			% NxCnt = CntRm, 
			% NxLines = AllLines
		% )
	% ).
directive_updates_export_lines( _Rec, if_pl(IfPls,Then,Else), Line, CntRm, AllLines, Pls, NxCnt, NxLines ) :-
	ThenMeta = (ThenMatch = then_match),
	ElseMeta = (ElseMatch = else_match),
	if_pl_then_else( IfPls, Pls, ThenMeta, ElseMeta, ThenMatch, ElseMatch, Tight ),
	( Tight == then ->
		if_pl_directive_to_replace_term( Then, Replace ),
		replace_export_line( Line, CntRm, AllLines, Replace, NxCnt, NxLines )
		;
		( Tight == else -> 
			if_pl_directive_to_replace_term( Else, Replace ),
			replace_export_line( Line, CntRm, AllLines, Replace, NxCnt, NxLines )
			;
			NxCnt = CntRm, 
			NxLines = AllLines
		)
	).
% if_pl_call_to_replace_term( Call, Term ) :-
% Term is the replacement term as expected from
% list_of_lines_with_portray_to_file/2.
% 
if_pl_directive_to_replace_term( Call, Term ) :-
	( Call == true ->
		Term = ''
		;
		Term = portray_directive(Call)
	).

% replace_export_line( +Line, +CntRm, +AllLines, +Replacement, -NxCnt, -NxLines ) :-
% Nth line of AllLines, pointed to by Line, is replaced by Replacement
% to produce NxLines. If Replacement = '' then the line is removed and
% NxCnt increases CntRm.
%
replace_export_line( Line, CntRm, AllLines, Replacement, NxCnt, NxLines ) :-
	Nth is Line - CntRm,
	nth( Nth, AllLines, _LibTrm, RmLines ),
	( Replacement = '' ->
		NxCnt is CntRm + 1,
		NxLines = RmLines
		;
		add_nth( RmLines, 1, Nth, Replacement, NxLines ),
		NxCnt = CntRm
	).

construct_directive( not_load_directive(_Dct), _, vacuous ) :- !.
construct_directive( not_matching, _, vacuous ) :- !.
construct_directive( (if_pl,Args,2), S1, Nw ) :-
	Args = [IfPls,TTrm1], 
	construct_directive( TTrm1, S1, Nw1 ),
	( Nw1 = vacuous ->
		Nw = vacuous 
		;
		Nw = if_pl(IfPls,Nw1)
	).
construct_directive( (if_pl,Args,[2,3]), [S1,S2], Nw ) :-
% construct_directive( if_pl, Args, [2,3], [S1,S2], Nw ) :-
	!,
	Args = [IfPls,TTrm1,TTrm2], 
	construct_directive( TTrm1, S1, Nw1 ),
	construct_directive( TTrm2, S2, Nw2 ),
	( Nw1 = vacuous -> 
		( Nw2 = vacuous ->
			Nw = vacuous 
			;
			Nw = if_pl(not(IfPls),Nw2)
		)
		;
		( Nw2 = vacuous ->
			Nw = if_pl(IfPls,Nw1)
			;
			Nw = if_pl(IfPls,Nw1,Nw2)
		)
	).
construct_directive( (Req,[Specs],1), ToLoad, Nw ) :-
	memeberchk( Req, [require,requires] ),
	!,
	to_load_systemic( ToLoad, ToSys ), 
	systemic_sieves( ToLoad, ToSys, Specs, SysSpecs ),
	( SysSpecs == [] ->
		Nw = vacuous
		;
		Nw =.. [Req,SysSpecs]
	).
construct_directive( (ensure_loaded,PrvFiles,1), ToLoad, Nw ) :-
	!,
	to_load_systemic( ToLoad, ToSys ), 
	to_list( PrvFiles, Files ),
	systemic_sieves( ToLoad, ToSys, Files, SysFiles ),
	( SysFiles == [] ->
		Nw = vacuous
		;
		Nw = ensure_loaded(SysFiles)
	).
construct_directive( (use_module,[File],1), ToLoad, Nw ) :-
	!,
	to_load_systemic( ToLoad, ToSys ), 
	( ToSys == [] ->
		Nw = vacuous
		;
		Nw = use_module(File)
	).
construct_directive( (use_module,[File,Imps],1), ToLoad, Nw ) :-
	!,
	to_load_systemic( ToLoad, ToSys ), 
	( ToSys == [] ->
		Nw = vacuous
		;
		Nw = use_module(File,Imps)
	).
construct_directive( (use_module,[Mod,File,Imps],1), ToLoad, Nw ) :-
	!,
	to_load_systemic( ToLoad, ToSys ), 
	( ToSys == [] ->
		Nw = vacuous
		;
		Nw = use_module(Mod,File,Imps)
	).
construct_directive( (Cnsmp,[Files],1), ToLoad, Nw ) :-
	memberchk( Cnsmp, [consult,compile,load_files] ),
	!,
	to_load_systemic( ToLoad, ToSys ), 
	systemic_sieves( ToLoad, ToSys, Files, SysFiles ),
	( ToSys == [] ->
		Nw = vacuous
		;
		Nw = [Cnsmp,SysFiles]
	).
construct_directive( (load_files,[Files,Opts],1), ToLoad, Nw ) :-
	!,
	to_load_systemic( ToLoad, ToSys ), 
	systemic_sieves( ToLoad, ToSys, Files, SysFiles ),
	( ToSys == [] ->
		Nw = vacuous
		;
		Nw = load_files(SysFiles,Opts)
	).

to_load_systemic( [], [] ).
to_load_systemic( [H|T], ToSys ) :-
	( H = library(LibF) ->
		exlibris_bb_system_libs( SysLs ), 
		Opts = [relative_to(SysL),file_errors(fail),file_type(source),access(read)],
		findall( Full,
			( member( SysL, SysLs ),
			  afn( LibF, Full, Opts )
			  ), AllFull ),
		( AllFull == [] ->
			ToSys = TailSys
			;
			ToSys = [LibF|TailSys]
		)
		;
		ToSys = TailSys
	),
	to_load_systemic( T, TailSys ).
	
systemic_sieves( [], _LibFs, [], [] ).
systemic_sieves( [H|T], LibFs, [F|Rem], MtcLibFs ) :-
	( LibFs = [Hlf|Tlf] ->
		( H = library(Hlf) ->
			MtcLibFs = [F|TMtcLf]
			;
			MtcLibFs = TMtcLf
		),
		systemic_sieves( T, Tlf, Rem, TMtcLf )
		;
		MtcLibFs = []
	).


% diff_pl_source_files( +FileA, +FileB, +ExpTerm ) :-
% Succeeds if the exported version of FileA is identical to FileB.
% ExpTerm = exp(Rec,Pls,Rm) and it controls the transformation of 
% FileA to its exported version.
%
diff_pl_source_files( Source, Dst, ExpTerm ) :-
	file_to_list_of_lines( Source, SourceLines ),
	file_to_export_lines( Source, SourceLines, ExpTerm, SrcExpLines ),
	file_to_list_of_lines( Dst, DstLines ),
	file_to_export_lines( Dst, DstLines, ExpTerm, DstExpLines ),
	( DstExpLines = SrcExpLines ->
		true
		;
		werr( [['Diff clash: ',Source,' to ',Dst], 'Aborting.'] ), abort
	).

% xrefs_to_werr_term( +Pairs, +End, -ErrLines ) :-
% The left part of each element of a -pairs list is places on a line by itself,
% according to what werr/1 understands. That is a list within a nested list.
% Final element of ErrLines is End.
%
xrefs_to_werr_term( [], End, [End] ).
xrefs_to_werr_term( [H-_Root|T], End, [[H]|Tw] ) :-
	xrefs_to_werr_term( T, End, Tw ).

% file_in_system_lib( +File ) :-
% Succeds if File is within the system's library directory part of the
% filesystem. Uses global exlibris_bb_system_lib. 
% that was a blackboard primitive but i think swi dont have them.
%
file_in_system_lib( File ) :-
	exlibris_bb_system_libs( SysLs ), 
	member( SysL, SysLs ),
	atom_prefix( File, SysL ),
	!.

% file_not_to_be_copied( +File, +Cp, +Ls ) :-
% Succeeds if File is not a library file and Cp is == recursive.
% This indicate that we assume file was copied with top recursive copy.
%
file_not_to_be_copied( File, Cp, Ls ) :-
	Cp == recursive,
	findall( L, (member(L, Ls),atom_prefix(File,L)), [_H|_T]).

% exp_hm_lib_directories( +HmLs, +AfnOpts, -HmPairs ),
% Each HmL has a corresponding pair in HmPairs.
% These pairs are identical AbsHmL-AbsHmL ones.
% This is derived using afn/3 with options AfnOpts.
% 
exp_hm_lib_directories( [], _Opts, [] ).
exp_hm_lib_directories( [H|T], Opts, AbsDirs ) :-
	if_pl( swi(_), expand_file_name(H,[ExpH|_]), ExpH=H ),
	( afn( ExpH, AbsH, Opts ) ->
		AbsDirs = [AbsH-AbsH|AbsT]
		;
		werr( ['Skipping library, ',H] ),
		AbsDirs = AbsT
	),
	exp_hm_lib_directories( T, Opts, AbsT ).
% exp_lib_directories( +List, +Opts, -AbsList ) :-
% For each member of list AbsList holds its absolute_file_name/3 counterpart,
% according to options Opts.
%
exp_lib_directories( [], _Opts, [] ).
exp_lib_directories( [H|T], Opts, [ExpH-ExpH|ExpT] ) :-
	% ( afn( H, AbsH, Opts ) ->
		% AbsDirs = [AbsH-AbsH|AbsT]
		% ;
		% werr( ['Skipping library, ',H] ),
		% AbsDirs = AbsT
	% ),
	if_pl( swi(_), expand_file_name(H,[ExpH|_]), ExpH=H ),
	exp_lib_directories( T, Opts, ExpT ).

% ensure_load_barred( Files ) :-
% Succeeds if Files is a single file, ending in add_library_directory.
%
ensure_load_barred( Files ) :-
	atom(Files),
	atom_concat( _Swhere, add_library_directory, Files ).

% Locate directory for add_library_directory. 
% This is used in top of all source files.
%
add_lib_location( LDirPairs, AbsAdd ) :-
	member( Lib-_, LDirPairs ),
	afn( add_library_directory, AbsAdd, [file_type(source),
				access(exist),file_errors(fail),relative_to(Lib)] ),
	!.
add_lib_location( _LDirPairs, AbsAdd ) :-
	afn( add_library_directory, AbsAdd, [file_type(source),
				access(exist),file_errors(fail),relative_to(lib)] ),
	!.

% write_libs( +HmLs, +SysLs, +LDirPairs ) :-
% Report all library locations if echo is on.
%
write_libs( HmLs, SysLs, LDirPairs ) :-
	( exlibris_bb_echo(true) ->
		write( home_libs ), nl,
		write_list_with_line_numbers(HmLs,1,"2"),
		write( sys_libs ), nl,
		write_list_with_line_numbers(SysLs,1,"2"),
		write( ldirs ), nl,
		kv_decompose( LDirPairs, LDirs, _ ),
		write_list_with_line_numbers(LDirs,1,"2")
		;
		true
	).

%%% generic auxilliary predicates 

% pls_list( +InPls, -Pls ) :-
% Ensure Pls is a standarised form of InPls, pls/1 option.
% Pls can only be a variable or a list that contains no 
% variable as one of its elements. If InPls is such a list
% then a warning is given and Pls must be a variable.
%
pls_list( InPls, Pls ) :-
	( is_list(InPls) ->
		InListPls = InPls
		;
		InListPls = [InPls] 
		% we can actually let Pls = [InPls] here since 
		% InPls shouldnt be a variable.
	),
	pls_list_1( InListPls, [], Pls ).

% pls_list_1( +InPls, +Acc, +Pls ) :-
% As in pls_list/2, but with Pls reversly accumulated in Acc.
%
pls_list_1( [], Acc, Pls ) :-
	reverse( Acc, Pls ).
pls_list_1( [H|T], Acc, Pls ) :-
	( var(H) -> 
		werr( ['Variable in pls/1. Setting pls(_).'] )
		;
		pls_list_1( T, [H|Acc], Pls )
	).

% delete_all( +These, +From, -Remaining ) :-
% Remaining is the sublist of From for which all elements of These
% have been removed.
%
delete_all( [], List, List ).
delete_all( [H|T], List, Del ) :-
	delete_mine( List, H, NxList ),
	delete_all( T, NxList, Del ). 

% delete_mine( +List, +Elem, -List ).
% sicstus' delete doesnt cope with vars properly.
%
delete_mine( [], _Elem, [] ).
delete_mine( [H|T], Elem, DeList ) :-
	( \+ \+ (H = Elem) ->
		DeList = TdeList
		;
		DeList = [Elem|TdeList]
	),
	delete_mine( T, Elem, TdeList ).

% AddList is List plus Element at position Index - Nth.
% if length of List is < Index - Nth then Element is added as the last element
%
add_nth( [], _ShouldBeNth, _Nth, Element, [Element] ).
add_nth( [H|T], Idx, Nth, Element, AddList ) :-
	( Idx =:= Nth -> 
		AddList = [Element,H|T]
		;
		NxIdx is Idx + 1,
		AddList = [H|TAddList],
		add_nth( T, NxIdx, Nth, Element, TAddList )
	).

% afn( +A, +B, +C ) :-
% A first attempt to bridge incompatibilities of absolute_file_name/3
% across different prolog systems.
%
afn( A, B, C ) :-
	% exlibris_bb_src_exts( Exts ),
	% replace( C, file_type(source), extensions(Exts), O ),
	absolute_file_name( A, B, C ).
	% if_pl( swi(_), absolute_file_name(A,C,B), absolute_file_name(A,B,C) ).
	% if_pl( sicstus(_), absolute_file_name(A,B,C) ).
	% use expand(true) for swi 506 or 507 >

% read_term_with_line( +Stream, -Term, -Line ) :-
% Term is the read from Stream and it occures in line Line.
% 
read_term_with_line( Stream, Term, Line ) :-
	if_pl( swi(_), read_term_with_line_swi(Stream,Term,Line) ),
	if_pl( sicstus(_), read_term_with_line_sicstus(Stream,Term,Line) ),
	if_pl( yap(_), read_term_with_line_yap(Stream,Term,Line) ).

read_term_with_line_sicstus( Stream, Term, Line ) :-
	catch( read_term(Stream,Term,[layout(Layout)]), All, read_in_handler(false,All,Succ) ),
	( Succ == false ->
		Term = unreadalbe_term
		;
		(Layout = [Line|_] -> true ; Layout=Line)
	).
read_term_with_line_swi( Stream, Term, Line ) :-
	catch( read_term(Stream,Term,[term_position(Pos)]), All, read_in_handler(false,All,Succ) ),
	( Succ == false ->
		Term = unreadalbe_term
		;
		Pos = '$stream_position'(_, Line, _)
	).
read_term_with_line_yap( Stream, Term, Line ) :-
	catch( read_term(Stream,Term,[term_position(Pos)]), All, read_in_handler(false,All,Succ) ),
	( Succ == false ->
		Term = unreadalbe_term
		;
		Pos = Line
	).

all_pls( Pls ) :-
 	( var(Pls) ; Pls == all; Pls = [all] ), !.

atoms_longest_prefix( Atom1, Atom2, LstPfx ) :-
	atom_codes( Atom1, Cs1 ),
	atom_codes( Atom2, Cs2 ),
	longest_prefix( Cs1, Cs2, LstPfxCs ),
	atom_codes( LstPfx, LstPfxCs ).

loading_predicates( LdPrds ) :-
	LdPrds = [ ensure_loaded(_),
	           load_files(_),
			 load_files(_,_),
			 consult(_),
			 reconsult(_),
			 compile(_),
			 [_H|_T],
			 compile(_),
			 use_module(_),
			 use_module(_,_),
			 use_module(_,_,_),
			 require(_),
			 requires(_),
			 defines(_),
			 defines(_,_),
			 if_pl(_,_),
			 if_pl(_,_,_) ]. 
