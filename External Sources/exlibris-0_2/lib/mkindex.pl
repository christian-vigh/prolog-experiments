% :- module( mkindex, [mkindex/1] ).
:- ensure_loaded( library(defines) ).		% /1.
:- defines( mkindex/1 ).

% :- requires( [append/3,reverse/2] ).
% ensure_loaded( library(lists) ).					% append/3, reverse/2/
:- ensure_loaded( library('file/directory_contents') ). 	% /4.
:- ensure_loaded( library('file/append_to_dir') ). 		% /3.
:- ensure_loaded( library('file/fname_stripping_mild') ). 	%
:- ensure_loaded( library('term/write_terms') ).			% /3.
:- ensure_loaded( library('meta/head_to_spec') ). 		% /2.
:- ensure_loaded( library('term/to_list') ). 			% /2.
:- ensure_loaded( library('option/select_preferred') ).	% /3.
:- ensure_loaded( library('term/tuple_to_list') ).		% /2.
%
:- ensure_loaded( library('compat/if_pl') ).				% reverse/2, append/3.
:- if_pl( not(swi(_)), ensure_loaded( library(lists)) ).
:- if_pl( yap(_), ensure_loaded( library('compat/yap/absolute_file_name')) ).% /3.

% Todo: add a list of predicates to be ignored (ie main/1, test/0).
%
mkindex( PrvArgs ) :-
	to_list( PrvArgs, Args ),
	mkindex_defaults( Defs ),
	select_preferred( dir(Dir), Args, Defs ),
	select_preferred( pick(PrvPicks), Args, Defs ),
	select_preferred( exts(PrvExts), Args, Defs ),
	select_preferred( block(PrvBlock), Args, Defs ),
	select_preferred( style(Style), Args, Defs ),
	pick_standards( PrvPicks, Picks ),
	to_list( PrvBlock, Block ),
	to_list( PrvExts, AtomExts ),
	atom_codes_list( AtomExts, Exts ),
	mkindex( Dir, Style, Picks, Exts, Block ).

mkindex( Dir, Style, Picks, Exts, Block ) :-
	absolute_file_name( Dir, AbsDir, [file_type(directory),access(exist)] ),
	directory_contents( Dir, PrvFiles, SubDirs, _Other ),
	delete( PrvFiles, 'Index.pl', Files ),
	source_files( Files, '', Exts, Block, Sources ),
	subdirs( SubDirs, '', Block, AppDirs ),
	working_directory( Current, AbsDir ),
	mkindex( Sources, Picks, '', AppDirs, AbsDir, Exts, Block, [], Index ),
	working_directory( _Trail, AbsDir ),
	sort( Index, Sorted ),
	to_index_files( Style, Sorted ),
	working_directory( _, Current ).

to_index_files( both, Sorted ) :-
	to_index_files( std, Sorted ),
	to_index_files( new, Sorted ),
	!.
to_index_files( std, Sorted ) :-
	!,
	index_to_clauses_std( Sorted, StdClauses ),
	write_terms( 'INDEX.pl', write, StdClauses, [quoted(true)] ).
to_index_files( new, Sorted ) :-
	index_to_clauses_new( Sorted, NewClauses ),
	Pre = [(:- multifile index/5),(:- dynamic index/5),[]],
	append( Pre, NewClauses, AllNew ),
	write_terms( 'Index.pl', write, AllNew, [quoted(true)] ).

mkindex_defaults( Defs ) :-
	Defs = [
		dir( '.' ),
		block( [] ),
		pick( all ),
		exts( ['.pl','.yap'] ),
		style( new )
		].

mkindex( [], Picks, _DirFx, Dirs, AbsDir, Exts, Block, AccIndex, Index ) :-
	( Dirs == [] ->
		Index = AccIndex
		;
		to_next_dir( Dirs, AbsDir, Exts, Block, NxFiles, NxDirFx, NxDirs ),
		mkindex( NxFiles, Picks, NxDirFx, NxDirs, AbsDir, Exts, Block, AccIndex, Index )
	).
mkindex( [H|T], Picks, DirFx, Dirs, AbsDir, Exts, Block, AccIndex, Index ) :-
	file_defines( H, Picks, DirFx, AccIndex, NxIndex ),
	mkindex( T, Picks, DirFx, Dirs, AbsDir, Exts, Block, NxIndex, Index ).

file_defines( File, Picks, DirFx, AccIndex, NxIndex ) :-
	defining_directives( Picks, Drctvs ),
	% ( File == 'debug_html_own.pl' -> trace ; true ),
	( memberchk(derive,Picks) ->
		load_terms_until_match_or_limit( File, 1, [(_,true)], Drctvs, Terms )
		;
		NoCount = [((:- ensure_loaded(library(Ld))),
		             atom_concat(_,defines,Ld)   )],
		load_terms_until_match_or_limit( File, 1, NoCount, Drctvs, Terms )
	),
	fname_stripping_mild( File, ".pl", Stem, _Full ),
	append_to_dir( DirFx, Stem, IdxFile ),
	terms_define( Terms, IdxFile, AccIndex, NxIndex ).
	% defines_into_index( Predicates, IdxFile, AccIndex, ITail ).

terms_define( Terms, IdxFile, AccIndex, NxIndex ) :-
	( module_defined(Terms,IdxFile,AccIndex,NxIndex) ->
		true
		;
		( directive_defined(Terms,IdxFile,AccIndex,NxIndex) ->
			true
			;
			terms_have_multifiles_n_pls( Terms, [], [], Multis, Pls ),
			extract_definitions(Terms,IdxFile,Pls,Multis,AccIndex,NxIndex)
		)
	).

terms_have_multifiles_n_pls( [], Multis, AccPls, Multis, Pls ) :-
	( AccPls == [] ->
		Pls = any
		;
		Pls = AccPls
	).
terms_have_multifiles_n_pls( [H|T], AccM, AccP, Multis, Pls ) :-
	( ( (H = (:- multifile Tuple),tuple_to_list(Tuple,List))
	 ;(H = (:- multifile(HMulti)),to_list(HMulti,List))
	  ) ->
		append( List, AccM, NxAccM ),
		NxAccP = AccP
		;
		NxAccM = AccM,
		( H = (:- for(PrvForPls)) ->
			to_list( PrvForPls, ForPls ),
			append( AccP, ForPls, NxAccP )
			;
			NxAccP = AccP
		)
	),
	terms_have_multifiles_n_pls( T, NxAccM, NxAccP, Multis, Pls ).

extract_definitions( [], _File, _Pls, _Multis, Index, Index ).
extract_definitions( [H|T], File, Pls, Multis, AccIndex, Index ) :-
	( directive(H) ->
		( H = (:- built_in(BinPls,PrvPreds)) ->
			to_list( PrvPreds, Preds ),
			add_specifications_to_index( Preds, File, BinPls, AccIndex, NxAccIndex)
			;
			NxAccIndex = AccIndex
		)
		;
		( rule( H, Name, Arity ) ->
			true
			;
			fact( H, Name, Arity )
		),
		( memberchk(Name/Arity,Multis) ->
			NxAccIndex = AccIndex
			;
			add_to_predicate_accumulator( Name, Arity, Pls, user, File, AccIndex, NxAccIndex )
		)
	),
	extract_definitions( T, File, Pls, Multis, NxAccIndex, Index ).

add_specifications_to_index( [], _File, _Pls, Acc, Acc ).
add_specifications_to_index( [Name/Arity|T], File, Pls, Acc, Index) :-
	add_to_predicate_accumulator( Name, Arity, Pls, built_in, File, Acc, NxAcc ),
	add_specifications_to_index( T, File, Pls, NxAcc, Index).

add_trips_to_predicate_accumulator( [], _File, Acc, Acc ).
add_trips_to_predicate_accumulator( [H|T], File, Acc, Index ) :-
	H = (Name,Arity,Pls,Mod),
	add_to_predicate_accumulator( Name, Arity, Pls, Mod, File, Acc, NxAcc ),
	add_trips_to_predicate_accumulator( T, File, NxAcc, Index ).

add_to_predicate_accumulator( Name, Arity, Pls,  Mod, File, Acc, NxAcc ) :-
	% findall( That, (
		% member((Name,Arity,ExPls,Mod,That),Acc),
		% pl_defs_overlap(Pls,ExPls)
		% Those ),
	Those = [], % till we define pl_defs_overlap/2.
	( Those == [] ->
		NxAcc = [(Name,Arity,Pls,Mod,File)|Acc]
		;
		Those = [This|_ShouldBeEmpty],
		NxAcc=Acc,
		( This == File ->
			true
			;
			addition_error(Name,Arity,Pls,_ExPlsMod,This,File)
		)
	).

directive( (:- _Goal) ).

rule( (H-->_B), Name, Arity ) :-
	head_to_spec( H, Name/OrgArity ),
	Arity is OrgArity + 2.
rule( (H:-_B), Name, Arity ) :-
	head_to_spec( H, Name/Arity ).

fact( H, Name, Arity ) :-
	head_to_spec( H, Name/Arity ).

module_defined( Terms, File, AccIndex, NxIndex ) :-
	M2 = (:- module(ModName,PredicateList)),
	M3 = (:- module(ModName,PredicateList,_Opts)),
	( nth( N, Terms, M2, _Rest ) ; nth( N, Terms, M3, _Rem ) ), 
	!,
	( N > 1 ->
		module_def_warning( File, M2, M3, N )
		;
		true
	),
	predicate_list_to_index_trips( PredicateList, any,  ModName, HTrips ),
	add_trips_to_predicate_accumulator( HTrips, File, AccIndex, NxIndex ).

directive_defined( Terms, File, AccIndex, NxIndex ) :-
	( D=(:- defines(Pls,InPrdList));(D=(:- defines(InPrdList)),Pls=any)),
	nth( N, Terms, D, _Rest ),
	( ( N > 2 ->
			wrongly_positioned_directive_warning( File, D, N ),
			NxIndex = AccIndex
			;
			to_list( InPrdList, PrdList ),
			predicate_list_to_index_trips( PrdList, Pls, user, HTrips ),
			add_trips_to_predicate_accumulator( HTrips, File, AccIndex, NxIndex )
		)
		;
		NxIndex = AccIndex
	),
	!.

predicate_list_to_index_trips( [], _Pls, _ModName, [] ).
predicate_list_to_index_trips( [Nm/Ar|T], Pls, Mod, [(Nm,Ar,Pls,Mod)|Trips] ) :-
	predicate_list_to_index_trips( T, Pls, Mod, Trips ).

to_next_dir( [H|T], AbsDir, Exts, Block, Sources, H, Dirs ) :-
	working_directory( _Old, AbsDir ),
	working_directory( _AbsHere, H ),
	directory_contents( '.', Files, SubDirs, _Other ),
	source_files( Files, H, Exts, Block, Sources ),
	subdirs( SubDirs, H, Block, AppDirs ),
	append( T, AppDirs, Dirs ).

index_to_clauses_std( [], [] ).
index_to_clauses_std( [(Nm,Ar,_Pls,Md,Fl)|T], [index(Nm,Ar,Md,Fl)|TI] ) :-
	index_to_clauses_std( T, TI ).

index_to_clauses_new( [], [] ).
index_to_clauses_new( [(Nm,Ar,Pls,Md,Fl)|T], [index(Nm,Ar,Pls,Md,Fl)|TI] ) :-
	index_to_clauses_new( T, TI ).

subdirs( [], _Pfx, _Block, [] ).
subdirs( [H|T], Pfx, Block, App ) :-
	append_to_dir( Pfx, H, Happ ),
	( memberchk(Happ,Block) ->
		write( blocking_dir(Happ) ), nl,
		App = Tapp
		;
		App = [Happ|Tapp]
	),
	subdirs( T, Pfx, Block, Tapp ).

source_files( [], _Dir, _Exts, _Block, [] ).
source_files( [H|T], Dir, Exts, Block, Sources ) :-
	( (  member( Ext, Exts ),
		fname_stripping_mild(H,Ext,_Stem,H),
		H\=='Index.pl',
		H\=='INDEX.pl'
	  ) ->
		append_to_dir( Dir, H, DirH ),
		( memberchk(DirH,Block) ->
			write( blocking(DirH) ), nl,
			Sources = TSources
			;
			Sources = [H|TSources]
		)
		;
		Sources = TSources
	),
	source_files( T, Dir, Exts, Block, TSources ).

pick_standards( InPick, StdPicks ) :-
	to_list( InPick, PickList ),
	pick_standards_1( PickList, [], [], NoMatch, PrvPicks ),
	( NoMatch == [] ->
		true
		;
		write( user_error, 'Unknown pick selections, ' ),
		write( user_error, NoMatch ),
		nl( user_error )
	),
	( PrvPicks == [] ->
		write( user_error, 'Found not known pick selection in, ' ),
		write( user_error, InPick ),
		nl( user_error ),
		StdPicks == [module,define,derive],
		write( user_error, 'Using, \`all\' (module,define,derive).' ),
		nl( user_error )
		;
		PrvPicks = StdPicks
	).

pick_standards_1( [], NMAcc, PcAcc, NoMatch, Picks ) :-
	reverse( NMAcc, NoMatch ),
	reverse( PcAcc, Picks ).
pick_standards_1( [H|T], NMAcc, PcAcc, NoMatch, Picks ) :-
	( memberchk(H,[module,define,derive]) ->
		NxNMAcc = NMAcc,
		( memberchk(H,PcAcc) ->
			NxPcAcc = PcAcc
			;
			NxPcAcc = [H|PcAcc]
		),
		NxT = T
		;
		( H == all ->
			append( [module,define,derive], T, NxT ),
			NxNMAcc = NMAcc
			;
			NxNMAcc = [H|NMAcc],
			NxT = T
		),
		NxPcAcc = PcAcc
	),
	pick_standards_1( NxT, NxNMAcc, NxPcAcc, NoMatch, Picks ).

defining_directives( Picks, Drctvs ) :-
	( memberchk(module,Picks) ->
		Dct1 = [
				(:- module(_,_) ),
				(:- module(_,_,_))
			]
			;
		Dct1 = []
	),
	( memberchk(define,Picks) ->
		Drctvs = [(:- defines(_,_)),(:- defines(_))|Dct1]
		;
		Drctvs = Dct1
	).

module_def_warning( File, M2, M3, N ) :-
	( var(M2) -> M = M3 ; M = M2 ),
	wrongly_positioned_directive_warning( File, M, N ).

atom_codes_list( [], [] ).
atom_codes_list( [H|T], [HCs|TCs] ) :-
	atom_codes( H, HCs ),
	atom_codes_list( T, TCs ).

wrongly_positioned_directive_warning( File, Drctv, N ) :-
	Drctv = (:- Goal),
	head_to_spec( Goal, Spec ),
	write( user_error, ' Warning: ' ),
	write( user_error, Spec ), 
	write( user_error, ' directive, ' ),
	write( user_error, Goal ), 
	write( user_error, ' appeared in line, ' ),
	write( user_error, N ), 
	write( user_error, ' of file, ' ),
	write( user_error, File ),
	write( user_error, '.' ), nl( user_error ).

addition_error( Name, Arity, Pls1, Pls2, Mod, ThatFile, File ) :-
	write( user_error, multiple_defined_predicate(Name,Arity,Mod) ),
	nl( user_error ),
	write( user_error, with_systems_defs(Pls1,Pls2) ),
	nl( user_error ),
	write( user_error, in_files(ThatFile,File) ), 
	nl( user_error ),
	write( user_error, skipping_file(File) ),
	nl( user_error ).

load_terms_until_match_or_limit( File, Limit, DnC, Drctvs, Terms ) :-
	open( File, read, Stream ),
	% File = 'genmakefile.pl',
	stream_load_terms_until_match_or_limit( Stream, File, 1, DnC, Limit, Drctvs, Terms ),
	close( Stream ).

stream_load_terms_until_match_or_limit( Stream, File, Current, DnC, Limit, Untils, Terms ) :-
	( catch( read( Stream, Term ), _Any, fail ) ->
		( Term == end_of_file   ->
			Terms = []
			;
			% maybe we should use \+ \+ since Term is last 
			% element anyway
			( memberchk(Term,Untils) ->
				Terms = [Term]
				;
				( (\+ \+ (memberchk((Term,Call),DnC),call(Call)) ) ->
					NxC is Current
					;
					NxC is Current + 1
				),
				( NxC > Limit ->
					Terms = [Term]
					;
					Terms = [Term|MoreTerms],
					stream_load_terms_until_match_or_limit( Stream, File, NxC, DnC, Limit, Untils, MoreTerms )
				)
			)
		)
		;
		write( user_error, 'Reading error, while processing file ' ),
		write( user_error, File ), nl( user_error ),
		Terms = []
	).
