:- ensure_loaded( library(defines) ).		% /2.
:- defines( yap(_), absolute_file_name/3 ).

:- ensure_loaded( library(lists) ).		% memberchk/2.
									% delete/3.
									% append/3.
									% member/2.
:- ensure_loaded( library('term/to_list') ).			% /2.
:- ensure_loaded( library('file/is_a_directory') ).	% /1.
:- ensure_loaded( library('file/is_a_regular_file') ).	% /1.
:- ensure_loaded( library('file/append_to_dir') ).	% /3.
:- ensure_loaded( library('option/option_match') ).	% /6.
:- ensure_loaded( library('option/options_report_superfluous') ). % /4.
:- ensure_loaded( library('list/replace') ). 	% /4.

absolute_file_name( File, AbsFile, InOpts ) :-
	working_directory( This, This ),
	( memberchk(relative_to(RelForD),InOpts) ->
		to_relative_file( File, RelForD, RelFile )
		;
		RelFile = File
	),
	( memberchk(ignore_underscores(true),InOpts) ->
		( TrFile = RelFile ;
		  ( atom_codes(RelFile,RelFileCs),
		    delete(RelFileCs,0'_,TrFileCs),
		    atom_codes(TrFile,TrFileCs),
		    TrFile \== RelFile
		  )
		)
		;
		TrFile = RelFile
	),
	afn_defaults( Defs ),
	ErrStr = (absolute_file_name/3,3),
	afn_values_n_opts( ExtVs, SolVs, ErrVs, TypVs, AccVs, Known ),
	option_match( file_type, Type, ExtVs, ErrStr, InOpts, true ),
	( ( \+ var(Type),
	    file_type_implies_extensions( Type, TypeExts )
	  ) ->
		replace( InOpts, file_type(Type), extensions(TypeExts), Opts )
		;
		Opts = InOpts
	),
	( has_extension(TrFile) -> 
		Exts = ['']
		;
		option_match( extensions, PrvExts, ExtVs, ErrStr, Opts, Defs ),
		to_list( PrvExts, Exts )
	),
	member( Ext, Exts ),
	atom_concat( TrFile, Ext, ExtFile ),
	absolute_file_name( ExtFile, AbsFile ),
	option_match( solutions, Sols, SolVs, ErrStr, Opts, Defs ),
	FlErr = [fileerrors,file_errors],
	option_match( FlErr, Errors, ErrVs, ErrStr, Opts, Defs ),
	option_match( file_type, Type, TypVs, none, Opts, Defs ),
	% option_match( file_type, Type, TypVs, ErrStr, Opts, Defs ),
	option_match( access, Access, AccVs, ErrStr, Opts, Defs ),
	options_report_superfluous( Opts, Known, ErrStr, _UnEx ), 
	fname_qualifies( AbsFile, Errors, Type, Access ),
	( Sols == first -> ! ; true ),
	working_directory( _MaybeThat, This ).

afn_defaults( Defs ) :- 
	Defs = [solutions(first),file_errors(error),file_type(text),
	        access(none),extensions([''])].

afn_values_n_opts( ExtVs, SolVs, ErrVs, TypVs, AccVs, Known ) :-
	ExtVs = _Any,
	SolVs = [first,all], 
	ErrVs = [fail,error],
	TypVs = [text,source,directory], 
	AccVs = [read,write,append,exist,none],
	Known = [solutions(_),fileerrors(_),file_errors(_),file_type(_),
	         relative_to(_),ignore_underscores(_),access(_),extensions(_)].

fname_qualifies( AbsFile, Errors, Type, Access ) :-
	( Access == none -> 
		true
		;
		current_prolog_flag( fileerrors, FlErrs ),
		( Errors == error ->
			set_prolog_flag( fileerrors, on )
			;
			set_prolog_flag( fileerrors, off )
		),
		( (Type==directory,Access==exist) ->
			( is_a_directory( AbsFile ) ->
				true
				;
				( Errors == error ->
					( throw( error(existence_error(source_sink,AbsFile),absolute_file_name/3) ) ; abort )
					;
					fail
				)
			)
			;
			( Access == exist ->
				is_a_regular_file(AbsFile)
				;
				catch( open( AbsFile, Access, Stream ),
				       _Any,
					  (set_prolog_flag(fileerrors,FlErrs),fail)),
				close( Stream )
			)
		),
		set_prolog_flag( fileerrors, FlErrs )
	).
	
file_type_implies_extensions( source, ['.pl','.yap'] ).

has_extension( File ) :-
	atom_codes( File, FileCs ), 
	append( _Pre, [0'.|_Post], FileCs ),
	!.

to_relative_file( File, RelForD, RelFile ) :-
	( file_exists(RelForD) ->
		RelFile = File,
		(is_a_directory(RelForD) ->
			working_directory( _Old, RelForD )
			;
			(is_a_regular_file(RelForD) ->
				fname_dir_name( RelForD, D, _F ),
				working_directory( _Old, D )
				;
				% "when" Yap/Sicstus support for linkto/1 ala ciao
				% is enabled, chase symlinks here.
				true
			)
		)
		;
		% compatible with sicstus 3.9.0 behaviour
		append_to_dir( RelForD, File, RelFile )
	).
