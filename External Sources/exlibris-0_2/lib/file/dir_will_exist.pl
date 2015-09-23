:- ensure_loaded( library('file/is_a_directory') ).		%.

dir_will_exist( Dir ) :- 
	( is_a_directory(Dir) ->
	    	true
		;
		make_directory( Dir )
	).
