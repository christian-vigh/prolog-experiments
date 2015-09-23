:- ensure_loaded( library('compat/if_pl') ).

:- if_pl( swi(_), ensure_loaded(library('compat/swi/file_exists'))).
