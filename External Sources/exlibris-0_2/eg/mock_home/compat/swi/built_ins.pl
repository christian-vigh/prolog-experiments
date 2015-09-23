:- ensure_loaded( library(defines) ).		% built_in/2.

:- built_in( swi(_), [is_list/1,append/3,member/2,memberchk/2,delete/3,select/3,nth0/3,nth1/3,last/2,reverse/2,flatten/2,length/2,merge/3] ). % list manipulation...

:- built_in( swi(_), [absolute_file_name/2,absolute_file_name/3]  ).
