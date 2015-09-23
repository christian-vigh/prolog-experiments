 % MODULE msg.pl 
 % For error messages

 :- module(msg,[ msg/1, msg/2]).
 :- use_module(gfutils,[writelist/1]).
	  
 msg(17) :- write(' => Trouble with the editor. (Wrong path or missing extension)'),nl.
 msg(18) :- write(' => Compilation finished'),nl.
 msg(24) :- write(' => Error in feature hierarchies '),nl.
 msg(25) :- write(' Wrong command. (Press ? for help)'),nl.
 msg(26) :- put(7), write(' File not found'),nl,fail.
 msg(27) :- write(' => Linguistic DB loaded'),nl.
 msg(29) :- put(7), write(' I don\'t know that sentence'),nl.	    
 msg(30) :- put(7), write(' No parsed expression'),nl.	 
 msg(31) :- put(7), write(' Representation dumped in file'),nl.
 msg(32) :- write(' => No grammar loaded!'),nl.

 % compiler messages

 msg(1) :- nl,write('ERROR: Functions not declared'),nl.
 msg(20) :- nl,write('ERROR: I can\'t compile the rule ').
 msg(22) :- nl,write('=> Generating top-down filter tables '),nl.
 msg(21) :- write('I can\'t open').
 msg(33) :- write('Unable to compile file:\nFunctions not declared or grammar file nor loaded'),nl.

 % Others

 msg(24,T) :- write(T),write(' sec.'), nl.
 msg(28,L) :- write(' Trying "'),writelist(L),write('"'),nl.   
