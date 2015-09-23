/* TEST.PL : Test compiled routines by interpreting them

						SPECIAL PURPOSE
						Lawrence
						Updated: 21 November 80
*/

%% Uses utility: FILES.PL %%


  :- public test/1,
	    untest/1.


  :- mode test(+),
	  untest(+),
	  test(+,+),
	  testzap(+,+),
	  reviveall(+).



			% test or untest a file list

test(X) :- test(X,test).


untest(X) :- test(X,untest).



			% Run down a file list

test(V,_)
     :-	var(V),
	!,
	ttynl, display('** TEST given a variable'), ttynl.

test([],_) :- !.

test([X|Rest],Type)
     :-	!,
	test(X,Type),
	test(Rest,Type).

test(File,test)
     :-	check_exists(File),
	!,
	[ -File ].

test(File,untest)
     :-	open(Old,File),
	repeat,
	   read(X),
	   testzap(X,File),
	close(File),
	see(Old),
	ttynl, display('Back to compiled version of '),
	display(File), ttynl,
	!.

test(File,Type)
     :-	ttynl, display('** Cannot '), display(Type),
	ttyput(" "), display(File), ttynl.



			% Get things back to normal by finding the
			%  public directive and reviving them all.
			%  Note that only the first directive is used,
			%  ie: all publics should be in one directive.

testzap(end_of_file,File)
     :-	!,
	display('(failed to find public directive in '),
	display(File), ttyput(")"), ttynl.

testzap(:-(public(Publics)),_)
     :-	reviveall(Publics).

% otherwise fail round again



			% Revive all Preds given in conjunct

reviveall((X,Rest))
     :-	!,
	reviveall(X),
	reviveall(Rest).

reviveall(Pred/Arity)
     :-	revive(Pred,Arity).



