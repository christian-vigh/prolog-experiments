/*  AutoTest.PL
    Shelved on the 21st of December 1987
*/


/*  PORTABILITY  */


/*  '$log_files'( List ):
    The name(s) of the log file.
*/
'$log_files'( [user,'AUTOTEST.LOG'] ).


/*  UTILITIES  */


/*  member( X+, L+ ):
    X is a member of list L.
*/
'$member'( X, [X|_] ) :- !.
'$member'( X, [_|T] ) :-
    '$member'( X, T ), !.


/*  OUTPUT  */


?- op( 40, xfy, <> ).
?- op( 40, xfy, ... ).


/*  '$writef'( V+ ):
    Write V to the COS.
    If V = nl, take a newline.
    If V = A<>B, '$writef' A, then B.
    Treat A...B as A<> ' ' <>B.
    If V = '$'(L), assume L is a list of ASCII codes, turn into an atom,
     and write that.
    Else, write V as it is.
*/
'$writef'( V ) :-
    var( V ), !, write(V).

'$writef'( A<>B ) :-
    !, '$writef'(A), '$writef'(B).

'$writef'( A...B ) :-
    !, '$writef'(A), '$writef'(' '), '$writef'(B).

'$writef'( '$'(L) ) :-
    !, name( A, L ), write( A ).

'$writef'( nl ) :-
    !, nl.

'$writef'( X ) :-
    write( X ).


/*  '$writef_to'( File+, Text+ ):
    Do '$writef'(Text), but tell(File) first, and restore the old
    COS after.
*/
'$writef_to'( File, Text ) :-
    telling( COS ),
    tell( File ),
    '$writef'( Text ),
    tell( COS ).


/*  LOG FILES  */


/*  '$writef_to_log'( Text+ ):
    writef(Text) to each log file.
*/
'$writef_to_log'( Text ) :-
    '$log_files'( LogFiles ),
    '$writef_to_logs'( LogFiles, Text ).


'$writef_to_logs'( [], Text ) :- !.
'$writef_to_logs'( [Log|Others], Text ) :-
    '$writef_to'( Log, Text ),
    '$writef_to_logs'( Others, Text ), !.


/*  switch_log_files_to( NewFiles ):
    Close any log files not named in NewFiles, and reset so that
    future '$writef_to_log's write to the new files.
*/
'$switch_log_files_to'( NewFiles ) :-
    '$close_log_files_except'( NewFiles ),
    '$set_new_log_files'( NewFiles ).


'$set_new_log_files'( NewFile ) :-
    atom( NewFile ),
    asserta( '$log_files'([NewFile]) ), !.

'$set_new_log_files'( NewFiles ) :-
    retract( '$log_files'(OldFiles) ),
    asserta( '$log_files'(NewFiles) ).


/*  '$close_log_files_except'( L+ ):
    Close any log files whose names are not in L.
*/
'$close_log_files_except'( NewFiles ) :-
    '$log_files'(OldFiles),
    '$close_log_files_except'( OldFiles, NewFiles ).


'$close_log_files_except'( [], _ ) :- !.

'$close_log_files_except'( [Old|OtherOlds], NewFiles ) :-
    member( Old, NewFiles ),
    '$close_log_files_except'( OtherOlds, NewFiles ), !.

'$close_log_files_except'( [Old|OtherOlds], NewFiles ) :-
    telling( COS ), tell( Old ), told, tell( COS ),
    '$close_log_files_except'( OtherOlds, NewFiles ), !.


/*  MAIN PROGRAM  */


/*  '$run'( F+ ):
    MAIN PREDICATE.
    Read filenames from steering file F, and run the tests in each file.
    Close any open log files.
*/
'$run'( F ) :-
    '$read_and_obey_steering_file'( F ),
    '$close_log_files_except'( [] ).


/*  '$run_test'( F+ ):
    MAIN PREDICATE.
    Run the tests in F.
    Close any open log files.
*/
'$run_test'( F ) :-
    '$run_test_file'( F ),
    '$close_log_files_except'( [] ).


'$read_and_obey_steering_file'( F ) :-
    seeing( CIS ),
    see( F ), seen, see( F ),
    '$read_and_obey_steering_file',
    seen, see( CIS ).


/*  '$read_and_obey_steering_file':
    Read the next line from the steering file.
    Unless at end-of-file, run the test file so named, and repeat.
*/
'$read_and_obey_steering_file' :-
    read( F ),
    F \= end_of_file,
    '$switch_files?'( F ).

'$read_and_obey_steering_file'.
    /*  Here if end of steering file.  */


'$switch_files?'( stay ) :-
    '$run_test_file'.

'$switch_files?'( D ) :-
    '$directive'( D ),
    '$obey_directive'( D ),
    '$read_and_obey_steering_file'.

'$switch_files?'( F ) :-
    '$run_test_file'( F ),
    '$read_and_obey_steering_file', !.


/*  '$run_test_file'( F+ ):
    F names a file of tests. Open it, run each test, and close,
    resetting CIS to its original value.
*/
'$run_test_file'( F ) :-
    seeing( CIS ),
    see( F ), seen, see( F ),
    '$run_test_file',
    seen, see( CIS ).


'$run_test_file' :-
    read( Term ),
    Term \= end_of_file,
    '$process_test_term'( Term ),
    '$run_test_file', !.

'$run_test_file'.
    /*  Here on end of test file.  */


/*  '$directive'( Term+ ):
    Term is a directive.
*/
'$directive'( log_all_tests(_) ) :- !.
'$directive'( log_to(_) ) :- !.


/*  obey_directive( D+ ):
    Obey directive D, and succeed.
*/                    
'$obey_directive'( log_to(F) ) :-
    '$switch_log_files_to'(F), !.

'$obey_directive'( log_all_tests(on) ) :-
    asserta( '$log_all_tests' ), !.

'$obey_directive'( log_all_tests(off) ) :-
    retractall( '$log_all_tests' ), !.


/*  Operator used to join a goal and its specification.  */
?- op( 255, xfx, :: ).


/*  '$process_test_term'( Term+ ):
    Term is one term from a test file.
    If it's a term of the form Goal::Spec, pass to '$test'.
    If it's a directive, obey it.
    Else, assume it's to be written.
*/
'$process_test_term'( D ) :-
    '$directive'( D ),
    '$obey_directive'( D ), !.

'$process_test_term'( Goal::Spec ) :-
    '$test'( Goal::Spec ), !.

'$process_test_term'( Term ) :-
    '$writef_to_log'( Term ).


/*  '$test'( Term+ ):
    Term is a goal plus its specified (expected) action.
    Extract the specification, call the goal, and match its
    behaviour against the specification.
*/
'$test'( Goal_and_Spec ) :-
    '$specification'( Goal_and_Spec, Spec, Goal ),
    '$test_against_specification'( Goal, Spec ), !.


/*  '$specification'( Term+, Spec-, Goal- ):
    Separates a term into its goal and specification.
*/
'$specification'( (G::s), succeeds, G ) :- !.
'$specification'( (G::succeeds), succeeds, G ) :- !.
'$specification'( (G::f), fails, G ) :- !.
'$specification'( (G::fails), fails, G ) :- !.
'$specification'( (G::c), crashes, G ) :- !.
'$specification'( (G::crashes), crashes, G ) :- !.
'$specification'( (G::c(ErrorType)), crashes_with_error_code(ErrorType), G ) :- !.
'$specification'( (G::crashes(ErrorType)), crashes_with_error_code(ErrorType), G ) :- !.
'$specification'( (G::PostTests), must_pass(PostTests), G ) :- !.


/*  '$test_against_specification'( Goal+, Spec+ ):
    Call Goal, and match its effect against Spec.
*/
'$test_against_specification'( Goal, Spec ) :-
    '$call_giving_sfe'( Goal, SFE ),
    '$match_against_sfe'( Goal, Spec, SFE ), !.


/*  '$call_giving_sfe'( Goal+, SFE- ):
    Call Goal.
    Set SFE to:
    'succeeds' if Goal succeeds;
    'fails' if it fails;
    'crashes_with_error_code(ErrorKind)' if it gives an error of kind ErrorKind;
    'crashes' if it gives an error whose kind can't be determined.
*/
'$call_giving_sfe'( Goal, succeeds ) :-
    call( Goal ), !.

'$call_giving_sfe'( Goal, fails ) :- !.


/*  '$matches_sfe'( Expected+, Actual+, Answer- ):
    Always succeeds.
    Answer is some English text specifying how the Actual result
    of a call matched the Expected result.
    If they matched OK, then Answer = ok.
*/
'$matches_sfe'( must_pass(Tests), succeeds, Text ) :-
    '$post_tests_ok'( Tests, Text ), !.

'$matches_sfe'( must_pass(Tests), failed_tests(FE), Text ) :-
    '$english_for_action'( FE, Actually ),
    Text = ('should have passed tests' ... Tests ... 'but' ... Actually <> nl ), !.

'$matches_sfe'( must_pass(Tests), FE, Text ) :-
    '$matches_sfe'( succeeds, FE, Text ), !.

'$matches_sfe'( SFE, SFE, ok ) :- !.

'$matches_sfe'( crashes, crashes_with_error_code(_), ok ) :- !.

'$matches_sfe'( crashes_with_error_code(_), crashes, ok ) :- !.

'$matches_sfe'( crashes_with_error_code(C), crashes_with_error_code(C), ok ) :- !.

'$matches_sfe'( Expected, Actual, Text ) :-
    '$english_for_action'( Actual, Actually ),
    '$english_for_action'( Expected, Expectedly ),
    Text = ('should have' ... Expectedly ... 'but' ... Actually <> nl ), !.


/*  '$match_against_sfe'( Goal+, Expected+, Actual+ ):
    If the expected result differs from the actual, then
    write to the log, saying so.
    Write to the log also if the test succeeded and 'log_all_tests' was on.
*/
'$match_against_sfe'( Goal, Expected, Actual ) :-
    '$matches_sfe'( Expected, Actual, What ),
    '$report_match'( Goal, What ).


'$report_match'( Goal, ok ) :-
    '$log_all_tests',
    '$writef_to_log'( 'Goal'...Goal...'passed test'<>nl ), !.

'$report_match'( Goal, ok ) :- !.

'$report_match'( Goal, NotOK ) :-
    '$writef_to_log'( 'Goal'...Goal...NotOK ), !.


'$english_for_action'( succeeds, succeeded ) :- !.
'$english_for_action'( fails, failed ) :- !.
'$english_for_action'( crashes, crashed ) :- !.
'$english_for_action'( crashes_with_error_code(ErrorKind),
                       'crashed with error' ... ErrorKind ) :- !.
'$english_for_action'( must_pass(Tests), 'passed tests'...Tests ) :- !.


/*  '$post_tests_ok'( Tests+, Text- ):
*/
'$post_tests_ok'( Tests, Text ) :-
    '$call_giving_sfe'( Tests, TestsSFE ),
    (
        TestsSFE = succeeds,
        Text = ok
    ;
        '$matches_sfe'( must_pass(Tests), failed_tests(TestsSFE), Text )
    ), !.
