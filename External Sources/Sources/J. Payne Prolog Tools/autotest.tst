/*  AUTOTEST.TST  */

/*  SAMPLE INPUT TO AUTOTEST.PL

    This input contains some terms which should provoke messages
    from AutoTest.

    This is the expected output:

Line 1 of AutoTest.TST
Goal 5 = 5 should have failed but suceeded
Goal 6 = 6 should have failed but suceeded
Goal 7 \= 7 should have suceeded but failed
Goal 8 \= 8 should have suceeded but failed
Goal 9 = 9 should have crashed but suceeded
Goal 10 = 10 should have crashed but suceeded
Goal 11 \= 11 should have crashed but failed
Goal 12 \= 12 should have crashed but failed
Goal 13 = 13 should have crashed with error 137 but suceeded
Goal 14 = 14 should have crashed with error 137 but suceeded
Goal 15 \= 15 should have crashed with error 137 but failed
Goal 16 \= 16 should have crashed with error 137 but failed
Goal functor(f(1, 2), f, 2) should have passed tests f = g , 2 = 3 but failed
Final line of AutoTest.TST

*/

'Line 1 of' ...
'AutoTest.TST'<>nl.             /*  Display  */

1 = 1 :: s.                     /*  OK  */

2 = 2 :: succeeds.              /*  OK  */

3 \= 3 :: f.                    /*  OK  */

4 \= 4 :: fails.                /*  OK  */

5 = 5 :: f.                     /*  Message  */

6 = 6 :: fails.                 /*  Message  */

7 \= 7 :: s.                    /*  Message  */

8 \= 8 :: succeeds.             /*  Message  */

9 = 9 :: c.                     /*  Message  */

10 = 10 :: crashes.             /*  Message  */

11 \= 11 :: c.                  /*  Message  */

12 \= 12 :: crashes.            /*  Message  */

13 = 13 :: c(137).              /*  Message  */

14 = 14 :: crashes(137).        /*  Message  */

15 \= 15 :: c(137).             /*  Message  */

16 \= 16 :: crashes(137).       /*  Message  */

17 = X :: X = 17.               /*  OK  */

functor( f(1,2), F, A ) ::
    F = f, A = 2.               /*  OK  */

functor( f(1,2), F, A ) ::
    F = g, A = 3.               /*  Message  */

'Final line of'<>
' AutoTest.TST'<>nl.            /*  Display  */
