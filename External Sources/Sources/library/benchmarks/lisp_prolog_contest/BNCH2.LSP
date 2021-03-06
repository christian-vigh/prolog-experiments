
; [2] List manipulation

; **** Tarai with list ****

(SETQ BASE 10. IBASE 10.)

(DEFUN LIST-TARAI (X Y Z)
  (COND ((LESSP (CAR X) (CAR Y))
         (LIST-TARAI (LIST-TARAI (COPYTREE (CDR X)) Y Z)
                     (LIST-TARAI (COPYTREE (CDR Y)) Z X)
                     (LIST-TARAI (COPYTREE (CDR Z)) X Y) ))
        (T Y) ))

; COPYTREE copies all level of conses in the argument.

(DEFUN COPYTREE (X) (SUBST NIL NIL X))

; [2-1:] Measure the following form: 

; (LIST-TARAI '(1 2. 3. 4. 5. 6. 7. 8. 9. 10.) '(5. 6. 7. 8. 9. 10.) '(9. 10.))
;
;        analogous to (tarai 8. 4. 0)
;        list-tarai is called 12605 times.
;        with 9453 copy's and 25210 car's.

(DEFMACRO BENCHMARK (N &REST BODY)
  `(LET (TIME1 TIME2 TIME3 GC RUN)
     (PRINT ',BODY)
     (GC)
     (SSTATUS GCTIME 0)
     (SETQ TIME1 (RUNTIME))
     (DO ((I 1 (1+ I)))
	 ((> I ,N))
       ,@BODY )
     (SETQ TIME2 (RUNTIME))
     (DO ((I 1 (1+ I))) ((> I ,N)))
     (SETQ TIME3 (RUNTIME))
     (SETQ GC (STATUS GCTIME))
     (SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
     (TERPRI)
     (PRINC "Total = ")
     (PRINC RUN)
     (PRINC "us,  Runtime = ")
     (PRINC (DIFFERENCE RUN GC))
     (PRINC "us, GC = ")
     (PRINC GC)
     (PRINC "us, for ")
     (PRINC ,N)
     (PRINC " iterations.")
     (TERPRI)
     ))

(DEFUN BENCH21 (ITER)
  (BENCHMARK ITER
	     (LIST-TARAI '(1 2 3 4 5 6. 7. 8. 9. 10.)
			 '(5. 6. 7. 8. 9. 10.) '(9. 10.)) ))

; If macro is not avaiable, use instead the followings:

'("*** Please this line and the last line. ***"

(DEFUN BENCH21 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (LIST-TARAI '(1 2 3 4 5 6. 7. 8. 9. 10.) '(5 6 7. 8. 9. 10.) '(9. 10.))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

"*** Please kill this line. ***" )

; **** Slow Reverse ****
;         written by Hideyuki Nakashima and Satoru Tomura

(DEFUN SREV (X)
  (COND ((NULL X) NIL)
        (T (SAPP (SREV (CDR X)) (NCONS (CAR X)))) ))

(DEFUN SAPP (X Y)
  (COND ((NULL X) Y)
        (T (SAPP (SREV (CDR (SREV X)))
                 (CONS (CAR (SREV X)) Y) ))))

; [2-2:]
(DEFUN BENCH22 (ITER) (BENCHMARK ITER (SREV '(1 2 3 4 5))))
; [2-3:]
(DEFUN BENCH23 (ITER) (BENCHMARK ITER (SREV '(1 2 3 4 5 6))))

; If macro is not avaiable, use instead the followings:
;
'("*** Please this line and the last line. ***"

(DEFUN BENCH22 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SREV '(1 2 3 4 5))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

(DEFUN BENCH23 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SREV '(1 2 3 4 5 6))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

"*** Please kill this line. ***" )

; **** Quicksort ****

(DEFUN QSORT (L R)
  (COND ((NULL L) R)
        (T ((LAMBDA (P)
              (QSORT (CAR P)
                     (CONS (CAR L) (QSORT (CDR P) R) )))
            (PARTITION (CDR L) (CAR L)) ))))

(DEFUN PARTITION (L X)
  (COND ((NULL L) (CONS NIL NIL))
        (T ((LAMBDA (P)
              (COND ((LESSP (CAR L) X)
                     (CONS (CONS (CAR L) (CAR P)) (CDR P)) )
                    (T (CONS (CAR P) (CONS (CAR L) (CDR P))))) )
            (PARTITION (CDR L) X) ))))

; ---- Common data definition ----

(DEFUN INDEX (START END STEP)
  (DO ((I START (+ I STEP)) RESULT)
      ((OR (AND (MINUSP STEP) (< I END))
           (AND (NOT (MINUSP STEP)) (> I END)) )
       RESULT )
      (SETQ RESULT (APPEND RESULT (LIST I))) ))

(SETQ LIST30 '( 1.  2.  3.  4.  5.  6.  7.  8.  9. 10.
               11. 12. 13. 14. 15. 16. 17. 18. 19. 20.
               21. 22. 23. 24. 25. 26. 27. 28. 29. 30. ))

(SETQ LIST50 '(27. 74. 17. 33. 94. 18. 46. 83. 65. 2.
               32. 53. 28. 85. 99. 47. 28. 82.  6. 11.
               55. 29. 39. 81. 90. 37. 10.  0. 66. 51.
                7. 21. 85. 27. 31. 63. 75.  4. 95. 99.
               11. 28. 61. 74. 18. 92. 40. 53. 59.  8. ))

; [2-4:] Sort a list of 50 element.
;             Compare the performance of quicksort in Prolog.

(DEFUN BENCH24 (ITER) (BENCHMARK ITER (QSORT LIST50 ())))

; If macro is not avaiable, use instead the followings:

'("*** Please this line and the last line. ***"

(DEFUN BENCH24 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (QSORT LIST50 ())
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

"*** Please kill this line. ***" )


; **** Naive reverse to compare with Prolog ****

(DEFUN NAPP (X Y)
  (COND ((PAIRP X)
         (CONS (CAR X) (NAPP (CDR X) Y)) )
        (T Y) ))

(DEFUN NREV (X)
  (COND ((PAIRP X)
         (NAPP (NREV (CDR X)) (LIST (CAR X))) )
        (T NIL) ))

; [2-5:]
(DEFUN BENCH25 (ITER) (BENCHMARK ITER (NREV LIST30)))

; If macro is not avaiable, use instead the followings:

'("*** Please this line and the last line. ***"

(DEFUN BENCH25 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (NREV LIST30)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))
"*** Please kill this line. ***" )

; **** Built-in (Non-/Destructive) Reverse ****

; Use the built-in destructive reverse function.
;  If it is not provided by the system, use the following function:

'("*** Please this line and the last line. ***"

(DEFUN NREVERSE (L)
  (COND ((NULL L) NIL)
        (T (DO ((CURRENT (CDR L)) (PREVIOUS L) NEXT)
               ((NULL CURRENT) (RPLACD L NIL) PREVIOUS)
             (SETQ NEXT (CDR CURRENT))
             (RPLACD CURRENT PREVIOUS)
             (SETQ PREVIOUS CURRENT CURRENT NEXT) ))))

"*** Please kill this line. ***" )

;  If built-in reverse is not available, use the following definition:

'("*** Please this line and the last line. ***"

(DEFUN REVERSE (L)
  (PROG (X)
   LOOP (COND ((NULL L) (RETURN X)))
        (SETQ X (CONS (CAR L) X))
        (SETQ L (CDR L))
        (GO LOOP) ))

"*** Please kill this line. ***" )

; [2-6:]
(DEFUN BENCH26 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (REVERSE LIST30)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-6:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-7:]
(DEFUN BENCH27 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N DUMMY)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SETQ LIST30 (NREVERSE list30))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (SETQ DUMMY LIST30)
        (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-7:]")
        (TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-8:]
(DEFUN BENCH28 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (REVERSE LIST10)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-8:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-9:]
(DEFUN BENCH29 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (REVERSE LIST100)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-9:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-10:]
(DEFUN BENCH210 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (REVERSE LIST1000)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-10:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-11:]
(DEFUN BENCH211 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (REVERSE LIST10000)
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-11:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-12:]
(DEFUN BENCH212 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SETQ LIST10 (NREVERSE LIST10))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (SETQ LIST10 LIST10)
        (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-12:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-13:]
(DEFUN BENCH213 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SETQ LIST100 (NREVERSE LIST100))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (SETQ LIST100 LIST100)
        (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-13:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-14:]
(DEFUN BENCH214 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SETQ LIST1000 (NREVERSE LIST1000))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (SETQ LIST1000 LIST1000)
        (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-14:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; [2-15:]
(DEFUN BENCH215 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (SETQ LIST10000 (NREVERSE LIST10000))
	(COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L1)))
	(SETQ TIME2 (RUNTIME))
	(SETQ N ITER)
   L2   (SETQ LIST10000 LIST10000)
        (COND ((GREATERP (SETQ N (SUB1 N)) 0) (GO L2)))
	(SETQ TIME3 (RUNTIME))
	(SETQ GC (STATUS GCTIME))
	(SETQ RUN (DIFFERENCE (PLUS TIME2 TIME2) TIME1 TIME3))
        (PRINT "[2-15:]")
	(TERPRI)
	(PRINC "Total = ")
	(PRINC RUN)
	(PRINC "us,  Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

; Now measure the benchmarks.

; (BENCH21 1)
; (BENCH22 10.)
; (BENCH23 1.)
; (BENCH24 10.)
; (BENCH25 10.)
; (BENCH26 100.)
; (BENCH27 100.)

(SETQ LIST10 '(1 2 3 4 5 6 7 8. 9. 10.))
; (BENCH28 100.)
; (BENCH212 100.)
(SETQ LIST100 (INDEX 1 100. 1))
; (BENCH29 10.)
; (BENCH213 1000.)
(SETQ LIST1000 (INDEX 1 1000. 1))
; (BENCH210 1.)
; (BENCH214 10000.)

; To create LIST1000, it takes too long.
; (SETQ LIST100 NIL LIST1000 NIL)
; (SETQ LIST10000 (INDEX 1 10000. 1))
; (BENCH215 100.)
; (BENCH211 1)
