; [5] Array manipulation

; **** Bubble sort ****
; Read data into array, sort data and return a list of sorted data.

(SETQ BASE 10. IBASE 10.)

; DATA is an array of size 50.

(*ARRAY 'DATA 'FIXNUM 50. )

(DEFUN BUBBLESORT (LS)
  (PROG (TOP BOUND)
        (SETQ BOUND 49. )
        (LOOP FOR I FIXNUM FROM 0 TO BOUND
              DO (STORE (DATA I) (POP LS)) )
   LOOP (SETQ TOP 0)
        (LOOP FOR J FIXNUM FROM 0 TO (1- BOUND)
              FOR X FIXNUM = (DATA J)
              DO (COND ((> (SETQ X (DATA J)) (DATA (1+ J)))
                        (STORE (DATA J) (DATA (1+ J)))
                        (STORE (DATA (1+ J)) X)
                        (SETQ TOP J) )))
        (COND ((ZEROP TOP)
               (RETURN (LOOP FOR J FIXNUM FROM 0 TO 49.
                             COLLECT (DATA J) INTO X
                             FINALLY (RETURN X) ))))
        (SETQ BOUND TOP)
        (GO LOOP) ))

; [5-1:] Sort 50 elements by bubble sort.

(DEFMACRO BENCHMARK (N &REST BODY)
  `(LET (TIME1 TIME2 TIME3 GC RUN)
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
     (PRINC "us, Runtime = ")
     (PRINC (DIFFERENCE RUN GC))
     (PRINC "us, GC = ")
     (PRINC GC)
     (PRINC "us, for ")
     (PRINC ,N)
     (PRINC " iterations.")
     (TERPRI)
     ))

(DEFUN BENCH51 (ITER)
  (BENCHMARK ITER (BUBBLESORT LIST50)) )

(SETQ LIST50 '(27. 74. 17. 33. 94. 18. 46. 83. 65.  2.
               32. 53. 28. 85. 99. 47. 28. 82.  6. 11.
               55. 29. 39. 81. 90. 37. 10.  0. 66. 51.
                7. 21. 85. 27. 31. 63. 75.  4. 95. 99.
               11. 28. 61. 74. 18. 92. 40. 53. 59.  8. ))

; If macro is not avaiable, use instead the followings:

'("*** Please this line and the last line. ***"

(DEFUN BENCH51 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (BUBBLESORT LIST50)
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

; Now measure the benchmark.
; (BENCH51 10. )
