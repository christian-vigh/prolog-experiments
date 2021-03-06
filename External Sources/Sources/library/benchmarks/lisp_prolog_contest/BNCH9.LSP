; [9] TPU

; *** TPU program ****
; Theorem Prover by Unity binary resulution
;         [See]  C-L. Chang  and  R.  C-T.  Lee,
;                Symbolic Logic  and  Mechanical Theorem Proving,
;                Academic Press, 1973.

(SETQ BASE 10. IBASE 10.)

(DEFCONST XLIST '(X1 X2 X3 X4 X5 X6 X7))
(DEFCONST YLIST '(Y1 Y2 Y3 Y4 Y5 Y6 Y7))
(DEFCONST ZLIST '(ZZ1 ZZ2 ZZ3 ZZ4 ZZ5 ZZ6 ZZ7))

(DEFUN RENAME (C XY)
    (PROG (VAR Z)
      (SETQ Z ZLIST)
      (SETQ VAR (CADR C))
 B1   (COND ((NULL VAR) (GO B2)))
      (SETQ C (SUBST (CAR Z) (CAR VAR) C))
      (SETQ Z (CDR Z))
      (SETQ VAR (CDR VAR))
      (GO B1)
 B2   (SETQ Z XY)
      (SETQ VAR (CADR C))
 B3   (COND ((NULL VAR) (RETURN C)))
      (SETQ C (SUBST (CAR Z) (CAR VAR) C))
      (SETQ Z (CDR Z))
      (SETQ VAR (CDR VAR))
      (GO B3) ))

(DEFUN INSIDE (A E)
    (COND ((ATOM E) (EQ A E))
          ((INSIDE A (CAR E)) T)
          (T (INSIDE A (CDR E))) ))

(DEFUN DISAGREE (E1 E2)
    (COND ((NULL E1) NIL)
          ((OR (ATOM E1) (ATOM E2))
           (COND ((EQUAL E1 E2) NIL) (T (LIST E1 E2))) )
          ((EQUAL (CAR E1) (CAR E2)) (DISAGREE (CDR E1) (CDR E2)))
          ((OR (ATOM (CAR E1)) (ATOM (CAR E2))) (LIST (CAR E1) (CAR E2)))
          (T (DISAGREE (CAR E1) (CAR E2))) ))

(DEFUN UNIFICATION (E1 E2)
    (PROG (D U D1 D2)
      (COND
       ((NOT (EQUAL (LENGTH E1) (LENGTH E2))) (RETURN 'NO)) )
 B1   (SETQ D (DISAGREE E1 E2))
      (COND ((NULL D) (RETURN (REVERSE U))))
      (SETQ D1 (CAR D))
      (SETQ D2 (CADR D))
      (COND ((OR (MEMBER D1 XLIST) (MEMBER D1 YLIST)) (GO B3)))
      (COND ((OR (MEMBER D2 XLIST) (MEMBER D2 YLIST)) (GO B4)))
 B2   (RETURN 'NO)
 B3   (COND ((INSIDE D1 D2) (GO B2)))
      (SETQ U (CONS D U))
      (SETQ E1 (SUBST D2 D1 E1))
      (SETQ E2 (SUBST D2 D1 E2))
      (GO B1)
 B4   (COND ((INSIDE D2 D1) (GO B2)))
      (SETQ U (CONS (REVERSE D) U))
      (SETQ E1 (SUBST D1 D2 E1))
      (SETQ E2 (SUBST D1 D2 E2))
      (GO B1) ))

(DEFUN DELETEV (X Y VAR)
    (PROG (VAR1 TX TX1 X1)
      (SETQ X (APPEND X Y))
 B1   (COND ((NULL VAR) (RETURN X)))
      (SETQ VAR1 (CAR VAR))
      (SETQ TX X)
      (SETQ X1 NIL)
 B2   (COND ((NULL TX) (GO B4)))
      (SETQ TX1 (CAR TX))
      (COND ((EQ TX1 VAR1) (GO B3)))
      (SETQ X1 (CONS TX1 X1))
      (SETQ TX (CDR TX))
      (GO B2)
 B3   (SETQ X (APPEND X1 (CDR TX)))
 B4   (SETQ VAR (CDR VAR))
      (GO B1) ))

(DEFUN URESOLVE (C1 C2 N)
    (PROG (L1 L2 VC1 VC2 X Y SIGN UNIF R RES VAR V1 V2 H HIST TC2)
      (SETQ C1 (RENAME C1 XLIST))
      (SETQ C2 (RENAME C2 YLIST))
      (SETQ L1 (CAR C1))
      (SETQ L2 (CAR C2))
      (SETQ VC1 (CADR C1))
      (SETQ VC2 (CADR C2))
      (SETQ C2 (CADDR C2))
      (SETQ X (CAR (CADDR C1)))
      (SETQ SIGN -1)
      (COND ((EQ (CAR X) 'NOT) (GO B7)))
      (SETQ SIGN 1)
 B1   (COND
       ((NULL C2) (RETURN (LIST (REVERSE RES) (REVERSE HIST) N))) )
      (SETQ Y (CAR C2))
      (COND ((EQ (CAR Y) 'NOT) (GO B2)))
      (GO B6)
 B2   (SETQ UNIF (UNIFICATION X (CDR Y)))
 B3   (COND ((EQUAL UNIF 'NO) (GO B6)))
      (SETQ R (APPEND (REVERSE TC2) (CDR C2)))
      (COND ((NULL R) (RETURN (LIST 'CONTRADICTION L1 L2))))
      (SETQ VAR NIL)
 B4   (COND ((NULL UNIF) (GO B5)))
      (SETQ V1 (CAAR UNIF))
      (SETQ V2 (CADAR UNIF))
      (SETQ VAR (CONS V1 VAR))
      (SETQ R (SUBST V2 V1 R))
      (SETQ UNIF (CDR UNIF))
      (GO B4)
 B5   (SETQ N (ADD1 N))
      (SETQ H (LIST N L1 L2 (ADD1 (LENGTH TC2))))
      (SETQ R (LIST N (DELETEV VC1 VC2 VAR) R))
      (SETQ RES (CONS R RES))
      (SETQ HIST (CONS H HIST))
 B6   (SETQ TC2 (CONS Y TC2))
      (SETQ C2 (CDR C2))
      (COND ((EQUAL SIGN 1) (GO B1)))
 B7   (COND
       ((NULL C2) (RETURN (LIST (REVERSE RES) (REVERSE HIST) N))) )
      (SETQ Y (CAR C2))
      (COND ((EQ (CAR Y) 'NOT) (GO B6)))
      (SETQ UNIF (UNIFICATION (CDR X) Y))
      (GO B3) ))

(DEFUN GUNIT (S1 S2 W C N)
    (PROG (L S3 SS3 W1 V U RES HIST M X)
      (COND ((NULL W) (RETURN (LIST RES HIST N))))
      (SETQ L (LENGTH (CADDR C)))
      (SETQ S3 (LIST (LIST 10000. C)))
      (SETQ SS3 S3)
 B1   (COND ((NULL W) (GO B7)))
      (SETQ W1 (CAR W))
 B2   (COND ((NULL SS3) (GO B4)))
      (SETQ V (CAR SS3))
      (COND ((GREATERP (CAR W1) (CAR V)) (GO B3)))
      (SETQ U (URESOLVE W1 (CADR V) N))
      (COND ((NULL (CAR U)) (GO B3)))
      (SETQ RES (APPEND RES (CAR U)))
      (SETQ HIST (APPEND HIST (CADR U)))
      (SETQ N (CADDR U))
 B3   (SETQ SS3 (CDR SS3))
      (GO B2)
 B4   (COND ((EQUAL (SUB1 L) 1) (GO B6)))
      (SETQ M (CAR W1))
 B5   (COND ((NULL RES) (GO B6)))
      (SETQ X (CONS (LIST M (CAR RES)) X))
      (SETQ RES (CDR RES))
      (GO B5)
 B6   (SETQ W (CDR W))
      (SETQ SS3 S3)
      (GO B1)
 B7   (SETQ L (SUB1 L))
      (COND ((EQUAL L 1) (RETURN (LIST RES HIST N))))
      (SETQ S3 X)
      (SETQ SS3 S3)
      (SETQ X NIL)
      (SETQ W (APPEND S1 S2))
      (GO B1) ))

(DEFUN PNSORT (RES)
    (PROG (C POS NEG)
 B1   (COND ((NULL RES) (RETURN (LIST (REVERSE POS) (REVERSE NEG)))))
      (SETQ C (CAAR (CDDAR RES)))
      (COND ((EQUAL (CAR C) 'NOT) (GO B3)))
      (SETQ POS (CONS (CAR RES) POS))
 B2   (SETQ RES (CDR RES))
      (GO B1)
 B3   (SETQ NEG (CONS (CAR RES) NEG))
      (GO B2) ))

(DEFUN FDEPTH (C)
    (PROG (N U)
      (SETQ C (CAR (CADDR C)))
      (COND ((EQUAL (CAR C) 'NOT) (GO B1)))
      (SETQ C (CDR C))
      (GO B2)
 B1   (SETQ C (CDDR C))
 B2   (SETQ N 0)
 B3   (COND ((NULL C) (GO B5)))
      (COND ((ATOM (CAR C)) (GO B4)))
      (SETQ U (APPEND (CDAR C) U))
 B4   (SETQ C (CDR C))
      (GO B3)
 B5   (COND ((NULL U) (RETURN N)))
      (SETQ N (ADD1 N))
      (SETQ C U)
      (SETQ U NIL)
      (GO B3) ))

(DEFUN FTEST (RES N4)
    (PROG (C U)
 B1   (COND ((NULL RES) (RETURN (REVERSE U))))
      (SETQ C (CAR RES))
      (COND ((GREATERP (FDEPTH C) N4) (GO B2)))
      (SETQ U (CONS C U))
 B2   (SETQ RES (CDR RES))
      (GO B1) ))

(DEFUN SUBSUME (C1 C2)
    (PROG (Z VAR U)
      (SETQ C1 (RENAME C1 XLIST))
      (SETQ C1 (CAR (CADDR C1)))
      (SETQ Z ZLIST)
      (SETQ VAR (CADR C2))
      (SETQ C2 (CAR (CADDR C2)))
 B1   (COND ((NULL VAR) (GO B2)))
      (SETQ C2 (SUBST (CAR Z) (CAR VAR) C2))
      (SETQ VAR (CDR VAR))
      (GO B1)
 B2   (SETQ U (UNIFICATION C1 C2))
      (COND ((EQUAL U 'NO) (RETURN NIL)))
      (RETURN T) ))

(DEFUN STEST (U RES)
    (PROG (R V W X1 Y Z)
 B1   (COND ((NULL RES) (GO B5)))
      (SETQ R (CAR RES))
      (SETQ Z (APPEND U V))
 B2   (COND ((NULL Z) (GO B3)))
      (COND ((SUBSUME (CAR Z) R) (GO B4)))
      (SETQ Z (CDR Z))
      (GO B2)
 B3   (SETQ V (CONS R V))
 B4   (SETQ RES (CDR RES))
      (GO B1)
 B5   (COND ((NULL V) (RETURN W)))
      (SETQ X1 (CAR V))
      (SETQ Z (CDR V))
 B6   (COND ((NULL Z) (GO B8)))
      (COND ((SUBSUME X1 (CAR Z)) (GO B7)))
      (SETQ Y (CONS (CAR Z) Y))
 B7   (SETQ Z (CDR Z))
      (GO B6)
 B8   (SETQ W (CONS X1 W))
      (SETQ V (REVERSE Y))
      (SETQ Y NIL)
      (GO B5) ))

(DEFUN CONTRADICT (U V)
    (PROG (X1 Y RES)
 B1   (COND ((OR (NULL U) (NULL V)) (RETURN NIL)))
      (SETQ X1 (CAR U))
      (SETQ Y V)
 B2   (COND ((NULL Y) (GO B3)))
      (SETQ RES (URESOLVE X1 (CAR Y) -1))
      (COND ((EQUAL (CAR RES) 'CONTRADICTION) (RETURN RES)))
      (SETQ Y (CDR Y))
      (GO B2)
 B3   (SETQ U (CDR U))
      (GO B1) ))

(DEFUN DTREE (Z HIST N1)
    (PROG (X TX X1 H M1 M2 M N)
      (SETQ HIST (REVERSE HIST))
      (SETQ X (CDR Z))
      (SETQ Z (LIST Z))
      (COND ((GREATERP (CAR X) (CADR X)) (GO B0)))
      (SETQ X (REVERSE X))
 B0   (COND ((GREATERP (CADR X) N1) (GO B1)))
      (SETQ X (LIST (CAR X)))
 B1   (COND ((NULL X) (RETURN Z)))
      (SETQ X1 (CAR X))
 B2   (COND ((EQUAL X1 (CAAR HIST)) (GO B3)))
      (SETQ HIST (CDR HIST))
      (GO B2)
 B3   (SETQ X (CDR X))
      (SETQ H (CAR HIST))
      (SETQ Z (CONS H Z))
      (SETQ HIST (CDR HIST))
      (SETQ M1 (CADR H))
      (SETQ M2 (CADDR H))
      (COND ((GREATERP M1 N1) (GO B5)))
 B4   (COND ((GREATERP M2 N1) (GO B6)))
      (GO B1)
 B5   (SETQ N 1)
      (SETQ M M1)
      (GO B7)
 B6   (SETQ N 2)
      (SETQ M M2)
 B7   (COND ((NULL X) (GO B8)))
      (SETQ X1 (CAR X))
      (COND ((EQUAL X1 M) (GO B10)))
      (COND ((GREATERP  X1 M) (GO B9)))
 B8   (SETQ X (APPEND (REVERSE TX) (CONS M X)))
      (GO B11)
 B9   (SETQ TX (CONS X1 TX))
      (SETQ X (CDR X))
      (GO B7)
 B10  (SETQ X (APPEND (REVERSE TX) X))
 B11  (SETQ TX NIL)
      (COND ((EQUAL N 2) (GO B1)))
      (GO B4) ))

(DEFUN TPU (S1 S2 S3 W N1 N2 N3 N4)
    (PROG (S W1 TS U1 U N K CK WCK V POS NEG HIST Y X1 X)
      (SETQ S (APPEND S1 S2))
      (SETQ S (REVERSE S))
 B1   (COND ((NULL W) (GO B6)))
      (SETQ W1 (CAR W))
 B2   (SETQ TS S)
      (COND ((NULL W1) (GO B5)))
 B3   (COND ((EQ (CAR W1) (CAAR TS)) (GO B4)))
      (SETQ TS (CDR TS))
      (GO B3)
 B4   (SETQ U1 (CONS (CAR TS) U1))
      (SETQ W1 (CDR W1))
      (GO B2)
 B5   (SETQ U (CONS U1 U))
      (SETQ W (CDR W))
      (SETQ U1 NIL)
      (GO B1)
 B6   (SETQ W (REVERSE U))
      (SETQ N N1)
      (SETQ U (CONTRADICT S1 S2))
      (COND ((NOT (NULL U)) (RETURN U)))
      (SETQ K 1)
 B7   (COND ((GREATERP K N2) (RETURN '(S IS NOT PROVED))))
      (SETQ CK (CAR S3))
      (SETQ WCK (CAR W))
      (SETQ V (GUNIT S1 S2 WCK CK N))
      (COND ((NULL (CAR V)) (GO B12)))
      (SETQ N (CADDR V))
      (SETQ HIST (APPEND HIST (CADR V)))
      (SETQ V (CAR V))
      (COND ((LESSP K N3) (GO B8)))
      (SETQ V (FTEST V N4))
 B8   (SETQ V (PNSORT V))
      (SETQ POS (STEST S1 (CAR V)))
      (SETQ NEG (STEST S2 (CADR V)))
      (COND ((NULL (APPEND POS NEG)) (GO B12)))
      (SETQ U (CONTRADICT S1 NEG))
      (COND ((NOT (NULL U)) (RETURN (DTREE U HIST N1))))
      (SETQ U (CONTRADICT POS S2))
      (COND ((NOT (NULL U)) (RETURN (DTREE U HIST N1))))
      (SETQ S1 (APPEND S1 POS))
      (SETQ S2 (APPEND S2 NEG))
      (SETQ W (CDR W))
      (SETQ Y (APPEND POS NEG))
 B9   (COND ((NULL W) (GO B10)))
      (SETQ X1 (APPEND Y (CAR W)))
      (SETQ X (CONS X1 X))
      (SETQ W (CDR W))
      (GO B9)
 B10  (SETQ W (APPEND (REVERSE X) (LIST Y)))
      (SETQ X NIL)
 B11  (SETQ S3 (APPEND (CDR S3) (LIST CK)))
      (SETQ K (ADD1 K))
      (GO B7)
 B12  (SETQ W (APPEND (CDR W) (LIST NIL)))
      (GO B11) ))

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

; [9-1:] TPU-1
(DEFUN BENCH91 (ITER) (BENCHMARK ITER (TPU1)))

(DEFUN TPU1 ()
(TPU
  '((1 (X Y) ((P (G X Y) X Y))) (2 (X Y) ((P X (H X Y) Y))))
  '((3 (X) ((NOT P (K X) X (K X)))))
  '((4 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
    (5 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
  '((3) NIL)
  5 2 3 0 )
)

; This form must return 
; ((6 3 4 4) (11 2 6 2) (15 1 11 1) (CONTRADICTION 1 15)) .

; [9-2:] TPU-2
(DEFUN BENCH92 (ITER) (BENCHMARK ITER (TPU2)))

(DEFUN TPU2 ()
(TPU
  '((1 (X) ((P E X X)))
    (2 (X) ((P X E X)))
    (3 (X) ((P X X E)))
    (4 NIL ((P A B C))) )
  '((5 NIL ((NOT P B A C))))
  '((6 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
    (7 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
  '((4) NIL)
  7 4 5 0 )
)

; This form must return
; ((8 4 6 1) (21 3 8 1) (30 2 21 1) (32 30 7 2) (42 3 32 1) (55 1 42 1)
;  (62 55 6 1) (112 5 62 3) (130 3 112 1) (CONTRADICTION 2 130)) .

; [9-3:] TPU-3
(DEFUN BENCH93 (ITER) (BENCHMARK ITER (TPU3)))

(DEFUN TPU3 ()
(TPU
  '((1 (X) ((P E X X))) (2 (X) ((P (I X) X E))))
  '((3 NIL ((NOT P A E A))))
  '((4 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
    (5 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
  '((3) (3))
  5 4 5 0 )
)

; This form must return
; ((13 3 5 4) (16 2 13 2) (17 1 16 2) (18 17 4 4) (23 2 18 3) (24 1 23 2)
;  (30 24 5 4) (46 2 30 2) (56 2 46 1) (CONTRADICTION 1 56)) .

; [9-4:] TPU-4
(DEFUN BENCH94 (ITER) (BENCHMARK ITER (TPU4)))

(DEFUN TPU4 ()
(TPU
   '((1 (X) ((P E X X))) (2 (X) ((P (I X) X E))))
   '((3 (X) ((NOT P A X E))))
   '((4 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
     (5 (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
   '((3) (3))
   5 4 5 0 )
)

; This form must return
; ((6 3 4 4) (11 2 6 3) (12 1 11 2) (20 12 5 4) (42 2 20 2) (62 2 42 1)
;  (CONTRADICTION 1 62)) .

; [9-5:] TPU-5
(DEFUN BENCH95 (ITER) (BENCHMARK ITER (TPU5)))

(DEFUN TPU5 ()
(TPU
   '((1 (X) ((P E X X)))
     (2 (X) ((P X W X)))
     (3 (X) ((P X (I X) E)))
     (4 (X) ((P (I X) X E)))
     (5 NIL ((S A))) )
   '((6 NIL ((NOT S E))))
   '((7 (X Y Z) ((NOT S X) (NOT S Y) (NOT P X (I Y) Z) (S Z)))
     (8. (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
     (9. (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
   '((6) NIL NIL)
   9. 4 5 0 )
)

; This form must return
; ((10 6 7 4) (14 5 10 2) (18 5 14 1) (CONDTRADICTION 3 18)) .

; [9-6:] TPU-6
(DEFUN BENCH96 (ITER) (BENCHMARK ITER (TPU6)))

(DEFUN TPU6 ()
(TPU
   '((1 (X) ((P E X X)))
     (2 (X) ((P X E X)))
     (3 (X) ((P X (I X) E)))
     (4 (X) ((P (I X) X E)))
     (5 NIL ((S B))) )
   '((6 NIL ((NOT S (I B)))))
   '((7 (X Y Z) ((NOT S X) (NOT S Y) (NOT P X (I Y) Z) (S Z)))
     (8. (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P X V W) (P U Z W)))
     (9. (X Y Z U V W) ((NOT P X Y U) (NOT P Y Z V) (NOT P U Z W) (P X V W))) )
   '((5 6) NIL NIL)
   9. 4 5 0 )
)

; This must return
; ((11 5 7 1) (19 5 11 1) (23 3 19 1) (152 23 7 1) (169 6 152 3) 
;  (186 5 169 1) (CONDTRADICTION 1 186)) .

; [9-7:] TPU-7
(DEFUN BENCH97 (ITER) (BENCHMARK ITER (TPU7)))

(DEFUN TPU7 ()
(TPU
   '((1 NIL ((P A))) (2 NIL ((M A (S C) (S B)))) (3 (X) ((M X X (S X)))))
   '((4 NIL ((NOT D A B))))
   '((5 (X Y Z) ((NOT M X Y Z) (M Y X Z)))
     (6 (X Y Z) ((NOT M X Y Z) (D X Z)))
     (7 (X Y Z U) ((NOT P X) (NOT M Y Z U) (NOT D X U) (D X Y) (D X Z))) )
   '((1 2 3 4) (1 2 3 4) (1 2 3 4))
   7 4 5 0 )
)

; This must return
; ((13 2 6 1) (16 13 7 3) (43 4 16 3) (66 4 43 3) (75 3 66 2)
;  (CONTRADICTION 1 75)) .

; [9-8:] TPU-8
(DEFUN BENCH98 (ITER) (BENCHMARK ITER (TPU8)))

(DEFUN TPU8 ()
(TPU
   '((1 NIL ((L 1 A))) (2 (X) ((D X X))))
   NIL
   '((3 (X) ((P X) (D (G X) X)))
     (4 (X) ((P X) (L 1 (G X))))
     (5 (X) ((P X) (L (G X) X)))
     (6 (X) ((NOT P X) (NOT D X A)))
     (7 (X Y Z) ((NOT D X Y) (NOT D Y Z) (D X Z)))
     (8. (X) ((NOT L 1 X) (NOT L X A) (P (F X))))
     (9. (X) ((NOT L 1 X) (NOT L X A) (D (F X) X))) )
   '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
   9. 20. 21. 0 )
)

; This must return
; ((10 2 6 2) (15 10 3 1) (16 10 4 1) (17 10 5 1) (23 17 8 2) (25 16 23 1)
;  (26 17 9 2) (28 16 26 1) (32 25 6 1) (33 32 7 3) (47 28 33 1)
;  (contradiction 15 47)) .

; [9-9:] TPU-9
(DEFUN BENCH99 (ITER) (BENCHMARK ITER (TPU9)))

(DEFUN TPU9 ()
(TPU
   '((1 (x) ((l x (f x)))))
   '((2 (x) ((not l x x))))
   '((3 (x y) ((not l x y) (not l y x)))
     (4 (x y) ((not d x (f y)) (l y x)))
     (5 (x) ((p x) (d (h x) x)))
     (6 (x) ((p x) (p (h x))))
     (7 (x) ((p x) (l (h x) x)))
     (8. (x) ((not p x) (not l a x) (l (f a) x))) )
   '((1 2) (1 2) (1 2) (1 2) (1 2) (1 2))
   8. 20. 21. 0 )
)

; This must return
; ((14 2 8 3) (16 1 14 2) (17 16 5 1) (18 16 6 1) (19 16 7 1) (20 19 3 1)
;  (23 17 4 1) (24 23 8 2) (28 20 24 2) (CONTRADICTION 18 28)) .

; If macro is not avaiable, use instead the followings:

'("*** Please this line and the last line. ***"

(DEFUN BENCH91 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU1)
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

(DEFUN BENCH92 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU2)
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
	(PRINC "us, Runtime = ")
	(PRINC (DIFFERENCE RUN GC))
	(PRINC "us, GC = ")
	(PRINC GC)
	(PRINC "us, for ")
	(PRINC ITER)
	(PRINC " iterations.")
	(TERPRI)
        ))

(DEFUN BENCH93 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU3)
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

(DEFUN BENCH94 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU4)
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

(DEFUN BENCH95 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU5)
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

(DEFUN BENCH96 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU6)
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

(DEFUN BENCH97 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU7)
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

(DEFUN BENCH98 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU8)
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

(DEFUN BENCH99 (ITER)
  (PROG (TIME1 TIME2 TIME3 GC RUN N)
        (GC)
	(SSTATUS GCTIME 0)
	(SETQ TIME1 (RUNTIME))
	(SETQ N ITER)
   L1   (TPU9)
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

; Now measure the benchmarks.
; (BENCH91 1)
; (BENCH92 1)
; (BENCH93 1)
; (BENCH94 1)
; (BENCH95 1)
; (BENCH96 1)
; (BENCH97 1)
; (BENCH98 1)
; (BENCH99 1)

