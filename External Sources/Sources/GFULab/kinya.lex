%%---------------------------------------------------------------------------
%%
%%                      KinyaRwanda Lexicon
%%
%%  Word classes are:
%%
%%  N = Noun                                V = Verbo
%%  ADJ = adjective                         ADV = adverbio
%%  CONC = subject-V agreement morpheme     ASP = morfema aspectual
%%  TMP = tense morpheme                    PAS = morfema de pasiva
%%  P = preposition                         COMP = subordinante
%%  NP = Proper name                        PN = prefijo de n£mero
%%  AVM = manner adverb
%%  CC = object semantic role marker
%%
%%  CONC,ASP0,ASP,TMP,PAS,P and CC are properly morphological, not
%%  syntactical, classes
%%
%%---------------------------------------------------------------------------

 %-------------------------%
 %    Lexical classes      %
 %-------------------------%

 CLASS N   : U/subcat=<>.
 CLASS ADJ : U/subcat=<>.
 CLASS NP  : U/subcat=<>.
 CLASS V : U/sem/onto=activ.

 CLASS P :
        U/subcat=<comp>
        U/sem=U/comp/sem.
 
 CLASS VT (V):
        U/subcat=<subj obj>
        U/sem/arg2 = U/obj/sem.

 CLASS VBT (V):
        U/subcat=<subj obj obj2>
        U/sem/arg2 = U/obj/sem
        U/sem/arg3 = U/obj2/sem.

 %-------------------------%
 %    Lexical entriess     %
 %-------------------------%
             
 a   = CONC : U/subj/num=sg.
 a   = ASP  : U/prog=+.
 a   = TMP  : U/tns=past.
 aab = PN : U/num=pl.

 aana = N : 
        U/head=aana
        U/sem/pred=child
        U/sem/onto=person. 

 ab  = PN : U/num=pl.
 aba = PN : U/num=pl.

 amata = N : 
        U/head = amata
        U/sem/pred = milk
        U/sem/onto=thing.

 andik = V (VT) : 
        U/head=andik
        U/obj/status=opt
        U/sem/pred=write
        U/sem/arg1/onto=hum
        U/sem/arg2/onto=semiot.

 andikeesh = V : 
        U/head=andik
        U/subcat=<subj &obj ch_obj>
        U/sem/pred=write
        U/sem/arg1/onto=hum
        U/sem/arg2 = U/obj/sem
        U/sem/arg2/onto=semiot
        U/sem/instr = U/ch_obj/sem.

 anditse = V (VT) : 
        U/head=andik
        U/obj/status=opt
        U/sem/pred=write
        U/sem/arg1/onto=hum
        U/sem/arg2/onto=semiot.

 anditseho = V (VT) : 
        U/head=andik
        U/subcat=<subj &obj ch_obj>
        U/sem/pred=write
        U/sem/arg1/onto=hum
        U/sem/arg2 = U/obj/sem
        U/sem/arg2/onto=semiot
        U/sem/location = U/ch_obj/sem.

 booyi = N : 
        U/head=booyi
        U/sem/pred=cook
        U/sem/onto=person.

 eesha = CC : U/sem/instr = U/ch_obj/sem.

 eje = ASP : U/prog=+.

 eye = ASP : U/prog=+.

 gabo = N : 
        U/head=gabo
        U/sem/pred=man
        U/sem/onto=person.

 gore  = N : 
        U/head=gore
        U/sem/pred=woman
        U/sem/onto=person.
        
 haa = V (VBT) : 
        U/head=haa
        U/sem/pred=give
        U/sem/arg1/onto=hum
        NOT U/sem/arg2/onto=hum
        U/sem/arg3/onto=animate.

 he = V (VBT) : 
        U/head=haa
        U/sem/pred=give
        U/sem/arg1/onto=hum 
        NOT U/sem/arg2/onto=hum
        U/sem/arg3/onto=animate.

 heer = V : 
        U/head=haa
        U/subcat=<subj obj obj2 ch_obj>
        U/sem/pred = give
        U/sem/arg1/onto = hum 
        U/sem/arg2 = U/obj/sem
        U/sem/arg3 = U/obj2/sem
        NOT U/sem/arg2/onto = hum
        U/sem/arg3/onto = animate
        U/sem/rec = U/ch_obj/sem.

 heesh = V : 
        U/head=haa
        U/subcat=<subj obj obj2 ch_obj>
        U/sem/pred = give
        U/subj/sem/onto = hum 
        U/sem/arg2 = U/obj/sem
        U/sem/arg3 = U/obj2/sem
        NOT U/obj/sem/onto = hum
        U/obj2/sem/onto = animate
        U/ch_obj/sem/onto = concr
        U/sem/instr = U/ch_obj/sem.

 ibaruwa = N : 
        U/head=ibaruwa
        U/sem/pred=letter
        U/sem/onto=semiot.

 ibitabo = N : 
        U/head=tabo
        U/num=pl
        U/sem/pred=book
        U/sem/onto=semiot.
        
 ikaramu  = N : 
        U/head=ikaramu
        U/sem/pred=pen
        U/sem/onto=artifact.

 inkoongooro  = N : 
        U/head=inkoongooro
        U/sem/pred=bowl
        U/sem/onto=artifact.

 isoko = N : 
        U/head=isoko
        U/sem/pred=marketplace
        U/sem/onto=place.
        
 kobwa = N :
         U/head=kobwa
         U/sem/pred=girl
         U/sem/onto=person.

 ku = P : U/theta=location.
 
 %ku = INF : U/inf=+ U/arg1/head=pro.

 kw = P : U/theta=location.
        
 lir = V : 
        U/head=lir
        U/subcat=<subj>
        U/sem/pred=cry
        U/sem/arg1/onto=hum.

 Maria  = NP : 
        U/head=Maria
        U/sem/pred=PERSON
        U/sem/name=Maria.

 meza  = N : 
        U/head=meza
        U/sem/pred=table
        U/sem/onto=artifact.

 mw = PRO: 
        U/obj2/head=pro
        U/obj2/num=sg
        U/obj2/sem/pred=she.

 n  = P : U/theta=instr.
        
 na = P : U/theta=instr.

 ooher = V (VT) : 
        U/head=ooher
        U/sem/pred=send
        U/sem/arg1/onto=hum.

 ooherer = V : 
        U/head=ooher
        U/subcat=<subj obj ch_obj>
        U/sem/pred=send
        U/sem/arg1/onto=hum
        U/sem/arg2 = U/obj/sem
        U/sem/rec = U/ch_obj/sem
        U/sem/rec/onto = hum.

 ooherejeho = V : 
        U/head=ooher
        U/subcat=<subj obj ch_obj>
        U/prog = +
        U/sem/pred=send
        U/sem/arg1/onto=hum
        U/sem/arg2 = U/obj/sem
        U/sem/location = U/ch_obj/sem.
        
 ra = TMP : U/tns=pre.
 umu = PN : U/num=sg.
 umw = PN : U/num=sg.
 y = CONC : U/subj/num=sg.
 ye = ASP.

 yi = PRO :
        U/obj/head=pro 
        U/obj/num=sg
        U/obj/sem/pred=pro.

 Yohani = NP : 
        U/head=Yohani
        U/sem/pred=PERSON
        U/sem/name=Yohani.

