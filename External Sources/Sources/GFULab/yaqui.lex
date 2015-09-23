
        %=============================================================%
        %                                                             %
        %             A small Yaqui lexicon for GFU-LAB               %
        %                                                             %
        %=============================================================%

 %-------------------------%
 %     Collocations        %
 %-------------------------%

 hia <- hunen hia.
 usi_hamut <-  usi hamut.
 huni <- nasuk huni.
 o <-  o sino.
 bicaa <- u bicaa.
 
 %-------------------------%
 %    Lexical classes      %
 %-------------------------%

 % Personal Pronouns
 
 CLASS PRO :
        U/subcat=<>.

 % Nouns
 
 CLASS N : U/subcat=<>.

 % Adjectives
 
 CLASS ADJ :
        U/subcat = <&subj>
        U/sem/arg1 = U/subj/sem.
        
 % Adverbs (of manner and time)
 
 CLASS ADV : U/subcat=<>.

 % Prepositional (place) adverbs
 
 CLASS PAV : U/subcat=<>
             U/theta=loc.

 % Postpostions
                 
 CLASS P :
        U/clitic=+
        U/subcat=<comp>
        U/comp/case=pp
        U/sem=U/comp/sem.

 % Verbs
         
 CLASS VERB : 
        U/subj/case=nom
        U/voice=active
        U/sem/arg1=U/subj/sem.

 % Transitive Verbs
 
 CLASS VTR (VERB) :
        U/subcat=<subj obj>
        U/obj/case=acc
        U/sem/arg2=U/obj/sem.

 % Intransitive Verbs
 
 CLASS VI (VERB) :
        U/subcat=<subj>.

 % Bitransitive verbs

 CLASS VBTR (VERB) :
        U/subcat=<subj obj iobj>
        U/obj/case=acc
        U/sem/arg2=U/obj/sem
        U/iobj/case=acc
        U/iobj/sem/onto=animate
        U/sem/arg3=U/iobj/sem.
 
 % Verbs governing a Postpositional Complement
         
 CLASS VPP (VERB) : 
        U/subcat=<subj &comp>
        U/sem/arg2=U/comp/sem.

 % Verbs taking 'xcomplements'
 
 CLASS VXC (VERB) :            
        U/subcat=<subj xcomp>
        U/xcomp/subj/head=PRO
        U/sem/arg2=U/xcomp/sem.

 % Raising verbs
 
 CLASS VR :
       U/subcat=<subj xcomp>
       U/subj/case=nom
       U/xcomp/subj/head=PRO
       U/sem/arg1=U/xcomp/sem
       U/sem/arg1/arg1=U/subj/sem.                   

 % Passive Verbs
 
 CLASS VPAS :
        U/subcat=<subj &bycomp>
        U/voice=passive
        U/subj/case=nom
        U/bycomp/head=e
        U/sem/arg1 = U/bycomp/sem
        U/sem/arg2 = U/subj/sem.
        
         %------------------------%                
         %       Templates        %
         %------------------------%
 
 @Loc    = U/comp/theta = loc.
 @Directional = @Loc U/comp/sem/ubic = AD.
 @SubjControl = U/subj/sem/index=U/xcomp/subj/sem/index.
 @EmptyHead = U/head=NULL
              U/sem/anaphor=+.
              
         %-----------------------%         
         %     Determiners       %
         %-----------------------%

 bem = DET : 
        U/det=bem
        U/sem/possessor/anaphor=+
        U/sem/possessor/quant=set.

 cikti = DET : 
        U/det=cikti
        U/sem/quant=distr
        U/num = sg.

 em = DET : 
        U/det=em
        U/sem/possessor/pred=youall.

 %gwae (f72)
  
 gwa'a = DET : 
        U/det=gwa
        U/sem/deixis=dist
        U/num = sg.
 
 gwame = DET : 
        U/det=gwa
        U/sem/deixis=dist
        U/num = pl.

 hita = DET :
        U/det=hita
        U/sem/quant=some.

 hitasa = DET :
        U/det=hitasa
        U/sem/deixis=interr.
        
 hu = DET :
        U/det=hu
        U/sem/deixis=prox
        U/num=sg.
  
 hu'u = DET : 
        U/det=hu
        U/sem/deixis=prox
        U/num = sg.

 huka = DET :
        U/det=hu
        U/sem/deixis=prox
        U/num=sg
        U/case=acc.
 
 hume = DET :
        U/det=hu
        U/sem/deixis=prox
        U/num=pl.

 hunu = DET : 
        U/det=hunu
        U/sem/deixis=prox
        U/num=sg
        U/case=nom.

 hunuka = DET : 
        U/det=hunu
        U/sem/deixis=prox
        U/num=sg
        U/case=acc.
 
 hunume = DET : 
        U/det=hunu
        U/sem/deixis=prox
        U/num = pl.

 ime = DET :
        U/det=i
        U/sem/deixis=prox
        U/num = pl.
 
 ime'e = DET :
        U/det=i
        U/sem/deixis=prox
        U/num = pl.
                
 in = DET : 
        U/det=in
        U/sem/possessor/pred=I.

 ini = DET : 
        U/det=ini
        U/sem/deixis=prox
        U/num = sg.

 inia = DET : 
        U/det=ini
        @EmptyHead
        U/sem/deixis=prox
        U/num=sg
        U/case=pp.
 
 inie = DET : 
        U/det=ini
        U/sem/deixis=prox
        U/num=sg
        U/case=pp.

 inime = DET : 
        U/det=ini
        U/sem/deixis=prox
        U/num = pl.
 
 itom = DET : 
        U/det=itom
        U/sem/possessor/pred=we.

 % Numerals
 
 bahi = Q : 
        U/posdet=bahi
        U/sem/card=3
        U/num=pl.

 mamni = Q : 
        U/posdet=mamni
        U/sem/card=5
        U/num=pl.
 
 naiki = Q : 
        U/posdet=naiki
        U/sem/card=4
        U/num=pl.

 naikim = Q : 
        U/posdet=naiki
        @EmptyHead
        U/sem/card=4
        U/num=pl.

 senu = Q : 
        U/posdet=senu
        U/sem/card=1
        U/num=sg.

 % senuk = one-Dep
  
 wepul = Q : 
        U/posdet=wepul
        U/sem/card=1
        U/num=sg.

 woi = Q : 
        U/posdet=woi
        U/sem/card=2
        U/num=pl.

 woim = Q : 
        U/posdet=woi
        U/sem/card=2
        U/num=pl.

 % Other post-determiners

 habe = Q : 
        U/posdet=habe
        U/sem/quant=indif.

 bu'u = Q : 
        U/posdet=bu'u
        U/sem/quant=mult.

 hwebena = Q : 
        U/posdet=hwebena
        U/sem/quant=mult
        U/num=pl.

 hwebena = Q : 
        U/posdet=hwebena
        U/sem/quant=mult
        U/num=pl.

 hwebenak = Q : 
        U/posdet=hwebena
        @EmptyHead
        U/case=dep
        U/sem/quant=mult
        U/num=pl.

 illiki = Q : 
        U/posdet=illiki
        U/sem/quant=pauc.
 
 illikik = Q : 
        U/posdet=illiki
        @EmptyHead
        U/case=dep
        U/sem/quant=pauc.

 illikim = Q : 
        U/posdet=illiki
        @EmptyHead
        U/case=dep
        U/num=pl
        U/sem/quant=pauc.
  
 si'ime = Q : 
        U/predet=si'ime
        U/sem/quant=total
        U/num=pl.

 si'imeta = Q : 
        U/predet=si'ime
        @EmptyHead
        U/sem/quant=total
        U/num=pl
        U/case=dep.

 % hitasa = cual?
  
         %----------------%           
         %   Pronouns     %
         %----------------%
 
 apo = PRO:
        U/head=apo
        U/num=sg
        U/case=nom
        U/sem/anaphor=+.

 ame = PRO :
        U/head=bempo
        U/case=pp
        U/num=pl
        U/sem/anaphor=+.
             
 bempo = PRO: 
        U/head=bempo
        U/case=nom
        U/num=pl
        U/sem/anaphor=+.

 eme = PRO : 
        U/head = eme
        U/case=nom
        U/num=pl
        U/sem/pred=youall.
 
 eme'e = PRO : 
        U/head = eme
        U/case=nom
        U/num=pl
        U/sem/pred=youall.

 empo = PRO: 
        U/head=empo
        U/case=nom
        U/num=sg
        U/sem/pred=you.

 enci = PRO: 
        U/head=empo
        U/case=acc
        U/num=sg
        U/sem/pred=you.
 
 inepo = PRO : 
        U/head = inepo
        U/case=nom
        U/num=sg
        U/sem/pred=I.

 itepo = PRO: 
        U/head=itepo
        U/case=nom
        U/num=pl
        U/sem/pred=we.

 ne = PRO : 
        U/head = inepo
        U/num=sg
        U/sem/pred=I.

 te = PRO :     % nosotros (clitico)
        U/head=itepo
        U/num=pl
        U/sem/pred=we.
        
 % Indefinite pronouns
 % Si la frase incluye un pronombre negativo ('kaabe' y 'kaita'), no aparece
 % el morfema de negaci¢n 'kaa'. Esto es bastante dif¡cil de expresar en
 % t‚rminos de unificaci¢n, ya que exige acceder a una estructura funcional
 % que no es local.
 
 kaabe = PRO :
        U/head=kaabe
        U/protype=neg
        U/sem/pred=nobody.

 kaita = PRO :
        U/head=kaita
        U/protype=neg
        U/sem/pred=nothing.
         
 habesa = PRO :
        U/head=habesa
        U/protype=interr
        U/sem/pred=who.
  
         %-----------------%
         %     Nouns       %
         %-----------------%
                           
 abaci = N: 
        U/head=abaci
        U/sem/pred=brother
        U/sem/onto=person.

 aca'im = N: 
        U/head=acai
        U/sem/pred=parent
        U/num=pl
        U/sem/onto=person.

 acai = N: 
        U/head=acai
        U/sem/pred=parent
        U/sem/onto=person.

 animal = N: 
        U/head=animal
        U/sem/pred=animal
        U/sem/onto=beast.

 animalim = N: 
        U/head=animal
        U/sem/pred=animal
        U/num=pl
        U/sem/onto=beast.

 asoa = N:
        U/head=asoa
        U/sem/pred=son
        U/sem/onto=person.

 baci = N: 
        U/head=baci
        U/sem/pred=corn
        U/sem/onto=planta.

 bakocim = N: 
        U/head=bakoc
        U/num=pl
        U/sem/pred=snake
        U/sem/onto=beast.
 
 batoi = N :
        U/head=batoi
        U/sem/pred=people
        U/sem/onto=person.
        
 bo'o = N: 
        U/head=bo'o
        U/sem/pred=path
        U/sem/onto=place.

 conim = N :
        U/head=coni
        U/num=pl
        U/sem/pred=hair
        U/sem/onto=bodypart.
 
 cu'u = N: 
        U/head=cu'u
        U/sem/pred=dog
        U/sem/onto=beast.

 cu'um = N: 
        U/head=cu'u
        U/num=pl
        U/sem/pred=dog
        U/sem/onto=beast.

 hamucim = N : 
        U/head = hamut
        U/num=pl
        U/sem/pred=woman
        U/sem/onto=person.  
 
 hamut = N : 
        U/head = hamut
        U/sem/pred=woman
        U/sem/onto=person.  

 hekka = N :
        U/head = hekka
        U/sem/pred=wind
        U/sem/onto=force.  

 % hiak = yaqui

 hiaki = N :
        U/head=hiaki
        U/sem/pred=yaqui
        U/sem/onto=person.
        
 ho'a = N :
        U/head = ho'a
        U/sem/pred=house
        U/sem/onto=place.  

 kaba'im = N: 
        U/head=kaba'i
        U/num=pl
        U/sem/pred=horse
        U/sem/onto=beast.
 
 kari = N: 
        U/head=kari
        U/sem/pred=house
        U/sem/onto=place.

 kuta = N : 
        U/head = kuta
        U/sem/pred=stick
        U/sem/onto=physobj.   
 
 kutam = N : 
        U/head = stick
        U/sem/pred=palo 
        U/num=pl
        U/sem/onto=physobj.   

 lominko =  N : 
        U/head = lominko
        U/sem/pred=sunday
        U/sem/onto = tmploc.   

 mama = N :
        U/head=mama
        U/sem/pred=hand
        U/sem/onto=bodypart.

 mara = N :
        U/head=mara
        U/sem/pred=daughter
        U/sem/onto=person.  

 maso = N: 
        U/head=maso
        U/sem/pred=deer
        U/sem/onto=beast.
 
 malawam = N : 
        U/head = malawa
        U/num=pl
        U/sem/pred=mother
        U/sem/onto=person.  

 misi = N: U/head=misi
           U/sem/pred=cat
           U/sem/onto=beast.
  
 misim = N: 
        U/head=misi
        U/num=pl
        U/sem/pred=cat
        U/sem/onto=beast.

 o'o = N : 
        U/head = o'o
        U/sem/pred=man
        U/sem/onto=person.  

 owim = N :
        U/head = o'o
        U/num = pl
        U/sem/pred=man
        U/sem/onto=person.  
 
 pahko = N : 
        U/head = pahko
        U/sem/pred=ceremony
        U/sem/onto=event.  

 pare = N : 
        U/head = pare
        U/sem/pred=priest
        U/sem/onto=person.  

 pino = N : 
        U/head = pino
        U/sem/pred=tree
        U/sem/onto=plant.  

 sakoba'i = N: 
        U/head=sakoba'i
        U/sem/pred=watermelon
        U/sem/onto=plant.
 
 sakoba'im = N: 
        U/head=sakoba'i
        U/num=pl
        U/sem/pred=watermelon
        U/sem/onto=plant.

 sewam = N : 
        U/head = sewa
        U/num = pl
        U/sem/pred=flower
        U/sem/onto=plant.  

 supem = N : 
        U/head = supem
        U/num=pl
        U/sem/pred=skirt
        U/sem/onto=artifact.  
  
 tahka'im = N: 
        U/head=tahka'i
        U/num=pl
        U/sem/pred=tortilla
        U/sem/onto=thing.

 ta'apo = N: 
        U/head=ta'apo
        U/sem/pred=day
        U/sem/onto=tmploc.
 
 teopo = N : 
        U/head = teopo
        U/sem/pred=temple
        U/sem/onto=place.  

 tomi = N : 
        U/head = tomi
        U/sem/pred=money
        U/sem/onto=thing.  

 usi = N : 
        U/head = usi
        U/sem/pred=boy
        U/sem/onto=person.

 usi_hamut = N : 
        U/head = usi
        U/sem/pred=girl
        U/sem/onto=person.
              
 usim = N : 
        U/head = usi
        U/num = pl
        U/sem/pred=boy
        U/sem/onto=person.

 yorim = N : 
        U/head = yori
        U/num = pl
        U/sem/pred=mexican
        U/sem/onto=person.

 yuke = N : 
        U/head = yuke
        U/sem/pred=rain
        U/sem/onto=force.  

 % Proper Nouns
 
 antonyo = N : 
        U/head = Antonio
        U/sem/pred=PERSON
        U/sem/name=Antonio.

 maria = N : 
        U/head = maria
        U/sem/pred=PERSON
        U/sem/name=María.

 peo = N : 
        U/head = peo
        U/sem/pred=PERSON
        U/sem/name=Pedro.

         %------------------------%
         %     Postpositions      %
         %------------------------%

 % becibo, betuk y beppa exigen siempre DEP
              
 becibo = P : 
        U/head=becibo
        U/clitic = -
        U/theta=cause
        U/comp/case=dep.

 beppa = P : 
        U/head=beppa
        U/clitic = -
        U/theta=standard
        U/comp/case=dep.
 
 betuk = P : 
        U/head=betuk
        U/clitic = -
        U/theta=loc
        U/sem/ubic=SUB
        U/comp/case=dep.

 bicaa = P : 
        U/head=bicca
        U/clitic = -
        U/theta=loc
        U/sem/ubic=AD
        U/comp/case=nom.
 
 huni = P : 
        U/head=huni
        U/clitic = -
        U/theta=restr.

 % Postpositional clitics (P)
 % po y beu nunca llevan dep; mak, u, e llevan opcionalm. dep

 e = P :
        U/head=e
        U/comp/num=sg
        U/theta=instr.
 
 mak = P : 
        U/head=mak
        U/sem/onto= animate
        U/theta = assoc.  

 mak = P :
        U/head=mak
        U/sem/onto=thing
        U/theta = instr.  

 mea = P :             % forma plural de -e (cf. Lindenfeld p. 38)
        U/head=e
        U/comp/num=pl
        U/theta=instr.
              
 po = P : 
        U/head=po
        U/comp/case=nom
        U/theta=loc
        U/sem/onto = concr
        U/sem/ubic = IN.

 po = P : 
        U/head=po
        U/theta=time
        U/sem/onto = temp
        U/sem/ubic = QUANDO.

 t  = P : 
        U/head=t
        U/theta = loc  
        U/sem/ubic = SUPER.

 u = P : 
        U/head=w
        U/theta = loc  
        U/sem/ubic=AD.

         %---------------------%
         %       Verbs         %
         %---------------------%

 ane = V (VPP) :
        U/head=ane
        U/sem/pred=be
        @Loc.
         
 attea = V (VTR) :
        U/head=attea
        U/sem/pred=have.

 bica = V (VTR) :
        U/head=bica
        U/sem/pred=see.

 bete = V (VI) : 
        U/head=bete
        U/sem/pred=burn.

 bwa'atua = V (VERB) :    % An instance of causative verb ('to make eat')
        U/head=bwa'atua
        U/subcat=<subj obj iobj>
        U/obj/case=dep
        U/iobj/case=dep
        U/iobj/sem/onto=animate
        U/sem/pred=CAUSE
        U/sem/arg2/pred=eat
        U/sem/arg2/arg1 = U/iobj/sem
        U/sem/arg2/arg2 = U/obj/sem.
        
 bwa'e = V (VTR) : 
        U/head=bwa'e
        U/obj/status=opt
        U/sem/pred=eat.

 bwibwikria = V (VERB) : 
        U/head=bwika
        U/subcat=<subj obj>
        U/obj/case=acc
        U/form=applicative
        U/sem/pred=sing
        U/freq=+
        U/sem/cause=U/obj/sem.
        
 bwika = V (VI) : 
        U/head=bwika
        U/sem/pred=sing.

 bwiksu = V (VI) : 
        U/head=bwika
        U/sem/pred=sing
        U/aspect =c realized.

 cukta = V (VTR) :
        U/head=cukta
        U/sem/pred=cut.

 eteho = V (VI) :
        U/head=eteho
        U/sem/pred=chat.

 etehosu = V (VI) :
        U/head=eteho
        U/sem/pred=chat
        U/aspect =c realized.
        
 gahtaroa = V (VTR) : 
        U/head=gahtaroa
        U/sem/pred=spend.

 hihibwa = V (VTR) : 
        U/head=bwa'e
        U/obj/status=opt
        U/sem/pred=eat
        U/freq=+.
 
 hinu = V (VTR) : 
        U/head=hinu
        U/sem/pred=buy
        U/sem/arg1/onto=hum.

 hipwe = V (VTR) : 
        U/head=hipwe
        U/sem/pred=have.

 kat = V (VPP) :
        U/head=kate
        U/sem/pred=be
        U/comp/status=obl
        @Loc.

 kate = V (VPP) :
        U/head=kate
        U/sem/pred=be
        U/comp/status=obl
        @Loc.
        
 koce = V (VI) : 
        U/head = koce
        U/sem/pred=sleep.
 
 koko = V (VI) : 
        U/head = muku 
        U/sem/pred=die
        U/subj/num = pl.

 mahae = V (VI) : 
        U/head=mahae
        U/sem/pred=fear.
 
 maka = V (VBTR) :
        U/head=maka
        U/sem/pred=give.
         
 me'a = V (VTR) :
        U/head= me'a
        U/sem/pred=kill.
         
 me'ewa = V (VPAS) :
        U/head= me'a
        U/sem/pred=kill.
 
 muku = V (VI) : 
        U/head = muku 
        U/sem/pred=die
        U/subj/num = sg.

 nok = V (VPP) : 
        U/head = noka
        U/sem/pred=talk
        @Directional.
 
 noka = V (VPP) : 
        U/head = noka
        U/sem/pred=talk
        @Directional.

 saha = V (VPP) :
        U/head=saha
        U/sem/pred=go
        @Loc.

 sika = V (VPP) :
        U/head=saha
        U/sem/pred=go
        @Loc
        U/aspect=realized.

 simsu = V (VPP) :
        U/head=saha
        U/sem/pred=go
        @Loc
        U/aspect =c realized.

 tahkae = V (VI) : 
        U/head=tahkae
        U/sem/pred=hmake_tortillas.

 tekipanoa = V (VI) : 
        U/head=tekipanoa
        U/sem/pred=work.

 tenne = V (VI) :
        U/head=wante
        U/sem/pred=run
        U/subj/num=pl.
         
 tu'ure = V (VTR) : 
        U/head=tu'ure
        U/sem/pred=like
        U/sem/arg1/onto=animate.
        
 tu'ute = V (VTR) : 
        U/head=tu'ute
        U/sem/pred=clean.
 
 ya'a = V (VTR) : 
        U/head=yaha
        U/sem/pred=make.
 
 yaha = V (VPP) :
        U/head=yaha
        U/sem/pred=come
        @Loc.

 yewe = V (VI) : 
        U/head=yewe
        U/sem/pred=play.

 yepsa = V (VPP) :
        U/head=yepsa
        U/sem/pred=arrive
        @Loc.

 yuku = V : 
        U/head = yuku
        U/sem/pred=rain
        U/subcat=<>.
 
 wante = V (VI) : 
        U/head=wante
        U/sem/pred=run
        U/subj/num=sg.

 weama = V (VI) :
        U/head=weama
        U/sem/pred=walk.

 wece = V (VI) :
        U/head=wece
        U/sem/pred=fall_down.
          
 weye = V (VPP) :
        U/head=weye
        U/sem/pred=go
        @Loc.

 ye'e = V (VI) : 
        U/head=ye'e
        U/sem/pred=dance.

 yeye'e = V (VI) : 
        U/head=ye'e
        U/sem/pred=dance
        U/freq=+.

 yi'i = V (VI) : 
        U/head=ye'e
        U/sem/pred=dance.
        
         %------------------%
         %   Clitic Verbs   %
         %------------------%

 bae = CLV (VXC) :
        U/head=bae
        U/sem/pred=want
        @SubjControl.

 baene = CLV (VXC) :
        U/head=bae
        U/sem/pred=want
        U/aspect=expected
        @SubjControl.
         
 pea = CLV (VXC) :
        U/head=pea
        U/sem/pred=like
        @SubjControl.

  taite = CLV (VR) :
        U/head=taite
        U/sem/pred=begin.
                 
         %---------------------%
         %   Verb morphemes    %
         %---------------------%

 'e = ASP1 : 
        U/mode=imperative
        U/subj/head=NULL
        U/sem/illoc=command
        U/sem/arg1/pred=you.
          
 k  = ASP1 : U/aspect=realized.

 % NE indicates that an action has not yet taken place.
 
 ne = ASP1 : U/aspect=expected.
     
 n  = ASP2 : U/sem/cont=progr.
 an =  ASP2 : U/sem/cont=progr.

 a'u = COMP.
 m = COMP.
   
         %----------------------%
         %     Adjectives       %
         %----------------------%

 allea = ADJ :
        U/head=allea
        U/sem/pred=happy.
 
 bwere = ADJ :
        U/head=bwe'u
        U/num=pl
        U/sem/pred=big.

 % bwerem = big-PL
  
 bwe'u = ADJ :
        U/head=bwe'u
        U/sem/pred=big.

 bwe'uk = ADJ :
        U/head=bwe'u
        U/case=dep
        U/sem/pred=big.

 ko'okwe = ADJ :
        U/head=ko'okwe
        U/sem/pred=sick.
        
 mwela = ADJ :
        U/head=mwela
        U/sem/pred=old.
 
 tosali = ADJ :
        U/head=tosali
        U/sem/pred=white.
 
 tu'i = ADJ :
        U/head=tu'i
        U/sem/pred=good.
 
 % tu'ik = good-Dep
  
 tutu'i = ADJ : 
        U/head=tu'i
        U/sem/pred=good
        U/num=pl.
 
 tutu'uli = ADJ : 
        U/head = tutu'uli
        U/sem/pred = pretty.
 
 % tutu'ulik pretty-Dep

 tutu'ulim = ADJ :
        U/head=tutu'uli
        U/num=pl
        U/sem/pred=pretty.
 
 tutu'ulimme = ADJ : 
        U/head = tutu'uli
        U/num=pl
        U/sem/pred=pretty.
      
         %----------------------%
         %     Particles        %
         %----------------------%

 % Adverbs

 haibu = ADV : 
        U/head=haibu
        U/sem/pred=already
        U/theta=time.

 haisa = ADV :
        U/head=haisa
        U/sem/pred=perhaps
        U/theta=modality.

 haisaakai = ADV :
        U/head=haisaakai
        U/sem/pred=why
        U/theta=cause.
          
 hu'ubwa = ADV : 
        U/head=hu'ubwa
        U/sem/pred=recently
        U/theta=time.

 ien = ADV : 
        U/head=ien
        U/sem/pred=now
        U/theta=time.

 ka = NEG : U/sem/pol=neg.             

 lauti = ADV : 
        U/head=lauti
        U/sem/pred=slowly
        U/theta = manner.

 si   = ADV : 
        U/head=si
        U/theta=degree
        U/sem/pred=much.

 tuisi = ADV :
        U/head=tuisi
        U/sem/head=much
        U/theta=quant.
        
 tuka = ADV : 
        U/head=tuka
        U/sem/head=yesterday
        U/theta = time.

 uttea = ADV :
        U/head=uttea
        U/sem/pred=quickly
        U/theta = manner.

 yoko = ADV :
        U/head=yoko
        U/sem/pred=tomorrow
        U/theta=time.

 ta = MC :
        U/num=sg
        U/case=dep.

 huni = ESP :
        U/sem/quant =c indif.

        %----------------------%
        %    Place Adverbs     %
        %----------------------%
        
 aman = PAV : 
        U/head=aman
        U/sem/pred=there.

 inim = PAV : 
        U/head=inim
        U/sem/pred=here.

 pa'ako = PAV : 
        U/head=pa'ako
        U/sem/pred=ouside
        U/sem/ubic=EXTRA.

 % haisaakai = why
 % haisa = INT

