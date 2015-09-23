% CG LEXICON
% ======================================================================
% Bob Carpenter                     Email: carp@research.bell-labs.com
% Lucent Technologies Bell Labs     WWW: http://macduff.andrew.cmu.edu/carpenter
% 600 Mountain Avenue, 2D-329       Voice: 908 582-5790
% Murray Hill, NJ  07974            Fax: 908 582-3306


% Written: 3 December 1995
% Copyright 1995, Bob Carpenter

% Updated: 6 February 1997
% Copyright 1997, Bob Carpenter


% Categories
% ======================================================================
% <bas_syn> ::= <noun_phrase>
%             | <expletive>
%             | <noun>
%             | <sentence>
%             | <comp_sentence>

% <noun phrase> ::=   np(<num>,<case>)
%    <num> ::= set | ind(<nummark>)
%       <nummark> ::= sng | plu
%    <case> ::= nm(<normcase>) | pp(<ppcase>)
%       <normcase> ::= nom | acc | gen
%       <ppcase> ::= of | to | ...
% <expletive> ::= ex(<ex>)
%    <ex> ::= it | th(<num>)
% <noun> ::= n(<num>)
% <sentence> ::= s(<vform>)
%    <vform> ::= bse | fin | psp | prp | inf
% <comp_sentence> ::= sc(<comp>,<vform>)
%    <comp> ::= that | if | whether | ...


% Macros
% ======================================================================

vp(VForm,Num) macro
  np(ind(Num),nm(nom)) \ s(VForm).

gq_vp(VForm,Num) macro
  scop(s,np(ind(Num),nm(nom))) \ s(VForm).

ex_vp(VForm,Expl) macro
  ex(Expl) \ s(VForm).

vp_vp(VForm) macro
  vp(inf,_) \ s(VForm).

iv(VForm,Num) macro
  vp(VForm,Num).

tv(VForm,Num) macro
  vp(VForm,Num) / obj.

tcv(VForm,Num,Case) macro
  vp(VForm,Num) / npcase(Case).

tccv(VForm,Num,Case1,Case2) macro
  vp(VForm,Num) / npcase(Case2) / npcase(Case1).

dv(VForm,Num) macro
  tv(VForm,Num) / obj.

ddv(VForm,Num) macro
  vp(VForm,Num) / npcase(to) / obj.

sv(VForm,Num) macro
  vp(VForm,Num) / s(fin).

evp(VForm,Exp) macro
  ex(Exp) \ s(VForm). 

aux(VForm1,VForm2,X) macro
  (X \ s(VForm1)) / (X \ s(VForm2)).  

%  X = np(Y,nm(Z))                -- number X
%      ex(th(X))                  -- number X
%      ex(it)                     -- number sing
%      scop(np(Y,nm(Z)),s(V))     -- number X
%      sc(V)                      -- number sing
%      np(Y,nm(Z)) \ s(inf)       -- number sing

raise_singular(np(ind(sng),nm(_Case))).
raise_singular(ex(it)).
raise_singular(ex(th(sng))).
raise_singular(q(np(ind(sng),nm(_Case)),s(VForm),s(VForm))).
raise_singular(sc(_Comp)).
raise_singular(np(ind(_IndNum),nm(_Case))\s(inf)).

raise_plural(np(set,nm(_Case))).
raise_plural(np(ind(plu)),nm(_Case)).
raise_plural(ex(th(plu))).
raise_plural(q(np(set,nm(_Case)),s(VForm),s(VForm))).
raise_plural(q(np(ind(plu),nm(_Case)),s(VForm),s(VForm))).

raise(X):-  raise_singular(X).
raise(X):-  raise_plural(X).

neg(VForm,X) macro
  aux(VForm,VForm,X).

scop(Cat1,Cat2) macro
  q(Cat1,Cat2,Cat2).

name macro
  np(ind(sng),nm(_)).

subj macro
  np(ind(_),nm(nom)).

obj macro
  np(ind(_),nm(acc)).

npcase(Case) macro
  np(ind(_),pp(Case)).


cn macro
  n(ind(sng)).
cn_plu macro
  n(ind(plu)).

adj macro
  n(ind(Num)) / n(ind(Num)).

pp macro
  n(ind(Num)) \ n(ind(Num)).

p macro
  pp / scop(obj,s(_)).


case(C) macro
  np(Num,pp(C)) / np(Num,nm(acc)). 

gq macro
  scop(np(ind(sng),nm(_)),s(_)).

gq_plu macro
  scop(np(ind(plu),nm(_)),s(_)).

gq_obj macro
  scop(np(ind(_),nm(acc)),s(_)).
gq_subj macro
  scop(np(ind(_),nm(nom)),s(_)).

gq_set macro
  scop(np(set,nm(_)),s(_)).
  

% Empty Categories
% ======================================================================

% Plural Morphology
% ----------------------------------------------------------------------
empty
  n(ind(plu)) \ n(set)
  : con(plu).

% Bare Plural Existential
% ----------------------------------------------------------------------
empty 
  scop(np(set,nm(_)),s(_)) / n(set)
  : con(some).

% Distributor
% ----------------------------------------------------------------------
empty
  scop(np(ind(plu),nm(C)), s(_)) / np(set,nm(C))
  : con(dist).

% Collector
% ----------------------------------------------------------------------
empty
  scop(np(ind(plu),nm(C)), s(_)) / np(set,nm(C))
  : con(coll). 


% Lexical Entries
% ======================================================================

% +Word:<word> ==> Cat:<cat>
% ----------------------------------------------------------------------
% Word has basic lexical entry Cat;  Cat is expanded by macros
% to produce a lexical entry
% ----------------------------------------------------------------------

% Proper Names
% ----------------------------------------------------------------------
'Brett' ==> 
  name 
  : con(b).
'Brooke' ==> 
  name 
  : con(b2).
'Carmen' ==> 
  name 
  : con(c).
'Chris' ==> 
  name 
  : con(c2).
'Dana' ==> 
  name 
  : con(d).
'Francis' ==> 
  name 
  : con(f).
'Jan' ==> 
  name 
  : con(j).
'Jo' ==> 
  name 
  : con(j2).
'Kim' ==> 
  name 
  : con(k).
'Kelly' ==> 
  name 
  : con(k2).
'Lee' ==> 
  name 
  : con(l).
'Leslie' ==> 
  name 
  : con(l2).
'Morgan' ==> 
  name 
  : con(m).
'Pat' ==> 
  name 
  : con(p).
'Robin' ==> 
  name 
  : con(r).
'Sandy' ==> 
  name 
  : con(s).
'Sal' ==> 
  name 
  : con(s2).
'Shawn' ==> 
  name 
  : con(s3).
'Terry' ==> 
  name 
  : con(t).
'Taylor' ==> 
  name 
  : con(t2).
'Tyler' ==> 
  name 
  : con(t3).
'Gerry' ==> 
  name 
  : con(g).
'Hilary' ==> 
  name 
  : con(h).
'Rene' ==> 
  name 
  : con(r).

'Pittsburgh' ==>
  name
  : con(pgh).
'Cleveland' ==>
  name
  : con(cle).
'Detroit' ==>
  name
  : con(det).

% Expletives
% ----------------------------------------------------------------------
it ==> 
  ex(it) 
  : con(1).
there ==>
  ex(th(_))
  : con(1).


% Case Markers
% ----------------------------------------------------------------------
to ==>
  case(to)
  : con(id).

about ==>
  case(about)
  : con(id).

of ==>
  case(of)
  : con(id).

on ==>
  case(on)
  : con(id).

with ==>
  case(with)
  : con(id).


% Intransitive Verbs
% ----------------------------------------------------------------------
run ==> 
  iv(bse,_)
  : con(run).
runs ==>
  iv(fin,sng)
  : con(pres_run).
run ==> 
  iv(fin,plu)
  : con(pres_run).
ran ==> 
  iv(fin,_)
  : con(past_run).
running ==>
  iv(prp,_)
  : con(prp_run).
run ==> 
  iv(psp,_)
  : con(psp_run).

jump ==> 
  iv(bse,_)
  : con(jump).
jumps ==>
  iv(fin,sng)
  : con(pres_jump).
jump ==> 
  iv(fin,plu)
  : con(pres_jump).
jumped ==> 
  iv(fin,_)
  : con(past_jump).
jumping ==>
  iv(prp,_)
  : con(prp_jump).
jumped ==> 
  iv(psp,_)
  : con(psp_jump).

  
sleep ==>
  iv(bse,_)
  : con(sleep).
sleeps ==>
  iv(fin,sng)
  : con(pres_sleep).
sleep ==>
  iv(fin,plu)
  : con(pres_leep).
slept ==>
  iv(fin,_)
  : con(past_sleep).
sleeping ==>
  iv(prp,_)
  : con(prp_sleep).
slept ==>
  iv(psp,_)
  : con(psp_sleep).

talk ==> 
  iv(bse,_)
  : con(talk).
talks ==> 
  iv(fin,sng)
  : con(pres_talk).
talk ==> 
  iv(fin,plu)
  : con(pres_talk).
talked ==> 
  iv(fin,_)
  : con(past_talk).
talking ==> 
  iv(prp,_)
  : con(prp_talk).
talked ==> 
  iv(psp,_)
  : con(psp_talk).



% Transitive Verbs
% ----------------------------------------------------------------------
like ==> 
  tv(bse,_)
  : con(like).
likes ==> 
  tv(fin,sng)
  : con(pres_like).
like ==> 
  tv(fin,plu)
  : con(pres_like).
liked ==> 
  tv(fin,_)
  : con(past_like).
liking ==> 
  tv(prp,_)
  : con(prp_like).
liked ==> 
  tv(psp,_)
  : con(psp_like).

dislike ==> 
  tv(bse,_)
  : con(dislike).
dislikes ==> 
  tv(fin,sng)
  : con(pres_dislike).
dislike ==> 
  tv(fin,plu)
  : con(pres_dislike).
disliked ==> 
  tv(fin,_)
  : con(past_dislike).
disliking ==> 
  tv(prp,_)
  : con(prp_dislike).
disliked ==> 
  tv(psp,_)
  : con(psp_dislike).

believe ==>
  tv(bse,_)
  : con(bel).
believes ==>
  tv(fin,sng)
  : con(pres_bel).
believe ==>
  tv(fin,plu)
  : con(pres_bel).
believed ==>
  tv(fin,_)
  : con(past_bel).
believing ==>
  tv(prp,_)
  : con(prp_bel).
believed ==>
  tv(psp,_)
  : con(prp_bel).

% Expletive Subject Verbs
% ----------------------------------------------------------------------
rain ==>
  evp(bse,it)
  : con(rain).
rains ==>
  evp(fin,it)
  : con(pres_rain).
rained ==>
  evp(fin,it)
  : con(past_rain).
raining ==>
  evp(prp,it)
  : con(prp_rain).
rained ==>
  evp(psp,it)
  : con(psp_rain).

% Existential 'be'
% ----------------------------------------------------------------------
be ==>
  ex(th(Num)) \ s(bse) / np(ind(Num),nm(acc))
  : con(be_ex).
is ==>
  ex(th(sng)) \ s(fin) / np(ind(sng),nm(acc))
  : con(pres_be_ex).
are ==>
  ex(th(plu)) \ s(fin) / np(ind(plu),nm(acc))
  : con(pres_be_ex).
was ==>
  ex(th(sng)) \ s(fin) / np(ind(sng),nm(acc))
  : con(past_be_ex).
were ==>
  ex(th(plu)) \ s(fin) / np(ind(plu),nm(acc))
  : con(past_be_ex).
being ==>
  ex(th(Num)) \ s(prp) / np(ind(Num),nm(acc))
  : con(prp_be_ex).
been ==>
  ex(th(Num)) \ s(psp) / np(ind(Num),nm(acc))
  : con(psp_be_ex).


% Transitive Verbs with Case-Marked Objects
% ----------------------------------------------------------------------
talk ==> 
  tcv(bse,_,to)
  : con(talk_to).
talks ==> 
  tcv(fin,sng,to)
  : con(pres_talk_to).
talk ==> 
  tcv(fin,plu,to)
  : con(pres_talk_to).
talked ==> 
  tcv(fin,_,to)
  : con(past_talk_to).
talking ==> 
  tcv(prp,_,to)
  : con(prp_talk_to).
talked ==> 
  tcv(psp,_,to)
  : con(psp_talk_to).

talk ==> 
  tcv(bse,_,about)
  : con(talk_about).
talks ==> 
  tcv(fin,sng,about)
  : con(pres_talk_about).
talk ==> 
  tcv(fin,plu,about)
  : con(pres_talk_about).
talked ==> 
  tcv(fin,_,about)
  : con(past_talk_about).
talking ==> 
  tcv(prp,_,about)
  : con(prp_talk_about).
talked ==> 
  tcv(psp,_,about)
  : con(psp_talk_about).



% Ditransitive Object Case-Marked Verbs
% ----------------------------------------------------------------------
talk ==> 
  tccv(bse,_,to,about)
  : con(talk_to_about).
talks ==> 
  tccv(fin,sng,to,about)
  : con(pres_talk_to_about).
talk ==> 
  tccv(fin,plu,to,about)
  : con(pres_talk_to_about).
talked ==> 
  tccv(fin,_,to,about)
  : con(pres_talk_to_about).
talking ==> 
  tccv(prp,_,to,about)
  : con(pres_talk_to_about).
talked ==> 
  tccv(psp,_,to,about)
  : con(pres_talk_to_about).

talk ==> 
  tccv(bse,_,about,to)
  : con(talk_about_to).
talks ==> 
  tccv(fin,sng,about,to)
  : con(pres_talk_about_to).
talk ==> 
  tccv(fin,plu,about,to)
  : con(pres_talk_about_to).
talked ==> 
  tccv(fin,_,about,to)
  : con(past_talk_about_to).
talking ==> 
  tccv(prp,_,about,to)
  : con(prp_talk_about_to).
talked ==> 
  tccv(psp,_,about,to)
  : con(psp_talk_about_to).




% Ditransitive Verbs
% ----------------------------------------------------------------------
give ==> 
  dv(bse,_)
  : con(give).
gives ==> 
  dv(fin,sng)
  : con(pres_give).
give ==> 
  dv(fin,plu)
  : con(pres_give).
gave ==> 
  dv(fin,_)
  : con(past_give).
giving ==> 
  dv(prp,_)
  : con(prp_give).
given ==> 
  dv(psp,_)
  : con(psp_give).

% Ditransitive Dative Verbs
% ----------------------------------------------------------------------
give ==> 
  ddv(bse,_)
  : con(give2).
gives ==> 
  ddv(fin,sng)
  : con(give2).
give ==> 
  ddv(fin,plu)
  : con(give2).
gave ==> 
  ddv(fin,_)
  : con(give2).
giving ==> 
  ddv(prp,_)
  : con(give2).
given ==> 
  ddv(psp,_)
  : con(give2).


% Sentential Verbs
% ----------------------------------------------------------------------
believe ==>
  sv(bse,_)
  : con(bel).
believes ==>
  sv(fin,sng)
  : con(bel).
believe ==>
  sv(fin,plu)
  : con(bel).
believed ==>
  sv(fin,_)
  : con(bel).
believing ==>
  sv(prp,_)
  : con(bel).
believed ==>
  sv(psp,_)
  : con(bel).

% Auxiliary 'be'
% ----------------------------------------------------------------------
be ==> 
  aux(bse,prp,X)
  : con(be)
  :- raise(X).
is ==>
  aux(fin,prp,X)
  : con(be)
  :- raise_singular(X).
isnt ==>
  aux(fin,prp,X)
  : con(be)
  :- raise_singular(X).
are ==>
  aux(fin,prp,X)
  : con(be)
  :- raise_plural(X).
arent ==>
  aux(fin,prp,X)
  : con(be_not)
  :- raise_plural(X).
was ==>
  aux(fin,prp,X)
  : con(be)
  :- raise_singular(X).
wasnt ==>
  aux(fin,prp,X)
  : con(be_not)
  :- raise_singular(X).
were ==>
  aux(fin,prp,X)
  : con(be)
  :- raise_plural(X).
werent ==>
  aux(fin,prp,X)
  : con(be_not)
  :- raise_plural(X).
being ==>
  aux(prp,prp,X)
  : con(be)
  :- raise(X).
been ==>
  aux(psp,prp,X)
  : con(be)
  :- raise(X).

% Auxiliary 'have'
% ----------------------------------------------------------------------
have ==>
  aux(bse,psp,X)
  : con(have)
  :- raise(X).
has ==>
  aux(fin,psp,X)
  : con(have)
  :- raise_singular(X).
hasnt ==>
  aux(fin,psp,X)
  : con(have_not)
  :- raise_singular(X).
have ==>
  aux(fin,psp,X)
  : con(have)
  :- raise_plural(X).
have ==>
  aux(fin,psp,X)
  : con(have_not)
  :- raise_plural(X).
had ==> 
  aux(fin,psp,X)
  : con(have)
  :- raise(X).
hadnt ==> 
  aux(fin,psp,X)
  : con(have_not)
  :- raise(X).
having ==>
  aux(prp,psp,X)
  : con(have)
  :- raise(X).

% Auxiliary 'do'
% ----------------------------------------------------------------------
does ==>
  aux(fin,bse,X)
  : con(do)
  :- raise_singular(X).
doesnt ==>
  aux(fin,bse,X)
  : con(do_not)
  :- raise_singular(X).
do ==>
  aux(fin,bse,X)
  : con(do)
  :- raise_plural(X).
dont ==>
  aux(fin,bse,X)
  : con(do_not)
  :- raise_plural(X).
did ==>
  aux(fin,bse,X)
  : con(do)
  :- raise(X).
didnt ==>
  aux(fin,bse,X)
  : con(do_not)
  :- raise(X).

% Modal Auxiliaries
% ----------------------------------------------------------------------
will ==>
  aux(fin,bse,X)
  : con(will)
  :- raise(X).
wont ==>
  aux(fin,bse,X)
  : con(will_not)
  :- raise(X).
should ==>
  aux(fin,bse,X)
  : con(should)
  :- raise(X).
shouldnt ==>
  aux(fin,bse,X)
  : con(should_not)
  :- raise(X).
could ==>
  aux(fin,bse,X)
  : con(could)
  :- raise(X).
couldnt ==>
  aux(fin,bse,X)
  : con(could_not)
  :- raise(X).
may ==>
  aux(fin,bse,X)
  : con(may)
  :- raise(X).
might ==>
  aux(fin,bse,X)
  : con(might)
  :- raise(X).
mightnt ==>
  aux(fin,bse,X)
  : con(might_not)
  :- raise(X).

% Negative 'particle'
% ----------------------------------------------------------------------

% Verbal Modifiers
not ==>
  neg(bse,X)
  : con(neg)
  :- raise(X).
not ==>
  neg(prp,X)
  : con(neg)
  :- raise(X).
not ==>
  neg(psp,X)
  : con(neg)
  :- raise(X).
not ==>
  neg(inf,X)
  : con(neg)
  :- raise(X).


% Quantifier Modifiers
not ==>
  scop(np(X,nm(Y)),s(V))
  / scop(np(X,nm(Y)),s(V))
  : con(neg_gq).

% Adjective Modifier
not ==>
  n(N) / n(N) / (n(N) / n(N))
  : con(neg_adj).

% Post-verbal Modifier
not ==>
  (X \ s(VForm) \ (X \ s(VForm)))
  \ (X \ s(VForm) \ (X \ s(VForm)))
  : con(neg_adv)
  :- raise(X).


% Common Nouns
% ----------------------------------------------------------------------
kid ==>
  cn
  : con(kid).
kids ==>
  cn_plu
  : con(kid).

class ==>
  cn
  : con(class).
classes ==>
  cn_plu
  : con(class).

box ==>
  cn
  : con(box).
boxes ==>
  cn_plu
  : con(box).

paper ==>
  cn
  : con(paper).
papers ==>
  cn_plu
  : con(paper).

student ==>
  cn
  : con(stud).
students ==>
  cn_plu
  : con(stud).



vegetarian ==>
  cn
  : con(veg).
vegetarians ==>
  cn_plu
  : con(veg).

socialist ==>
  cn
  : con(soc).
socialists ==>
  cn_plu
  : con(soc).

picture ==>
  cn
  : con(pict0).
pictures ==>
  cn_plu
  : con(pict0).


% Common Nouns with Complements
% ----------------------------------------------------------------------
picture ==>
  cn / npcase(of)
  : con(pict).
pictures ==>
  cn_plu / npcase(of)
  : con(pict).




% Adjectives
% ----------------------------------------------------------------------
red ==>
  adj
  : con(red).

white ==>
  adj
  : con(white).

smart ==>
  adj
  : con(smart).
stupid ==>
  adj
  : con(stupid).

angry ==>
  adj
  : con(angry).
happy ==>
  adj
  : con(happy).
big ==>
  adj
  : con(big).
small ==>
  adj
  : con(small).
  


% Objectless Nominal Prepositions
% ----------------------------------------------------------------------
outside ==>
  pp
  : con(outside).

% Nominal Prepositions
% ----------------------------------------------------------------------
in ==>
  p
  : con(in).


on ==>
  p
  : con(on).


% Collective Nominal Prepositions
% ----------------------------------------------------------------------
around ==>
  p
  : con(around).

% the following behavior might be subsumed under the null predicative 
% relative analyzing 'kids [around the fire]' like 'kids [carrying a box]'
% one approach would be a null relativizer, a second would be a lex rule
around ==> 
  n(set) \ n(set) / obj
  : con(around).

% Verbal Objectless Prepositions
% ----------------------------------------------------------------------
outside ==>
  vp(VF,N) \ vp(VF,N)
  : con(outside_v).


% Verbal Prepositions
% ----------------------------------------------------------------------
in ==> 
  vp(VF,N) \ vp(VF,N) / gq_obj
  : con(in_v).

on ==> 
  vp(VF,N) \ vp(VF,N) / gq_obj
  : con(on_v).


% Temporal Adverbials
% ----------------------------------------------------------------------
yesterday ==>
  vp(VF,N) \ vp(VF,N)
  : con(yest).

today ==>
  vp(VF,N) \ vp(VF,N)
  : con(today).


% Definite Articles
% ----------------------------------------------------------------------

the ==>
  np(ind(sng),nm(_)) / n(ind(sng))
  : con(the).
the ==>
  np(set,nm(_)) / n(set)
  : con(the_p).


% Possessive
% ----------------------------------------------------------------------

% Possession
% s ==>
%  scop(np(ind(_),nm(_)),s(_)) \ (np(set,nm(_)) / n(set)) 
%  : con(poss).
% 
% s ==>
%   scop(np(ind(_),nm(_)),s(_)) \ (np(ind(sng),nm(_)) / n(ind(sng))) 
%   : con(poss).
% 
% % 'of' Role Filler
% s ==>
%   scop(np(ind(_),nm(_)),s(_)) \ ( np(ind(sng),nm(_)) 
%                                   / ( n(ind(sng)) / np(ind(sng),pp(of)) ))
%   : con(poss_of).
% 
% s ==>
%   scop(np(ind(_),nm(_)),s(_)) \ ( np(set,nm(_)) 
%                                   / ( n(ind(sng)) / np(ind(sng),pp(of)) ))
%   : con(poss_of).




% Generalized Quantifiers
% ----------------------------------------------------------------------

everything ==>
  gq
  : con(every1).
everyone ==> 
  gq
  : con(every1).
everybody ==> 
  gq
  : con(every1).

nobody ==> 
  gq
  : con(no1).
nothing ==>
  gq
  : con(no1).

someone ==> 
  gq
  : con(some1).
somebody ==> 
  gq
  : con(some1).
something ==> 
  gq
  : con(some1).

most ==>
  gq_plu
  : con(most1).
few ==>
  gq_plu
  : con(few1).


% Generalized Determiners
% ----------------------------------------------------------------------

% Singular
% --------
some ==>
  gq / cn
  : con(some).
a ==>
  gq / cn
  : con(some).
every ==>
  gq / cn
  : con(every).


% Plural
% ------
most ==>
  gq_plu / cn_plu
  : con(most).
few ==>
  gq_plu / cn_plu
  : con(few).



% Coordinators
% ----------------------------------------------------------------------

and ==>
  coor
  : con(and).
but ==>
  coor
  : con(and).
or ==>
  coor
  : con(or).



% Relativizers
% ----------------------------------------------------------------------

who ==>
  n(X) \ n(X) / ( np(X,nm(nom)) \ s(fin) )
  : con(who).

whom ==>
  n(X) \ n(X) / (s(fin) - np(X,nm(acc)))
  : con(who).
who ==>
  n(X) \ n(X) / (s(fin) - np(X,nm(acc)))
  : con(who).

which ==>
  n(X) \ n(X) / ( np(X,nm(nom)) \ s(fin) )
  : con(who).
which ==>
  n(X) \ n(X) / (s(fin) - np(X,nm(acc)))
  : con(who).


% Sentential Complementizers
% ----------------------------------------------------------------------
that ==>
  sc(th(bse)) / s(bse)
  : con(that_sc).
that ==>
  sc(th(fin)) / s(fin)
  : con(that_sc).



% NOTES
% ======================================================================
% Could handle detransitivization with an empty object of normal/pp form: 
%    np(ind(sng),nm(acc))
%    np(ind(sng),pp(_))
% saves lots of lexical entries or lexical rules;
% could mark np's with info as to whether they can be empty or not;
% existentials introduced in this way shoudl always be narrow

% could deal with quantifier dependencies by using nested storage

% need to filter out hypotheticals inside of derived categories;
% as is, type raising happens to objects in query:
%      Sandy talked der(about Terry) der(to Kim)

% need to add back in code that provides:
%    - GIF to click on -- probably saves scheduling problems
%    - .eps to download
%    - LaTeX source to download (warning, warning)

% consider sequent-style output formatting;  should be able to 
% convert from ND and vice-versa

% revise the Fitch style to handle all of the wrapping, etc., involved
% with quantifiers and other categories

% add products

% write a real Prolog term input parser -- need operator hierarchy,
% lists, precedences, non-ambiguity, etc.  -- probably LC or other
% bottom-up approach -- should be determinizable

% add person distinctions (esp. aux and pronoun)
% add question forms of auxiliaries


% add a help facility

% add meaning postulate expansion -- allow a button to select for
% this being on or off; assume the following form:

con(plu) means
  var(Y)^var(X)^con(and)@(con(every)@var(X)@var(Y))
                        @(con(geq)@(con(card)@var(X))@con(2)).

con(dist) means
  con(every).

con(coll) means
  var(X)^con(some)@(con(group)@var(X)).

con(id) means
  var(X)^var(X).

con(rain) means
  var(X)^con(rain_p).

con(be_ex) means
  var(X)^var(Y)^con(exist)@var(X).

con(talk_about_to) means
  con(talk).
con(talk_to_about) means
  var(X)^var(Y)^con(talk)@var(Y)@var(X).  
con(talk_about) means
  var(X)^var(Z)^con(some1)@(var(Y)^con(talk)@var(X)@var(Y)@var(Z)).
con(talk1) means
  var(X)^var(Z)^con(some1)@(var(Y)^con(talk)@var(Y)@var(X)@var(Z)).
con(talk0) means
  var(Z)^con(some1)@(var(X)^con(some1)@(var(Y)^con(talk)@var(X)@var(Y)@var(Z))).

con(dislike) means
  var(Y)^var(X)^con(not)@(con(like)@var(Y)@var(X)).

con(give2) means
  var(Y)^var(X)@con(give)@var(X)@var(Y).

con(be) means
  var(Y)^var(X)^var(Y)@var(X).
con(be_not) means
  var(Y)^var(X)^con(not)@(var(Y)@var(X)).

con(have) means
  var(Y)^var(X)^var(Y)@var(X).
con(have_not) means
  var(Y)^var(X)^con(not)@(var(Y)@var(X)).

con(do) means
  var(Y)^var(X)^var(Y)@var(X).
con(do_not) means
  var(Y)^var(X)^con(not)@(var(Y)@var(X)).

con(will_not) means
  var(Y)^var(X)^con(not)@(con(will)@var(Y)@var(X)).

con(should_not) means
  var(Y)^var(X)^con(not)@(con(should)@var(Y)@var(X)).

con(could_not) means
  var(Y)^var(X)^con(not)@(con(could)@var(Y)@var(X)).

con(might_not) means
  var(Y)^var(X)^con(not)@(con(might)@var(Y)@var(X)).

con(neg) means
  var(Y)^var(X)^con(not)@(var(Y)@var(X)).

con(neg_gq) means
  var(Y)^var(X)^con(not)@(var(Y)@var(X)).

con(neg_adj) means
  var(Y)^var(X)^var(Z)^con(not)@(var(Y)@var(X)@var(Z)).

con(neg_adv) means
  var(Y)^var(X)^var(Z)^con(not)@(var(Y)@var(X)@var(Z)).

con(pict0) means
  var(Y)^con(some)@(var(X)^con(pict)@var(X)@var(Y)).

con(prop_mod) means
  var(R)^var(P)^var(X)^con(and)@(var(P)@var(X))@(var(R)@var(X)).

con(red) means
  con(prop_mod)@con(red_p).
con(white) means
  con(prop_mod)@con(white_p).
con(smart) means
  con(prop_mod)@con(smart_p).
con(stupid) means
  con(prop_mod)@con(stupid_p).
con(angry) means
  con(prop_mod)@con(angry_p).
con(happy) means
  con(prop_mod)@con(happy_p).
con(big) means
  con(prop_mod)@con(big_p).
con(small) means
  con(prop_mod)@con(small_p).

con(outside) means
  con(prop_mod)@con(outside_p).

con(pp_mod) means
  var(R)^var(X)^con(prop_mod)@(var(R)@var(X)).

con(in) means
  con(pp_mod)@con(in_p).
con(on) means
  con(pp_mod)@con(on_p).
con(around) means
  con(pp_mod)@con(around_p).

con(most1) means
  con(most)@var(_).
con(few1) means
  con(few)@var(_).

con(who) means
  var(P)^var(Q)^var(X)^con(and)@(var(P)@var(X))@(var(Q)@var(X)).

con(that_sc) means
  con(id).


