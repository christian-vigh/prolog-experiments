% Syntax layer

s(A,C,s(X,Y)):-np(A,B,X),vp(B,C,Y).     %  S -> NP VP
np(A,B,np(X)):-pn(A,B,X).               % NP -> PN
np(A,B,np(X)):-sn(A,B,X).               % NP -> SN   (special noun)
np(A,C,np(X,Y)):-det(A,B,X),n(B,C,Y).   % NP -> Det N
vp(A,C,vp(X,Y)):-v(A,B,X),np(B,C,Y).    % VP -> V NP
pp(A,C,pp(X,Y)):-p(A,B,X),np(B,C,Y).    % PP -> P NP
%vp(A,C,vp(X,Y)):-vp(A,B,X),pp(B,C,Y).  % VP -> VP PP (goes into loop)
% unfold VP to avoid left recursion
vp(A,D,vp(vp(X,Y),Z)):-v(A,B,X),np(B,C,Y),pp(C,D,Z). % VP -> V NP PP

det([a|X],X,a).
det([the|X],X,the).
p([to|X],X,to).
p([for|X],X,for).
pn([tom|X],X,tom).
pn([bob|X],X,bob).
n([dog|X],X,dog).
n([ball|X],X,ball).
sn([charity|X],X,charity). % special noun (used without a det)
v([threw|X],X,threw).
v([gave|X],X,gave).

% Semantic layer

sem(s(np(Agent),vp(Verb,np(_,Object))),
    sem([action=Action,agent=Agent,object=Object])) :-
   person(Agent),
   object(Object),
   meaning(Verb,_,Action,_).
sem(s(np(Agent),vp(vp(Verb,np(_,Object)),pp(P,NP))),
    sem([action=Action,agent=Agent,object=Object,PP_type=N])) :-
   person(Agent),
   object(Object),
   (object(N);person(N)),
   N\==Object,
   meaning(Verb,P,Action,PP_type),
   get_noun(NP,N).

% word meanings

meaning(threw,to,propel,direction).
meaning(threw,for,sponsor,recipient).
meaning(gave,to,giving,subject).

person(bob).
person(tom).

object(ball).
object(dog).


get_noun(np(PN),PN).
get_noun(np(_,N),N).


% Correct parses:

% [tom,threw,a,ball]             -> [action=propel,agent=tom,object=ball]
% [tom,threw,a,ball,for,charity] -> [action=sponsor,agent=tom,object=ball,recipient=charity]
% [tom,threw,a,ball,to,the,dog]  -> [action=propel,agent=tom,object=ball,direction=dog]
% [tom,gave,a,ball,to,the,dog]   -> [action=giving,agent=tom,object=ball,subject=dog]
% [tom,gave,a,ball,to,bob]       -> [action=giving,agent=tom,object=ball,subject=bob]
% [tom,gave,the,dog,to,bob]      -> [action=giving,agent=tom,object=dog,subject=bob]
% [tom,threw,a,ball,for,bob]     -> [action=sponsor,agent=tom,object=ball,recipient=bob]
% [tom,threw,a,ball,for,the,dog] -> [action=sponsor,agent=tom,object=ball,recipient=dog]

