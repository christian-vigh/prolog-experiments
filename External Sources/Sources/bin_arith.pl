% Computational Intelligence: a logical approach. 
% Prolog Code. Binary arithmetic from Exercise 3-9
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% Here we define arithmetic using a binary representation.

% The representation is defines as follows.
% one denoted the number 1; b(X,zero) denotes 2*X
% where X is a number in binary representation; b(X,one) denotes
% 2*X+1 where X is a number in binary representation.

% For example, six (binary 110) is represented as
% b(b(one,one),zero). Thirteen (binary 1101) is
% represented as b(b(b(one,one),zero),one).

% bnumber(N) is true if N is a binary number
bnumber(one).
bnumber(b(S,one)) :- bnumber(S).
bnumber(b(S,zero)) :- bnumber(S).

% succ(N,M) is true if M is N+1
succ(one,b(one,zero)).
succ(b(X,zero),b(X,one)).
succ(b(X,one),b(XS,zero)) :-
   succ(X,XS).

% bplus(X,Y,Z) is true if X+Y=Z in binary representation
bplus(one,X,S) :- succ(X,S).
bplus(b(N,D),one,S) :- succ(b(N,D),S).
bplus(b(R,zero),b(S,zero),b(RS,zero)):-
   bplus(R,S,RS).
bplus(b(R,zero),b(S,one),b(RS,one)) :-
   bplus(R,S,RS).
bplus(b(R,one),b(S,zero),b(RS,one)):-
   bplus(R,S,RS).
bplus(b(R,one),b(S,one),b(SRS,zero)) :-
   bplus(R,S,RS),
   succ(RS,SRS).

% btimes(X,Y,Z) is true if X*Y=Z in binary representation
btimes(one,X,X).
btimes(b(R,zero),S,b(RS,zero)) :-
   btimes(R,S,RS).
btimes(b(R,one),S,RSS) :-
   btimes(R,S,RS),
   bplus(S,b(RS,zero),RSS).


% this can be ignored for now. it lets me use ``iz'' as an infix
% operator (like ``is'').
:- op(700, xfx, iz).

% ``N iz E'' is true is E is an expression that has value the number N.
X iz X :-
   bnumber(X).
X iz Y+Z :-
   Y1 iz Y,
   Z1 iz Z,
   bplus(Y1,Z1,X).
X iz Y*Z :-
   Y1 iz Y,
   Z1 iz Z,
   btimes(Y1,Z1,X).
X iz Y-Z :-
   Y1 iz Y,
   Z1 iz Z,
   bplus(Z1,X,Y1).


% an example of use:
% | ?- X iz b(b(one,one),zero) + b(b(one,one),zero) + one.
% X = b(b(b(one,one),zero),one) 
% | ?- X iz (b(b(one,one),zero) + b(b(one,one),zero) + one) 
%            * b(b(one,one),zero) + one.

% X = b(b(b(b(b(b(one,zero),zero),one),one),one),one)