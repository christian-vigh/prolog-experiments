% Illustrative game tree (from Ivan Bratko's book)

moves(a,[b,c]).
moves(b,[d,e]).
moves(c,[f,g]).
moves(d,[h,i]).
moves(e,[j,k]).
moves(f,[l,m]).
moves(g,[n,o]).

utility(P,V) :- 
    utility1(P,V), 
    write('utility at node '),writeln(P=V).

utility1(h,1).
utility1(i,4).
utility1(j,5).
utility1(k,6).
utility1(l,2).
utility1(m,1).
utility1(n,1).
utility1(o,1).

max(a).
max(d).
max(e).
max(f).
max(g).

min(b).
min(c).
min(h).
min(i).
min(j).
min(k).
min(l).
min(m).
min(n).
min(o).

