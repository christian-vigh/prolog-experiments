%   File   : ASSOC.PL
%   Author : R.A.O'Keefe
%   Updated: 9 November 1983
%   Rewritten: K Johnson 25-5-87
%   Purpose: Binary tree implementation of "association lists".

%   Note   : the keys should be ground, the associated values need not be.

%   This stores association pairs in a tree structure; the empty tree
%   is just "t". For example, to store the pair "foo-moo"  in a hitherto
%   empty tree
%   put_assoc(foo,t,moo,T)
%   To add to tree T the pair "bee-flea" giving the tree U
%   put_assoc(bee,T,flea,U)
%   Test data are at the end of the file to help

% Create a new tree by putting a new pair into an existing one.
% Call: put_assoc(+Key,+Old_tree,?Value,-New_Tree).

put_assoc(Key, t(Key,_,L,R), Val, t(Key,Val,L,R)).  % Key = K. Replace.

put_assoc(Key, t(K,V,L,R), Val, t(K,V,Tree,R)) :-   % Key < K
    Key @< K,
    !,
    put_assoc(Key,L,Val,Tree).

put_assoc(Key, t(K,V,L,R), Val, t(K,V,L,Tree)) :-   % Key > K
    !,
    put_assoc(Key,R,Val,Tree).

put_assoc(Key, t, Val, t(Key,Val,t,t)).         % Empty tree

% get_assoc gets the Val associated with Key in a tree.
%   get_assoc(foo,+Tree,-V)
% will find the value associated with "foo" in "Tree".
% If Key is uninstantiated then get_assoc will work sensibly for the calling
% patterns
%   get_assoc(-Key,+Tree,-V)
% which will find every K-V pair on back tracking and
%   get_assoc(-Key,+Tree,+V)
% although the pattern get_assoc(-K,+T,+V) is *time consuming*.

get_assoc(Key, t(K,Val,_,_), Val) :-
    nonvar(Key),
    Key == K,
    !.

get_assoc(Key, t(K,_,L,_), Val) :-
    nonvar(Key),
    Key @< K,
    !,
    get_assoc(Key,L,Val).

get_assoc(Key, t(_,_,_,R), Val) :-
    nonvar(Key),
    !,
    get_assoc(Key,R,Val).


get_assoc(Key,t(_,_,L,_), Val) :-
    var(Key),
    get_assoc(Key,L,Val).

get_assoc(Key,t(K,Val,_,_), Val) :- % Note: if you put t(Key,Val...) in the
    var(Key),           % title line here, then var(Key) will
    Key = K.            % fail: the match instantiates it.

get_assoc(Key,t(_,_,_,R),Val) :-
    var(Key),
    get_assoc(Key,R,Val).

%
% assoc_to_list(+Assoc,-List)
% Converts the tree to a list of the form [Key1-Val1, Key2-Val2...]
% The other mode is possible: see below.
%

assoc_to_list(Assoc, List) :-
    var(List),
    assoc_to_list(Assoc, List, []).

assoc_to_list(t(Key,Val,L,R), List, Rest) :-
    var(List),
    assoc_to_list(L, List, [Key-Val|More]),
    assoc_to_list(R, More, Rest).

assoc_to_list(t, List, L) :-
    var(List),
    List = L.

%
% assoc_to_list(-Assoc,+List)
% produces the shortest possible Assoc tree
%

assoc_to_list(Assoc,List) :-
    var(Assoc),
    keysort(List, Keys),
    length(Keys, N),
    list_to_assoc(N, Keys, Assoc, []).


list_to_assoc(0, List, t, List) :-
    !.

list_to_assoc(N, List, t(Key,Val,L,R), Rest) :-
        A is (N-1) div 2,
        Z is (N-1)-A,
        list_to_assoc(A, List, L, [Key-Val|More]),
        list_to_assoc(Z, More, R, Rest).

%
% map_assoc(+Pred,+In_tree,-Out_tree)
% Calls Pred(X,Y) for every X on the tree.
% Constructs a tree of Ys.
%

map_assoc(Pred, t(Key,Val,L0,R0), t(Key,Ans,L1,R1)) :- !,
    functor(Term,Pred,2),
    arg(1,Term,Val),
    arg(2,Term,Ans),
    call(Term),
    map_assoc(Pred, L0, L1),
    map_assoc(Pred, R0, R1).

map_assoc(_, t, t).

% % Test
% 
% insert(K,V) :-            % Insert pair K,V into the recorded
%   recorded(tree,T,Ref),       % tree. Note, the code above does not
%   put_assoc(K,T,V,T1),        % record the tree anywhere. You have to
%   erase(Ref),         % do it yourself.
%   record(tree,T1,_).
% 
% test(_) :-                % Test(T) will build up a small tree
%   recorded(tree,_,Ref),       % Remove any existing tree(s)
%   erase(Ref),
%   fail.
% 
% test(T) :-
%   record(tree,t,_),       % Create an empty tree
%   insert(mean, bean),     % Hang some rhyming pairs off it
%   insert(hoe,go),
%   insert(foo,you),
%   insert(bee,flea),
%   insert(jack,stack),
%   insert(nick,quick),
%   insert(why,sky),
%   insert(word,bird),
%   insert(funny,money),
%   insert(ping,sing),
%   recorded(tree,T,_).
%                   % Usage of assoc_to_list
% balance_tree(T,B) :-          % This balances the tree +T giving -B
%   assoc_to_list(T,L),     % If you need balanced trees, of course,
%   assoc_to_list(B,L).     % there are better ways than this
% 
% % a test for map_assoc: the "abbrev" predicate deletes all letters
% % in a name after the third
% 
% test_map(T,U) :-      % Call test_map(-T,-U)
%   recorded(tree,T,_),
%   map_assoc(abbrev,T,U).
% 
% abbrev(Long,Cut) :-
%   name(Long,Letters),
%   abbrev_list(3,Letters,Fewer_letters),
%   name(Cut,Fewer_letters).
% 
% abbrev_list(_,[],[]) :- !.
% abbrev_list(0,_,[]) :- !.
% abbrev_list(N,[H|T],[H|U]) :-
%   M is N - 1,
%   abbrev_list(M,T,U).
