:- def_record( node(key,left,right) ).


go :-
    writeln( 'Enter numbers followed by period (999 to quit)' ),
    loop( Tree ),
    inorder( Tree, print_node ), nl,
    field( Tree..left..right..key, X ),
    writeln( tree..left..right=X ).


loop( Root ) :-
    write( 'Number:' ), read(X),
    (
        X = 999
    ->
        true
    ;
        inst( node, N ),
        field( N..key, X ),
        insert( N, Root ),
        !,
        loop( Root )
    ).


insert( Node, Tree ) :-
    (
        var( Tree )
    ->
        Tree = Node
    ;
        field( Tree..key, Kt ),
        field( Node..key, Kn ),
        (
            Kn < Kt
        ->
            field( Tree..left, L ), insert( Node, L )
        ;
            field( Tree..right, R ), insert( Node, R )
        )
    ).


inorder( Tree, Op ) :-
    (
        var( Tree )
    ->
        true
    ;
        field( Tree..left, L ),
        field( Tree..right, R ),
        inorder( L, Op ),
        apply( Op, [Tree] ),
        inorder( R, Op )
    ).


print_node( N ) :-
    field( N..key, K ),
    writeln( K ).


writeln( X ) :-
    write(X), nl.


apply( S, L ) :-
    C =.. [S|L],
    call(C).
