/*  VAUCHER.PL  */
/*  Shelved on the 15th of August 1989  */           


/*  Change to this declaration for DEC-10 operator precedences.
:- op( 500, xfy, '..' ).
*/
:- op( 120, xfy, '..' ).


def_record( Def ) :-
    functor( Def, Type, Arity ),
    functor( D1, Type, Arity ),  D1 =.. [ _ | Pars1 ],
    functor( D2, Type, Arity ),  D2 =.. [ _ | Pars2 ],
    assert( inst( Type, D1 ) ),
    (
        for( I, 1, Arity ),
            arg( I, Def, Key ),
            mergeparams( I, Pars1, Pars2, F1, F2 ),
            assert( 'Fld'( Key, D1, F1 ) ),
            assert( 'New_fld'( Key, D1, D2, F1, F2 ) ),
        fail
    ;
        true
    ).


field( Obj..F, X ) :-
    (
        'Fld'( F, Obj, X )
    ;
        F = ( F1..Fs ),
        'Fld'( F1, Obj, R ),
        field( R..Fs, X )
    ;
        list( Obj ),
        member( F-X, Obj )
    ).


update( O1..F, Old/NewV, O2 ) :-
    !,
    (
        'New_fld'( F, O1, O2, OldV, NewV )
    ;
        F = F1..Fs,
        'New_fld'( F1, O1, O2, Oo1, Oo2 ),
        update( Oo1..Fs, NewV, Oo2 )
    ;
        list( O1 ),
        modlist( F, O1, O2, _, NewV )
    ).

update( O1..F, NewV, O2 ) :-
    (
        'New_fld'( F, O1, O2, _, NewV )
    ;
        F = F1..Fs,
        'New_fld'( F1, O1, O2, Oo1, Oo2 ),
        update( Oo1..Fs, NewV, Oo2 )
    ;
        list( O1 ),
        modlist( F, O1, O2, _, NewV )
    ).


modlist( Key, [Key-Old|Rest], [Key-New|Rest], Old, New ).

modlist( Key, [Pair|Rest], [Pair|Restx], Old, New ) :-
    modlist( Key, Rest, Restx, Old, New ).


mergeparams( 1, [F1|Tail], [F2|Tail], F1, F2 ).

mergeparams( N, [F|T1], [F|T2], F1, F2 ) :-
    N > 1,
    N2 is N-1,
    mergeparams( N2, T1, T2, F1, F2 ).


for( I, I, U ).

for( I, L, U ) :-
    L < U,
    L2 is L+1,
    for( I, L2, U ).


list( [] ).

list( [_|_] ).


member(X, [X|_]).

member(X, [_|Y]) :-
    member(X, Y).    
