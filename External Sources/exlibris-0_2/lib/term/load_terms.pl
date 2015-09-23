load_terms( Filename, Terms ) :-
        open( Filename, read, Stream ),
        load_stream_terms( Stream, Terms ),
        close( Stream ).

load_stream_terms( Stream, Terms ) :-
        read( Stream, Term ),
        ( Term == end_of_file   ->
                Terms = []
                ;
                Terms = [Term|MoreTerms],
               load_stream_terms( Stream, MoreTerms )
        ).
