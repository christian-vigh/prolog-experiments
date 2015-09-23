% put in autonomous file, 16ht june 1999. Nicos.

:- ensure_loaded( library('io/fget_line') ). % /2.

%%%  file+stream to_list_of_lines
file_to_list_of_lines( File, Lines ) :-
        open( File, read, Stream ),
        stream_to_list_of_lines( Lines, Stream ),
        close( Stream ).

stream_to_list_of_lines( [L|Ls], Stream ) :-
        fget_line( Stream, L ),
        ( at_end_of_stream(Stream) -> Ls = []
                                 |  stream_to_list_of_lines( Ls, Stream )
	).
