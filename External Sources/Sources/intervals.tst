/*  Intervals.TST  */


/*  Input to AutoTest, for testing Intervals.PL.  */


intervals_are_disjoint( 1--9, 10--19 ) :: s.
intervals_are_disjoint( 1--10, 10--19 ) :: f.
intervals_are_disjoint( 10--19, 1--9 ) :: s.
intervals_are_disjoint( 10--19, 1--10 ) :: f.


intervals_meet( 1--9, 10--19 ) :: f.
intervals_meet( 1--10, 10--19 ) :: s.
intervals_meet( 10--19, 1--9 ) :: f.
intervals_meet( 10--19, 1--10 ) :: s.


interval_union( 1--9, 1--9, 1--9 ) :: s.
interval_union( 1--9, 0--10, 0--10 ) :: s.
interval_union( 0--10, 1--9, 0--10 ) :: s.
interval_union( 0--8, 1--20, 0--20 ) :: s.
interval_union( 1--20, 0--8, 0--20 ) :: s.
interval_union( 1--20, 0--8, V ), V \= (0--20) :: f.
interval_union( 1--9, 10--20, 1--20 ) :: s.
interval_union( 10--20, 1--9, 1--20 ) :: s.
interval_union( 1--10, 10--20, 1--20 ) :: s.
interval_union( 10--20, 1--10, 1--20 ) :: s.


interval_intersection( 1--9, 1--9, 1--9 ) :: s.
interval_intersection( 1--9, (-1)--10, 1--9 ) :: s.
interval_intersection( (-1)--10, 1--9, 1--9 ) :: s.
interval_intersection( (-1)--8, 1--20, 1--8 ) :: s.
interval_intersection( 1--20, (-1)--8, 1--8 ) :: s.
interval_intersection( 1--20, (-1)--8, V ), V \= (1--8) :: f.


interval_difference( (-100)--(-50), 50--100, [(-100)--(-50)] ) :: s.
interval_difference( 50--100, (-100)--(-50), [50--100] ) :: s.
interval_difference( 50--75, 50--75, [] ) :: s.
interval_difference( 50--75, 50--100, [] ) :: s.
interval_difference( 50--75, 60--100, [ 50--59 ] ) :: s.
interval_difference( 50--75, 75--100, [ 50--74 ] ) :: s.
interval_difference( 50--75, 76--100, [ 50--75 ] ) :: s.
interval_difference( 50--75, 40--75, [] ) :: s.
interval_difference( 50--75, 40--60, [ 61--75 ] ) :: s.
interval_difference( 50--75, 40--50, [ 51--75 ] ) :: s.
interval_difference( 50--75, 40--49, [ 50--75 ] ) :: s.
interval_difference( 50--100, 60--70, [ 50--59, 71--100 ] ) :: s.
interval_difference( 60--70, 50--100, [] ) :: s.
interval_difference( 60--70, 50--100, X ), X\= [] :: f.
interval_difference( 50--75, 76--100, X ), X \= [ 50--75 ] :: f.


interval_is_less_and_not_coalescable( 1--2, 3--4 ) :: f.
interval_is_less_and_not_coalescable( 1--1, 3--4 ) :: s.
interval_is_less_and_not_coalescable( 3--4, 1--1 ) :: f.
interval_is_less_and_not_coalescable( 3--4, 1--2 ) :: f.
interval_is_less_and_not_coalescable( 1--2, 1--3 ) :: f.
interval_is_less_and_not_coalescable( 1--3, 1--2 ) :: f.


interval_is_less( 1--2, 3--4 ) :: s.
interval_is_less( 1--1, 3--4 ) :: s.
interval_is_less( 3--4, 1--1 ) :: f.
interval_is_less( 3--4, 1--2 ) :: f.
interval_is_less( 1--3, 1--2 ) :: f.
interval_is_less( 1--2, 1--3 ) :: f.


intervals_are_coalescable( 1--9, 10--20 ) :: s.
intervals_are_coalescable( 10--20, 1--9 ) :: s.
intervals_are_coalescable( 1--10, 10--20 ) :: s.
intervals_are_coalescable( 10--20, 1--10 ) :: s.
intervals_are_coalescable( 1--8, 10--20 ) :: f.
intervals_are_coalescable( 10--20, 1--8 ) :: f.
intervals_are_coalescable( 1--19, 10--20 ) :: s.
intervals_are_coalescable( 10--20, 1--19 ) :: s.
intervals_are_coalescable( 1--24, 10--20 ) :: s.
intervals_are_coalescable( 10--20, 1--24 ) :: s.


intervals_are_not_coalescable( 1--9, 10--20 ) :: f.
intervals_are_not_coalescable( 10--20, 1--9 ) :: f.
intervals_are_not_coalescable( 1--10, 10--20 ) :: f.
intervals_are_not_coalescable( 10--20, 1--10 ) :: f.
intervals_are_not_coalescable( 1--8, 10--20 ) :: s.
intervals_are_not_coalescable( 10--20, 1--8 ) :: s.
intervals_are_not_coalescable( 1--19, 10--20 ) :: f.
intervals_are_not_coalescable( 10--20, 1--19 ) :: f.
intervals_are_not_coalescable( 1--24, 10--20 ) :: f.
intervals_are_not_coalescable( 10--20, 1--24 ) :: f.


interval_ends_first( (-100)--2, 2--4 ) :: s.
interval_ends_first( (-100)--4, 2--2 ) :: f.
interval_ends_first( 2--2, (-100)--4 ) :: s.
interval_ends_first( 2--4, (-100)--2 ) :: f.


iset_union( [1--9], [1--9], [1--9] ) :: s.
iset_union( [1--9], [0--10], [0--10] ) :: s.
iset_union( [0--10], [1--9], [0--10] ) :: s.
iset_union( [0--8], [1--20], [0--20] ) :: s.
iset_union( [1--20], [0--8], [0--20] ) :: s.
iset_union( [1--20], [0--8], V ), V \= [0--20] :: f.
iset_union( [1--9], [10--20], [1--20] ) :: s.
iset_union( [10--20], [1--9], [1--20] ) :: s.
iset_union( [1--10], [10--20], [1--20] ) :: s.
iset_union( [10--20], [1--10], [1--20] ) :: s.

iset_union( [], [], [] ) :: s.
iset_union( [ 1--2, 10--12 ], [], [ 1--2, 10--12 ] ) :: s.
iset_union( [], [ 1--2, 10--12 ], [ 1--2, 10--12 ] ) :: s.
iset_union( [ 1--2, 10--20 ], [ 1--2, 10--20 ], [ 1--2, 10--20 ] ) :: s.
iset_union( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
            [ 3--13, 20--24, 62--69, 200--200 ],
            X )
          :: X = [ 1--13, 17--27, 35--39, 50--57, 61--100, 200--200 ].
iset_union( [ 3--13, 20--24, 62--69, 200--200 ],
            [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
            X )
          :: X = [ 1--13, 17--27, 35--39, 50--57, 61--100, 200--200 ].
iset_union( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
            [ 3--13, 20--24, 62--69, 200--200 ],
            X ),
          X \= [ 1--13, 17--27, 35--39, 50--57, 61--100, 200--200 ] :: f.


iset_intersection( [1--9], [1--9], [1--9] ) :: s.
iset_intersection( [1--9], [(-1)--10], [1--9] ) :: s.
iset_intersection( [(-1)--10], [1--9], [1--9] ) :: s.
iset_intersection( [(-1)--8], [1--20], [1--8] ) :: s.
iset_intersection( [1--20], [(-1)--8], [1--8] ) :: s.
iset_intersection( [1--20], [(-1)--8], V ), V \= [1--8] :: f.

iset_intersection( [], [], [] ) :: s.
iset_intersection( [ 1--2, 10--12 ], [], [] ) :: s.
iset_intersection( [], [ 1--2, 10--12 ], [] ) :: s.
iset_intersection( [ 1--2, 10--20 ], [ 1--2, 10--20 ], [ 1--2, 10--20 ] ) :: s.
iset_intersection( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                   [ 3--13, 20--24, 62--69, 200--200 ],
                   X )
           :: X = [ 3--5, 8--10, 23--24, 62--62, 69--69 ].
iset_intersection( [ 3--13, 20--24, 62--69, 200--200 ],
                   [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                   X )
           :: X = [ 3--5, 8--10, 23--24, 62--62, 69--69 ].
iset_intersection( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                   [ 3--13, 20--24, 62--69, 200--200 ],
                   X ),
                 X \= [ 3--5, 8--10, 23--24, 62--62, 69--69 ] :: f.


iset_difference( [(-100)--(-50)], [50--100], [(-100)--(-50)] ) :: s.
iset_difference( [50--100], [(-100)--(-50)], [50--100] ) :: s.
iset_difference( [50--75], [50--75], [] ) :: s.
iset_difference( [50--75], [50--100], [] ) :: s.
iset_difference( [50--75], [60--100], [ 50--59 ] ) :: s.
iset_difference( [50--75], [75--100], [ 50--74 ] ) :: s.
iset_difference( [50--75], [76--100], [ 50--75 ] ) :: s.
iset_difference( [50--75], [40--75], [] ) :: s.
iset_difference( [50--75], [40--60], [ 61--75 ] ) :: s.
iset_difference( [50--75], [40--50], [ 51--75 ] ) :: s.
iset_difference( [50--75], [40--49], [ 50--75 ] ) :: s.
iset_difference( [50--100], [60--70], [ 50--59, 71--100 ] ) :: s.
iset_difference( [60--70], [50--100], [] ) :: s.
iset_difference( [60--70], [50--100], X ), X\= [] :: f.
iset_difference( [50--75], [76--100], X), X \= [ 50--75 ] :: f.

iset_difference( [1--2], [], [1--2] ) :: s.
iset_difference( [1--10], [15--20], [1--10] ) :: s.
iset_difference( [1--10], [15--20,22--24], [1--10] ) :: s.
iset_difference( [1--10], [(-10)--0], [1--10] ) :: s.
iset_difference( [1--10], [(-10)--0,11--20], [1--10] ) :: s.
iset_difference( [1--10], [1--10], [] ) :: s.
iset_difference( [1--10], [(-5)--(-1),1--10,12--14], [] ) :: s.
iset_difference( [1--10], [3--10], [1--2] ) :: s.
iset_difference( [1--10], [1--5], [6--10] ) :: s.
iset_difference( [1--10], [3--5], [1--2,6--10] ) :: s.
iset_difference( [1--10], [0--0,3--5,11--12], [1--2,6--10] ) :: s.

iset_difference( [], [], [] ) :: s.
iset_difference( [ 1--2, 10--12 ], [], [ 1--2, 10--12 ] ) :: s.
iset_difference( [], [ 1--2, 10--12 ], [] ) :: s.
iset_difference( [ 1--2, 10--20 ], [ 1--2, 10--20 ], [] ) :: s.
iset_difference( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                 [ 3--13, 20--24, 62--69, 200--200 ],
                 X )
               :: X = [ 1--2, 17--19, 25--27, 35--39, 50--57, 61--61, 70--100 ].
iset_difference( [ 3--13, 20--24, 62--69, 200--200 ],
                 [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                 X )
               :: X = [ 6--7, 11--13, 20--22, 63--68, 200--200 ].
iset_difference( [ 1--5, 8--10, 17--19, 23--27, 35--39, 50--57, 61--62, 69--100 ],
                 [ 3--13, 20--24, 62--69, 200--200 ],
                 X ),
               X \= [ 1--2, 17--19, 25--27, 35--39, 50--57, 61--61, 70--100 ] :: f.


iset_complement( 1--2, [], [1--2] ) :: s.
iset_complement( 1--10, [15--20], [1--10] ) :: s.
iset_complement( 1--10, [15--20,22--24], [1--10] ) :: s.
iset_complement( 1--10, [(-10)--0], [1--10] ) :: s.
iset_complement( 1--10, [(-10)--0,11--20], [1--10] ) :: s.
iset_complement( 1--10, [1--10], [] ) :: s.
iset_complement( 1--10, [(-5)--(-1),1--10,12--14], [] ) :: s.
iset_complement( 1--10, [3--10], [1--2] ) :: s.
iset_complement( 1--10, [1--5], [6--10] ) :: s.
iset_complement( 1--10, [3--5], [1--2,6--10] ) :: s.
iset_complement( 1--10, [0--0,3--5,11--12], [1--2,6--10] ) :: s.


canonical_iset( [], [] ) :: s.
canonical_iset( [ 1--2 ], [ 1--2 ] ) :: s.
canonical_iset( [ 1--5, 4--9 ], [ 1--9 ] ) :: s.
canonical_iset( [ 4--9, 1--5 ], [ 1--9 ] ) :: s.
canonical_iset( [ 4--9, 1--5 ], X ) :: X = [ 1--9 ].
canonical_iset( [ 1--3, 4--9 ], X ) :: X = [ 1--9 ].
canonical_iset( [ 1--3, 4--9 ], [ 1--9 ] ) :: s.
canonical_iset( [ 9--20, 1--1, 4--9, 21--21, (-2)--0 ], X ) ::
              X = [ (-2)--1, 4--21 ].
canonical_iset( [ 9--20, 1--1, 4--9, 21--21, (-2)--0 ], X ),
              X \= [ (-2)--1, 4--21 ] :: f.


interval_includes( 1--4, 1--4 ) :: s.
interval_includes( 1--4, 1--1 ) :: s.
interval_includes( 1--4, 3--4 ) :: s.
interval_includes( 1--4, 2--4 ) :: s.
interval_includes( 1--4, 0--4 ) :: f.
interval_includes( 1--4, 1--5 ) :: f.
interval_includes( 1--4, (-1)--6 ) :: f.
interval_includes( 1--4, 5--7 ) :: f.
interval_includes( 1--4, 4--7 ) :: f.
interval_includes( 1--4, (-5)--0 ) :: f.
interval_includes( 1--4, (-5)--1 ) :: f.
interval_includes( 1--4, (-5)--5 ) :: f.


interval_contains( 1--4, (-1) ) :: f.
interval_contains( 1--4, 1 ) :: s.
interval_contains( 1--4, 3 ) :: s.
interval_contains( 1--4, 4 ) :: s.
interval_contains( 1--4, 5 ) :: f.


iset_includes( [1--4], [1--4] ) :: s.
iset_includes( [1--4], [1--1] ) :: s.
iset_includes( [1--4], [3--4] ) :: s.
iset_includes( [1--4], [2--4] ) :: s.
iset_includes( [1--4], [0--4] ) :: f.
iset_includes( [1--4], [1--5] ) :: f.
iset_includes( [1--4], [(-1)--6] ) :: f.
iset_includes( [1--4], [5--7] ) :: f.
iset_includes( [1--4], [4--7] ) :: f.
iset_includes( [1--4], [(-5)--0] ) :: f.
iset_includes( [1--4], [(-5)--1] ) :: f.
iset_includes( [1--4], [(-5)--5] ) :: f.

iset_includes( [], [1--1] ) :: f.
iset_includes( [1--1], [] ) :: s.

iset_includes( [1--4,6--10,12--23,56--60], [2--3,6--10,12--12,14--14] ) :: s.
iset_includes( [1--4,6--10,12--23,56--60], [0--3,6--10,12--12,14--14] ) :: f.
iset_includes( [1--4,6--10,12--23,56--60], [1--3,6--10,12--12,14--14,61] ) :: f.


iset_contains( [1--4], (-1) ) :: f.
iset_contains( [1--4], 1 ) :: s.
iset_contains( [1--4], 3 ) :: s.
iset_contains( [1--4], 4 ) :: s.
iset_contains( [1--4], 5 ) :: f.

iset_contains( [], 1 ) :: f.
iset_contains( [0--1,12--13], 1 ) :: s.
iset_contains( [0--1,12--13], 13 ) :: s.
iset_contains( [0--1,12--13], 14 ) :: f.
iset_contains( [0--1,12--13], (-100) ) :: f.
