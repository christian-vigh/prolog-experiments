interval uses xinterval.

/*
pre_fetch(test(interval)):- fetch(forced('int.ut.te')).
pre_fetch(test(Y)):- subtest(interval,Y), fetch(forced('int.ut.te')).
*/
test(interval) uses forced('int.ut.te').
test(Y) uses forced('int.ut.te') :- subtest(interval,Y).

subtest(interval,intmisc).
subtest(interval,intadd).
subtest(interval,inttimes).
subtest(interval,intsqrt).
subtest(interval,intsolve).

xinterval uses intdef.
xinterval uses mach_dep.

subtest(xinterval,xintadd).
subtest(xinterval,xintround).
subtest(xinterval,xintmult).
subtest(xinterval,xinttimes).
subtest(xinterval,xintmisc).

intdef uses macros.
intdef uses debug.
intdef uses utilities.

test(neg) uses forced('int.ut.te').
test(neg) uses interval.
neg uses simple.
neg uses debug.
neg uses xinterval.
neg uses intdef.

all(interval).
all(xinterval).
all(intdef).
all(utilities).


