Exlibris v0.2

What is it ?
------------
ExLibris is  a simple Prolog source  code configuration tool
that  operates  on  libraries (the  Prolog kind)  and can be
used to extract from local libraries all  code relevant to a
particular  project. It is not designed to  address problems
arising in code production lines, but rather, to support the
needs of individual or  small teams of researchers  who wish
to communicate their Prolog programs. In the process we also
wish  to accommodate and  encourage the writing  of reusable
code.There is a paper from ICLP affiliated Workshop on Logic
Programming  Environment (WPLE 2002) describing parts of the
software. This can be found in doc/WPLE02-ExLibris-0_2.ps.gz

Installation
------------
ExLibris is distributed in Prolog source code so there is no
much  to  do. Simply  copy  somewhere and start  using.  One
thing  you  might want  is to run  mkindex/1 for each system
library you want ExLibris to support. If you want to use the
conditional  execution and the compatibility predicates then
you need to copy lib/compat/if_pl.pl  to your `home library'
directory (see documentation on what a home library is) from
where your  programs can load in the usual ways.  This  file
has  been  tested   with,  sicstus(3:9:0),  swi(5:0:5),  and
yap(4:3:21).  Similarly  ExLibris  can deal  with may_load/1
directives which have no effect on normal  execution. To use
this, also copy file  lib/ensure/may_load.pl  to  your  home
library.

Supported Systems
-----------------
Currently  ExLibris  runs  on  SICStus 3.9.0,  Swi 5.0.7 and
Yap 4.3.23,  all under Unix. Note  that it  does not  run on
earlier versions of SICStus or Yap because it relies heavily
on absolute_file_name/3.We hope to support more systems such
as  Ciao and  GNU-Prolog.  Depending  on  the various Prolog
systems' support  for  OS  independent file referring, maybe
MS Windows.

Testing
-------

% {sicstus,swi,yap}

...
?- [exlibris].
...
?- exlibris([source('eg/simple/one_file'),dest('exlibris_example'),copy(selective),homelibs('eg/mock_home')]).
...
?- halt.

% cd exlibris_example


% {sicstus,swi,yap}
?- one_file( [a,b,c] ).
flat(abcabc)
% ...
...
% ...
basedir(xyz)
?


Compare with:

?- exlibris([source('eg/simple/one_file'),dest('exlibris_example'),copy(selective),homelibs('eg/mock_home'),pls(sicstus(_))]).

and 

?- exlibris([source('eg/simple/one_file'),dest('exlibris_example_sicstus'),copy(selective),homelibs('eg/mock_home'),pls(sicstus(_))]).

or (equivalently to the last query) 

?- exlibris([source('eg/simple/one_file'),dest('exlibris_example_sicstus'),copy(selective),homelibs('eg/mock_home'),pls(yap(_))]).

Contact
-------
Although I will not reply to emails concerning installation
and requiring help, I will consider adding interesting features,
and provide fixes for obvious bugs (in the unlikely event
that any exist, of course).

Nicos Angelopoulos, June 2002.
nicos@doc.ic.ac.uk
