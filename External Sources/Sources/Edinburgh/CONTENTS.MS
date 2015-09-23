.\" define the number of this note
.ds No 103
.\"
.\" define the title of this note
.ds Nt \s+2Structure and Contents of the Prolog library\s-2
.\"
.\" define the day and month of issue of this note
.ds N1 12th November
.\" and the year
.ds N2 87
.\"
.\" define a version number if required
.ds Nv v.6
.\"
.\" define this string if in draft form; otherwise define as empty
.ds Nd
.\"
.\" define this string if needed; otherwise define as empty
.ds Nc
.\"
.\"
.ND
.LP
.rm CM
.nr LL 6.25i
.nr LT 6.25i
.ll 6.25i
.lt 6.25i
.ds FM 0.5i
.ds LH AIAI/PSG\\*(No/\\*(N2
.ds CH \*(Nd
.ds RH Page %
.ds LF
.ds CF \*(Nc
.ds RF \*(Nv
.LP
.nh
.B
.tl 'UNIVERSITY OF EDINBURGH'\*(Nd'AIAI/PSG\*(No/\*(N2'
.sp
AI APPLICATIONS INSTITUTE
.sp
.tl 'PROGRAMMING SYSTEMS GROUP''Issued by'
.tl 'Note No. \*(No''Ken Johnson'
.tl '''Robert Rae'
.tl '\*(Nt''\*(N1 19\*(N2'
\l'\n(.lu'
.R
.LP
.sp 0.5
The DEC-10 Prolog Library was an extraordinary and catholic collection of
Prolog routines, largely written by research workers and students in Professor
Alan Bundy's Mathematical Reasoning Group at the Department of Artificial
Intelligence at the University of Edinburgh.
In summer 1987 we sifted through the enormous amount of material in this
library, grouping similar material together and converting some of the more
used programs into Edinburgh Prolog.
.LP
These programs are all examples of Prolog programming to deal with objects and
problems of many kinds.
(Some of these examples are very good examples, others are not so;
some are well commented, some have separate documentation, some have none.)
You may be able to load tools for low-level operations into your code
ready-made, or you may gain insight into how to write good Prolog (as we did)
through just browsing amongst the source code here.
.LP
Files which have their names in lower case have been checked, converted, and
are believed to work in Edinburgh Prolog running under UNIX.
They have had any DEC-10/TOPS-10 specific features eliminated and replaced by
UNIX equivalents.
Some of the DEC-10 file names were changed after the files were converted,
normally expanding the six character abbreviation into something more
meaningful.
Files that have their names in upper case are still in DECsystem-10 Prolog.
.LP
It is difficult to sort things into categories.
We have tried to do it and to avoid a rag-bag "Miscellaneous" category, but
undoubtedly there will be files that you'll think have been put into the wrong
place.
.LP
The top level directories that we have identified are \fIdata\fP, \fIdemo\fP,
\fIio\fP, \fIprolog\fP, \fItools\fP and \fIutil\fP.
Most of these are reasonably self-explanatory:
.RS
.IP data 6
contains files relating to data structure definition and manipulation;
.IP demo
contains files of demonstration and teaching material;
.IP io
contains files relating to input and output of structured and unstructured
data;
.IP prolog
contains files of common extensions to the Prolog language and definitions of
parts of the Prolog system in Prolog;
.IP tools
contains files relating to the development of Prolog programs;
.IP util
all the files in this directory are links to files which already exist in
some other part of the library.
They were identified originally for use with the PRESS system and were grouped
together for convenience.
They have proved useful to other systems since.
.RE
.LP
.sp 0.3
\l'\n(.lu'
.sp 0.5
.B
These files are all supplied "as is", with NO guarantee of any kind.
If things don't work, fix them and mail the fix to us, if you can.
Otherwise complain and we will fix them if we can.
Obviously we cannot undertake to do this within any particular time limit.
.R
.LP
Electronic mail to nip@uk.ac.ed will reach us.
.LP
These files are all in the "public domain" so you can use them freely,
copy them, incorporate them into programs of your own and so forth
without payment.
The work of producing them in the first place and of organising them as
detailed here has been funded over the years at Edinburgh University mainly by
the Science and Engineering Research Council.
Their dissemination has been encouraged by the Alvey Special Interest Group:
Artificial Intelligence.
We would appreciate it if you were to acknowledge these bodies when you use or
re-distribute any of these files.
.br
\l'\n(.lu'
.LP
.bp
The files of the original DEC-10 Prolog Library have been divided up into
categories to give the following directory structure:
.sp
.2C
.IP data/
.RS
.IP data/arrays/
.DS L
arrays.pl
logarr.pl
.DE
.IP data/lists/
.DS L
LISTUT.HLP
PROJEC.pl
SORTS.pl
assoc.lists.pl
lazy.pl
listut.pl
multil.pl
order.pl
samsort.pl
.DE
.IP data/misc/
.DS L
TREES.pl
flags.pl
flat.pl
graphs.pl
heaps.pl
map.pl
queues.pl
.DE
.IP data/numbers/
.DS L
GELRAN.pl
arith.ops
arith.pl
between.pl
long.pl
random.pl
.DE
.IP data/sets/
.DS L
BAGUTL.HLP
bagutl.pl
ordset.pl
setof.pl
setutl.pl
.DE
.IP data/terms/
.DS L
APPLIC.HLP
applic.pl
metutl.pl
struct.pl
.DE
.RE
.sp 10
.IP demo/
.RS
.IP demo/evans/
.DS L
evans.pl
figure.pl
solution.txt
.DE
.IP demo/grammars/
.DS L
CLAUSE.pl
DCSG.HLP
DCSG.ex
DCSG.pl
XGPROC.pl
.DE
.IP demo/mandc/
.DS L
bfs.pl
dfs.pl
heu.pl
.DE
.IP demo/misc/
.DS L
CRYPTA.pl
OXO.pl
QUEENS.pl
logodb.pl
mycin.pl
royalty.pl
tidy.pl
.DE
.IP demo/plan/
.DS L
EXPON.pl
WPLANC.pl
WPO.pl
.DE
.IP demo/rules/
.DS L
RULES.pl
SUBTRA.pl
SUM.SOL.pl
SUM1.pl
SUM2.pl
SUM3.pl
.DE
.IP demo/search/
.DS L
BREADTH_FIRST.pl
DEPTH_FIRST.pl
EIGHT_PUZZLE.pl
GUESS_FIRST.pl
.DE
.RE
.bp
.IP demo/
.RS
.IP demo/winston/
.DS L
ARCH1.PRB
ARCH3.PRB
ARHC2.PRB
CONN
FOCUS
INFER
ISOLAX.PRB
LEARN
MAKERC
PIM.PRB
UTIL
WINST
WINST.REF
WINSTO.HLP
.DE
.RE
.IP io/
.RS
.IP io/files/
.DS L
BACKUP.pl
BUNDLE.pl
LIB.pl
LIB2.pl
PUTSTR.pl
RECON.pl
files.pl
getfile.pl
trysee.pl
type.pl
.DE
.IP io/formatted_write/
.DS L
PORSTR.pl
STRIO.pl
WRITEF.HLP
writef.pl
.DE
.IP io/read_in/
.DS L
ask.pl
read_in.pl
read_sent.pl
.DE
.RE
.sp 20
.IP prolog/
.DS L
ARC3.pl
CC.pl
CTYPES.pl
DEC10.pl
DECONS.pl
DISTFIX.ex
DISTFIX.pl
FEACH.pl
IDBACK.def
IDBACK.pl
IMISCE.pl
INVOCA.pl
MODULE.pl
NOT.HLP
NOT.pl
OCCUR.pl
RDTOK.gen
RDTOK.pl
READ.pl
SYSTEM.pl
TOPLEVEL.pl
UNFOLD.pl
UPDATE.pl
expand.pl
gensym.pl
system_preds.pl
.DE
.bp
.IP tools/
.RS
.IP tools/editing/
.DS L
edit.pl
keep.pl
.DE
.IP tools/inspecting/
.DS L
COUNT.HLP
IXREF.HLP
PP.HLP
PROLOG.TYP
VCHECK.HLP
VCHECK.pl
XREF.DEF
XREF.HLP
XREF.pl
XRF.pl
XRFCOL.pl
XRFDEF.pl
XRFMOD.pl
XRFOUT.pl
XRFTST.BAR
XRFTST.FOO
XRFTTY.pl
count.pl
depth.pl
ixref.def
ixref.pl
pp.pl
termin.pl
typecheck.pl
.DE
.IP tools/on_line_doc/
.DS L
HELP.pl
HELP2.pl
HELPER.HLP
HELPER.pl
help_directories.txt
helper_files
.DE
.IP tools/timing/
.DS L
TIMING.pl
TIMING.POP
.DE
.IP tools/tracing/
.DS L
ADVICE.HLP
ANDOR.pl
TEST.pl
advice.pl
medic.pl
trace.pl
.DE
.RE
.sp 10
.IP util/
.DS L
IMISCE.pl
INVOCA.pl
MAKE_UTIL
OCCUR.pl
PROJEC.pl
applic.pl
arith.ops
arith.pl
ask.pl
bagutl.pl
between.pl
edit.pl
files.pl
flags.pl
gensym.pl
getfile.pl
listut.pl
long.pl
metutl.pl
read_in.pl
setutl.pl
struct.pl
tidy.pl
trace.pl
trysee.pl
type.pl
util.ops
util.sav
writef.pl
.DE
.LP
.1C
This is the contents of the Prolog Library in alphabetical order of file name.
Any files whose names end in .HLP are text files which explain another Prolog
file, and they are not listed below.
All the others are listed with a one-line description.
.sp 0.5
.IP advice.pl 20
Interlisp like advice package.
.IP ANDOR.pl
Meta circular interpreter maintaining extended and-or tree.
.IP applic.pl
Function application routines based on "apply".
.IP ARC3.pl
Mackworth's AC-3 algorithm.
.IP ARCH1.PRB
Winston arch domain. Inference version.
.IP ARCH3.PRB
Winston arch domain.
.IP ARHC2.PRB
Winston arch domain, with inference rules.
.IP arith.ops
Arithmetic operator declarations.
.IP arith.pl
Arithmetic operations.
.IP arrays.pl
Updateable arrays.
.IP ask.pl
Ask questions that have a one-character answer.
.IP assoc.lists.pl
Association lists.
.LP
.IP BACKUP.pl 20
Rename a file according to a back-up convention.
.IP bagutl.pl
Utilities for "bags".
.IP between.pl
Generate successive integers.
.IP bfs.pl
Missionaries and Cannibals: breadth first search.
.IP BREADTH_FIRST.pl
Define a schema for breadth-first search.
.IP BUNDLE.pl
Bundle and unbundle files.
.LP
.IP CC.pl 20
Conditional compilation.
.IP CLAUSE.pl
Convert a formula in FOPC to clausal form.
.IP CONN
Operator definitions for logical connectives.
.IP Contents
This file.
.IP count.pl
Information about a valid Prolog file.
.IP CRYPTA.pl
Solve cryptarithmetic puzzles.
.IP CTYPES.pl
Character classification.
.LP
.IP DCSG.ex 20
Example grammar for DCSG.pl.
.IP DCSG.pl
Definite Clause Slash Grammar.
.IP DEC10.pl
DEC-10 compatibility file for C-Prolog 1.4a.
.IP DECONS.pl
Construct and take apart Prolog control structures.
.IP depth.pl
Find or check the depth of a term.
.IP DEPTH_FIRST.pl
Define a schema for a depth-first search.
.IP dfs.pl
Missionaries and Cannibals depth first search.
.IP DISTFIX.ex
Load DISTFIX.pl and define some examples.
.IP DISTFIX.pl
Read Prolog terms with extended syntax.
.LP
.IP edit.pl 20
Invoke an editor and return to Prolog.
.IP EIGHT_PUZZLE.pl
Illustrate the searching methods.
.IP evans.pl
Evans geometric analogy program.
.IP expand.pl
Simple macro expansion.
.IP EXPON.pl
Synthesis of an exponentiation routine.
.LP
.IP figure.pl 20
Figures for the Evans program.
.IP files.pl
Routines for playing with files.
.IP flags.pl
Global variables.
.IP flat.pl
Flatten trees to lists and back.
.IP FOCUS
Reconstruction of Winston learning program.
.IP FEACH.pl
Redefine foreach/5.
.LP
.IP GELRAN.pl 20
Random number package.
.IP gensym.pl
Create new atoms.
.IP getfile.pl
Prompt for a file name.
.IP graphs.pl
Graph processing utilities.
.IP GUESS_FIRST.pl
Define a schema for a guess-first search.
.LP
.IP heaps.pl 20
Implement "heaps".
.IP HELP.pl
Prints extracts from help files.
.IP HELP2.pl
Extracts predicate names and descriptions from files.
.IP HELPER.pl
Prints extracts from help files.
.IP help_directories.txt
List of directories that contain ".HLP" files.
.IP heu.pl
Missionaries and Cannibals: Heuristic search version.
.LP
.IP IDBACK.def 20
Unit interface clauses for IDBACK.pl.
.IP IDBACK.pl
Intelligent backtracking.
.IP IMISCE.pl
Miscellaneous interpreted routines.
.IP INFER
Inference package for focus program.
.IP INVOCA.pl
Fancy control structures.
.IP ISOLAX.PRB
Description space for learning isolate rule.
.IP ixref.def
Definitions for ixref.pl.
.IP ixref.pl
Interactive cross referencer.
.LP
.IP keep.pl 20
Keep predicate(s) in a file.
.LP
.IP lazy.pl 20
Lazy lists.
.IP LEARN
Part of Winston's program.
.IP LIB.pl
Version of Vax "lib" predicate.
.IP LIB2.pl
Version of Vax "lib" predicate.
.IP listut.pl
List handling utilities.
.IP logarr.pl
Arrays with logarithmic access time.
.IP logodb.pl
Logo-like inference package.
.IP long.pl
Rational arithmetic.
.LP
.IP MAKERC 20
Make records from lists of relations.
.IP MAKE_UTIL
Create the utilities baseload from this library.
.IP map.pl
Implement finite maps.
.IP medic.pl
Mode error diagnosis in interpreted code.
.IP metutl.pl
Meta logical operations.
.IP MODULE.pl
Elementary module system for DEC-10 Prolog.
.IP multil.pl
List-of-lists utilities.
.IP mycin.pl
Version of the "mycin" program.
.LP
.IP NOT.pl 20
Suspicious negation.
.LP
.IP OCCUR.pl 20
Routines for checking number and place of occurrence.
.IP order.pl
Define the "ordered" predicates.
.IP ordset.pl
Ordered set manipulation.
.IP OXO.pl
Noughts and crosses production system.
.LP
.IP PIM.PRB 20
Artificial inference testing example for focussing.
.IP PORSTR.pl
Portray lists of characters as strings.
.IP pp.pl
Prolog pretty printer.
.IP PROJEC.pl
Select k'th argument of each element of a list.
.IP PROLOG.TYP
Definition of Prolog types for typecheck.pl.
.IP PUTSTR.pl
Write out large blocks of text.
.LP
.IP QUEENS.pl 20
Solve the N queens problem.
.IP queues.pl
Queue operations.
.LP
.IP random.pl 20
Random number generator.
.IP RDTOK.gen
Tokeniser in reasonably standard Prolog.
.IP RDTOK.pl
Reads tokens up to next ".".
.IP READ.pl
Read Prolog terms in DEC-10 syntax.
.IP read_in.pl
Read in a sentence as a list of words.
.IP read_sent.pl
A flexible input facility.
.IP RECON.pl
Version of consult and reconsult.
.IP royalty.pl
Royal family data base.
.IP RULES.pl
Production rules system.
.LP
.IP samsort.pl 20
A sorting routine that exploits existing order.
.IP setof.pl
Implementations of setof, bagof and findall.
.IP setutl.pl
Set manipulation utilities.
.IP solution.txt
Solution printed by Evans' program.
.IP SORTS.pl
Definition of keysort and sort.
.IP STRIO.pl
Prolog input and output to character strings.
.IP struct.pl
General term hacking.
.IP SUBTRA.pl
Production rules for subtraction by borrowing.
.IP SUM.SOL.pl
Cryptarithmetic solution.
.IP SUM1.pl
Example sum for use with RULES and SUBTRA.
.IP SUM2.pl
Example sum for use with RULES and SUBTRA.
.IP SUM3.pl
Example sum for use with RULES and SUBTRA.
.IP SYSTEM.pl
Table of built-in predicates.
.IP system_preds.pl
Table of built-in predicates.
.LP
.IP termin.pl 20
Test for missing base cases.
.IP TEST.pl
Test compiled routines by interpreting them.
.IP tidy.pl
Algebraic expression simplifier.
.IP TIMING.pl
Time execution of predicate.
.IP TIMING.POP
Time execution of predicate (POP2 component).
.IP TOPLEVEL.pl
Prolog top level.
.IP trace.pl
Produce tracing messages.
.IP TREES.pl
Updateable binary trees.
.IP trysee.pl
Search directories and extensions to find a file.
.IP type.pl
Command to display files.
.IP typecheck.pl
Prolog type checker.
.LP
.IP UNFOLD.pl 20
Unit resolution.
.IP UPDATE.pl
For updating data base relations.
.IP UTIL
Utilities for focussing program.
.IP util.ops
Operator declarations for utilities package.
.IP util.sav
Saved state for Edinburgh Prolog ver 1.5.01 (14 Aug 1987).
.LP
.IP VCHECK.pl 20
Check for mis-spelt variables.
.LP
.IP WINST 20
Consult all focussing files.
.IP WINST.MIC
.IP WINST.REF
Focussing cross reference.
.IP WINST2.CMD
.IP WPLANC.pl
Conditional plan generator.
.IP WPO.pl
Operator declarations for WPLANC.pl.
.IP writef.pl
Formatted write.
.LP
.IP XGPROC.pl 20
Translate XGs to Prolog.
.IP XREF.DEF
Cross referencer definitions.
.IP XREF.pl
Cross referencer.
.IP XRF.pl
Cross referencer program.
.IP XRFCOL.pl
Collecting-up module of the cross referencer.
.IP XRFDEF.pl
Handles .def files for the cross referencer.
.IP XRFMOD.pl
Update declarations in Prolog source file.
.IP XRFOUT.pl
Output module for the cross referencer.
.IP XRFTST.BAR
Cross referencer test file.
.IP XRFTST.FOO
Cross referencer test file.
.IP XRFTTY.pl
Terminal interaction for cross referencer.
.LP
