@device(sanders)
@make(report)
@style(fontfamily Helvesan10)
@libraryfile(Myscrb)
@use(bibliography="bib:biblio.bib")
@modify(equation, group, spacing 1.5)
@modify(heading, capitalized off)
@modify(Section, TitleEnv HD2)
@textform(o1=
"@hsp(0)@begin(transparent, script +1line)@ovp(_)@end(transparent)@parm(text)",
approxeq="@rsc11047(K)"
)

@heading[A polymorphic type system for Prolog

@flushleft[@tabdivide(2)
@=Alan Mycroft@=Richard A. O'Keefe
@=Dept of Computer Science@=Dept of Artificial Intelligence
@=Edinburgh University@=Edinburgh University
]]

@subheading(Abstract)
We describe a polymorphic type scheme for Prolog which
makes static type checking possible.
Polymorphism gives a good degree of flexibility to the type system, and
makes it intrude very little on a user's programming style.
The only additions to the language are type declarations, which an
interpreter can ignore if it so desires, with the guarantee that a
well-typed program will behave identically with or without type
checking.
Our implementation is discussed and we observe that the type resolution
problem for a Prolog program is another Prolog (meta-) program.

@section(Introduction)
Prolog currently lacks any form of type checking, being designed as a
language with a single type (the term).  While this is useful for
learning it initially and for fast construction of sketch programs, it
has several deficiencies for its use as a serious tool for building
large systems.  These centre around the facts that type errors can only be
detected at run-time and that modules cannot have secure representations.

We have observed that a theorem prover which reasons about Prolog
programs can be more powerful if it has type information available.
One indication as to why this is so can be seen from the fact that the
traditional definition of @i(append) has append(nil,3,3) deducible from
its definition.

Prolog programs can contain many kinds of errors.  Tools exist to
detect several error classes.  Two of the commoner error classes
for which no tool previously existed are transposed arguments and
omitted cases.
Type-checking often catches the first error and provides a method 
for determining whether all the cases in a Prolog predicate have
been considered.  For example, a predicate defined by
@equation[@i(type) neglist(list(int),list(int))
neglist(cons(A,L),cons(B,M))  @backarrow()  negate(A,B), neglist(L,M)]
will never succeed, since we have probably omitted the clause
@equation{neglist(nil,nil) @backarrow()}
A type system would enable us to detect this by checking for
exhaustive specification of argument patterns for a given data-type.
Of course, if we really did want a certain case to fail,
then adding a clause such as
@equation{neglist(nil,nil)  @backarrow()  fail}
would be an explicit way of requesting such an event without leaving
first-order logic
(and would facilitate later reading of the program).

Moreover, our type system can be used as the basis of an
encapsulation providing an abstract data type facility.
The ability to hide the internal details of a given object greatly
aids the reliability of a large system built from a library of modules.

Finally, we note that static type checking cannot of itself
provide a great increase in speed of Prolog programs, due to the fact
that term unification must still be performed, as in the dynamic case.
However, typed Prolog can improve the speed of compiled clauses of a
given predicate by using a mapping of data constructors onto small
adjacent integers to enable faster selection of the clause(s) to be
invoked.
By far the greatest gain is that of programmer time provided by early
detection of errors.

As far as we know this work is the first application of a polymorphic
type scheme to Prolog, but related work includes Milner's
work@cite(Milner78) on typing a simple applicative language which
is used in the ML@cite(ML) type checker and the HOPE language which uses
a version of Milner's algorithm extended to permit overloading.
However, this work differs from these in several respects.
Firstly, the formulation of Prolog as clauses means that the problems
of generic and non-generic variables are much reduced.
All predicate and functor definitions naturally receive generic
polymorphic types which can be used at different type instances within
the program whilst all variables receive non-generic types.
Moreover, our formulation for Prolog removes a restriction in Milner's
scheme in which all mutually recursive definitions can only be used
non-generically within their bodies.
Thus in ML the (rather contrived) program
@equation[@i(let @!rec) I x = x
@/@i(and) f x = I(x+1)
@/@i(and) g x = if I(x) then 1 else 2]
would be ill-typed.
Since all Prolog clauses are defined mutually recursively, this
restriction would have the effect of making the polymorphism useless.

@section(Mathematics)
We assume the notion of substitution, a map from variables (and terms by
extension) to terms, ranged over by @g(q) and @g(f).
An invertible substitution is called a renaming.
If a term, u, is obtained from another, v, by substitution then we say
that u is an instance of v, and write u@le()v.
We write u@approxeq()v if u@le()v and v@le()u.  This means that u and v
only differ in the names of their variables and that the substitutions
involved are renamings.
Also assumed is the notion of most-general unifier (MGU) of two terms.

For any class of objects S, the notation S@+(*) will be used to indicate the
class of objects consisting of finite sequences of elements of S.

@section(Prolog)
The simple variant of Prolog we consider will be defined by the following
syntax (we assume the existence of disjoint sets of symbols called Var, Pred
and Functor, representing variables, predicates and functors symbols
respectively):
@equation[@tabclear()
Term  ::=  Var | Functor(Term@+(*))
Atom  ::=  Pred(Term@+(*))
Clause  ::=  Atom @backarrow() Atom@+(*)
Sentence  ::=  Clause@+(*)
Program  ::=  Sentence; Atom
Resolvent  ::=  Atom@+(*)]
By definition of clause form each, implicitly universally quantified,
variable appears in at most one clause.
To make the formal description of typing simpler, we assume that the
textual names of variables also follow this rule.
A program then is given by a finite list of (Horn) clause declarations,
followed by an Atom (short for atomic formula), called the query,
to evaluate in their context.
It specifies an initial resolvent by taking the query
and treating it as a one-element list.

The evaluation mechanism for Prolog is very simple, and based on the notion of
SLD-resolution as the computation step:

SLD-resolution is the one-step evaluation which transforms a Resolvent.
Given a resolvent
@equation[R  =  A@-(1),...,A@-(n)]
we select an Atom, the @i(selected atom), say A@-(k), (this is often
A@-(1) in real Prolog interpreters) and perform resolution with it and a
matching clause.
So, choose a clause of the program, the @i(selected clause), say Q, given by
@equation[C  @backarrow()  B@-(1),...,B@-(m)]
and suppose that R has no variables in common with it
(otherwise we must rename its (Q's) free variables since they are
implicitly universally quantified for the clause).

Now let @g(q) be MGU(A@-(k),C) if this exists.
If it does, then we can rewrite R into R' given by
@equation[@g(q)(A@-(1),...,A@-(k-1),B@-(1),...B@-(m),A@-(k+1),...,A@-(n)).]
The most common form of Prolog interpreter uses k=1 when this expression
simplifies somewhat.

An answer is produced when the resolvent is rewritten into a sequence
of zero atoms.  The associated answer to such a rewriting sequence is
the composition of most-general unifiers encountered during the
rewriting process, or rather its restriction to the variables in the query.

Observe that the above specification only told us how we could produce
an answer (if one exists) from a Prolog program.
For computation the choices above (the selected atom and clause) must be
incorporated into a deterministic tree searching algorithm, which we
take time to explain below for the reader's benefit.
However, we would like to stress now that the results on type-checking
given in section @ref(Welltypedness) work for @i(any) order of
evaluation (choices of atoms and clauses) of Prolog programs
(depth-first/@|breadth-first/@|coroutining/@|parallel).

@subsection(Digression: SLD-trees)
@label(SLDtrees)
The idea of SLD-resolution above, leads to the idea of an SLD-tree:
whenever we are forced to select a @i(clause) then, instead of
irreversibly choosing a given matching clause, we construct a tree
of resolvents (an SLD-tree) where a resolvent has a son resolvent for
each clause which matches with the selected atom.
A sensible computation (the standard implementation of Prolog) is then
to search this tree in depth-first left-right manner.

Some branches die out, in that no clause matches the selected atom,
whereas others have more than one subtree contribute to the answer.
This is often referred to as the non-determinacy of Prolog.

Finally, we remark that there is never any need to seek alternatives to
the selected @i(atom) - in fact doing so would merely lead to duplication of
answers exhibited elsewhere in the SLD-tree.
(For more details on this aspect see@cite(AptEmden82)).

@section(Types)

@label(Types)
The scheme of types (Type) we allow are given by the following grammar and
are essentially the same as those which occur in ML@cite(ML).
We assume disjoint sets of type constructors (Tcons, ranged over by
roman words) and type variables (Tvar, ranged over by greek letters like
@g(a),@g(b),@g(g)).
These are also assumed to be disjoint from Var, Pred and Functor.
@equation[Type  ::=  Tvar | Tcons(Type@+(*))]
Type will be ranged over by @g(rst)...@ .
A type is called a monotype if it has no type variables.
Otherwise it is a polytype.

For examples, we suppose that Tcons includes the nullary constructor
@i(int) and the unary @i(list).
Example types are then
@equation[list(@g(a)), int, list(list(int)), etc.]
Note that the third type is an instance of the first.

@Subsection[Digression: the Unary Predicate Calculus]

The type systems used @i<in> many AI programs are variants or restrictions of
the Unary Predicate Calculus.  However, UPC is not adequate as the single
type system @i<for> an AI programming language.  Rules such as
@Begin[equation]
(@forall() N,L) integer(N) & int_list(L) @k<doublearrow> int_list(cons(N,L))
(@forall() L) int_list(L) @k<doublearrow> (L=nil @or() integer(car(L)))
@End[equation]
cannot be expressed in it.

@section(Well-typing of Prolog)
@label(WellTypedness)

This section contains the central definition of a Prolog program being
well-typed, together with precursor and auxiliary definitions.
Many of the ideas appear in@cite(Milner78) where a polymorphic
applicative language is typed, but our formulation for Prolog
poses new problems and simplifies old ones as we discussed in the
introduction.

Let Q the clause @w[C@backarrow()B@-(1),...B@-(m)] and
P be a finite subset of Var@union()Pred@union()Functor containing all the
symbols of Q.@:
We define a typing @o1(P) of P to be an association of an extended
type to each symbol occurring in Q.@:
The types are members of a given algebra as defined in section @ref(types).
Predicates and Functors are associated with extended types as given below.
Types and extended types will be written as a
superscript on the object they are associated with.
@g(s)@-(i) and @g(t) will represent (non-extended) types.
For each variable X occurring in Q, @o1(P) will contain an element of the form
X@+(@g(t)).
For each predicate a of arity k in Q, @o1(P) will contain an element
of the form a@+(@+(@g(s)@-(1),...,@g(s)@-(k))).
For each functor f of arity k in Q, @o1(P) will contain an element
of the form f@+(@+[(@g(s)@-(1),...,@g(s)@-(k))@arrow()@g(t)]).

Similarly, the clause Q will be written as a typed clause @o1(Q) by
the writing of a type on each term (this includes variables).

As an example of a clause and its typing consider the clause Q, given by
@equation[app(cons(A,L), M, cons(A,N))  @backarrow()  app(L,M,N)]
The set P = {A,L,M,N,app,cons} gives its set of symbols, and a typing
(which will turn out to be a well-typing considered later)
can be given by @o1(P):
@equation[{A@+(@g(a)), L@+(@g(t)), M@+(@g(t)), N@+(@g(t)), @~
	app@+(@g(t),@g(t),@g(t)), @~
	cons@+[(@g(a),@g(t))@arrow()@g(t)]}]
where @g(t) is used for a shorthand for list(@g(a)) and
the associated clause typing @o1(Q) given by:
@equation[app@!(@~
	cons(A@+(@g(a)),L@+(@g(t)))@+(@g(t)), @~
	M@+(@g(t)), @~
	cons(A@+(@g(a)),N@+(@g(t)))@+(@g(t)))  @~
		@backarrow()  app(L@+(@g(t)),M@+(@g(t)),N@+(@g(t)))]
@o1(P) will be called the typed premise of @o1(Q) due to the relation to
theorem proving.

Fortunately, it will turn out that most of the mess of types written
above are inter-dependent and the above expression can be well-typed
much more succinctly - see later.

We will now define @o1(Q) to be a well-typing of Q under @o1(P), written
@w[@o1(P) @turnstile() @o1(Q)] if the following conditions hold:
@begin(enumerate,group,blanklines hinge)
@o1(P) @turnstile() (A @backarrow() B@-(1),...,B@-(m)) if
@begin(equation, above 0)
A = a(t@ovp(@-(1))@+(@+(@g(t)@-(1))),...,@~
      t@ovp(@-(k))@+(@+(@g(t)@-(k)))) and a@+(@+(@g(r)))@in()@o1(P)
with (@g(t)@-(1),...,@g(t)@-(k))@approxeq()@g(r)
and @o1(P) @turnstile() t@ovp(@-(i))@+(@+(@g(t)@-(i))) (1@le()i@le()k)
and @o1(P) @turnstile() B@-(i) (1@le()i@le()m).
@end(equation)

@o1(P) @turnstile() A if A is an Atom and
@begin(equation, above 0)
A = a(t@ovp(@-(1))@+(@+(@g(t)@-(1))),...,@~
      t@ovp(@-(k))@+(@+(@g(t)@-(k)))) and a@+(@+(@g(r)))@in()@o1(P)
with (@g(t)@-(1),...,@g(t)@-(k))@le()@g(r)
and @o1(P) @turnstile() t@ovp(@-(i))@+(@+(@g(t)@-(i))) (1@le()i@le()k).
@end(equation)

@o1(P) @turnstile() u@+(@g(s)) if u is a Term and
@begin(equation, above 0)
u = f(t@ovp(@-(1))@+(@+(@g(t)@-(1))),...,@~
      t@ovp(@-(k))@+(@+(@g(t)@-(k)))) and f@+(@+(@g(r)))@in()@o1(P)
with ((@g(t)@-(1),...,@g(t)@-(k))@arrow()@g(s))@le()@g(r)
and @o1(P) @turnstile() t@ovp(@-(i))@+(@+(@g(t)@-(i))) (1@le()i@le()k).
@end(equation)

@o1(P) @turnstile() X@+(@g(s))  if  X@+(@g(s)) @in() @o1(P).
@end(enumerate)

Now, we will define a program to be well-typed under a typed premise @o1(P)
if each of its clauses is well-typed under @o1(P) and if its query atom is.
Similarly a resolvent is well-typed if each of its atoms are.

Well-typing as a mathematical concept is of little use, unless we relate
it to computation.  This we will now do, under the motto
"Well-typed programs do not go wrong".


@section(Well-typed programs do not go wrong)

What we desire to show, is the semantic soundness condition
that if a program can be well-typed, then one step
of SLD-resolution will take a well-typed resolvent into a new well-typed
resolvent.
Thus any SLD-evaluation of a well-typed program will remain well-typed.
It is trivially the case that the initial resolvent is well-typed
if the program is.
Moreover, we should show that the variables in the query can only be
instantiated to terms specified by their types given by the well-typing.

The first condition is simply proved:
Let R be the resolvent @w[A@-(1),...,A@-(n)]
and let Q be a clause @w[C @backarrow() B@-(1),...,B@-(m)]
which has no variables in common with R
(the case where Q and R have variables in common will be discussed later).
Without loss of generality (symmetry) let A@-(1) be the selected atom
and suppose @g(q)=MGU(A@-(1),C) exists.
The resolvent produced by one-step evaluation is R' given by
@equation[@g(q)(B@-(1),...,B@-(m),A@-(2),...,A@-(n)).]
We will now show how to well-type this from the well-typing of R.

Let us suppose that there is a P with typing @o1(P) and associated
well-typings @o1(R) and @o1(Q) such that
@w[@o1(P) @turnstile() @o1(R)] and @w[@o1(P) @turnstile() @o1(Q)]
(note this provides well-typings @o1(A)@-(i), @o1(C), @o1(B)@-(i)).
Moreover, let us suppose that @o1(R) and @o1(Q) have no type variables in
common (again, we will discuss this later, but note that the typing
rules never rely on the 'absolute' names of the type variables).

Let the type of the predicate symbol of C in @o1(P) be
@w[c@+(@+(@g(r)@-(1),...,@g(r)@-(k)))].
Now the well-typing determines that @o1(C) can be written
@w[c(s@ovp(@-(1))@+(@+(@g(s)@-(1))),...,s@ovp(@-(k))@+(@+(@g(s)@-(k))))]@;
and @o1(A)@-(1) as
@w[c(t@ovp(@-(1))@+(@+(@g(t)@-(1))),...,t@ovp(@-(k))@+(@+(@g(t)@-(k))))]@;
where
@equation[(@g(s)@-(1),...,@g(s)@-(k)) @approxeq() (@g(r)@-(1),...,@g(r)@-(k))
(@g(t)@-(1),...,@g(t)@-(k)) @le() (@g(r)@-(1),...,@g(r)@-(k)).]
This means that there is a substitution @g(f) on type variables
(actually @g(f)@approxeq()@~
MGU((@g(s)@-(1),...,@g(s)@-(k)),@|(@g(t)@-(1),...,@g(t)@-(k))))
such that (@g(t)@-(1),...,@g(t)@-(k)) = @g(f)((@g(s)@-(1),...,@g(s)@-(k))).

The claim is that
@equation[@o1(P) @turnstile() @~
	@g(q)(@g(f)(@o1(B)@-(1)),...,@g(f)(@o1(B)@-(m)),@~
	@o1(A)@-(2),...,@o1(A)@-(n))]
gives a well-typing of R', where applying @g(f) (a type substitution)
to a typed atom means that it is to be applied to the type variables in types
associated with terms occurring within that atom.

We now address the problem of there being variables, or type variables, in
common between R and Q.@:
These are really the same problem (the perennial one of renaming in Prolog).
A simple solution is the following:
Whenever we come to perform resolution between a clause Q and a resolvent R
we rename Q such that all its variables (using a renaming @g(y)) and all
its type variables (using a renaming @g(h)) are distinct from the
variables (and type variables) in R and the other clauses.
This can always be done since R can only contain a finite number of
different variables.
Moreover this does not change the meaning of Q.@:
This strictly breaks the type scheme, since the new variables appearing
in Q do not appear in @o1(P).
However, a simple addition to @o1(P) of @g(y)(X)@+[@g(h)(@g(t))] for each
variable X in the original Q which appeared as X@+(@g(t)) in @o1(P)
serves to correct this and preserve the typing.
We are now back in the case where Q and R have no variables or type
variables in common.

We now return to the problem of showing that a well-typed program can
only instantiate the variables of its query to values having types
as dictated by the typed premise.
To see that this is the case, it is merely necessary to observe that
each resolution step (as above) is performed between an Atom, A, and a
(type) instance of a clause @w[C @backarrow() B@-(1),...,B@-(m)].
such that the types of A and this instance of C are identical except for
the names of type variables.
Thus variables in A can only be instantiated to Terms (possibly other
variables) having identical types.
The whole result is proved by induction on the length of computation
leading to a refutation.

@section(Specification of the type information to Prolog)

We suggest that the type specification be performed by annotations to
the Prolog system.
The well-typing required three sets of information to be supplied:
@itemize[the types of the predicates

the types of the functors

the types of the variables.]
We suggest that declarations be supplied which
give the type of the first two but the type
of variables can easily be determined from them.
This can be seen by observing that a well-typed Atom or Term labels
the type of each argument Term, and so each variable is labelled
with a type.  The most-general unifier of all the types associated with
a single variable (if it exists) gives a type for that variable.
(This is also convenient since the scope of variables in Prolog is
a single clause, whereas the other objects have a global scope.)

It is convenient to specify the names of types along with the functors
which create them from other types.  This has been demonstrated by
HOPE@cite(HOPE) and we do not expect to better this idea.

So one of the declarations, or meta-commands is one of the form
@equation[Declaration ::= @~
	'@i(type)' Tcons(Tvar@+(*)) '=>' Functor(Type@+(*))@+(*).]
Examples would be (the second somewhat improper)
@equation{@i(type) list(@g(a))  =>  nil, cons(@g(a),list(@g(a)))
@i(type) int  =>  0, 1, -1, 2, -2, 3, -3, ...}

The second declaration specifies the type of predicates.  Suggested
syntax is
@equation{Declaration ::= '@i(pred)' Pred(Type@+(*)).}
and an example for the 'equal' function defined by
@equation[equal(X,X) @backarrow()]
would be
@equation[@i(pred) equal(@g(a),@g(a)).]

We note here, that, given the types of the functors, then would seem possible
to determine the types of the predicates involved without any great
amount of work (as in ML@cite(Milner78)).
However, this seems to depend on an analysis of the whole
program at once, rather than any form of interaction.@foot{Moreover
there is a small technical problem concerning recursive definitions
which makes checking of type specifications of such definitions much
easier than their derivation.}
We would also claim that
the documentation provided by the written form of the types
facilitates human understanding of programs in much the same way that
explicit specification of mode information (input/output use of
parameters) for predicates does.


@subsection(Abstract data types)
We observe that the above declarations furnish a form of abstract
data typing.
Providing a 'module' construct and exporting from it
a given type name, and predicates which operate on that type, but
@i(not) the constructor functors for that type,
enables us to use a type, but not to determine anything about its
representation.
HOPE has such a construct, and we think it would greatly benefit Prolog.

@section(Overloading)

The above discussion has centred on a formalism for well-typing Prolog.
However, it does not allow for one feature which we have found to be useful,
and which is very easy to build into the type system.
This feature is overloading and appears in a similar form in HOPE@cite(HOPE).

The observation, is, that quite often, we may wish a given function,
predicate or functor name to stand for more than one distinct operation.
This is common in mathematics and computer science, where an operator
(eg '+') may be used to denote a different function at different types.
In Prolog this can be useful too.
For example, we may wish to have types specified by
@equation{list(@g(a))  =>  nil, cons(@g(a),list(@g(a)))
tree(@g(a))  =>  nil, leaf(@g(a)), cons(tree(@g(a)),tree(@g(a)))}
where the constructors nil and cons(_,_) have different meanings
according to whether they act on lists or trees.
(Of course we @i(could) give them different names, but this is not
always helpful to the programmer.)

Similarly, we may want certain predicate symbols to refer to different
predicates according to the type of their arguments.
A typical example would be some sort of 'size' predicate.

We formalise this by permitting the typed premises used above to
contain more than one type associated with any given functor or predicate
symbol.

@section(Implementation)

We have built such a system in Prolog which implements the overloaded
type checker by backtracking.
Note that this is not particularly difficult since
our well-typing rules given in section @ref(welltypedness) are
essentially Horn clauses.
There are merely two points to observe.  Firstly, the 'occur-check' of
unification (which is often omitted by Prolog implementations) is
essential for this typechecking scheme.  Secondly, the use of @le()
can be simulated by instantiation of a copy of the functor or predicate
type and the use of @approxeq() by a common meta-linguistic predicate
(@i(numbervars)) which instantiates variables in a term to ground terms
to avoid their further instantiation.  Copies of the code can be
obtained from the authors or could be included as an appendix.

That the well-typing rules (which define when a given program has a given type)
can be used to determine the type of a given program is a simple consequence of
the Horn clause input/output duality.
Moreover, when the well-typing rules are used in the fashion on a
given program, T say, then the standard SLD-resolution will produce a
@i(terminating) evaluation giving the most-general types associated with T.@:
The basic idea is that if the well-typing
problem has no solution, the the program is ill-typed.
If it has exactly one the the program is well-typed, and if it has more
than one then some overloaded operator is ambiguous.

@section(Higher order objects)
This section is much more tentative and more in the manner of
suggestion than the rest of the paper and we would be grateful for any
comments on its inclusion or its contents
It is included because we want to
discuss the well-typing of objects which do not form part
of first-order Prolog, in particular the @i(call) and @i(univ)
operators.

The definition of @i(call) is based on the fact that most Prolog
implementations use the same set of symbols for predicates and functors
(this causes no syntactic ambiguity) and thus a Term has a naturally
corresponding Atom.  Hence @i(call) is defined to be that predicate such
that call(X) is equivalent to Y where Y is the Atom corresponding to the
Term X.@:
Thus @i(call) provides a method of evaluating a Term which has been
constructed in a program and is accordingly related to @i(EVAL) in LISP.
We would like to argue that such a predicate is more powerful than is
required and indeed encourages both bad programming style and
inefficient code.
It is certainly the case that most uses of @i(call) are used in the
restricted case of applying a certain functor passed as a parameter to
arguments determined locally (as in mapping predicates).
Functions or predicates like EVAL or @i(call) do not appear to 
have sensible types
and are thus generally omitted from strongly typed languages in favour
of some form of APPLY construct.

We would like then to change our definition of Prolog and its typing
to introduce this construct.
To do this we introduce a family of abstract data types, called
@equation[pred(@g(a)), pred(@g(a),@g(b)), pred(@g(a),@g(b),@g(c)), ...]
and a family of predicates with types given by
@equation[@i(pred) apply(pred(@g(a)),@g(a)), @~
	apply(pred(@g(a),@g(b)),@g(a),@g(b)), ...]
The only way to introduce object of type pred is by a special piece of
syntax given by
@equation[Term ::= `Pred]
which has the effect of associating the @i(definition) of the given
predicate with Term, which then receives the type
pred(@g(a)@-(1),...,@g(a)@-(n)) if the predicate has type
(@g(a)@-(1),...,@g(a)@-(n)).
(It may be desirable to use such syntax as `foo/3 or `foo(_,_,_)
if several predicates of different arities have the name foo.)
Such values can only be used in @i(apply) and have the effect
of using the associated predicate value together with Terms as arguments
to produce an Atom to evaluate.
It can be shown that such a scheme is type secure.
For example, the map predicate can be defined and used by:
@equation[map(F,cons(A,L),cons(B,M)) @backarrow() apply(F,A,B), map(L,M)
map(F,nil,nil) @backarrow()
neglist(X,Y) @backarrow() map(`negate,X,Y)]
assuming that negate is defined as a diadic predicate.
The type of map so defined would be
@w[(pred(@g(a),@g(b)),list(@g(a)),list(@g(b)))].

The other higher-order object frequently used is the @i(univ) predicate (often
written '=..') which can be used to transform a Term into a list of
Terms derived from the former's top-level substructure.
(This is typically used for analysing terms read with input functions.)
Thus
@equation{univ(f(g(X,a),Y), [f,g(X,a),Y])}
is true.
As it stands this clearly breaks the type-scheme we are proposing
since the elements of the list represented by the second parameter need not
be of the same type.
We observe again, that such a predicate is not commonly used in its full
generality, but rather to allow arbitrary terms to be input.
As such, we suspect that introducing a new type 'input_term'
which specifies the type of
objects generated by input routines and giving @i(univ) the type
(input_term,list(input_term)),
together with a notation for treating a Term as an input_term would give
much of the power of @i(univ) within a strong typing discipline.

The difficulty of typing @i<univ> arise from the conflation of object and
meta levels in one language, which requires the same object to
@i<simulatenously> possess at least two types, in a stronger sense than
overloading.  A satisfactory resolution of this problem waits on the
introduction of an explicit meta-level or the construction of a
genuinely reflective Prolog@Cite(3LISP). 

@section(Conclusions and further work)
We have shown how to well-type that subset of Prolog described by
first-order logic and indicated how this might be extended to allow
higher order objects.
It is an interesting result that the well-typing problem for a Prolog
program can itself be regarded as a Prolog meta-program. 

@section(Acknowledgments)
This work was supported by the British
Science and Engineering Research Council.

@section(References)
@bibliography

