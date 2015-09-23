/****h* Modules/Stack
 ===============================================================================
 *
 * NAME
 *	Stack - Stack-handling predicates.
 *
 * FILE
 *	Modules/Stack.pro
 *
 * CONTENTS
 *	The Stack package implements stack manipulation predicates, such as 
 *	push(), pop() and so on. It also implements some of the Forth language
 *	stack-manipulation words.
 *
 *	The implementation choice for a stack is storing it as a rule, such as :
 *		
 *		stack(Name, List)
 *
 *	where [Name] is the stack name, and [List] its contents. Every update to
 *	a stack using one of the stack predicates supersedes its previous contents
 *	(rule definition). This implementation choice avoids using too many
 *	temporary variables ; for example, instead of writing :
 *
 *		push(X, value, NewX).
 *
 *	One can write :
 *
 *		push(X, value).
 *
 *	The preceding stack() definition is retracted and replace by a new stack
 *	definition, whose [List] argument now contains the original stack contents
 *	plus the added value.		
 *
 *	A stack is created using the create/1 predicate and deleted using delete/1.
 *	A value can be pushed using the push/2 predicate, and popped using pop/2.
 *	The top of stack can be examined without removing the value by using peek/2.
 *	The sizeof/2 predicate gives the size of the stack, whereas is_empty/1
 *	succeeds if the stack is empty.
 *
 * USES
 *	list.pro
 *
 * AUTHOR
 *	Christian Vigh, February 2007.
 *
 ===============================================================================
 ******/

:- module(stack).

:- 	import(list).

:-	export(abs/1).
:-	export(acos/1).
:-	export(add/1).
:-	export(and/1).
:-	export(asin/1).
:-	export(atan/1).
:-	export(ceiling/1).
:-	export(copy/2).
:-	export(cos/1).
:-	export(create/1).
:-	export(dec).
:-	export(delete/1).
:-	export(div/1).
:-	export(divmod/1).
:-	export(dup/1).
:-	export(dup2/1).
:-	export(dup3/1).
:-	export(e/1).
:-	export(empty/1).
:-	export(eq/1).
:-	export(eq0/1).
:-	export(exp/1).
:-	export(false/1).
:-	export(floor).
:-	export(ge/1).
:-	export(ge0/1).
:-	export(gt/1).
:-	export(gt0/1).
:-	export(inc/1).
:-	export(int/1).
:-	export(is_empty/1).
:-	export(le/1).
:-	export(le0/1).
:-	export(leq/1).
:-	export(lge/1).
:-	export(lgt/1).
:-	export(lle/1).
:-	export(lne/1).
:-	export(log/1).
:-	export(log10/1).
:-	export(lshift/1).
:-	export(llt/1).
:-	export(lt/1).
:-	export(lt0/1).
:-	export(lwithin/1).
:-	export(max/1).
:-	export(min/1).
:-	export(minus/1).
:-	export(mod/1).
:-	export(mul/1).
:-	export(ndup/2).
:-	export(ne/1).
:-	export(ne0/1).
:-	export(neg/1).
:-	export(or/1).
:-	export(over/1).
:-	export(peek/2).
:-	export(peekto/2).
:-	export(peekto/3).
:-	export(pi/1).
:-	export(pop/1).
:-	export(pop/2).
:-	export(pop2/1).
:-	export(popall/2).
:-	export(popto/2).
:-	export(popto/3).
:-	export(power/1).
:-	export(print/1).
:-	export(push/2).
:-	export(random/1).
:-	export(rotate/1).
:-	export(round/1).
:-	export(rshift/1).
:-	export(sin/1).
:-	export(sizeof/1).
:-	export(sqrt/1).
:-	export(sub/1).
:-	export(swap/1).
:-	export(tan/1).
:-	export(true/1).
:-	export(under/2).
:-	export(within/1).
:-	export(xor/1).

:- end_module(stack).


:- body(stack).



/****f* Modules.Stack/stack manipulation operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	create/1
 *	delete/1
 *	empty/1
 *	copy/1
 *
 * SYNTAX
 *	create(StackName)
 *	delete(StackName)
 *	empty(StackName)
 *	copy(Stack1, Stack2)
 *
 * PURPOSE
 *	create/1 creates the stack [StackName]. The new stack is asserted through the
 *	following fact :
 *
 *		stack(StackName, []).
 *
 *	The empty list means that the stack is initially empty.
 *	delete/1 deletes the specified stack.
 *	empty/1 empties the specified stack without removing it.
 *	copy/1 copies [Stack1] and its contents to a new stack called [Stack2].
 *	If [Stack2] already exists, it will be removed prior to the operation.
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Stack to create.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

copy(Stack1, Stack2) :-
	retractall( stack(Stack2, _) ),
	stack( Stack1, List ),
	assert( stack( Stack2, List ) ).

create(StackName) :-
	retractall( stack(StackName, _) ),
	assert( stack( StackName, [] ) ).

delete(StackName) :-
	retractall( stack(StackName, _) ).

empty(StackName) :-
	delete(StackName),
	create(StackName).

/******/





/****f* Modules.Stack/push operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	push/2
 *
 * SYNTAX
 *	push(StackName, Value)
 *	push(StackName, List)
 *
 * PURPOSE
 *	Pushes the specified [Value] on top of [StackName]. If a [List] is specified,
 *	then each individual value of [List] is pushed on the stack [NewStack].
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack where the element is to be pushed.
 *
 *	[Value] (i) -
 *		Value to push.
 *
 *	[List] (i) -
 *		List of values to push.
 *
 * NOTES
 *	If the contents of the stack before the push is described with the fact :
 *		stack(StackName, [contents])
 *	then the new fact will be : 
 *		stack(StackName, [Value|content]).
 *	The same is applicable for a list, except that list elements are reversed,
 *	as if its elements were pushed individually, from first to last.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

push(StackName, List) :-
	is_list(List),
	stack(StackName, StackList),
	retractall( stack(StackName, _) ),
	reverse(List, ReverseList),
	append( ReverseList, StackList, NewList ),
	assert( stack(StackName, NewList) ).
	
push(StackName, Value) :-
	stack(StackName, List),
	do_push(List, Value, List2),
	retractall( stack(StackName, _) ),
	assert( stack(StackName, List2) ).

do_push([], Value, [Value]).	
do_push([H|T], Value, [Value, H|T]).

/******/





/****f* Modules.Stack/pop operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	pop/1
 *	pop/2
 *	pop2/1
 *	popall/2
 *	popto/2
 *	popto/3
 *
 * SYNTAX
 *	pop(StackName)
 *	pop(StackName, Value)
 *	pop2(StackName)
 *	popall(StackName, List)
 *	popto(Stack1, Stack2)
 *	popto(Stack1, Stack2, Value)
 *
 * PURPOSE
 *	The pop/1 predicate pops a value from stack [StackName].
 *	pop/2 performs the same operations, but unifies [Value] with the popped value.
 *	pop2/1 pops two values from the stack [StackName].
 *	popall/2 pops all values from the stack [StackName], and puts the contents
 *	into [List].
 *	popto/2 pops a value from [Stack1] then pushes it to [Stack2].
 *	popto/3 performs the same operation, but unifies [Value] with the popped value.
 *
 * ARGUMENTS
 *	[StackName], [Stack1] (i) -
 *		Stack for which to apply the pop operation.
 *
 *	[Stack2] (i) -
 *		Stack where the popped value is to be pushed.
 *
 *	[Value] (o) -
 *		Unifies to the popped value.
 *
 *	[List] (o) -
 *		Unifies to the list of values contained in the stack [StackName]. 
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

pop(StackName) :-
	pop(StackName, _).
pop(StackName, Value) :-
	stack(StackName, [Value|T]),
	retractall( stack(StackName, _) ),
	assert( stack(StackName, T) ).

pop2(StackName) :-
	stack(StackName, [H1, H2|T]),
	retractall( stack(StackName, _) ),
	assert( stack(StackName, T) ).

popall(StackName, List) :-
	stack(StackName, List),
	delete(StackName),
	create(StackName).

popto(Stack1, Stack2) :-
	popto(Stack1, Stack2, _).
popto(Stack1, Stack2, Value) :-
	pop(Stack1, Value),
	push(Stack2, Value).

/******/





/****f* Modules.Stack/peek operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	peek/2
 *	peekto/2
 *	peekto/3
 *	peekunder/2
 *
 * SYNTAX
 *	peek(StackName, Value)
 *	peekto(Stack1, Stack2)
 *	peekto(Stack1, Stack2, Value)
 *	peekunder(StackName, Value)
 *
 * PURPOSE
 *	peek/2 retrieves a value from [StackName] without removing it, and unifies
 *	[Value] with the result.
 *	peekto/2 peeks a value from [Stack1] and pushes it to [Stack2].
 *	peekto/3 performs the same operation, and unifies [Value] with the popped
 *	value.
 *	peekunder/2 unifies [Value] with the second value behind the top of stack 
 *	[StackName].
 *
 * ARGUMENTS
 *	[StackName], [Stack1] (i) -
 *		Stack where the value is to be poked from.
 *
 *	[Stack2] (i) -
 *		Stack where to push the poked value.
 *
 *	[Value] (o) -
 *		Unifies to the poked value.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

peek(StackName, Value) :-
	stack(StackName, [Value | _]).

peekto(Stack1, Stack2) :-
	peekto(Stack1, Stack2, _).
peekto(Stack1, Stack2, Value) :-
	peek(Stack1, Value),
	push(Stack2, Value).

peekunder(StackName, Value) :-
	stack(StackName, [ _, Value | _ ] ).

/******/




/****f* Modules.Stack/stack query operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	is_empty/1
 *	sizeof/2
 *
 * SYNTAX
 *	is_empty(StackName)
 *	sizeof(StackName, Length)
 *
 * PURPOSE
 *	is_empty/1 succeeds if the stack [StackName] contains no element.
 *	sizeof/2 unifies [Length] with the number of elements present in [StackName].
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack to check.
 *
 *	[Length] (o) -
 *		Number of elements in the stack.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */


is_empty(StackName) :-
	stack(Stack, [] ).


sizeof(StackName, Size) :-
	stack(StackName, List),
	length(List, Size).

/******/	





/****f* Modules.Stack/other stack operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	dup/1
 *	dup2/1
 *	dup3/1
 *	ndup/2
 *	over/1
 *	rotate/1
 *	swap/1
 *	swap/2
 *
 * SYNTAX
 *	dup(StackName)
 *	dup2(StackName)
 *	dup3(StackName)
 *	ndup(StackName, Count)
 *	over(StackName)
 *	rotate(StackName)
 *	swap(StackName)
 *	swap(Stack1, Stack2)
 *
 * PURPOSE
 *	dup/1 duplicates the topmost element of the stack [StackName] and puts it
 *	on top of stack.
 *	dup2/1 performs the same operation for the two topmost elements.
 *	dup3/1 performs the same operation for the three topmost elements.
 *	ndup/2 duplicates [Count] times the topmost element of [StackName].
 *	over/1 duplicates the second topmost element of the stack [StackName].
 *	rotate/1 rotates the three topmost elements of [StackName].
 *	swap/1 swaps the two topmost elements from [StackName], whereas swap/2
 *	exchanges between [Stack1] and [Stack2] topmost elements.
 *
 * ARGUMENTS
 *	[StackName], [Stack1] (i) -
 *		Stack where the operation is to be performed.
 *
 *	[Stack2] (i) -
 *		(for swap/2) Stack whose topmost element is to be swapped with [Stack1].
 *
 *	[Count] (i) -
 *		(for ndup/2) Number of times to copy [StackName] topmost element
 *		onto the stack.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Swaps the two topmost values of a stack
swap(Stack) :-
	stack(Stack, [A, B | T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [B, A | T]) ).

% Swaps top elements of Stack1 and Stack2
swap(Stack1, Stack2) :-
	pop(Stack1, V1),
	pop(Stack2, V2),
	push(Stack1, V2),
	push(Stack2, V1).

% Dulicates the top element of Stack
dup(Stack) :-
	stack(Stack, [H|T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [H, H|T]) ).

% Duplicates the two topmost elements of stack
dup2(Stack) :-
	stack(Stack, [H1, H2|T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [H1, H2, H1, H2 | T]) ).

% Duplicates the three topmost elements of stack
dup3(Stack) :-
	stack(Stack, [H1, H2, H3|T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [H1, H2, H3, H1, H2, H3 | T]) ).
	
% Duplicates N times the topmost element of Stack
ndup(Stack, N) :-
	stack(Stack, [H|T]),
	retractall( stack(Stack, _) ),
	duplicateatom(H, N, Dups),
	append(Dups, T, NewList),
	assert( stack(Stack, NewList) ).

% Copies the second element in Stack to the top of stack
over(Stack) :-
	stack(Stack, [H1, H2|T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [H2, H1, H2|T]) ).

% Rotates the three topmost elements of the Stack
rotate(Stack) :-
	stack(Stack, [H1, H2, H3|T]),
	retractall( stack(Stack, _) ),
	assert( stack(Stack, [H2, H3, H1|T]) ).

/******/	





/****if* Modules.Stack/call operator functions
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	call_op1/2
 *	call_op1/3
 *	call_op2/2
 *
 * SYNTAX
 *	call_op1(StackName, Op)
 *	call_op1(StackName, Op, Operand2)
 *	call_op2(StackName, Op)
 *
 * PURPOSE
 *	call_op1/2 pops a value from [StackName] and applies the unary operator [Op]
 *	to it.
 *	call_op1/3 pops a value from [StackName] and applies the binary operator [Op]
 *	together with [Operand2].
 *	call_op2/2 pops two values from [StackName] and applies the binary operator
 *	[Op] to them.
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack on which the operation is to be performed.
 *
 *	[Op] (i) -
 *		Unary (call_op1/2) or binary (call_op1/3 or call_op2/2) operator to apply.
 *
 *	[Operand2] (i) -
 *		For call_op1/3, second operand for the binary operator [Op].
 *
 * NOTES
 *	These predicates are used by arithmetic functions such as inc, dec, add, sqrt, etc.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

% Pop 1 element, then apply Op
call_op1(Stack, Op) :-
	pop(Stack, X),				% Pop one value into X
	Operation =.. [Op, X],			% Build the Op X operation
	Goal =.. [is, NewX, Operation],		% Then build : Result is Operation
	call(Goal),				% Call the goal to unify Result
	push(Stack, NewX).			% and push it on the stack

% Pop 1 element, then apply Op with Operand2 as the second operand
call_op1(Stack, Op, Operand2) :-
	pop(Stack, X),				% Pop one value into X
	Operation =.. [Op, X, Operand2],	% Build the X Op Operand2 operation
	Goal =.. [is, NewX, Operation],		% Then build : Result is Operation
	call(Goal),				% Call the goal to unify Result
	push(Stack, NewX).			% and push it on the stack

% Pop 2 elements, then apply binary Op
call_op2(Stack, Op) :-
	pop(Stack, X1), pop(Stack, X2),		% Pop two values into X1 and X2
	Operation =.. [Op, X1, X2],		% Build the X1 Op X2 operation
	Goal =.. [is, NewX, Operation],		% Then build : Result is Operation
	call(Goal),				% Call the goal to unify Result
	push(Stack, NewX).			% and push it on the stack

/******/




/****f* Modules.Stack/mathematical operations on stack
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	abs/1, acos/1, add/1, asin/1, atan/1,
 *	ceiling/1, cos/1,
 *	dec/1, div/1, divmod/1,
 *	e/1, exp/1,
 *	floor/1,
 *	inc/1, int/1,
 *	log/1, log10/1,
 *	max/1, min/1, minus/1, mod/1, mul/1,
 *	pi/1, power/1,
 *	random/1, round/1,
 *	sin/1, sqrt/1, sub/1, 
 *	tan/1
 *
 * SYNTAX
 *	abs(StackName) etc.
 *
 * PURPOSE
 *	abs/1 replaces the top of stack with its absolute value.
 *	acos/1 replaces the top of stack with its arc-cosine.
 *	add/1 pops the two values at the top of stack and replace them with their sum.
 *	asin/1 replaces the top of stack with its arc-sine.
 *	atan/1 replaces the top of stack with its arc-tan.
 *	ceiling/1 replaces the top of stack with its ceiled value.
 *	cos/1 replaces the top of stack with its cosine.
 *	dec/1 decrements the value on the top of stack.
 *	div/1 performs a division between the two values at the top of stack and replace
 *	them with the result.
 * 	divmod/1 performs a division between the two values at the top of stack and
 *	replaces them with the quotient and the remainder.
 *	e/1 push the value of e onto the top of stack.
 *	exp/1 replaces the top of stack with its exp() value.
 *	floor/1 replaces the top of stack with its floor-value.
 *	inc/1 increments the value on the top of stack.
 *	int/1 replaces the value on the top of stack with its integer representation.
 *	log/1, log10 replaces the top of stack with its log/log10 value.
 *	min/1, max/1 replace the two values at the top of stack by their minimum/maximum.
 *	minus/1 negates the value at the top of stack.
 *	mod/1 divides the two values at the top of stack and replaces them with their
 *	quotient.
 *	mul/1 multiplies the two values at the top of stack and replaces them with
 *	the result.
 *	pi/1 pushes the pi value on top of stack.
 *	power/1 performs the power of top of stack by top of stack-1, and replaces
 *	them by the result.
 *	random/1 pushes a random value on top of stack.
 *	round/1 replaces the top of stack with its rounded value.
 *	sin/1 replaces the top of stack with its sine.
 *	sqrt/1 replaces the top of stack with its square root.
 *	sub/1 substract the value on top of stack with the value on top of stack - 1,
 *	then replaces them with the result.
 *	tan/1 replaces the top of stack with its tangent.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

abs(Stack) :-
	call_op1(Stack, abs).
	
acos(Stack) :-
	call_op1(Stack, acos).

add(Stack) :-
	call_op2(Stack, +).
	
asin(Stack) :-
	call_op1(Stack, asin).
	
atan(Stack) :-
	call_op1(Stack, atan).

ceiling(Stack) :-
	call_op1(Stack, ceiling).

cos(Stack) :-
	call_op1(Stack, cos).
	
dec(Stack) :-
	call_op1(Stack, -, 1).
	
div(Stack) :-
	call_op2(Stack, /).
	
divmod(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	Div is X / Y,
	Mod is X mod Y,
	push(Stack, [Mod, Div]).

e(Stack) :-
	push(Stack, e).
	
exp(Stack) :-
	call_op1(Stack, exp).

floor(Stack) :-
	call_op1(Stack, floor).

inc(Stack) :-
	call_op1(Stack, +, 1).	

int(Stack) :-
	call_op1(Stack, integer).

log(Stack) :-
	call_op1(Stack, log).

log10(Stack) :-
	call_op1(Stack, log10).

max(Stack) :-
	call_op2(Stack, max).
	
min(Stack) :-
	call_op2(Stack, min).

minus(Stack) :-
	call_op1(Stack, -).
	
mod(Stack) :-
	call_op2(Stack, mod).
	
mul(Stack) :-
	call_op2(Stack, *).
	
pi(Stack) :-
	push(Stack, pi).
	
power(Stack) :-
	call_op2(Stack, **).
	
random(Stack) :-
	call_op1(Stack, random).
	
round(Stack) :-
	call_op1(Stack, round).

sin(Stack) :-
	call_op1(Stack, sin).
	
sqrt(Stack) :-
	call_op1(Stack, sqrt).

sub(Stack) :-
	call_op2(Stack, -).
	
tan(Stack) :-
	call_op1(Stack, tan).

/******/




/****f* Modules.Stack/bitwise stack operations
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	and/1, or/1, neg/1, xor/1, lshift/1, rshift/1
 *
 * SYNTAX
 *	and(StackName) etc...
 *
 * PURPOSE
 *	and/1 performs a bitwise-and operation between the two values on top of
 *	stack, and replaces them with the result.
 *	or/1 performs a bitwise-or operation between the two values on top of
 *	stack, and replaces them with the result.
 *	xor/1 performs a bitwise-xor operation between the two values on top of
 *	stack, and replaces them with the result.
 *	neg/1 performs a bitwise-not operation on the value on top of stack, and 
 *	replaces it with the result.
 *	lshift/1 bitwise-shifts left the value on top of stack times the value on
 *	top of stack - 1.
 *	rshift/1 bitwise-shifts right the value on top of stack times the value on
 *	top of stack - 1.
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack on which to perform the operation.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

and(Stack) :-
	call_op2(Stack, /\).
	
or(Stack) :-
	call_op2(Stack, \/).

neg(Stack) :-
	call_op1(Stack, \).

xor(Stack) :-
	call_op2(Stack, xor).

lshift(Stack) :-
	call_op2(Stack, <<).

rshift(Stack) :-
	call_op2(Stack, >>).

/******/




/****f* Modules.Stack/boolean operations on stack (numbers)
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	true/1, false/1
 *	within/1
 *	lt0/1, le0/1, gt0/1, ge0/1, eq0/1, ne0/1
 *	lt/1 , le/1 , gt/1 , ge/1 , eq/1 , ne/1
 *
 * SYNTAX
 *	true(StackName) etc.
 *
 * PURPOSE
 *	All the predicates operate on numeric values.
 *
 *	true/1 and false/1 respectively push the value true (1) and false (0) onto
 *	the top of stack.
 *
 *	within/1 tests if the value on top of stack is between the values on top
 *	of stack-1 and top of stack-2. It replaces the 3 values with the comparison
 *	result, either 0 or 1.
 *
 *	The lt0/1, le0/1, gt0/1, ge0/1, eq0/1, ne0/1 predicates check the value on
 *	top of stack against zero and replaces it with the result of the comparison
 *	(either 0 or 1). The result is :
 *	* lt0 : the value on top of stack is less than 0
 *	* le0 : the value on top of stack is less than or equal to zero
 *	* gt0 : the value on top of stack is greater than zero
 *	* ge0 : the value on top of stack is greater than or equal to zero
 *	* eq0 : the value on top of stack is equal to zero
 *	* ne0 : the value on top of stack is different from zero
 *
 *	The lt/1 , le/1 , gt/1 , ge/1 , eq/1 , ne/1 predicates performs a comparison
 *	between the two values on top of stack and replaces them with the result
 *	(either 0 or 1). The result will be true (1) if :
 *	* lt : TOS-1 is less than TOS-2
 *	* le : TOS-1 is less than or equal to TOS-2
 *	* gt : TOS-1 is greater than TOS-2
 *	* ge : TOS-1 is greater or equal to TOS-2
 *	* ne : TOS-1 is different from TOS-2
 *	* eq : TOS-1 equals TOS-2
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack on which the operation is to be performed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

true(Stack) :-
	pop(Stack, X),
	X \== 0.
	
false(Stack) :-
	pop(Stack, X),
	X == 0.
	
	
within(Stack) :-
	pop(Stack, X),
	pop(Stack, Low),
	pop(Stack, High),
	(
		( X >= Low, X =< High, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).
	
	
lt0(Stack) :-
	pop(Stack, X),
	(
		( X < 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).
	 
le0(Stack) :-
	pop(Stack, X),
	(
		( X =< 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

gt0(Stack) :-
	pop(Stack, X),
	(
		( X > 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

ge0(Stack) :-
	pop(Stack, X),
	(
		( X >= 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

eq0(Stack) :-
	pop(Stack, X),
	(
		( X =:= 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

ne0(Stack) :-
	pop(Stack, X),
	(
		( X =\= 0, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

lt(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X < Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

le(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X =< Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).
	 
gt(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X > Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

ge(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X >= Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

eq(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X =:= Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

ne(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X =\= Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

/******/




/****f* Modules.Stack/boolean operations on stack (strings)
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	lwithin/1
 *	llt/1 , lle/1 , lgt/1 , lge/1 , leq/1 , lne/1
 *
 * SYNTAX
 *	lwithin(StackName) etc.
 *
 * PURPOSE
 *	All the predicates operate on string/atom values (comparisons are performed
 *	lexically).
 *
 *	lwithin/1 tests if the value on top of stack is between the values on top
 *	of stack-1 and top of stack-2. It replaces the 3 values with the comparison
 *	result, either 0 or 1.
 *
 *	The llt/1 , lle/1 , lgt/1 , lge/1 , leq/1 , lne/1 predicates performs a
 *	comparison between the two values on top of stack and replaces them with
 *	the result (either 0 or 1). The result will be true (1) if :
 *	* llt : TOS-1 is less than TOS-2
 *	* lle : TOS-1 is less than or equal to TOS-2
 *	* lgt : TOS-1 is greater than TOS-2
 *	* lge : TOS-1 is greater or equal to TOS-2
 *	* lne : TOS-1 is different from TOS-2
 *	* leq : TOS-1 equals TOS-2
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Name of the stack on which the operation is to be performed.
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */
	 
llt(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X @< Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

lle(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X @=< Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).
	 
lgt(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X @> Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

lge(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X @>= Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

leq(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X == Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

lne(Stack) :-
	pop(Stack, X),
	pop(Stack, Y),
	(
		( X \== Y, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

lwithin(Stack) :-
	pop(Stack, X),
	pop(Stack, Low),
	pop(Stack, High),
	(
		( X @>= Low, X @=< High, push(Stack, 1) ) ;
		push(Stack, 0)
	 ).

/******/



/****f* Modules.Stack/print
 -------------------------------------------------------------------------------
 *
 * PREDICATE
 *	stack_print/1
 *
 * SYNTAX
 *	stack_print(StackName)
 *
 * PURPOSE
 *	Prints the specified stack and its contents.
 *
 * ARGUMENTS
 *	[StackName] (i) -
 *		Stack to print/
 *
 -------------------------------------------------------------------------------
 * SOURCE
 */

stack_print(Stack) :-
	stack(Stack, List),
	write('Stack '), write(Stack), write( ' : '), write(List), nl.

/******/
	
:- end_body(stack).