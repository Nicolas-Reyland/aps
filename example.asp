/*			example of a algebraic strcuture definition			*/


// Definition of K (set of elements on which the operators will operate)
K :: N // natural numbers

/*
Could also do :
K :: R // real numbers
K :: Z // relative numbers
K :: C // complexe numbers
K :: Q // rational numbers
K :: ? // (default) undefined : no information on K
K :: R3 // R x R x R (3-dim vectors of real numbers)
K :: ?N // (replace N with natural number) N-dim vectors of undefined elements
*/

K << {
	0, 1 // recognize 0 and 1 as special values
	// Could write X too
}

// To recognize a set as being a special value :
// K <| N
// or K <| R
// etc

/* Definition of operators and their properties */
+ :: {
	A + B = B + A
	(A + B) + C = A + (B + C)
	A + 0 = A
}

* :: {
	// commutativity
	A * B = B * A
	// associativity
	(A * B) * C = A * (B * C)
	A * 1 = A
	A * 0 = 0
}

^ :: {
	A ^ 0 = 1
	A ^ 1 = A
}

square 	:: A -> A ^ 2
sqrt	:: A ^ 2 -> A

// Use _ to define properties using multiple operators, not specific to one operator
_ :: {
	// distributivity
	A * (B + C) = A * B + A * C
	// implying
	//A * B = A * C => B = C
	//A + B = A + C => B = C
	// ^ to * sequence
	A ^ B = 1 $ * A $#B
}

/* Solve some problem */
# square(A + B) ?= A^2 + 2AB + B^2

/* Theoretical output of the program */
--------- Meet in the middle ---------

input : square(A + B)
-> (A + B)^2
-> 1 * (A + B) * (A + B)
-> (A + B) * (A + B)
-> (A + B) * A + (A + B) * B
-> A * (A + B) + (A + B) * B
-> A * (A + B) + B * (A + B)
result ::  A * A + A * B + B * A + B * B

input : A^2 + 2AB + B^2
.
.
.
result : (A + B)^2

beautified : A^2 + 2 * (A * B) + B^2
