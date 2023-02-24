# aps-lang
**Algebraic Proof System Language**

A language written in rust to generate proofs of algebraic statements, based on algebraic structures.

# Installation

Clone and cd into the repo :
```sh
git clone https://github.com/Nicolas-Reyland/apsl-lang.git
cd apsl-lang
```

Install rust and cargo :
```sh
cargo build --release
```

# Usage
To get a repl shell, run :

```sh
cargo run --release
```

Let's say your goal is to generate algebraic proofs in bizarre Algebraic Structures.
First of all, you'll need to describe your Structure. In ASPL, you can only work with one structure at a time (yet).
Let's write an algebraic structure that has a `+` operator and `@` operator.
```
// comments are C-like comments

// operator properties are listed in the 'property-group'. The sign
// you put in front of the group is not relevant, but adds clarity about
// which operator is being described.
+ :: {
    // Rules are written like so : left-expression = right-expression ;

    // Let's make + a commutative operator
    A + B = B + A ;
    // Let's add an identity
    A + 0 = A ;
    // Since we have the commutative property of +, we also know that 0 + A = A
}

// Let's add another operator
@ :: {
    A @ 1 = A ;
    A @ A = 0 ;
}

// We can add properties using both operators (we'll use _ as a prefix,
// but as said previously, it doesn't matter what we put there)
_ :: {
    A @ (B + C) = (A @ B) + (A @ C) ;
}
```

All that should be written to a file, for example `unusable.apsl`.
Such *apsl* files can be imported by adding them to the `cargo run` command like so : `cargo run file1.apsl file2.apsl ...`.

Once inside the repl-shell, you can ask for a proof of **X @ ((Z @ Z) + Y) @ 1 = Y @ X** like so :
```
Algebraic Proof System Language 〉prove X @ ((Z @ Z) + Y) = X @ Y
```
I'll let you see what it outputs :) (there are examples of outputs in the [examples](#examples) section)

Please type `help` to get some more infos about the available commands.

# Examples

## Basics
Let's write a file with the following rules/axioms, defining an algebraic structure with one operation `+` :
```
+ :: {
  A + B = B + A ;
  (A + B) + C = A + (B + C) ;
}
```
Let's call that file *plus.apsl*.

Now, let's start a repl (`cargo run plus.apsl`). You can also just run `cargo run`, then use the `import` command as such: `import plus.apsl` (pay attention to the current working directory).
To check that the rules are well loaded, type `ctx`. This should give you the following output :
```
 Properties :
 | A + B = B + A
 | (A + B) + C = A + (B + C)

```

Now that the rules defining our `+` operation are loaded, you can ask for a prove of `A + B + C = C + B + A` with the following command : `prove A + B + C = C + B + A`. That should give you the following result :

```
 Solution found for 'X + Y + Z = Z + Y + X' :
  (X + Y) + Z
 = Z + (X + Y)		|	A + B = B + A
 = Z + (Y + X)		|	A + B = B + A
 = Z + Y + X		|	(A + B) + C = A + (B + C)
```

## A bit trickier case
Now that we've proven that **A + B + C = C + B + A** (wow 😲), let's prove that **(X + Y)^2 = (X^2) + (2\*X\*Y) + (Y^2)** ! (I bet you didn't know that was a thing in maths, ey ? 😏)

Note that in these expressions, we have to use parentheses, because no order has been established between the `+`and `*`operations (none will be established in this example). To prove this, we'll need our custom rules defining our `+` and `*` operations, which are in the `examples/numbers.apsl`.

We can start a repl with `cargo run examples/numbers.apsl`, and make sure we have all the rule we need :

If you don't want to start a new shell, you can do the following :
```
Algebraic Proof System Language 〉ctx clear
 Cleared context

Algebraic Proof System Language 〉ctx
 No Properties

Algebraic Proof System Language 〉import examples/numbers.apsl
 imported 'examples/numbers.apsl'

```
Let's check the current context.
```
Algebraic Proof System Language 〉ctx
 Properties :
 | A + B = B + A
 | (A + B) + C = A + (B + C)
 | A + 0 = A
 | A * B = B * A
 | (A * B) * C = A * (B * C)
 | A * 1 = A
 | A * 0 = 0
 | A ^ 0 = 1
 | A ^ 1 = A
 | A * (B + C) = (A * B) + (A * C)
 | A * 2 = A + A
 | A ^ 2 = A * A

 K Properties :
 | K :: R1

 Auto break : true
 
Algebraic Proof System Language 〉
```

Finally, let's ask it to prove the equality :
```
Algebraic Proof System Language 〉prove (X + Y)^2 = (X*X) + (2 * (X * Y)) + (Y*Y)
```
Which gives us a proof, using the rules we have defined :
```
 Solution found for '(X + Y) ^ 2 = (X ^ 2) + (2 * X * Y) + (Y ^ 2)' :
  (X + Y) ^ 2
 = (X + Y) * (X + Y)					|	A ^ 2 = A * A
 = ((X + Y) * X) + ((X + Y) * Y)			|	A * (B + C) = (A * B) + (A * C)
 = (X * (X + Y)) + ((X + Y) * Y)			|	A * B = B * A
 = (X * X) + (X * Y) + ((X + Y) * Y)			|	A * (B + C) = (A * B) + (A * C)
 = (X * X) + (X * Y) + (Y * (X + Y))			|	A * B = B * A
 = (X * X) + (X * Y) + ((Y * X) + (Y * Y))		|	A * (B + C) = (A * B) + (A * C)
 = (X * X) + (X * Y) + ((X * Y) + (Y * Y))		|	A * B = B * A
 = (X * X) + (X * Y) + ((X * Y) + (Y ^ 2))		|	A ^ 2 = A * A
 = (X * X) + (X * Y) + (X * Y) + (Y ^ 2)		|	(A + B) + C = A + (B + C)
 = (X ^ 2) + (X * Y) + (X * Y) + (Y ^ 2)		|	A ^ 2 = A * A
 = (X ^ 2) + ((X * Y) + (X * Y)) + (Y ^ 2)		|	(A + B) + C = A + (B + C)
 = (X ^ 2) + (X * Y * 2) + (Y ^ 2)			|	A * 2 = A + A
 = (X ^ 2) + (2 * (X * Y)) + (Y ^ 2)			|	A * B = B * A
 = (X ^ 2) + (2 * X * Y) + (Y ^ 2)			|	(A * B) * C = A * (B * C)
```
You may notice that some parentheses are missing for the rule on the right to be applied.
This is because the expressions on the left are *pretty-printed*. To disable this behaviour, you should
execute :
```
Algebraic Proof System Language 〉settings expr-pretty-print off
```
