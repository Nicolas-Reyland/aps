# apsl-lang
**Algebraic Proof System Language**

A language written in rust to generate proofs of algebraic statements, based on algebraic structures.

# Installation

Clone and cd into the repo :
```sh
git clone https://github.com/Nicolas-Reyland/apsl-lang.git
cd apsl-lang
```

Install rust and cargo, then run :
```sh
cargo build --release
```

# Usage
To get a repl shell, run :

```sh
cargo run --release
```

You can also import *apsl* files by adding them to the `cargo run` command like so : `cargo run file1.apsl file2.apsl ...` .

Once inside the repl-shell, you can type `help` to get some more infos about the available commands.

# Examples

## Basics
Let's write a file with the following rules/axioms, defining an algebraic structure with one operation `+` :
```
+ :: {
  A + B = B + A ;
  (A + B) + C = A + (B + C) ;
  A + B + ... = (A + B) + ... ;
}
```
Let's call that file *plus.apsl*.

Now, let's start a repl (`cargo run plus.apsl`). You can also just run `cargo run`, then use the `import` command as such: `import plus.apsl`.
To check that the rules are well loaded, type `ctx`. This should give you the following output :
```
 Properties :
 | A + B = B + A
 | (A + B) + C = A + (B + C)
 | A + B + ... = (A + B) + ...

 Functions :

 K Properties :

 Auto break : true
```

Now that the rules defining our `+` operation are loaded, you can ask for a prove of `A + B + C = C + B + A` with the following command : `prove A + B + C = C + B + A`. That should give you the following result :

```
 Solution found for 'A + B + C = C + B + A' :
  A + B + C
 = (A + B) + C          |       A + B + ... = (A + B) + ...
 = C + (A + B)          |       A + B = B + A
 = C + (B + A)          |       A + B = B + A
 = (C + B) + A          |       (A + B) + C = A + (B + C)
 = C + B + A            |       A + B + ... = (A + B) + ...

```

## Bit more tricky case
Now that we've proven that `A + B + C = C + B + A` (wow 😲), let's prove that `(X + Y)^2 = (X^2) + (2*(X*Y)) + (Y^2)` ! (I bet you didn't know that identity was a thing, ey ? 😏)

Note that in these expressions, we have to use parentheses, because no order has been established between the `+`and `*`operations (none will be established in this example). To prove this, we'll need our custom rules defining our `+` and `*` operations, which are in the `examples/numbers.apsl`.

We can start a repl with `cargo run examples/numbers.apsl`.
Then, we're going to define a rule about the `*` operation (we could define a more global one, but this one suffices) :
```
Algebraic Proof System Language 〉def _ :: { (A * B) + (A * B) = 2 * (A * B) ; }
```
Which should output :
```
 Added object(s) to context.
```

Then, we can make sure we have all the rule we need :
```
Algebraic Proof System Language 〉ctx
```
Which outputs :
```
 Properties :
 | A + B = B + A
 | A + B + ... = (A + B) + ...
 | (A + B) + C = A + (B + C)
 | A + 0 = A
 | A * B = B * A
 | A * B * ... = (A * B) * ...
 | (A * B) * C = A * (B * C)
 | 1 * ... = ...
 | A * 0 = 0
 | A ^ N = 1 } $ * A $ # N
 | A * (B + C) = (A * B) + (A * C)
 | (A * B) + (A * B) = 2 * (A * B)

 Functions :

 K Properties :
 | K :: N1

 Auto break : true

```

Finally, let's ask it to prove the equality :
```
Algebraic Proof System Language 〉prove (X + Y)^2 = (X*X) + (2 * (X * Y)) + (Y*Y)
```
Which gives us a proof, using the rules we have defined :
```
Solution found for '(X + Y) ^ 2 = (X * X) + (2 * (X * Y)) + (Y * Y)' :
  (X + Y) ^ 2
 = 1 * (X + Y) * (X + Y)                        |       A ^ N = 1 } $ * A $ # N
 = (1 * (X + Y)) * (X + Y)                      |       A * B * ... = (A * B) * ...
 = ((1 * X) + (1 * Y)) * (X + Y)                |       A * (B + C) = (A * B) + (A * C)
 = (2 * (1 * Y)) * (X + Y)                      |       (A * B) + (A * B) = 2 * (A * B)
 = (2 * Y) * (X + Y)                            |       1 * ... = ...
 = (X * X) + ((2 * Y) * Y)                      |       A * (B + C) = (A * B) + (A * C)
 = (X * X) + (2 * (Y * Y))                      |       (A * B) * C = A * (B * C)
 = (X * X) + ((2 * (X * Y)) + (Y * Y))          |       (A * B) + (A * B) = 2 * (A * B)
 = ((X * X) + (2 * (X * Y))) + (Y * Y)          |       (A + B) + C = A + (B + C)
 = (X * X) + (2 * (X * Y)) + (Y * Y)            |       A + B + ... = (A + B) + ...

```
