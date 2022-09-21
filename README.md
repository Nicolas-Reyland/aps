# apsl-lang
**Algebraic Proof System Language**

A language written in rust to generate proofs of algebraic statements, based on algebraic structures.

# Installation

Clone and cd into the repo :
```sh
git clone https://github.com/Nicolas-Reyland/apsl-lang.git
cd aspl-lang
```

Install rust and cargo, then run :
```sh
cargo build --release
```

# Usage
To get a repl shell, run :

```sh
cargo run
```

You can also import *aspl* files by adding them to the `cargo run` command like so : `cargo run file1.aspl file2.aspl ...` .

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
Let's call that file *plus.aspl*.

Now, let's start a repl (`cargo run plus.aspl`). You can also just run `cargo run`, then use the `import` command as such: `import plus.aspl`.
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

Now that the rules defining the `+` operation are loaded, you can ask for a prove of `A + B + C = C + B + A` with the following command : `prove A + B + C = C + B + A`. That should give you the following result :

```
 Solution found for 'A + B + C = C + B + A' :
  A + B + C
 = (A + B) + C          |       A + B + ... = (A + B) + ...
 = C + (A + B)          |       A + B = B + A
 = C + (B + A)          |       A + B = B + A
 = (C + B) + A          |       (A + B) + C = A + (B + C)
 = C + B + A            |       A + B + ... = (A + B) + ...

```

