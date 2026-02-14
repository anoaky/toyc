# Statements

There are five kinds of statements: `if` statements, `while` loops, blocks, `let` statements, and expression statements. 
Statements are a subset of [expressions](./expressions.md) that are typically used for their side-effects, rather than as values. They do, however, have value.

## If Statements

The formal grammar for if statements is as follows:

```text
if-statement ::= "if" , "(" , expression , ")" , statement , { "else" , statement } 
```

These are all valid if statements:

```akyno
if (x == 1) {
  foo();
} else {
  bar();
}

if (y) foobar();

if (x) {
  fizz();
} else if (y) {
  buzz();
} else if (z) {
  fizzbuzz();
}
```


