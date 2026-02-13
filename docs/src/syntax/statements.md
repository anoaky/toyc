# Statements

There are five kinds of statements: `if` statements, `while` loops, blocks, `let` statements, and expression statements. 
All statements have a value, like [expressions](./expressions.md), but the two are not interchangeable: all expressions are statements, but not vice versa.

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


