## Guide to toc's source code

This document is intended for people who would like to develop toc.

Structure of the toc compiler:
tokenizer => parser => typing (types.c) => cgen 

toc tries to continue even after the first error. 
If one stage fails, the following ones do not
start.

toc's memory management works using an allocator which never frees anything.
This is because most of toc's data is kept around until the end of the program anyways.
Use the allocator for "permanent" allocations, and err\_malloc/err\_calloc/err\_realloc for temporary
allocations (to avoid having it take up space for a long time).

Memory leaks can happen if the compilation fails at any point, but they
should not happen if the compilation succeeds. Usually if there's an error
which causes a memory leak, the leak will be very small.

Functions which can fail (i.e. print an error message and stop) return a Status,
which is a bool, but GCC/clang warns about not using the return value.

Almost all of toc's types are defined in types.h.
Sometimes a type ending in Ptr is defined, e.g. typedef Declaration \*DeclarationPtr. This is
for the arr\_foreach macro, and not meant for normal use.

The fixed-width types U8/16/32/64 and I8/16/32/64 have been defined.
data\_structures.c contains a dynamic array implementation and string hash table which are very useful.

### Notes

Identifiers are kept in a hash table in the block in which they reside.
They are resolved during typing (i.e. each identifier is associated with its
declaration).

It is assumed that the number of identifiers in a declaration, or parameters to a function
will fit in an int, since a function with (at least) 32768 parameters is ridiculous.

### Miscellaneous thoughts

#### includes during parsing
Wouldn't it be nice if `#include` could be done during typing, so that the filename wouldn't have to be a
string literal? Well, unfortunately we end up in situations like this:
```
asdf ::= fn() {
	x ::= bar();
	a: [x]int;
	// ...
}
#include "foo.toc"; // foo defined here
bar ::= fn() int {
	return foo(17);
}
```
The way we evaluate `bar()` is by typing the function `bar` when we come across it in the declaration `x ::= bar();`.
In order to evaluate `foo`, we need to know what it refers to, and we don't know at this point that it comes from the
include, because we haven't expanded it yet. We could potentially expand all includes preceding any function which
is evaluated during typing, but that seems a bit cumbersome and would probably cause other problems.


#### Why `#if` is complicated
Allowing arbitrary `#if` conditions leads to the following problem:
```
bar1 ::= fn() bool { ... };
bar2 ::= fn() bool { ... };
foo1 ::= fn() bool { ... };
foo2 ::= fn() bool { ... };

#if foo() {
	bar ::= bar1;
} else {
	bar ::= bar2; 
}

#if bar() {
	foo ::= foo1;
} else {
	foo ::= foo2;
}
```

In order to determine which version of `bar` to use, we need to call `foo`, and in order to determine which version of `foo`
to use, we need to call `bar`. You could just error on circular dependencies like these, but there is still the fact that you
have to figure out where the declaration for `foo` is when `foo()` is evaluated, and it could be in some other `#if`. To
avoid things getting complicated, we restrict `#if`s to make this situation impossible. While it's not ideal, it is necessary
to avoid some edge cases where we can't find out which declaration you're referring to.
