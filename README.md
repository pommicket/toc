## toc

`toc` is a language which compiles to C.

---

### About

`toc` is currently in development. **It is not a stable language,
and there are almost definitely bugs right now.**
I would recommend against using it for anything big or important.
Many parts of it may change in the future.

`toc` improves on C's syntax (and semantics) in many ways,
To declare `x` as an integer and set it to 5,
you can do:

```
x := 5; // Declare x and set x to 5 (infer type)  
x : int = 5; // Explicitly make the type int.  
x : int; x = 5; // Declare x as an integer, then set it to 5.  
```

`toc` is statically typed and has many of C's features, but
it is nearly as fast in theory.

See `docs` for more information (in progress).

`tests` has some test programs written in `toc`.

To compile the compiler on a Unix-y system, use
