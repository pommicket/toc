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
data\_structures.c contains a dynamic array implementation which is very useful.

### Notes

Identifiers are kept in a hash table in the block in which they reside.
They are resolved during typing (i.e. each identifier is associated with its
declaration).

It is assumed that the number of identifiers in a declaration, or parameters to a function
will fit in an int, since a function with (at least) 32768 parameters is ridiculous.

