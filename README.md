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

To compile the compiler on a Unix-y system, just run `build.sh`. You can supply a compiler by running `CC=tcc build.sh`, or built it in release mode with `./build.sh release` (which will help speed up compiling large programs). 

On other systems, you can just compile main.c with a C compiler. toc uses several C99 and a couple of C11 features, so it might not work on all compilers. But it does compile on quite a few, including `clang`, `gcc`, and `tcc`. It can also be compiled as if it were C++, but it does break the standard in a few places\*. So, MSVC can also compile it. The *outputted* code should be C99-compliant.

---

### `toc` Source Code

`toc` is written in C, for speed and portability. It has no dependencies, other than the C runtime library.

#### Build system
`toc` is set up as a unity build, meaning that there is only one translation unit. So, `main.c` `#include`s `toc.c`, which `#include`s all of `toc`'s files. This improves (from scratch) compilation speeds, since you don't have to include headers a bunch of times for each translation unit. This is more of a problem in C++, where, for example, doing `#include <map>` ends up turning into 25,000 lines after preprocessing. All of toc's source code, which includes most of the C standard library, at the time of this writing (Dec 2019) is only 22,000 lines after preprocessing; imagine including all of that once for each translation unit which includes `map`. It also obviates the need for fancy build systems like CMake.

#### New features

Here are all the C99 features which `toc` depends on (I might have forgotten some...):

- Declare anywhere
- `inttypes.h`
- Non-constant struct literal initializers (e.g. `int x[2] = {y, z};`)
- Flexible array members

The last three of those could all be removed fairly easily.

And here are all of its C11 features:

- Anonymous structures/unions
- `max_align_t` and `alignof` - It can still compile without these but it won't technically be standard-compliant

#### More

See `main.c` for a bit more information.

---


\* for those curious, it has to do with `goto`. In C, this program:
<pre><code>
int main() {  
	goto label;  
	int x = 5;  
	label:  
	return 0;  
}
</code></pre>
Is completely fine. `x` will hold an unspecified value after the jump (but it isn't used so it doesn't really matter). Apparently, in C++, this is an ill-formed program. This is a bit ridiculous since
<pre><code>
int main() {  
	goto label;  
	int x; x = 5;  
	label:  
	return 0;  
}
</code></pre>
is fine. So that's an interesting little "fun fact": `int x = 5;` isn't always the same as `int x; x = 5;` in C++.
