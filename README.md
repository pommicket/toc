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

To compile the compiler on a Unix-y system, just run `./build.sh release` (or `make release`). You can supply a compiler by running `CC=tcc ./build.sh release`, or build it in debug mode without the `release`. To disable compile time foreign function support (which you will need to do if you don't have ffcall/dl), prefix this with `COMPILE_TIME_FOREIGN_FN_SUPPORT=no`.

On Windows, first make sure the compiler works from the command line. You can follow the instructions here:
https://docs.microsoft.com/en-us/cpp/build/building-on-the-command-line .
You can run `build32.bat release` or `build64.bat release` depending on whether you want a 32-bit or 64-bit version of toc. 

On other systems, you can just compile main.c with a C compiler. 
By default, toc will use libdl and libffcall. If you don't have these installed, add the preprocessor define `COMPILE_TIME_FOREIGN_FN_SUPPORT=0`.
This is usually done with `-DCOMPILE_TIME_FOREIGN_FN_SUPPORT=0`.

`toc` uses several C99 and a couple of C11 features, so it might not work on all compilers. But it does compile on quite a few, including `clang`, `gcc`, `tcc`, and MSVC. It can also be compiled as if it were C++, so and `g++` can also compile it (it does rely on implicit casting of  `void *` though). The *outputted* code should be C99-compliant.

#### Why it compiles to C

`toc` compiles to C. Here are some reasons why:

- Speed. C is one of the most performant programming languages out there. It also has compilers which are very good at optimizing (better than anything I could write). 
- Portability. C is probably the most portable language. It has existed for >30 years and can run on practically anything. Furthermore, all major languages nowadays can call functions written in C.

---

### `toc` Compiler Source Code

Most of the source code for the `toc` compiler is licensed under the GNU General Public License, version 3, and the rest (some small general utilities) is in the public domain. Each source file begins with a comment explaining its license.

See `LICENSE` for the GNU General Public License.

`toc` is written in C, for speed and portability. It has no dependencies, other than the C runtime library. If you want to be able to call external C functions at compile time, however, you will need `libffcall` and `libdl` (so this is only currently supported on Unix-y systems).

#### Build system
`toc` is set up as a unity build, meaning that there is only one translation unit. So, `main.c` `#include`s `toc.c`, which `#include`s all of `toc`'s files.
##### Why?
This improves compilation speeds (especially from scratch), since you don't have to include headers a bunch of times for each translation unit. This is more of a problem in C++, where, for example, doing `#include <map>` ends up turning into 25,000 lines after preprocessing. All of toc's source code, which includes most of the C standard library, at the time of this writing (Dec 2019) is only 22,000 lines after preprocessing; imagine including all of that once for each translation unit which includes `map`. It also obviates the need for fancy build systems like CMake.

#### "New" features

Here are all the C99 features which `toc` depends on (I might have forgotten some...):

- Declare anywhere
- `inttypes.h`
- Non-constant struct literal initializers (e.g. `int x[2] = {y, z};`)
- Flexible array members
- `snprintf`

And here are all of its (mandatory) C11 features:

- Anonymous structures/unions

#### More

See `main.c` for a bit more information.

---

### Version history

Here are the major versions of `toc`.

<table>
<tr><th>Version</th><th>Description</th><th>Date</th></tr>
<tr><td>0.0</td><td>Initial version.</td><td>2019 Dec 6</td></tr>
<tr><td>0.1</td><td>Constant parameter inference.</td><td>2019 Dec 15</td></tr>
<tr><td>0.2</td><td>Foreign functions and <code>#include</code>.</td><td>2020 Jan 29</td></tr>
<tr><td>0.3</td><td><code>struct</code> parameters</td><td>2020 Feb 25</td></tr>
<tr><td>0.4</td><td>Variadic functions</td><td>2020 Mar 13</td></tr>
<tr><td>0.5</td><td><code>break</code>, <code>continue</code>, <code>defer</code></td><td>2020 Mar 19</td></tr>
<tr><td>0.6</td><td><code>use</code></td><td>2020 Apr 26</td></tr>
</table>

---

### Report a bug

If you find a bug, you can report it through [GitHub's issue tracker](https://github.com/pommicket/toc/issues), or by emailing pommicket at pommicket dot com.

Just send me the `toc` source code which results in the bug, and I'll try to fix it. 
