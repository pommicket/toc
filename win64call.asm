;;; Call C functions with a dynamic number of arguments x64 MSVC ;;;
;;; Written in NASM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is free and unencumbered software released into the public domain.
;; 
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;; 
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;; 
;; For more information, please refer to <http://unlicense.org/>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To compile this:
;;  first get nasm: https://nasm.us/ 
;;  add it to your path, then do:
;;   nasm -f win64 win64call.asm
;;  You will get win64call.obj
;;To use this in a C/C++ program:
;;  typedef void (*FnPtr)();
;;  extern unsigned long long win64_call(FnPtr fn, void *args, long long nargs);
;;  extern float win64_callf(FnPtr fn, void *args, long long nargs);
;;  extern double win64_calld(FnPtr fn, void *args, long long nargs);
;;  extern SomeType win64_call_other(FnPtr fn, void *args, long long nargs);
;; (all of these refer to the same actual function)
;; With MSVC's calling convention, all arguments are treated as if they were 64-bit.
;; This means you need to convert integer arguments to unsigned long long/uint64_t before using them.
;; So if you have integer arguments, you probably want to pass an unsigned long long * for args.
;; Floating point arguments can either be "reinterpreted" as unsigned long longs (see 2nd example), or 
;; you can pass a double *for args instead.
;; &((unsigned long long *)args)[i] should be a pointer to the ith argument.
;; If you have a 1, 2, 4, or 8 byte struct argument, convert it to an integer, then pass it (keep in mind that
;; if your struct is 8 bytes but not aligned to 8 bytes, the *(uint64_t *)&x trick will cause an unaligned read).
;; Otherwise, pass it by pointer.
;; For returning structs: if your type is 1, 2, 4, or 8 bytes and is POD, it will be returned as an integer.
;; Otherwise, you need to pass a pointer to the struct as the first argument.
;; for more info see https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention
;;So, if you want to call it:
;; simple example (integer arguments):
;;   int foo(int a, int b, int c) {
;;       return a+b+c;
;;   }
;;   int main() {
;;       FnPtr fn = (FnPtr)foo;
;;       unsigned long long args[3] = {
;;            -1000, -3, 65
;;       };
;;       int ret = (int)win64_call(fn, args, 3);
;;       printf("%d\n", ret);
;;   }
;; more involved example (with floating-point numbers):
;;   float bar(float a, double b, int c, double d, long e) {
;;       return a-(float)b + sinf((float)c) - (float)cos(d) + (float)e;
;;   }
;;   int main() {
;;       FnPtr fn = (FnPtr)bar;
;;       float a = -1.6f;
;;       double b = 3.0, d = 33.7;
;;       unsigned long long args[5] = {
;;            *(uint32_t *)&a, *(uint64_t *)&b, -12, *(uint64_t *)&d, 4
;;       };
;;       float ret = win64_callf(fn, args, 5);
;;       printf("%f\n", ret);
;;   }
;;Why might you need this?
;; Sometimes you don't know how many arguments you're gonna call a function with at compile time. 
;; For example, maybe you're making an interpreted programming language which calls out to C.
;; (python uses libffi but that's not easy to compile...)
;; Usually it will be with a function pointer you got from GetProcAddress.
;;If you find a bug...
;; Please let me know by emailing pommicket@pommicket.com.

global win64_call
global win64_callf
global win64_calld
global win64_call_other

section .text

; takes:
; rcx - fn - pointer to function
; rdx - args - pointer to arguments
; r8 - nargs - number of arguments
win64_call:
win64_callf:
win64_calld:
win64_call_other:
	; use "shadow store" to save rsi
	mov [rsp+24], rsi
	mov rax, rcx ; function pointer (rcx may be overwritten)
	mov r11, rdx ; args (rdx may be overwritten)
	mov r10, r8 ; index_of_argument
	mov rsi, rsp ; save original stack pointer
	
	; we need to make sure the stack pointer is aligned to 16 bytes when the function is called.
	; for some reason, even though we have to align it or stuff breaks, sometimes when
	; our function is called, it's not 16-byte aligned :/
	; find number of stack arguments:
	cmp r8, 4
	jg .align_stack
	mov r8, 0   ; if there are <=4 arguments, set the number of stack arguments to 0
.align_stack:
	and r8, 1 ; is the number of stack arguments even or odd?
	lea r8, [rsp+8*r8]
	; r8 is now equivalent to where the stack pointer will be (mod 16) when we call the function
	and r8, 0xf ; take r8 mod 16
	sub rsp, r8 ; align the stack pointer so when we call the function it's 16-byte aligned
	lea r11, [r11+8*r10] ; go to end of arguments--we go from right to left
	            ; because that's the order things are pushed onto the stack

	cmp r10, 0
	je .loop_end ; no arguments
.loop:
	dec r10 ; --index_of_argument
	sub r11, 8 ; --arg
	cmp r10, 0
	jg .after_1st
; NOTE: we have to set both the integer and floating-point register for every argument because
;  a. we don't know if it's integer or floating point
;  b. varargs expects to have the value in both registers
	; 1st argument
	mov rcx, qword [r11]
	movsd xmm0, qword [r11]
	jmp .continue
.after_1st:
	cmp r10, 1
	jg .after_2nd
	; 2nd argument
	mov rdx, qword [r11]
	movsd xmm1, qword [r11]
	jmp .continue
.after_2nd:
	cmp r10, 2
	jg .after_3rd
	; 3rd argument
	mov r8, qword [r11]
	movsd xmm2, qword [r11]
	jmp .continue
.after_3rd:
	cmp r10, 3
	jg .after_4th
	; 4th argument
	mov r9, qword [r11]
	movsd xmm3, qword [r11]
	jmp .continue
.after_4th:
	; additional argument
	push qword [r11]
.continue:
	cmp r10, 0 ; if index_of_argument > 0
	jg .loop
.loop_end:
	sub rsp, 32 ; "shadow store"
	call rax ; function pointer stored here before
	
	mov rsp, rsi ; restore original stack pointer
	; restore rsi
	mov rsi, [rsp+24]
	ret
