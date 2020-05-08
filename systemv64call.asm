;;; Call SystemV x64 functions dynamically
;;; Written in NASM
;;; This implements the SystemV calling convention (which is used by Linux and OS X) so that
;;; you can call functions with a variable (i.e. not known at compile time) number of arguments.
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
;;   nasm -f elf64 systemv64call.asm
;;  You will get systemv64call.o
;;You can use it like this (in C/C++):
;;  typedef void (*FnPtr)();
;;  extern unsigned long systemv64_call(FnPtr fn, void *args, long nargs, bool *is_float);
;;  extern float systemv64_callf(FnPtr fn, void *args, long nargs, bool *is_float);
;;  extern double systemv64_calld(FnPtr fn, void *args, long nargs, bool *is_float);
;;  extern SomeType systemv64_call_other(FnPtr fn, void *args, long nargs, bool *is_float);
;; (all of these refer to the same actual function)
;; Let's say you want to call the function:
;;  float foo(double a, float b, int c, unsigned long long d) {
;;    return (float)a + b*(float)c - (float)d;
;;  }
;; with the arguments a = 3.4, b = 3.5f, c = -73, d = 46.
;; First, you need to convert all the arguments to a single data type, so that
;; they can call be stored in one array. You should probably convert them all to
;; as unsigned long/uint64_ts (you can also do all doubles). For integers, you can just
;; cast them to unsigned long/uint64_t. For floating point numbers, you need to do something
;; a bit dubious:
;;  double x = 3.4;
;;  uint64_t num = *(uint64_t *)&x;
;;  float y = 3.5f;
;;  uint64_t num2 = (uint64_t) *(uint32_t *)&y;
;; (The cast to uint64_t on that last line is unnecessary, unless you have very strict warnings on.)
;; Now store them all in an array:
;;  uint64_t args[4] = {num, num2, (uint64_t)-73, 46};
;; Finally, you will need an is_float array. is_float[i] is true if the ith argument is floating-point,
;; and false if it's an integer/pointer. So in this case it would be:
;;  bool is_float[4] = {true, true, false, false};
;; (if you are using C89, you can use unsigned char instead of bool, 0 instead of false, and 1 instead of true)
;; Now use systemv64_callf (since the function returns a float). 
;;  float ret = systemv64_callf((FnPtr)foo, args, is_float, 4);
;;  printf("foo returned: %f\n", ret);
;; Here's the full code:
;;  extern float systemv64_callf(FnPtr fn, void *args, bool *is_float, long nargs);
;;  float foo(double a, float b, int c, unsigned long long d) {
;;    return (float)a + b*(float)c - (float)d;
;;  }
;;  int main(void) {
;;    double x = 3.4;
;;    uint64_t num = *(uint64_t *)&x;
;;    float y = 3.5f;
;;    uint64_t num2 = (uint64_t) *(uint32_t *)&y;
;;    uint64_t args[4] = {num, num2, (uint64_t)-73, 46};
;;    bool is_float[4] = {true, true, false, false};
;;    float ret = systemv64_callf((FnPtr)foo, args, 4, is_float);
;;    printf("foo returned: %f\n", ret);
;;    return 0;
;;  }
;;Why might you need this?
;; Sometimes you don't know how many arguments you're gonna call a function with at compile time. 
;; For example, maybe you're making an interpreted programming language which calls out to C.
;; Usually it will be with a function pointer you got from dlsym.
;;If you find a bug,
;; Please email pommicket@pommicket.com
global systemv64_call
global systemv64_callf
global systemv64_calld
global systemv64_call_other

; rdi - fn - function pointer
; rsi - args - arguments to the function
; rdx - nargs - number of arguments 
; rcx - is_float - is each argument floating point?
systemv64_call:
systemv64_callf:
systemv64_calld:
systemv64_call_other:
	sub rsp, 32
	mov [rsp+24], rbx ; save non-volatile register values
	mov [rsp+16], rbp
	mov [rsp+8], r12
	mov [rsp], r13

	mov rbp, rsp ; save stack pointer
	mov r13, rdi ; save function pointer

	; this section here calculates the number of floating point and integer arguments
	mov r10, 0 ; integer index
	mov r11, 0 ; floating point index
	mov rbx, 0 ; arg index
.fp_loop:
	cmp rbx, rdx ; if arg_index >= nargs
	jge .fp_loop_end
	cmp byte [rcx], 0
	jne .fp_loop_float
	; integer argument
	add r10, 1
	jmp .fp_continue
	.fp_loop_float:
	add r11, 1
	.fp_continue:
	inc rbx ; ++arg_index
	inc rcx ; ++is_float
	jmp .fp_loop
.fp_loop_end:

	; r10 now holds the number of integer arguments, and r11 holds the number of floating point arguments

	; we need to calculate the number of stack arguments so we can align the stack properly
	mov rbx, 0 ; num_stack_args
	lea r12, [r10-6] ; number of integer args
	cmp r12, 0
	jle .skip_int
	add rbx, r12 ; add int stack args
	.skip_int:
	lea r12, [r11-8] ; number of float args
	cmp r12, 0
	jle .skip_flt
	add rbx, r12 ; add float stack args
	.skip_flt:

	; align the stack
	lea rbx, [rsp+8*rbx] ; where rsp will be after we push the arguments
	and rbx, 0xf ; calculate future alignment
	sub rsp, rbx ; align the stack

	mov rax, r11 ; save number of floating point arguments (needs to be in rax, at least for varargs. I'm not sure why)
	; we go right to left because that's the order stuff's put on the stack
	lea r12, [rsi+8*rdx] ; arg = &args[nargs]
	mov rbx, rcx ; is_arg_fp = &is_fp[nargs]
.loop:
	; if r10 (int index) and r11 (float index) are both 0, ...
	cmp r10, 0
	jne .skip_fp_check
	cmp r11, 0
	je .loop_end ; ... break out of the loop
.skip_fp_check:
	sub r12, 8 ; --arg
	dec rbx ; --is_arg_fp
	
	cmp byte [rbx], 0 ; is it float?
	jne .float

	dec r10 ; --int_arg_index

	cmp r10, 0
	jg .after_1st
	; 1st integer argument
	mov rdi, qword [r12]
	jmp .loop
.after_1st:
	cmp r10, 1
	jg .after_2nd
	; 2nd int argument
	mov rsi, qword [r12]
	jmp .loop
.after_2nd:
	cmp r10, 2
	jg .after_3rd
	; 3rd int argument
	mov rdx, qword [r12]
	jmp .loop
.after_3rd:
	cmp r10, 3
	jg .after_4th
	; 4th int argument
	mov rcx, qword [r12]
	jmp .loop
.after_4th:
	cmp r10, 4
	jg .after_5th
	; 5th int argument
	mov r8, qword [r12]
	jmp .loop
.after_5th:
	cmp r10, 5
	jg .stack_arg
	; 6th int argument
	mov r9, qword [r12]
	jmp .loop

.float:
	dec r11 ; --float_arg_index
	cmp r11, 0
	jg .after_1stf
	; 1st float argument
	movsd xmm0, qword [r12]
	jmp .loop
.after_1stf:
	cmp r11, 1
	jg .after_2ndf
	; 2nd float argument
	movsd xmm1, qword [r12]
	jmp .loop
.after_2ndf:
	cmp r11, 2
	jg .after_3rdf
	; 3rd float argument
	movsd xmm2, qword [r12]
	jmp .loop
.after_3rdf:
	cmp r11, 3
	jg .after_4thf
	; 4th float argument
	movsd xmm3, qword [r12]
	jmp .loop
.after_4thf:
	cmp r11, 4
	jg .after_5thf
	; 5th float argument
	movsd xmm4, qword [r12]
	jmp .loop
.after_5thf:
	cmp r11, 5
	jg .after_6thf
	; 6th float argument
	movsd xmm5, qword [r12]
	jmp .loop
.after_6thf:
	cmp r11, 6
	jg .after_7thf
	; 7th float argument
	movsd xmm6, qword [r12]
	jmp .loop
.after_7thf:
	cmp r11, 7
	jg .stack_arg
	; 8th float argument
	movsd xmm7, qword [r12]
	jmp .loop
.stack_arg:
	; argument pushed on the stack (>6th integer, >8th float argument)
	push qword [r12]
	jmp .loop
.loop_end:

	call r13
	
	mov rsp, rbp ; restore stack pointer (stored here at top of function)
	mov r13, [rsp]; restore non-volatile register values
	mov r12, [rsp+8]
	mov rbp, [rsp+16]
	mov rbx, [rsp+24]
	add rsp, 32
	ret
