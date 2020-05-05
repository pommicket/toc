/* 
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>

Output should be:
-12387387222 54873482123 4598345 -4
1 returned: 54873482123
Hello 5 6 7 8 9
Hello 5 6 7 8 9 hey 19249488282934 -1
2 returned: 38
3 returned: 1.414214
4 returned: 1.414214
5.600000 -1.300000 2.000000 -6 2.000000 55 1200.000000
5 returned: 4.300000
6 returned: -1.600000
7 returned: -938
8 returned: 0.590889
*/


#include <stdlib.h>
#include <stdio.h>
#include <windows.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

typedef void (*FnPtr)();
extern uint64_t win64_call(FnPtr fn, void *args, int64_t nargs);
extern float win64_callf(FnPtr fn, void *args, int64_t nargs);
extern double win64_calld(FnPtr fn, void *args, int64_t nargs);
float asdf(float a, double b, int c, double d, long e, float f) {
    return (float)f;
}
void foobar(void) {
    FnPtr fn = (FnPtr)asdf;
    float a = -1.6f;
    double b = 3.0, d = 33.7;
    unsigned long long args[6] = {
         *(uint32_t *)&a, *(uint64_t *)&b, -12, *(uint64_t *)&d, 4, *(uint32_t *)&a
    };
    float ret = win64_callf(fn, args, 6);
    printf("6 returned: %f\n", ret);
}

uint64_t test_fn(long long a, uint64_t b, uint64_t c, int d) {
	printf("%lld %llu %llu %d\n",a,b,c,d);
	return b;
}

double test_fp(double a, double b, float c, int d, float e, int f, double g) {
	printf("%f %f %f %d %f %d %f\n", a,b,c,d,e,f,g);
	return a+b;
}
#define arr_size(a) (sizeof (a) / sizeof *(a))


int foo(int a, int b, int c) {
    return a+b+c;
}
void main1(void) {
    FnPtr fn = (FnPtr)foo;
    unsigned long long args[3] = {
         -1000, -3, 65
    };
    int ret = (int)win64_call(fn, args, 3);
    printf("7 returned: %d\n", ret);
}
float bar(float a, double b, int c, double d, long e) {
    return a-(float)b + sinf((float)c) - (float)cos(d) + (float)e;
}
void main2(void) {
    FnPtr fn = (FnPtr)bar;
    float a = -1.6f;
    double b = 3.0, d = 33.7;
    unsigned long long args[5] = {
         *(uint32_t *)&a, *(uint64_t *)&b, -12, *(uint64_t *)&d, 4
    };
    float ret = win64_callf(fn, args, 5);
    printf("8 returned: %f\n", ret);
}

int main(void) {
	uint64_t args[] = {-12387387222, 54873482123, 4598345, -4};
	uint64_t x = win64_call(test_fn, args, arr_size(args));
	printf("1 returned: %llu\n", x);

	printf("Hello %d %d %d %d %d\n",5,6,7,8,9);


	uint64_t args2[] = {(uint64_t)"Hello %d %d %d %d %d %s %llu %d\n", 5, 6, 7, 8, 9, (uint64_t)"hey", 19249488282934, -1};
	uint64_t x2 = win64_call(printf, args2, arr_size(args2));
	printf("2 returned: %llu\n",x2);

#if 0
	{
		float a = 3.7f;
		int b = 1;
		int c = 2;
		int d = 3;
		float e = 4.5f;
		double f = -1.2;
		float ret = test_fp(a,b,c,d,e,f);
		printf("%f\n",ret);
	}
#endif
	
	double args3[] = {2.0};
	double x3 = win64_calld(sqrt, args3, arr_size(args3));
	printf("3 returned: %f\n",x3);

	float two = 2.0f;

	uint64_t args4[] = {*(uint32_t *)&two};
	float x4 = win64_callf(sqrtf, args4, arr_size(args4));
	printf("4 returned: %f\n",x4);
	
	double num1 = 5.6;
	double num2 = -1.3;
	double num3 = 1200;
	uint64_t args5[] = {
		*(uint64_t *)&num1,
		*(uint64_t *)&num2,
		*(uint32_t *)&two,
		-6, 
		*(uint32_t *)&two,
		55,
		*(uint64_t *)&num3

	};
	double x5 = win64_calld(test_fp, args5, arr_size(args5));
	printf("5 returned: %f\n",x5);
	
	foobar();
	main1();
	main2();

	return 0;

}
