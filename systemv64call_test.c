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
 
should output:
123 456 789 101 112 -131 -415 -161 -718 19 999 888 2626183732628
2626183734690
1.414214
Hello 3.000000 -2848239.700000 -123 456 789 43873243.234982 111.100000 222.200000 333.300000 444.400000 555.500000 666.600000
126
foo returned: -298.100006
that's all!
*/

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>


typedef void (*FnPtr)();

extern uint64_t systemv64_call(FnPtr fn, void *args, int64_t nargs, bool *is_fp);
extern float systemv64_callf(FnPtr fn, void *args, int64_t nargs, bool *is_fp);
extern double systemv64_calld(FnPtr fn, void *args, int64_t nargs, bool *is_fp);
typedef struct {
	double x,y,z;
} Point;
extern Point systemv64_call_other(FnPtr fn, void *args, bool *is_fp, int64_t nargs);

#define arr_sz(x) (sizeof (x) / sizeof *(x))


float foo(double a, float b, int c, unsigned long long d) {
  return (float)a + b*(float)c - (float)d;
}
int main2(void) {
  double x = 3.4;
  uint64_t num = *(uint64_t *)&x;
  float y = 3.5f;
  uint64_t num2 = (uint64_t) *(uint32_t *)&y;
  uint64_t args[4] = {num, num2, (uint64_t)-73, 46};
  bool is_float[4] = {true, true, false, false};
  float ret = systemv64_callf((FnPtr)foo, args, 4, is_float);
  printf("foo returned: %f\n", ret);
  return 0;
}

long long bar(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, long long m) {
	printf("%d %d %d %d %d %d %d %d %d %d %d %d %lld\n", a,b,c,d,e,f,g,h,i,j,k,l,m);
	return a+b+c+d+e+f+g+h+i+j+k+l+m;
}

Point mkpoint(int a, int b, int c) {
	Point ret = {a,b,c};
	return ret;
}

void end(void) {
	puts("that's all!");
	exit(0);
}

int main(void) {
	uint64_t params[13] = {
		123,
		456,
		789,
		101,
		112,
		-131,
		-415,
		-161,
		-718,
		19,
		999,
		888,
		2626183732628LL
	};
	bool is_fp[arr_sz(params)] = {
		0,0,0,0,0,0,0,0,
		0,0,0,0,0
	};
	long long ret = (long long)systemv64_call((FnPtr)bar, params, arr_sz(params), is_fp);
	printf("%lld\n",ret);

	float two = 2.0f;
	uint64_t params2[] = {
		*(uint32_t *)&two
	};
	bool is_fp2[arr_sz(params)] = {1};
	float ret2 = systemv64_callf((FnPtr)sqrtf, params2, arr_sz(params2), is_fp2);
	printf("%f\n",ret2);


	double nums[] = {
		3.0,
		-2848239.7,
		43873243.234982,
		111.1,
		222.2,
		333.3,
		444.4,
		555.5,
		666.6,
		777.7,
		888.8,
		999.9
	};

	uint64_t params3[] = {
		(uint64_t)"Hello %f %f %d %d %d %f %f %f %f %f %f %f\n",
		*(uint64_t *)&nums[0],
		*(uint64_t *)&nums[1],
		-123,
		456,
		789,
		*(uint64_t *)&nums[2],
		*(uint64_t *)&nums[3],
		*(uint64_t *)&nums[4],
		*(uint64_t *)&nums[5],
		*(uint64_t *)&nums[6],
		*(uint64_t *)&nums[7],
		*(uint64_t *)&nums[8],
	
	};

	bool is_fp3[arr_sz(params3)] = {0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1};
	int ret3 = systemv64_call((FnPtr)printf, params3, arr_sz(params3), is_fp3);
	printf("%d\n",ret3);
	main2();
	systemv64_call(end, NULL, 0, NULL);
	puts("this shouldn't be printed");
	return 0;
}
