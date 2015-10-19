#include <stdio.h>

#define A C1
#define B C2
#define C1 C
#define C2 C
#define C D
#define D E
#define E F
#define F G
#define G H
#define H D

int A = 17;
int E = 18;
int F = 19;
int G = 20;
int H = 21;

int main()
{
    int i = A;
    int j = B;

        #define E A

    int k = F;

        #define E G

    int l = F;

	#define G F
	#define F B

    int A = F;

	#define G G

    int m = B;

        #define G F

    printf("C=%d, D=%d, E=%d, F=%d, G=%d, H=%d,\n", C, D, E, F, G, H);
    printf("i=%d, j=%d, k=%d, l=%d, m=%d\n", i, j, k, l, m);
    return 0;
}
