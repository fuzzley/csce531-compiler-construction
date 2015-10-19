#include <stdio.h>

#define LEFT1 17
#define LEFT2 LEFT1
#define LEFT3 LEFT2
#define LEFT4 LEFT3
#define LEFT5 LEFT4
#define LEFT6 LEFT5

#define RIGHT1 2
#define RIGHT2 RIGHT1
#define RIGHT3 RIGHT2
#define RIGHT4 RIGHT3
#define RIGHT5 RIGHT4
#define RIGHT6 RIGHT5

#define FORK1 LEFT6
#define FORK2 FORK1
#define FORK3 FORK2

int i = FORK3;

#define FORK1 RIGHT6

int j = FORK3;

int main()
{
    printf("i = %d, j = %d\n", i,j);
    return 0;
}
