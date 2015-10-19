#include <stdio.h>

int i;
long l;

int main()
{
	printf("int = %d\nlong = %d\n", sizeof(int), sizeof(long));
	i = 0x80000000;
	printf("i = %d\n", i);
	return 0;
}
