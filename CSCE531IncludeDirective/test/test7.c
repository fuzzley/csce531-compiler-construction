#define   	   EENIE	"  	 	    \n"    
	#define    MEENIE   	+17
  #define          meinie       -17
#define		   moe          "#define FOO 6"

char *s = EENIE;
char *t = moe;
int i = MEENIE;
int j = meinie;
int k = FOO;

int main()
{
    #define ID1 ID2
    int i = ID1;
    #define ID2 ID3
    int j = ID1;
    #define ID1 "ID2"
    char *k = ID1;
}
