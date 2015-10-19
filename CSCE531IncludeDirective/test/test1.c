#define SOME_INT 17
#define SOME_STR "foobar"
#define SOME_ID foo
#define SOME_OTHER_ID SOME_INT
#define ID2 ID3
#define ID3 ID4
#define ID4 ID5
#define ID5 ID6
#define ID6 ID7
#define ID7 SOME_INT
#define CIRCLE3 CIRCLE4
#define CIRCLE1 CIRCLE2
#define CIRCLE5 CIRCLE6
#define CIRCLE2 CIRCLE3
#define CIRCLE4 CIRCLE5
#define CIRCLE6 CIRCLE4

int SOME_ID;
char *CIRCLE1, *CIRCLE2, *CIRCLE3, *CIRCLE4, *CIRCLE5, *CIRCLE6;
double SOME_ID;

#define CIRCLE6 SOME_STR

int main()
{
    s = CIRCLE1;
    SOME_ID = SOME_OTHER_ID;
    return ID2;
}
