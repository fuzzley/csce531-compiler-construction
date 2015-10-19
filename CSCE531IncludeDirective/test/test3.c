#define FOO BAR
#define BAT FAT

char *FOO = 0;

#define FAT 5
#define BAR BAT

int main()
{
    return FOO;
}
