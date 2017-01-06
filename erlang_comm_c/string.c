#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LOG_FILE    "string.log"

typedef unsigned char byte;

int read_exact(byte* buf, int len);
int write_exact(byte* buf, int len);

int read_cmd(byte* buf)
{
    int len;
    if (read_exact(buf, 2) != 2)
        return -1;
    len = (buf[0] << 8) | buf[1];
    return read_exact(buf, len);
}

int write_cmd(byte* buf, int len)
{
    byte li;

    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    li = len & 0xff;
    write_exact(&li, 1);

    return write_exact(buf, len);
}

int read_exact(byte* buf, int len)
{
    int i, got = 0;
    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return i;
        got += i;
    } while (got < len);

    return len;
}

int write_exact(byte* buf, int len)
{
    int i, wrote = 0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return i;
        wrote += i;
    } while (wrote < len);

    return len;
}

void split_string(char* s, char** s1, char** s2)
{
	*s1 = s;
	*s2 = s + strlen(s) + 1;	
}

int main()
{
    int fn, res, len, i;
    char *s1, *s2;
    
	byte buf[100] = {0};
    
    FILE* f = fopen(LOG_FILE, "w");
    if (f == NULL)
    	return -1;

    while ( (len=read_cmd(buf)) > 0) {
        fn = buf[0];
        
        fprintf(f, "Data: ");
        for (i = 0; i < len; ++i)
        	fprintf(f, "0x%02x ", buf[i]);
        fprintf(f, "\n");
        
        if (fn == 1) {
            res = strlen(buf + 1);
        } else if (fn == 2) {
        	split_string(buf + 1, &s1, &s2);
        	fprintf(f, "s1: %s, s2: %s\n", s1, s2);
            res = strcmp(s1,s2);
        }

        buf[0] = res;
        write_cmd(buf, 1);
        memset(buf, 0, sizeof(buf));
    }

	fclose(f);
	
    return 0;
}
