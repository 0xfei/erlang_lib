#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "comm.h"

#define LOG_FILE "einterface.log"

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
            res = strlen((char*)buf + 1);
        } else if (fn == 2) {
        	split_string((char*)buf + 1, &s1, &s2);
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
