#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fillcstr( char* fstring, char** cstring ) {
    int len = strlen(fstring);
    *cstring = (char*) malloc(len);
    if ( *cstring == NULL ) {
        return 1;
    }
    strcpy(*cstring, fstring);
    return 0;
}

