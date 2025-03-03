#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

// Convert "rwxr-xr-x" style string to mode_t
mode_t parse_mode(const char* mode_str) {
    mode_t mode = 0;

    if (strlen(mode_str) != 9)
        return -1;  // Invalid length

    // User permissions
    if (mode_str[0] == 'r')
        mode |= S_IRUSR;
    if (mode_str[1] == 'w')
        mode |= S_IWUSR;
    if (mode_str[2] == 'x')
        mode |= S_IXUSR;

    // Group permissions
    if (mode_str[3] == 'r')
        mode |= S_IRGRP;
    if (mode_str[4] == 'w')
        mode |= S_IWGRP;
    if (mode_str[5] == 'x')
        mode |= S_IXGRP;

    // Others permissions
    if (mode_str[6] == 'r')
        mode |= S_IROTH;
    if (mode_str[7] == 'w')
        mode |= S_IWOTH;
    if (mode_str[8] == 'x')
        mode |= S_IXOTH;

    return mode;
}

// Create directory with symbolic mode
int make_directory_sym(const char* path, const char* mode_str) {
    mode_t mode = parse_mode(mode_str);
    if (mode == (mode_t)-1) {
        return 1;  // Invalid mode
    }

    if (mkdir(path, mode) != 0) {
        return 2;  // Error creating directory
    }

    if (chmod(path, mode) != 0) {
        return 3;
    }

    return 0;
}

int is_directory(const char* path) {
    struct stat s;
    if (stat(path, &s) == 0 && S_ISDIR(s.st_mode))
        return 1;
    return 0;
}

int make_directory(const char* path, const char* mode_str) {

    int err = mkdir(path, parse_mode(mode_str));

    return err;
}

int set_perm(const char* path, const char* mode_str) {
    mode_t mode = parse_mode(mode_str);
    if (mode == (mode_t)-1) {
        return 1;
    }

    if (chmod(path, mode) != 0) {
        return 2;
    }

    return 0;
}