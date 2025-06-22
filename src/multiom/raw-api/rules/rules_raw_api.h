#ifndef MULTIO_GRIB2_RULES_H
#define MULTIO_GRIB2_RULES_H

int multio_grib2_rules_open(
    void*       options,
    void**      handle,
    const char* fname,
    int         len
);

int multio_grib2_rules_close(
    void** handle
);

int multio_grib2_rules_serch(
    void*  handle,
    void*  mars_dict,
    char** rule_name
);

#endif