#ifndef multio_api_multio_c_H
#define multio_api_multio_c_H

#ifdef __cplusplus
extern "C" {
#endif

/** Human readable release version e.g. 1.2.3 */
int multio_version(const char** version);

/** Version under VCS system, typically a git sha1. Not useful for computing software dependencies. */
int multio_vcs_version(const char** sha1);

enum MultioErrorValues {
    MULTIO_SUCCESS                  = 0,
    MULTIO_ERROR_ECKIT_EXCEPTION    = 1,
    MULTIO_ERROR_GENERAL_EXCEPTION  = 2,
    MULTIO_ERROR_UNKNOWN_EXCEPTION  = 3
};

struct multio_metadata_t;
typedef struct multio_metadata_t multio_metadata_t;

struct multio_handle_t;
typedef struct multio_handle_t multio_handle_t;

// TODO: Shall we allow passing in a configuration path here?
// int multio_new_handle(const char* configuration_path, multio_handle_t** multio);
int multio_new_handle(multio_handle_t** multio);
int multio_delete_handle(multio_handle_t* multio);

// TODO: Shall we allow passing in a configuration path here?
// int multio_start_server(const char* configuration_path);
int multio_start_server();

int multio_open_connections(multio_handle_t* multio);
int multio_close_connections(multio_handle_t* multio);

int multio_write_step_complete(multio_handle_t* multio);
int multio_write_domain(multio_handle_t*, const char* dname, int* data, int size);
int multio_write_mask(multio_handle_t*, const char* mname, const double* data, int size);
int multio_write_field(multio_handle_t*, const char* fname, const double* data, int size);


int multio_new_metadata(multio_metadata_t** metadata);
int multio_delete_metadata(multio_metadata_t* metadata);

int multio_metadata_set_int_value(multio_metadata_t* metadata, const char* key, int value);
int multio_metadata_set_string_value(multio_metadata_t* metadata, const char* key,
                                     const char* value);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
