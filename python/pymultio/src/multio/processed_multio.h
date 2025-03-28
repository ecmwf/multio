struct multio_configuration_t;
typedef struct multio_configuration_t multio_configuration_t;
struct multio_metadata_t;
typedef struct multio_metadata_t multio_metadata_t;
struct multio_handle_t;
typedef struct multio_handle_t multio_handle_t;
struct multio_failure_info_t;
typedef struct multio_failure_info_t multio_failure_info_t;
enum MultioErrorValues
{
    MULTIO_SUCCESS = 0,
    MULTIO_ERROR_ECKIT_EXCEPTION = 1,
    MULTIO_ERROR_GENERAL_EXCEPTION = 2,
    MULTIO_ERROR_UNKNOWN_EXCEPTION = 3
};
const char* multio_error_string(int err);
const char* multio_error_string_info(int err, multio_failure_info_t* info);

typedef void (*multio_failure_handler_t)(void*, int error_code, multio_failure_info_t* info);

int multio_initialise();
int multio_version(const char** version);
int multio_vcs_version(const char** sha1);

int multio_new_configuration(multio_configuration_t** cc);
int multio_new_configuration_from_filename(multio_configuration_t** cc, const char* configuration_file_name);
int multio_delete_configuration(multio_configuration_t* cc);
int multio_config_set_path(multio_configuration_t* cc, const char* configuration_path);
int multio_config_set_failure_handler(multio_configuration_t* config, multio_failure_handler_t handler,
                                      void* usercontext);

int multio_mpi_allow_world_default_comm(multio_configuration_t* cc, bool allow);
int multio_mpi_parent_comm(multio_configuration_t* cc, int parent_comm);
int multio_mpi_return_client_comm(multio_configuration_t* cc, int* return_client_comm);
int multio_mpi_return_server_comm(multio_configuration_t* cc, int* return_server_comm);

int multio_new_handle(multio_handle_t** mio, multio_configuration_t* cc);
int multio_delete_handle(multio_handle_t* mio);
int multio_open_connections(multio_handle_t* mio);
int multio_close_connections(multio_handle_t* mio);

int multio_start_server(multio_configuration_t* cc);
int multio_flush(multio_handle_t* mio, multio_metadata_t* md);
int multio_notify(multio_handle_t* mio, multio_metadata_t* md);
int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size);
int multio_write_mask_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size);
int multio_write_mask_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);
int multio_write_field_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size);
int multio_write_field_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);
int multio_write_grib_encoded(multio_handle_t* mio, void* data, int size);
int multio_field_accepted(multio_handle_t* mio, const multio_metadata_t* md, bool* accepted);

int multio_new_metadata(multio_metadata_t** md, multio_handle_t* mio);
int multio_copy_metadata(multio_metadata_t** md, multio_metadata_t* mdFrom);
int multio_delete_metadata(multio_metadata_t* md);
int multio_metadata_set_int(multio_metadata_t* md, const char* key, int value);
int multio_metadata_set_string(multio_metadata_t* md, const char* key, const char* value);
int multio_metadata_set_bool(multio_metadata_t* md, const char* key, bool value);
int multio_metadata_set_double(multio_metadata_t* md, const char* key, double value);
