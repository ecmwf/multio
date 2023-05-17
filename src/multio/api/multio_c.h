#pragma once

#ifdef __cplusplus
extern "C" {
#endif

/** Types */

///@{
struct multio_configuration_t;
typedef struct multio_configuration_t multio_configuration_t;

struct multio_metadata_t;
typedef struct multio_metadata_t multio_metadata_t;

struct multio_handle_t;
typedef struct multio_handle_t multio_handle_t;

struct multio_failure_info_t;
typedef struct multio_failure_info_t multio_failure_info_t;

///@}

/** \defgroup Error Handling */
/** @{*/

/** Return codes */
enum MultioErrorValues
{
    /** The function completed successfully */
    MULTIO_SUCCESS = 0,
    /** A known ** *eckit::Exception* was encountered. Call ``multio_error_string()`` with the
       returned code for details. */
    MULTIO_ERROR_ECKIT_EXCEPTION = 1,
    /** A known ** *std::exception* was encountered. Call ``multio_error_string()`` with the
       returned code for details. */
    MULTIO_ERROR_GENERAL_EXCEPTION = 2,
    /** An unexpected or unknown error was encountered. Call ``multio_error_string()`` with the
       returned code for details. */
    MULTIO_ERROR_UNKNOWN_EXCEPTION = 3
};

/** Returns a human-readable error message for the last error given an error code
 * \param err Error code (#MultioErrorValues)
 * \returns Error message
 */
const char* multio_error_string_global(int err);
      
/** Returns a human-readable error message for the last error given an error code
 * \param err Error code (#MultioErrorValues)
 * \param info Pointer to additional error information provided through failure handler. Behaves like multio_error_string_global if null is provided.
 * \returns Error message
 */
const char* multio_error_string(int err, multio_failure_info_t* info);

/** Error handler callback function signature
 * \param Error handler (user defined context to pass through)
 * \param error_code Error code (#MultioErrorValues)
 */
typedef void (*multio_failure_handler_t)(void*, int error_code, multio_failure_info_t* info);

/** Sets an error handler which will be called on error with the supplied  and an error code
 * \param handler Error handler function
 * \param  Error handler
 */
int multio_set_failure_handler(multio_configuration_t* conf, multio_failure_handler_t handler, void*);

/** @} */


/** \defgroup Initialisation */
/** @{ */

/** Initialises API, must be called before any other function
 * \note This is only required if being used from a  where **eckit::Main()** is not otherwise
 * initialised.
 * \returns Return code (#MultioErrorValues)
 */
int multio_initialise();

/** @} */

/** \defgroup Version Accessors */
/** @{ */

/** Retrieves the release version of the library in human-readable format, e.g. ``1.3.0``
 * \param version Return variable for release version. Returned pointer valid throughout program
 * lifetime.
 * \returns Return code (#MultioErrorValues)
 */
int multio_version(const char** version);

/** Retrieves version control checksum of the latest change, e.g. ``a88011c007a0db48a5d16e296934a197eac2050a``
 * \param sha1 Return variable for version control checksum. Returned pointer valid throughout program lifetime.
 * \returns Return code (#MultioErrorValues)
 */
int multio_vcs_version(const char** sha1);

/** @} */

/** \defgroup Data-routing */
/** @{ */

/** Creates a multio configuration  object with default configuration file name (environment variable:
 * MULTIO_SERVER_CONFIG_FILE) \param cc Return a handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_new_configuration(multio_configuration_t** cc);

/** Creates a multio configuration  object with custom configuration file name
 * \param configuration_file_name Absolute path to the YAML configuration file
 * \param cc Return a handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_configuration_from_filename(multio_configuration_t** cc, const char* configuration_file_name);

/** Deletes a multio configuration  object
 * \param cc Handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_configuration(multio_configuration_t* cc);


/** Sets the configuration path which some components might use to read configuration files (default: environment
 * variable: MULTIO_SERVER_CONFIG_PATH) \param configuration_path Absolute path to configuration file folder - may be
 * used in subcomponents? \param cc Handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_conf_set_path(multio_configuration_t* cc, const char* configuration_path);


/** Overwrite global MPI options for default splitting.
 *
 * \param allow Specifies if multio is supposed to use the WORLD communicator as default if a group has not been added
 * to eckit::mpi yet. \param cc Handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_conf_mpi_allow_world_default_comm(multio_configuration_t* cc, bool allow);


/** Set MPI specific initalization parameters
 *
 * \param parent_comm Parent MPI intra communicator containing all servers and clients.
 * \param cc Handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_parent_comm(multio_configuration_t* cc, int parent_comm);

/** Set MPI specific initalization parameters
 *
 * \param return_client_comm Pointer to an integer specifying the client communicator that the multio may set on
 * initialization \param cc Handle to the multio configuration  object \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_return_client_comm(multio_configuration_t* cc, int* return_client_comm);

/** Set MPI specific initalization parameters
 *
 * \param return_server_comm Pointer to an integer specifying the server communicator that the multio may set on
 * initialization \param cc Handle to the multio configuration  object \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_return_server_comm(multio_configuration_t* cc, int* return_server_comm);

/** Set MPI specific initalization parameters
 *
 * \param client_id  String containing the client id (provided for backwards compatibility).
 * \param cc Handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_client_id(multio_configuration_t* cc, const char* client_id);

/** Creates a multio (client) instance
 * \param cc Handle to configuration
 * \param mio Return a handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_handle(multio_handle_t** mio, multio_configuration_t* cc);


/** Deletes a multio (client) instance
 * \param mio Handle to the multio (client) instance to be deleted
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_handle(multio_handle_t* mio);


/** Initialises and starts server
 *
 * \note This will be running until it receives a 'close' message from all of clients
 * \param cc Handle to configuration
 * \returns Return code (#MultioErrorValues)
 */
int multio_start_server(multio_configuration_t* cc);

/** Opens connections to the server
 * \note This will open connections to all processes associated with the server
 * \returns Return code (#MultioErrorValues)
 */
int multio_open_connections(multio_handle_t* mio);


/** Closes connections to the server
 * \note This will close connections to all processes associated with the server
 * \param mio Handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_close_connections(multio_handle_t* mio);


/** Indicates all servers that a given step is complete
 * \note Can be used for checkpointing
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \returns Return code (#MultioErrorValues)
 */
int multio_flush(multio_handle_t* mio, multio_metadata_t* md);


/** Notifies all servers (e.g. step notification)
 *  and potentially performs triggers on sinks.
 * \note Can be used for checkpointing
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \returns Return code (#MultioErrorValues)
 */
int multio_notify(multio_handle_t* mio, multio_metadata_t* md);


/** Writes domain information (e.g. local-to-global index mapping) to the server
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \param data Pointer to the data containing the index mapping
 * \param size Size of the data containing the index mapping
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size);


/** Writes masking information (e.g. land-sea mask) to the server
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the mask
 * \param data Pointer to the (float) data containing the masking values
 * \param size Size of the data containing the masking values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_mask_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size);


/** Writes masking information (e.g. land-sea mask) to the server
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the mask
 * \param data Pointer to the (double) data containing the masking values
 * \param size Size of the data containing the masking values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_mask_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);


/** Writes (partial) fields
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the field
 * \param data Pointer to the (float) data containing the (partial) field values
 * \param size Size of the data containing the (partial) field values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_field_float(multio_handle_t* mio, multio_metadata_t* md, const float* data, int size);


/** Writes (partial) fields
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the field
 * \param data Pointer to the (double) data containing the (partial) field values
 * \param size Size of the data containing the (partial) field values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_field_double(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);


/** @} */


/** \defgroup Metadata setting */
/** @{ */

/** Creates a multio metadata object
 * \param md Return a handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_metadata(multio_metadata_t** md, multio_handle_t* mio);


/** Deletes a multio metadata object
 * \param md Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_metadata(multio_metadata_t* md);

/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int(multio_metadata_t* md, const char* key, int value);


/** Sets a metadata key-value pair for long values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Long value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_long(multio_metadata_t* md, const char* key, long value);


/** Sets a metadata key-value pair for long long values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Long long value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_longlong(multio_metadata_t* md, const char* key, long long value);


/** Sets a metadata key-value pair for string values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value C-string value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_string(multio_metadata_t* md, const char* key, const char* value);

/** @} */

/** Sets a metadata key-value pair for boolean values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Boolean/logical value
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_bool(multio_metadata_t* md, const char* key, bool value);


/** Sets a metadata key-value pair for float values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value float value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_float(multio_metadata_t* md, const char* key, float value);


/** Sets a metadata key-value pair for double values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value double value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_double(multio_metadata_t* md, const char* key, double value);


/** Determines if the pipelines are configured to accept the specified data
 *
 * \param mio Handle to the multio (client) instance
 * \param md Name of the field
 * \param accepted Pointer to a boolean to store the result
 * \returns Return code (#MultioErrorValues)
 */
int multio_field_accepted(multio_handle_t* mio, const multio_metadata_t* md, bool* accepted);

#ifdef __cplusplus
} /* extern "C" */
#endif
