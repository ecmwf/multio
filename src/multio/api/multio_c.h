#ifndef multio_api_multio_c_H
#define multio_api_multio_c_H

#ifdef __cplusplus
extern "C" {
#endif

/** \defgroup Error Handling */
/** @{ */

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
const char* multio_error_string(int err);

/** Error handler callback function signature
 * \param context Error handler context
 * \param error_code Error code (#MultioErrorValues)
 */
typedef void (*multio_failure_handler_t)(void* context, int error_code);

/** Sets an error handler which will be called on error with the supplied context and an error code
 * \param handler Error handler function
 * \param context Error handler context
 */
int multio_set_failure_handler(multio_failure_handler_t handler, void* context);

/** @} */

/** Types */

///@{
struct multio_configurationcontext_t;
typedef struct multio_configurationcontext_t multio_configurationcontext_t;

struct multio_metadata_t;
typedef struct multio_metadata_t multio_metadata_t;

struct multio_handle_t;
typedef struct multio_handle_t multio_handle_t;

///@}

/** \defgroup Initialisation */
/** @{ */

/** Initialises API, must be called before any other function
 * \note This is only required if being used from a context where **eckit::Main()** is not otherwise
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

/** Retrieves version control checksum of the latest change, e.g.
 * ``a88011c007a0db48a5d16e296934a197eac2050a`` \param version Return variable for version control
 * checksum. Returned pointer valid throughout program lifetime. \returns Return code
 * (#OdcErrorValues)
 */
int multio_vcs_version(const char** sha1);

/** @} */

/** API */
/** @{*/
      
/** Creates a multio configuration context object with default configuration file name (environment variable: MULTIO_SERVER_CONFIG_FILE)
 * \param md Return a handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_configurationcontext(multio_configurationcontext_t** cc);
      
/** Creates a multio configuration context object with custom configuration file name
 * \param configuration_file_name Absolute path to the YAML configuration file
 * \param cc Return a handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_configurationcontext_from_filename(multio_configurationcontext_t** cc, const char* configuration_file_name);

/** Deletes a multio configuration context object
 * \param md Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_configurationcontext(multio_configurationcontext_t* cc);
      
      
/** Sets the configuration path which some components might use to read configuration files (default: environment variable: MULTIO_SERVER_CONFIG_PATH)
 * \param configuration_path Absolute path to configuration file folder - may be used in subcomponents?
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_set_path(multio_configurationcontext_t* cc, const char* configuration_path);
      

/** Overwrite global MPI options for default splitting.
 *
 * \param allow Specifies if multio is supposed to use the WORLD communicator as default if a group has not been added to eckit::mpi yet.
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_allow_world_default_comm(multio_configurationcontext_t* cc, bool allow);


/** Overwrite global MPI options for default splitting.
 *
 * \param color Specifies the color with which multio is allowed to split from the parent group to create a client only intra communicator.
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_split_client_color(multio_configurationcontext_t* cc, int color);


/** Overwrite global MPI options for default splitting.
 *
 * \param color Specifies the color with which multio is allowed to split from the parent group to create a server only intra communicator.
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_split_server_color(multio_configurationcontext_t* cc, int color);
      
/** Set MPI specific initalization parameters
 *
 * \param parent_comm Parent MPI intra communicator containing all servers and clients.
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_parent_comm(multio_configurationcontext_t* cc, int parent_comm);
      
/** Set MPI specific initalization parameters
 *
 * \param return_client_comm Pointer to an integer specifying the client communicator that the multio may set on initialization
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_return_client_comm(multio_configurationcontext_t* cc, int* return_client_comm);
      
/** Set MPI specific initalization parameters
 *
 * \param return_server_comm Pointer to an integer specifying the server communicator that the multio may set on initialization
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_return_server_comm(multio_configurationcontext_t* cc, int* return_server_comm);
      
/** Set MPI specific initalization parameters
 *
 * \param client_id  String containing the client id (provided for backwards compatibility). 
 * \param cc Handle to the multio configuration context object
 * \returns Return code (#MultioErrorValues)
 */
int multio_conf_mpi_client_id(multio_configurationcontext_t* cc, const char* client_id);
      
      
// /** Creates a multio (client) instance
//  * \param configuration_path Path to YAML configuration file
//  * \param mio Return a handle to the multio (client) instance
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_new_handle_from_config(multio_handle_t** multio, const char* configuration_path);
      
// /** Creates a multio (client) instance and allows passing a MPI communicator as well as an clientID.
//  * This function is provided for compatibility with existing API bindings to other IO servers.
//  * 
//  * Attention: The used configuration file (provided by MULTIO_SERVER_CONFIG_FILE) must also specify MPI as transport layer. 
//  * Then the provided communicator can be used.
//  *
//  * \param retComm MPI communicator for clients created by MULTIO. Multio will split a client and a server communicator from the passed parentComm. The client communicator will be written to retComm. This requires clientId to be non-null.
//  * \param parentComm MPI parent communicator containing all clients and servers
//  * \param clientId  Null or string containing the client id (provided for backwards compatibility). By using eckit::mpi, a global [communicator name -> communicator] map is maintained. The clientId should be unique in the sense that no other library registers a communicator with the same name.
//  * \param mio Return a handle to the multio (client) instance
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_new_handle_mpi(multio_handle_t** multio, const char* clientId, int parentComm, int* retComm);


// /** Creates a multio (client) instance
//  * \param mio Return a handle to the multio (client) instance
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_new_handle(multio_handle_t** mio);
      
/** Creates a multio (client) instance
 * \param cc Handle to configuration context
 * \param mio Return a handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_handle(multio_handle_t** multio, multio_configurationcontext_t* cc);


/** Deletes a multio (client) instance
 * \param mio Handle to the multio (client) instance to be deleted
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_handle(multio_handle_t* mio);


// /** Initialises and starts server
//  * \note This will be running until it receives a 'close' message from all of clients
//  * \param server_name_key Name of the server as listed in the yaml configuration file
//  * \param configuration_path Path to the YAML configuration file
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_start_server_from_config(const char* configuration_path, const char* server_name_key);

// /** Initialises and starts server
//  * \note This will be running until it receives a 'close' message from all of clients
//  *
//  * \param parent_comm Parent MPI intra communicator containing all servers and clients.
//  * \param server_name_key Name of the server as listed in the yaml configuration file
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_start_server_mpi(const char* server_name_key, int parent_comm);

// /** Initialises and starts server
//  * \note This will be running until it receives a 'close' message from all of clients
//  * \param server_name_key Name of the server as listed in the yaml configuration file
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_start_server(const char* server_name_key);

/** Initialises and starts server
 * \note This will be running until it receives a 'close' message from all of clients
 * \param cc Handle to configuration context
 * \param server_name_key Name of the server as listed in the yaml configuration file
 * \returns Return code (#MultioErrorValues)
 */
int multio_start_server(multio_configurationcontext_t* cc, const char* server_name_key);


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


/** Indicates that a given step is complete
 * \note Can be used for checkpointing
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_step_complete(multio_handle_t* mio, multio_metadata_t* md);


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
 * \param data Pointer to the data containing the masking values
 * \param size Size of the data containing the masking values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);


/** Writes (partial) fields
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the field
 * \param data Pointer to the data containing the (partial) field values
 * \param size Size of the data containing the (partial) field values
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_field(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);


/** @} */


/** Creates a multio metadata object
 * \param md Return a handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_metadata(multio_metadata_t** md);


/** Deletes a multio metadata object
 * \param md Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_metadata(multio_metadata_t* md);


// /** Resets/clears a multio metadata object to make the allocated memory reusable efficiently
//  *
//  * TODO: Discuss - std::move is also used very often for efficiency. Everything is fine for
//  * sequentially processing. Problems could arise if at one end the data is not copy and instead
//  * accessed in an asynchronous manner.
//  *
//  * \param md Handle to the multio metadata object
//  * \returns Return code (#MultioErrorValues)
//  */
// int multio_reset_metadata(multio_metadata_t* md);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int_value(multio_metadata_t* md, const char* key, int value);


/** Sets a metadata key-value pair for long values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Long value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_long_value(multio_metadata_t* md, const char* key, long value);


/** Sets a metadata key-value pair for long long values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Long long value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_longlong_value(multio_metadata_t* md, const char* key, long long value);


/** Sets a metadata key-value pair for string values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value C-string value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_string_value(multio_metadata_t* md, const char* key, const char* value);


/** Sets a metadata key-value pair for boolean values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Boolean/logical value
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_bool_value(multio_metadata_t* md, const char* key, bool value);


/** Sets a metadata key-value pair for float values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value float value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_float_value(multio_metadata_t* md, const char* key, float value);


/** Sets a metadata key-value pair for double values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value double value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_double_value(multio_metadata_t* md, const char* key, double value);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
