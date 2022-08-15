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

/** Retrieves version control checksum of the latest change, e.g. ``a88011c007a0db48a5d16e296934a197eac2050a``
 * \param version Return variable for version control checksum. Returned pointer valid throughout program lifetime.
 * \returns Return code (#OdcErrorValues)
 */
int multio_vcs_version(const char** sha1);

/** @} */

/** API */
/** @{ */


// TODO: Shall we allow passing in a configuration path here?
/** Creates a multio (client) instance
 * \param configuration_path Path to YAML configuration file
 * \param mio Return a handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_handle_from_config(multio_handle_t** multio, const char* configuration_path);

/** Creates a multio (client) instance
 * \param mio Return a handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_handle(multio_handle_t** mio);
/** Deletes a multio (client) instance
 * \param mio Handle to the multio (client) instance to be deleted
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_handle(multio_handle_t* mio);

// TODO: Shall we allow passing in a configuration path here?
// int multio_start_server(const char* configuration_path);

/** Initialises and starts server
 * \note This will be running until it receives a 'close' message from all of clients
 * \returns Return code (#MultioErrorValues)
 */
int multio_start_server();

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
/** Creates a multio metadata object by parsing a YAML/JSON string
 * \param md Return a handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_metadata_from_yaml(multio_metadata_t** md, const char* yaml_json_str);
/** Deletes a multio metadata object
 * \param md Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_metadata(multio_metadata_t* md);
/** Resets/clears a multio metadata object to make the allocated memory reusable efficiently
 * 
 * TODO: Discuss - std::move is also used very often for efficiency. Everything is fine for sequentially processing.
 * Problems could arise if at one end the data is not copy and instead accessed in an asynchronous manner.
 * 
 * \param md Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_reset_metadata(multio_metadata_t* md);

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
/** Sets a metadata key-value pair for recursive metadata maps.
 * 
 * \todo Do we need nested metadata?
 * 
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value metadata value (ordered map) to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_map_value(multio_metadata_t* md, const char* key, multio_metadata_t*);
/** Sets a metadata key-value pair for recursive metadata maps directly parsed from yaml/json.
 * 
 * \todo discuss - is this required? I just used it for the nemo c api test to read in some metadata... but that's actually not required
 * 
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value yaml/json string parsed from
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_map_value_from_yaml(multio_metadata_t* md, const char* key, const char* yaml_json_str);

/**
 * 
 * \todo Do we need arrays in metadata? Are there use cases for that? If so, are single type arrays fine or should we support mixed type arrays (however we would express that with c...)
 */



#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
