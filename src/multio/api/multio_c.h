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
 * \param sha1 Return variable for version control checksum. Returned pointer valid throughout program lifetime.
 * \returns Return code (#MultioErrorValues)
 */
int multio_vcs_version(const char** sha1);

/** @} */

/** \defgroup Data-routing */
/** @{ */

// TODO: Shall we allow passing in a configuration path here?
// int multio_new_handle(const char* configuration_path, multio_handle_t** multio);

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
 * \note This will be running until it receives a 'close' message from all the clients
 * \param server_name Name (key) of the subconfiguration for the server
 * \returns Return code (#MultioErrorValues)
 */
int multio_start_server(const char* server_name);

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


/** \defgroup Metadata setting */
/** @{ */

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

/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int_value(multio_metadata_t* md, const char* key, int value);
/** Sets a metadata key-value pair for string values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value C-string value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_string_value(multio_metadata_t* md, const char* key, const char* value);

/** @} */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
