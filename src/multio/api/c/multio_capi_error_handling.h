#pragma once

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif
/** \defgroup Error Handling */
/** @{*/
/** Returns a human-readable error message for the last error given an error code
 * \param err Error code (#MultioErrorValues)
 * \returns Error message
 */
const char* multio_error_string(int err);

/** Returns a human-readable error message for the last error given an error code
 * \param err Error code (#MultioErrorValues)
 * \param info Pointer to additional, handle/instance specific error information provided through failure handler.
 * Behaves like multio_error_string if null is provided. \returns Error message
 */
const char* multio_error_string_info(int err, multio_failure_info_t* info);

/** Error handler callback function signature
 * \param usercontext User defined context to pass through
 * \param error_code Error code (#MultioErrorValues)
 * \param info Object with information about the error. Used with multio_error_string_info.
 */
typedef void (*multio_failure_handler_t)(void* usercontext, int error_code, multio_failure_info_t* info);

/** Sets an error handler which will be called on error with the supplied usercontext and an error code
 * \param config Configuration object on which to set up the failure handler.
 * \param handler C-Function pointer to the failure handling function
 * \param  usercontext Any data the user wish to pass to the failure handler. Can be null.
 */
int multio_config_set_failure_handler(multio_configuration_t* config, multio_failure_handler_t handler,
                                      void* usercontext);

/** @} */
#ifdef __cplusplus
} /* extern "C" */
#endif