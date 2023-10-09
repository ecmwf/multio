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

/** @} */
#ifdef __cplusplus
} /* extern "C" */
#endif
