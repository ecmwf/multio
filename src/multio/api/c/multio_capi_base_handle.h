#pragma once

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Creates a multio (client) instance without an configuration. Behaves like passing a default configuration directly
 * to the handle without making further customizations (configuration file read through environment variable). \param
 * mio Return a handle to the multio (client) instance \returns Return code (#MultioErrorValues)
 */
int multio_new_handle_default(multio_handle_t** mio);


/** Creates a multio (client) instance
 * \param cc Handle to configuration (if null, a default handle will be created)
 * \param mio Return a handle to the multio (client) instance
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_handle(multio_handle_t** mio, multio_configuration_t* cc);


/** Deletes a multio (client) instance
 * \param mio Handle to the multio (client) instance to be deleted
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_handle(multio_handle_t* mio);


/** Sets an error handler which will be called on error with the supplied usercontext and an error code
 * If the failure handler has been already set on the multio_configuration_t it is reused.
 * \param mio MultIO client handle on which to set up the failure handler.
 * \param handler C-Function pointer to the failure handling function
 * \param  usercontext Any data the user wish to pass to the failure handler. Can be null.
 */
int multio_handle_set_failure_handler(multio_handle_t* mio, multio_failure_handler_t handler, void* usercontext);


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

#ifdef __cplusplus
} /* extern "C" */
#endif
