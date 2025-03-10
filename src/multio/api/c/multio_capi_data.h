#pragma once

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \defgroup Data setting */
/** @{ */

/** Creates a multio data object
 * \param d Return a handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_new(multio_data_t** d, multio_handle_t* mio);


/** Deletes a multio data object
 * \param d Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_delete(multio_data_t* d);


/** Set to zero the data in the multio data object
 * \param d Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_zero(multio_data_t* d);


/** Resize the data in the multio data object
 * \param d Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_resize(multio_data_t* d, int new_size);


/** Returns the size of a multio data object
 * \param d Handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_size(multio_data_t* d, int* size);

/** Sets a float data into the multio data object in a specific position
 * \param d Handle to the multio data object
 * \param value Integer value to be set
 * \param pos position in which put the value
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_set_float_scalar(multio_data_t* d, float* value, int pos);


/** Sets a double data into the multio data object in a specific position
 * \param d Handle to the multio data object
 * \param value Integer value to be set
 * \param pos position in which put the value
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_set_double_scalar(multio_data_t* d, double* value, int pos);


/** Sets a float chunk of data into the multio data object in a specific position
 * \param d Handle to the multio data object
 * \param value Integer value to be set
 * \param pos position in which put the value
 * \param size of the chunk to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_set_float_chunk(multio_data_t* d, float* value, int pos, int size);


/** Sets a double chunk of data into the multio data object in a specific position
 * \param d Handle to the multio data object
 * \param value Integer value to be set
 * \param pos position in which put the value
 * \param size of the chunk to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_data_set_double_chunk(multio_data_t* d, double* value, int pos, int size);

/** @} */

#ifdef __cplusplus
} /* extern "C" */
#endif
