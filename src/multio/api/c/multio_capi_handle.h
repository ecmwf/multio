#pragma once

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \ingroup Data-routing */
/** @{*/

/** Initialises and starts server
 *
 * \note This will be running until it receives a 'close' message from all of clients
 * \param cc Handle to configuration
 * \returns Return code (#MultioErrorValues)
 */
int multio_start_server(multio_configuration_t* cc);


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


/** Writes static metadata information to clients and all servers
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_parametrization(multio_handle_t* mio, multio_metadata_t* md);

/** Writes static metadata information to clients and all servers
 * \param mio Handle to the multio (client) instance
 * \param key Key/Name for the data array
 * \param data Pointer to the data containing the index mapping
 * \param size Size of the data containing the index mapping
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_parametrization_int32_array(multio_handle_t* mio, const char* key, const int32_t* data, int size);
int multio_write_parametrization_int64_array(multio_handle_t* mio, const char* key, const int64_t* data, int size);
int multio_write_parametrization_float_array(multio_handle_t* mio, const char* key, const float* data, int size);
int multio_write_parametrization_double_array(multio_handle_t* mio, const char* key, const double* data, int size);


/** Writes domain information (e.g. local-to-global index mapping) to the server
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the domain
 * \param data Pointer to the data containing the index mapping
 * \param size Size of the data containing the index mapping
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_domain_int64(multio_handle_t* mio, multio_metadata_t* md, int64_t* data, int size);
int multio_write_domain_int32(multio_handle_t* mio, multio_metadata_t* md, int32_t* data, int size);


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


/** Legacy: Writes (partial) fields already grib encoded
 * \param mio Handle to the multio (client) instance
 * \param gribdata Pointer to grib message
 * \param gribsize Length of the grib message in number of bytes
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_grib_encoded(multio_handle_t* mio, void* gribdata, int gribsize);


/** Writes (partial) fields
 * \param mio Handle to the multio (client) instance
 * \param md Metadata information about the field
 * \param d Pointer to the (eckit::Buffer) data containing the (partial) field values
 * \param byte_size Size of the packed data (i.e.4 for float 8 for double)
 * \returns Return code (#MultioErrorValues)
 */
int multio_write_field_buffer(multio_handle_t* mio, multio_metadata_t* md, multio_data_t* d, int byte_size);


/** Determines if the pipelines are configured to accept the specified data
 *
 * \param mio Handle to the multio (client) instance
 * \param md Name of the field
 * \param accepted Pointer to a boolean to store the result
 * \returns Return code (#MultioErrorValues)
 */
int multio_field_accepted(multio_handle_t* mio, const multio_metadata_t* md, bool* accepted);
/** @} */

#ifdef __cplusplus
} /* extern "C" */
#endif
