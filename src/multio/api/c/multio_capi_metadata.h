#pragma once

#include <cstdint>
#include <cstddef>
#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \defgroup Metadata setting */
/** @{*/
/** Creates a multio metadata object
 * \param md Return a handle to the multio metadata object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_metadata(multio_metadata_t** md, multio_handle_t* mio);


/** Creates a multio metadata object from an existing metadata object
 * \param md Return a handle to the multio metadata object
 * \param mdFrom Metadata to copy from
 * \returns Return code (#MultioErrorValues)
 */
int multio_copy_metadata(multio_metadata_t** md, multio_metadata_t* mdFrom);


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




/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int8(multio_metadata_t* md, const char* key, int8_t value);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int16(multio_metadata_t* md, const char* key, int16_t value);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int32(multio_metadata_t* md, const char* key, int32_t value);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int64(multio_metadata_t* md, const char* key, int64_t value);



/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int8_array(multio_metadata_t* md, const char* key, int8_t* value, size_t count);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int16_array(multio_metadata_t* md, const char* key, int16_t* value, size_t count);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int32_array(multio_metadata_t* md, const char* key, int32_t* value, size_t count);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_int64_array(multio_metadata_t* md, const char* key, int64_t* value, size_t count);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_float_array(multio_metadata_t* md, const char* key, float* value, size_t count);


/** Sets a metadata key-value pair for integer values
 * \param md Handle to the multio metadata object
 * \param key C-string key to be set
 * \param value Integer value to be set
 * \returns Return code (#MultioErrorValues)
 */
int multio_metadata_set_double_array(multio_metadata_t* md, const char* key, double* value, size_t count);



/** @} */

#ifdef __cplusplus
} /* extern "C" */
#endif
