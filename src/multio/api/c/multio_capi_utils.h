#pragma once

#ifdef __cplusplus
extern "C" {
#endif
/** \defgroup Initialisation */
/** @{ */
/** Initialises API, must be called before any other function
 * \note This is only required if being used from a  where **eckit::Main()** is not otherwise
 * initialised.
 * \returns Return code (#MultioErrorValues)
 */
int multio_initialise();


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

#ifdef __cplusplus
} /* extern "C" */
#endif
