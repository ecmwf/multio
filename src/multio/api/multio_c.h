#ifndef multio_api_multio_c_H
#define multio_api_multio_c_H

#ifdef __cplusplus
extern "C" {
#endif

/** Human readable release version e.g. 1.2.3 */
int multio_version(const char** version);

/** Version under VCS system, typically a git sha1. Not useful for computing software dependencies. */
int multio_vcs_version(const char** version);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
