#pragma once

#include "multio_capi_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/** Creates a multio configuration  object with default configuration file name (environment variable:
 * MULTIO_SERVER_CONFIG_FILE) \param cc Return a handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_new_configuration(multio_configuration_t** cc);


/** Creates a multio configuration  object with custom configuration file name
 * \param configuration_file_name Absolute path to the YAML configuration file
 * \param cc Return a handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_new_configuration_from_filename(multio_configuration_t** cc, const char* configuration_file_name);


/** Deletes a multio configuration  object
 * \param cc Handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_delete_configuration(multio_configuration_t* cc);


/** Sets the configuration path which some components might use to read configuration files (default: environment
 * variable: MULTIO_SERVER_CONFIG_PATH) \param configuration_path Absolute path to configuration file folder - may be
 * used in subcomponents? \param cc Handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_conf_set_path(multio_configuration_t* cc, const char* configuration_path);


/** Overwrite global MPI options for default splitting.
 *
 * \param allow Specifies if multio is supposed to use the WORLD communicator as default if a group has not been added
 * to eckit::mpi yet. \param cc Handle to the multio configuration  object \returns Return code
 * (#MultioErrorValues)
 */
int multio_mpi_allow_world_default_comm(multio_configuration_t* cc, bool allow);


/** Set MPI specific initalization parameters
 *
 * \param parent_comm Parent MPI intra communicator containing all servers and clients.
 * \param cc Handle to the multio configuration  object
 * \returns Return code (#MultioErrorValues)
 */
int multio_mpi_parent_comm(multio_configuration_t* cc, int parent_comm);


/** Set MPI specific initalization parameters
 *
 * \param return_client_comm Pointer to an integer specifying the client communicator that the multio may set on
 * initialization \param cc Handle to the multio configuration  object \returns Return code (#MultioErrorValues)
 */
int multio_mpi_return_client_comm(multio_configuration_t* cc, int* return_client_comm);


/** Set MPI specific initalization parameters
 *
 * \param return_server_comm Pointer to an integer specifying the server communicator that the multio may set on
 * initialization \param cc Handle to the multio configuration  object \returns Return code (#MultioErrorValues)
 */
int multio_mpi_return_server_comm(multio_configuration_t* cc, int* return_server_comm);


#ifdef __cplusplus
} /* extern "C" */
#endif