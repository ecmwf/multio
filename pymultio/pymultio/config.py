import os

from .lib import ffi, lib

class Config:
    """This is the main container class for Multio Configs"""

    def __init__(self, config_path=''):
        self.__config_path = config_path

        config = ffi.new("multio_configuration_t**")
        configuration_file_name = ffi.new("char[]", self.__config_path.encode('ascii'))
        if os.environ.get('MULTIO_SERVER_CONFIG_FILE'):
            lib.multio_new_configuration(config)
        else:
            lib.multio_new_configuration_from_filename(config, configuration_file_name)

        # Set free function
        self.__config = ffi.gc(config[0], lib.multio_delete_configuration)

    def set_conf_path(self, conf_path):

        configuration_file_name = ffi.new("char[]", conf_path.encode('ascii'))
        lib.multio_conf_set_path(self.__config, configuration_file_name)

    def conf_mpi_allow_world_default_comm(self, allow=0):

        multio_allow = ffi.cast("_Bool", allow)
        lib.multio_conf_mpi_allow_world_default_comm(self.__config, multio_allow)

    def conf_mpi_parent_comm(self, parent_comm=0):

        multio_par_comm = ffi.cast("int", parent_comm)
        lib.multio_conf_mpi_parent_comm(self.__config, multio_par_comm)

    def conf_mpi_return_client_comm(self, return_client_comm):

        multio_rcc = ffi.new("int[]", return_client_comm)
        lib.multio_conf_mpi_return_client_comm(self.__config, multio_rcc)

    def conf_mpi_return_server_comm(self, return_server_comm):

        multio_rsc = ffi.new("int[]", return_server_comm)
        lib.multio_conf_mpi_return_client_comm(self.__config, multio_rsc)

    def start_server(self):
        # TODO: Currently does not work
        lib.multio_start_server(self.__config)

    def get_pointer(self):
        return self.__config


