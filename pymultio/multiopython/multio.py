import os

from .lib import ffi, lib
#from .config import Config
from .handler import Handler
from .metadata import Metadata
"""
def error_handling(func):
    def Inner_Function(*args, **kwargs):
        retval = func(*args, **kwargs)
        if retval not in (
            lib.MULTIO_SUCCESS,
        ):
            error_str = "Error in function {}: {}".format(func.__name__, ffi.string(lib.multio_error_string(retval)))
            raise MultioException(error_str)
    return Inner_Function
"""
class Multio:
    """This is the main interface class for Multio that users will interact with"""

    def __init__(self, config_path=None, allow_world=None, parent_comm=None, client_comm=None, server_comm=None):

        self.__conf = _Config(config_path=config_path, allow_world=allow_world, parent_comm=parent_comm, client_comm=client_comm, server_comm=server_comm)

        self.__handle = Handler(self.__conf)

        self.__metadata = None

    def __version__(self):
        tmp_str = ffi.new("char**")
        lib.multio_version(tmp_str)
        versionstr = ffi.string(tmp_str[0]).decode("utf-8")
        return versionstr

    def set_config_path(self, conf_path):
        self.__conf.set_config_path(conf_path)

    def start_server(self):
        self.__conf.start_server()

    def create_metadata(self, md=None):

        self.__metadata = Metadata(self.__handle, md=md)

    def open_connections(self):
        self.__handle.open_connections()

    def close_connections(self):
        self.__handle.close_connections()

    def flush(self):
        if self.__metadata is not None:
            self.__handle.flush(self.__metadata)
        else:
            raise AttributeError(f"No metadata object instantiated")

    def notify(self):
        if self.__metadata is not None:
            self.__handle.notify(self.__metadata)
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_domain(self, data):
        if self.__metadata is not None:
            self.__handle.write_domain(self.__metadata, data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_mask(self, data):
        if self.__metadata is not None:
            self.__handle.write_mask_float(self.__metadata, data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_field(self, data):
        if self.__metadata is not None:
            self.__handle.write_field_float(self.__metadata, data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def field_accepted(self):
        return self.__handle.field_accepted(self.__metadata)


class _Config:
    """This is the main container class for Multio Configs"""

    def __init__(self, config_path=None, allow_world=None, parent_comm=None, client_comm=None, server_comm=None):
        self.__config_path = config_path

        config = ffi.new("multio_configuration_t**")
        if self.__config_path != None:
            configuration_file_name = ffi.new("char[]", os.fsencode(self.__config_path))
            error = lib.multio_new_configuration_from_filename(config, configuration_file_name)
            print(error)
        else:
            lib.multio_new_configuration(config)

        # Set free function
        self.__config = ffi.gc(config[0], lib.multio_delete_configuration)

        if allow_world is not None:
            self.conf_mpi_allow_world_default_comm(allow_world)

        if parent_comm is not None:
            self.conf_mpi_parent_comm(parent_comm)

        if client_comm is not None:
            self.conf_mpi_return_client_comm(client_comm)

        if server_comm is not None:
            self.conf_mpi_return_server_comm(server_comm)

    def set_conf_path(self, conf_path):

        configuration_file_name = ffi.new("char[]", os.fsencode(conf_path))
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
