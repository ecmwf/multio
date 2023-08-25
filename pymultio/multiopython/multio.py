import os

from .lib import ffi, lib
#from .handler import Handler
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

        handle = ffi.new("multio_handle_t**")
        lib.multio_new_handle(handle, self.__conf.get_pointer())

        self.__handle = ffi.gc(handle[0], lib.multio_delete_handle)

        self.__metadata = None

    def __enter__(self):
        self.open_connections()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close_connections()

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

    # This function may be removed as now __enter__ opens connections, but may want to keep this functionality for users?
    def open_connections(self):
        lib.multio_open_connections(self.__handle)

    # This function may be removed as now __exit__ closes connections, but may want to keep this functionality for users?
    def close_connections(self):
        lib.multio_close_connections(self.__handle)

    def flush(self):
        if self.__metadata is not None:
            lib.multio_flush(self.__handle, self.__metadata.get_pointer())
        else:
            raise AttributeError(f"No metadata object instantiated")

    def notify(self):
        if self.__metadata is not None:
            lib.multio_notify(self.__handle, self.__metadata.get_pointer())
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_domain(self, data):
        if self.__metadata is not None:
            data = ffi.new(f'int[{len(data)}]', data)
            size = ffi.cast("int", len(data))
            lib.multio_write_domain(self.__handle, self.__metadata.get_pointer(), data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_mask(self, data):
        if self.__metadata is not None:
            data = ffi.new(f'float[{len(data)}]', data)
            size = ffi.cast("int", len(data))
            lib.multio_write_mask_float(self.__handle, self.__metadata.get_pointer(), data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def write_field(self, data):
        if self.__metadata is not None:
            data = ffi.new(f'float[{len(data)}]', data)
            size = ffi.cast("int", len(data))
            lib.multio_write_field_float(self.__handle, self.__metadata.get_pointer(), data, len(data))
        else:
            raise AttributeError(f"No metadata object instantiated")

    def field_accepted(self):
        accepted = False
        accept = ffi.new("bool*", accepted)
        lib.multio_field_accepted(self.__handle, self.__metadata.get_pointer(), accept)
        return bool(accept[0])



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
            self.mpi_allow_world_default_comm(allow_world)

        if parent_comm is not None:
            self.mpi_parent_comm(parent_comm)

        if client_comm is not None:
            self.mpi_return_client_comm(client_comm)

        if server_comm is not None:
            self.mpi_return_server_comm(server_comm)

    def set_conf_path(self, conf_path):

        configuration_file_name = ffi.new("char[]", os.fsencode(conf_path))
        lib.multio_conf_set_path(self.__config, configuration_file_name)

    def mpi_allow_world_default_comm(self, allow=0):

        multio_allow = ffi.cast("_Bool", allow)
        lib.multio_conf_mpi_allow_world_default_comm(self.__config, multio_allow)

    def mpi_parent_comm(self, parent_comm=0):

        multio_par_comm = ffi.cast("int", parent_comm)
        lib.multio_conf_mpi_parent_comm(self.__config, multio_par_comm)

    def mpi_return_client_comm(self, return_client_comm):

        multio_rcc = ffi.new("int[]", return_client_comm)
        lib.multio_conf_mpi_return_client_comm(self.__config, multio_rcc)

    def mpi_return_server_comm(self, return_server_comm):

        multio_rsc = ffi.new("int[]", return_server_comm)
        lib.multio_conf_mpi_return_client_comm(self.__config, multio_rsc)

    def start_server(self):
        # TODO: Need to test
        lib.multio_start_server(self.__config)

    def get_pointer(self):
        return self.__config
