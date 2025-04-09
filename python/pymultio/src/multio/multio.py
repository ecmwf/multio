import importlib.util
import os

from .lib import ffi, lib
from .metadata import Metadata

numpy_spec = importlib.util.find_spec("numpy")
haveNumpy = numpy_spec is not None

if haveNumpy:
    import numpy as np


class _Config:
    """This is the main container class for Multio Configs"""

    def __init__(self, config_path, allow_world, parent_comm, client_comm, server_comm):
        self.__config_path = config_path

        config = ffi.new("multio_configuration_t**")
        if self.__config_path is not None:
            configuration_file_name = ffi.new("char[]", os.fsencode(self.__config_path))
            error = lib.multio_new_configuration_from_filename(config, configuration_file_name)
            print(error)
        else:
            lib.multio_new_configuration(config)

        # Set free function
        self.config_pointer = ffi.gc(config[0], lib.multio_delete_configuration)

        if allow_world is not None:
            self.mpi_allow_world_default_comm(allow_world)

        if parent_comm is not None:
            self.mpi_parent_comm(parent_comm)

        if client_comm is not None:
            self.mpi_return_client_comm(client_comm)

        if server_comm is not None:
            self.mpi_return_server_comm(server_comm)

    def mpi_allow_world_default_comm(self, allow=0):
        multio_allow = ffi.cast("_Bool", allow)
        lib.multio_mpi_allow_world_default_comm(self.config_pointer, multio_allow)

    def mpi_parent_comm(self, parent_comm=0):
        multio_par_comm = ffi.cast("int", parent_comm)
        lib.multio_mpi_parent_comm(self.config_pointer, multio_par_comm)

    def mpi_return_client_comm(self, return_client_comm):
        multio_rcc = ffi.new("int[]", return_client_comm)
        lib.multio_mpi_return_client_comm(self.config_pointer, multio_rcc)

    def mpi_return_server_comm(self, return_server_comm):
        multio_rsc = ffi.new("int[]", return_server_comm)
        lib.multio_mpi_return_client_comm(self.config_pointer, multio_rsc)


class Multio:
    """
    This is the main interface class for Multio that users will interact with. It takes in
    a config file and creates a multio handle, through this class a user can write data
    and interact with the multio c api.

    Parameters:
        config_path(str|file): A file-like object to where a plan is found.
                               If not provided MULTIO_SERVER_CONFIG_FILE is checked
        allow_world(bool): Overwrite global MPI options for default splitting.
        parent_comm(array): Set MPI specific initalization parameters for parent comm.
        client_comm(array): Set MPI specific initalization parameters for client comm.
        server_comm(array): Set MPI specific initalization parameters for server comm.

    """

    def __init__(self, config_path=None, allow_world=None, parent_comm=None, client_comm=None, server_comm=None):
        self.__conf = _Config(
            config_path=config_path,
            allow_world=allow_world,
            parent_comm=parent_comm,
            client_comm=client_comm,
            server_comm=server_comm,
        )

        handle = ffi.new("multio_handle_t**")
        lib.multio_new_handle(handle, self.__conf.config_pointer)

        self._handle = ffi.gc(handle[0], lib.multio_delete_handle)

        self.__dummy_metadata_field = Metadata(self, md={})
        self.__dummy_metadata_domain = Metadata(self, md={})
        self.__dummy_metadata_mask = Metadata(self, md={})
        self.__dummy_metadata_flush = Metadata(self, md={})
        self.__dummy_metadata_notification = Metadata(self, md={})

    def open_connections(self):
        lib.multio_open_connections(self._handle)

    def close_connections(self):
        lib.multio_close_connections(self._handle)


    def __version__(self):
        tmp_str = ffi.new("char**")
        lib.multio_version(tmp_str)
        versionstr = ffi.string(tmp_str[0]).decode("utf-8")
        return versionstr

    def __check_metadata(self, metadata, dummy_metadata=None):
        if metadata is None:
            if dummy_metadata is None:
                return Metadata(self, md={})
            else:
                return dummy_metadata
        elif isinstance(metadata, dict):
            return Metadata(self, md=metadata)
        elif isinstance(metadata, Metadata):
            return metadata
        else:
            raise TypeError(f"Can not handle type {type(metadata)} as metadata")

    def start_server(self):
        lib.multio_start_server(self.__conf)

    def flush(self, metadata=None):
        """
        Indicates all servers that a given step is complete
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
        """
        md = self.__check_metadata(metadata, self.__dummy_metadata_flush)
        lib.multio_flush(self._handle, md._handle)

    def notify(self, metadata):
        """
        Notifies all servers (e.g. step notification)
        and potentially performs triggers on sinks.
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
        """
        md = self.__check_metadata(metadata, self.__dummy_metadata_notification)
        lib.multio_notify(self._handle, md._handle)

    def write_domain(self, metadata, data):
        """
        Writes domain information (e.g. local-to-global index mapping) to the server
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
            data(array): Data of a single type usable by multio in the form an array
        """
        md = self.__check_metadata(metadata, self.__dummy_metadata_domain)

        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and isinstance(data, np.ndarray) and (data.dtype == np.int_):
            intArr = ffi.from_buffer("int*", data)
            lib.multio_write_domain(self._handle, md._handle, intArr, sizeInt)
        else:
            intArr = ffi.new(f"int[{size}]", data)
            lib.multio_write_domain(self._handle, md._handle, intArr, sizeInt)

    def write_mask(self, metadata, data):
        """
        Writes masking information (e.g. land-sea mask) to the server
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
            data(array): Data of a single type usable by multio in the form an array
        """
        md = self.__check_metadata(metadata, self.__dummy_metadata_mask)

        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and isinstance(data, np.ndarray) and ((data.dtype == np.float32) or (data.dtype == np.float64)):
            if data.dtype == np.float32:
                floatArr = ffi.from_buffer("float*", data)
                lib.multio_write_mask_float(self._handle, md._handle, floatArr, sizeInt)
            else:
                doubleArr = ffi.from_buffer("double*", data)
                lib.multio_write_mask_double(self._handle, md._handle, doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f"double[{size}]", data)
            lib.multio_write_mask_double(self._handle, md._handle, doubleArr, sizeInt)

    def write_field(self, metadata, data):
        """
        Writes fields
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
            data(array): Data of a single type usable by multio in the form an array
        """
        md = self.__check_metadata(metadata, self.__dummy_metadata_field)

        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and isinstance(data, np.ndarray) and ((data.dtype == np.float32) or (data.dtype == np.float64)):
            if data.dtype == np.float32:
                floatArr = ffi.from_buffer("float*", data)
                lib.multio_write_field_float(self._handle, md._handle, floatArr, sizeInt)
            else:
                doubleArr = ffi.from_buffer("double*", data)
                lib.multio_write_field_double(self._handle, md._handle, doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f"double[{size}]", data)
            lib.multio_write_field_double(self._handle, md._handle, doubleArr, sizeInt)

    def field_accepted(self, metadata):
        """
        Determines if the pipelines are configured to accept the specified data
        Parameters:
            md(dict|Metadata): Either a dict to be converted to Metadata on the fly or an existing Metdata object
        Returns:
            boolean with True if accepted, otherwise False
        """
        md = self.__check_metadata(metadata)

        accepted = False
        accept = ffi.new("bool*", accepted)
        lib.multio_field_accepted(self._handle, md._handle, accept)
        return bool(accept[0])

    def write_grib(self, data):
        if type(data) is bytes:
            size = len(data)
            sizeInt = ffi.cast("int", size)
            voidArr = ffi.from_buffer("void*", data)
            lib.multio_write_grib_encoded(self._handle, voidArr, sizeInt)

        elif haveNumpy and (type(data) is np.ndarray):
            size = len(data) * np.dtype.itemsize
            sizeInt = ffi.cast("int", size)
            voidArr = ffi.from_buffer("void*", data)
            lib.multio_write_grib_encoded(self._handle, voidArr, sizeInt)
        else:
            size = len(data)
            sizeInt = ffi.cast("int", size)
            charArr = ffi.new("char*", data)
            lib.multio_write_grib_encoded(self._handle, ffi.cast("void*", charArr), sizeInt)
