import os

from .lib import ffi, lib
from .config import Config

class Handler:
    """This is the container class for the Multio Handles"""

    def __init__(self, config):

        self.__config_pointer = config.get_pointer()

        handle = ffi.new("multio_handle_t**")
        lib.multio_new_handle(handle, self.__config_pointer)

        self.__handle = ffi.gc(handle[0], lib.multio_delete_handle)

    def open_connections(self):

        lib.multio_open_connections(self.__handle)

    def close_connections(self):

        lib.multio_close_connections(self.__handle)

    def flush(self, metadata):

        lib.multio_flush(self.__handle, metadata.get_pointer())

    def notify(self, metadata):

        lib.multio_notify(self.__handle, metadata.get_pointer())

    def write_domain(self, metadata, data, size):

        data = ffi.new(f'int[{size}]', data)
        size = ffi.cast("int", size)
        lib.multio_write_domain(self.__handle, metadata.get_pointer(), data, size)

    def write_mask_float(self, metadata, data, size):

        data = ffi.new(f'float[{size}]', data)
        size = ffi.cast("int", size)
        lib.multio_write_mask_float(self.__handle, metadata.get_pointer(), data, size)

    def write_mask_double(self, metadata, data, size):

        data = ffi.new(f'double[{size}]', data)
        size = ffi.cast("int", size)
        lib.multio_write_mask_double(self.__handle, metadata.get_pointer(), data, size)

    def write_field_float(self, metadata, data, size):

        data = ffi.new(f'float[{size}]', data)
        size = ffi.cast("int", size)
        lib.multio_write_field_float(self.__handle, metadata.get_pointer(), data, size)

    def write_field_double(self, metadata, data, size):

        data = ffi.new(f'double[{size}]', data)
        size = ffi.cast("int", size)
        lib.multio_write_field_double(self.__handle, metadata.get_pointer(), data, size)

    def field_accepted(self, metadata):
        accepted = False
        accept = ffi.new("bool*", accepted)
        lib.multio_field_accepted(self.__handle, metadata.get_pointer(), accept)
        return bool(accept[0])


    def get_pointer(self):
        return self.__handle
