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

    def get_pointer(self):
        return self.__handle
