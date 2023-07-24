import os

from .lib import ffi, lib

class Metadata:
    """This is the main container class for Multio Metadata"""

    def __init__(self, handle, md=None):

        self.__handle_pointer = handle.get_pointer()

        metadata = ffi.new("multio_metadata_t **")
        lib.multio_new_metadata(metadata, self.__handle_pointer)

        self.__metadata = ffi.gc(metadata[0], lib.multio_delete_metadata)

        if md is not None:
            for key, value in md.items():
                self.metadata_set(key, value)

    def metadata_set(self, key, value):
        #Is there a nicer way to do this, an equivilent of generics in python?
        # Could use switch case if using python 3.10 or above
        # Need to figure out how to use long, longlong, and double
        if isinstance(value, int):
            self.metadata_set_int(key, value)
        #elif type(value) == long:
        #    self.metdata_set_long(key, value)
        elif isinstance(value, str):
            self.metadata_set_string(key, value)
        elif isinstance(value, bool):
            self.metadata_set_bool(key, value)
        elif isinstance(value, float):
            self.metadata_set_float(key, value)
        #elif type(value) == double:
        #    self.metadata_set_double(key, value)
        else:
            raise TypeError(f"{type(value).__name__} is not allowed for metadata")

    def metadata_set_int(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("int", value)

        lib.multio_metadata_set_int(self.__metadata, key, value)

    def metadata_set_long(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("long", value)

        lib.multio_metadata_set_long(self.__metadata, key, value)

    def metadata_set_longlong(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("long long", value)

        lib.multio_metadata_set_longlong(self.__metadata, key, value)

    def metadata_set_string(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.new("char[]", value.encode('ascii'))

        lib.multio_metadata_set_string(self.__metadata, key, value)

    def metadata_set_bool(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("_Bool", value)

        lib.multio_metadata_set_bool(self.__metadata, key, value)

    def metadata_set_float(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("float", value)

        lib.multio_metadata_set_float(self.__metadata, key, value)

    def metadata_set_double(self, key, value):

        key = ffi.new("char[]", key.encode('ascii'))
        value = ffi.cast("double", value)

        lib.multio_metadata_set_double(self.__metadata, key, value)

    def delete_metadata(self):
        lib.multio_delete_metadata(self.__metadata)

    def get_pointer(self):
        return self.__metadata