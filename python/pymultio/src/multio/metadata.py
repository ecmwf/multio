from .lib import ffi, lib


class Metadata:
    """This is the main container class for Multio Metadata"""

    def __init__(self, parent_multio, md=None):
        metadata = ffi.new("multio_metadata_t **")
        lib.multio_new_metadata(metadata, parent_multio._handle)

        self._handle = ffi.gc(metadata[0], lib.multio_delete_metadata)

        if md is not None:
            for key, value in md.items():
                self.__setitem__(key, value)

    def __setitem__(self, key, value):
        if isinstance(value, int):
            self._set_int(key, value)
        elif isinstance(value, str):
            self._set_string(key, value)
        elif isinstance(value, bool):
            self._set_bool(key, value)
        elif isinstance(value, float):
            self._set_float(key, value)
        else:
            raise TypeError(f"{type(value).__name__} is not allowed for metadata")

    def _set_int(self, key, value):
        key = ffi.new("char[]", key.encode("ascii"))
        value = ffi.cast("int", value)
        lib.multio_metadata_set_int(self._handle, key, value)

    def _set_string(self, key, value):
        key = ffi.new("char[]", key.encode("ascii"))
        value = ffi.new("char[]", value.encode("ascii"))

        lib.multio_metadata_set_string(self._handle, key, value)

    def _set_bool(self, key, value):
        key = ffi.new("char[]", key.encode("ascii"))
        value = ffi.cast("_Bool", value)

        lib.multio_metadata_set_bool(self._handle, key, value)

    def _set_float(self, key, value):
        key = ffi.new("char[]", key.encode("ascii"))
        value = ffi.cast("double", value)

        lib.multio_metadata_set_double(self._handle, key, value)
