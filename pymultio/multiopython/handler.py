import importlib.util
import os

from .config import Config
from .lib import ffi, lib

numpy_spec = importlib.util.find_spec("numpy")
haveNumpy = numpy_spec is not None

if haveNumpy:
    import numpy as np



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

    def write_domain(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and (data.dtype == np.int_):
            intArr = ffi.cast("int*", ffi.from_buffer(data))
            lib.multio_write_domain(self.__handle, metadata.get_pointer(), intArr, sizeInt)
        else:
            intArr = ffi.new(f'int[{size}]', data)
            lib.multio_write_domain(self.__handle, metadata.get_pointer(), intArr, sizeInt)

    def write_mask(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and ((data.dtype == np.float32) or (data.dtype == np.float64)):
            if data.dtype == np.float32:
                floatArr = ffi.cast("float*", ffi.from_buffer(data))
                lib.multio_write_mask_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)
            else:
                doubleArr = ffi.cast("double*", ffi.from_buffer(data))
                lib.multio_write_mask_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f'double[{size}]', data)
            lib.multio_write_mask_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)

    def write_mask_float(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and (data.dtype == np.float32):
            floatArr = ffi.cast("float*", ffi.from_buffer(data))
            lib.multio_write_mask_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)
        else:
            floatArr = ffi.new(f'float[{size}]', data)
            lib.multio_write_mask_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)

    def write_mask_double(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and (data.dtype == np.float64):
            doubleArr = ffi.cast("double*", ffi.from_buffer(data))
            lib.multio_write_mask_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f'double[{size}]', data)
            lib.multio_write_mask_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)

    def write_field(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and ((data.dtype == np.float32) or (data.dtype == np.float64)):
            if data.dtype == np.float32:
                floatArr = ffi.cast("float*", ffi.from_buffer(data))
                lib.multio_write_field_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)
            else:
                doubleArr = ffi.cast("double*", ffi.from_buffer(data))
                lib.multio_write_field_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f'double[{size}]', data)
            lib.multio_write_field_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)

    def write_field_float(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and (data.dtype == np.float32):
            floatArr = ffi.cast("float*", ffi.from_buffer(data))
            lib.multio_write_field_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)
        else:
            floatArr = ffi.new(f'float[{size}]', data)
            lib.multio_write_field_float(self.__handle, metadata.get_pointer(), floatArr, sizeInt)

    def write_field_double(self, metadata, data):
        size = len(data)
        sizeInt = ffi.cast("int", size)
        if haveNumpy and (type(data) == np.ndarray) and (data.dtype == np.float64):
            doubleArr = ffi.cast("double*", ffi.from_buffer(data))
            lib.multio_write_field_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)
        else:
            doubleArr = ffi.new(f'double[{size}]', data)
            lib.multio_write_field_double(self.__handle, metadata.get_pointer(), doubleArr, sizeInt)

    def write_grib_encoded(self, data):
        if type(data) == bytes:
            size = len(data)
            sizeInt = ffi.cast("int", size)
            voidArr = ffi.cast("void*", ffi.from_buffer(data))
            lib.multio_write_grib_encoded(self.__handle, voidArr, sizeInt)

        elif haveNumpy and (type(data) == np.ndarray):
            size = len(data) * np.dtype.itemsize
            sizeInt = ffi.cast("int", size)
            voidArr = ffi.cast("void*", ffi.from_buffer(data))
            lib.multio_write_grib_encoded(self.__handle, voidArr, sizeInt)
        else:
            size = len(data)
            sizeInt = ffi.cast("int", size)
            charArr = ffi.new(f'char*', data)
            lib.multio_write_grib_encoded(self.__handle, ffi.cast("void*", charArr), sizeInt)

    def field_accepted(self, metadata):
        accepted = False
        accept = ffi.new("bool*", accepted)
        lib.multio_field_accepted(self.__handle, metadata.get_pointer(), accept)
        return bool(accept[0])


    def get_pointer(self):
        return self.__handle
