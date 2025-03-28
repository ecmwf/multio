#
# Copyright 2017-2020 European Centre for Medium-Range Weather Forecasts (ECMWF).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os

import cffi
import findlibs
from pkg_resources import parse_version

__multio_version__ = "1.9.0"

ffi = cffi.FFI()


class MultioException(RuntimeError):
    pass


class CFFIModuleLoadFailed(MultioException):
    pass


class PatchedLib:
    """
    Patch a CFFI library with error handling

    Finds the header file associated with the Multio C API and parses it, loads the shared library,
    and patches the accessors with automatic python-C error handling.
    """

    def __init__(self):
        ffi.cdef(self.__read_header())

        libname = findlibs.find("multio-api", "multiolib")
        self.__lib = None

        if libname is None:
            raise RuntimeError("Multio is not found")

        try:
            self.__lib = ffi.dlopen(libname)
        except Exception as e:
            raise RuntimeError("Error loading the following library: {}".format(libname)) from e

        # All of the executable members of the CFFI-loaded library are functions in the multio
        # C API. These should be wrapped with the correct error handling. Otherwise forward
        # these on directly.

        for f in dir(self.__lib):
            try:
                attr = getattr(self.__lib, f)
                setattr(self, f, self.__check_error(attr, f) if callable(attr) else attr)
            except Exception as e:
                print(e)
                print("Error retrieving attribute", f, "from library")

        # Initialise the library, and set it up for python-appropriate behaviour

        self.multio_initialise()

        # Check the library version

        tmp_str = ffi.new("char**")
        self.multio_version(tmp_str)
        versionstr = ffi.string(tmp_str[0]).decode("utf-8")

        if parse_version(versionstr) < parse_version(__multio_version__):
            raise RuntimeError("Version of libmultio found is too old. {} < {}".format(versionstr, __multio_version__))

    def __read_header(self):
        with open(os.path.join(os.path.dirname(__file__), "processed_multio.h"), "r") as f:
            return f.read()

    def __check_error(self, fn, name):
        """
        If calls into the multio library return errors, ensure that they get detected and reported
        by throwing an appropriate python exception.
        """

        def wrapped_fn(*args, **kwargs):
            retval = fn(*args, **kwargs)
            if retval not in (self.__lib.MULTIO_SUCCESS,):
                error_str = "Error in function {}: {}".format(
                    name, ffi.string(self.__lib.multio_error_string(retval))
                ).replace("\\n", "\n")
                raise MultioException(error_str)
            return retval

        return wrapped_fn


# Bootstrap the library

try:
    lib = PatchedLib()
except CFFIModuleLoadFailed as e:
    raise ImportError() from e
