multio
======

[![Build Status](https://img.shields.io/github/actions/workflow/status/ecmwf/multio/ci.yml?branch=develop)](https://github.com/ecmwf/multio/actions/workflows/ci.yml?query=branch%3Adevelop)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/ecmwf/multio/blob/develop/LICENSE)

multio is a package developed by ECMWF that provides an application programming interface (API) for

   * I/O-server functionality for distributed earth-system models,
   * post-processing pipelines of user-programmable list of actions.

The fundamental design principle behind multio is that data is decoupled from metadata, and that the
metadata is used for routing the data through the post-processing pipelines.

Documentation
------------

The documentation can be found at https://multio.readthedocs.io.

Requirements
------------

Runtime dependencies:

- eccodes -- http://github.com/ecmwf/eccodes
- eckit -- http://github.com/ecmwf/eckit
- metkit -- http://github.com/ecmwf/metkit

Optional runtime dependencies:

- fdb -- http://github.com/ecmwf/fdb
- fckit -- http://github.com/ecmwf/fckit (for the fortran example code only)

Build dependencies:

- CMake -- For use and installation see http://www.cmake.org/
- ecbuild -- ECMWF library of CMake macros (available via apt or see https://github.com/ecmwf/ecbuild)
- MPI -- an implementation of MPI may be required for the I/O-server functionality, see https://www.mpi-forum.org/

Installation
------------

multio employs an out-of-source build/install based on CMake.

Make sure ecbuild is installed and the ecbuild executable script is found ( `which ecbuild` ).

Now proceed with installation as follows.

1. Set up environment as appropriate
   ```bash
   srcdir=$(pwd)
   builddir=build
   installdir=$HOME/local
   ```

2. Run Cmake/ecbuild
   ```bash
   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install> $srcdir
   ```

   The package is in active development and the I/O-server functionality is not enabled by default, so
   it needs to be turned on explicitly. In addition, if built with FDB support, some compilers will
   require linking to be forced.
   ```bash
   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install> -DECBUILD_EXE_LINKER_FLAGS=-Wl,--no-as-needed $srcdir
   ```

3. Compile, test and install
   ```bash
   make -j10
   ctest
   make install
   ```

Copyright and license
------------

(C) Copyright 2005- ECMWF.

This software is licensed under the terms of the Apache Licence Version 2.0 which can be obtained at
http://www.apache.org/licenses/LICENSE-2.0.

In applying this licence, ECMWF does not waive the privileges and immunities granted to it by virtue
of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
