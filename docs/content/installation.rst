Installation
============

.. index:: Dependencies

Dependencies
------------

.. index::
   single: Dependencies; Required

Required
~~~~~~~~

* C/C++, Fortran compiler
* `CMake`_
* `ecbuild`_
* `eckit`_
* `metkit`_
* `eccodes`_

.. index:: Dependencies; Optional

Optional
~~~~~~~~

* MPI
* `fdb`_

.. index:: Build, Install
   :name: build-install

Build & Install
---------------

1. Clone repository

   .. code-block:: shell

   git clone https://github.com/ecmwf/multio

2. Set up environment as appropriate

   .. code-block:: shell

   srcdir=$(pwd)
   builddir=build
   installdir=$HOME/local

3. Run Cmake/ecbuild

   .. code-block:: shell

   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install> -DENABLE_MULTIO_SERVER=ON $srcdir

   The package is in active development and the I/O-server functionality is not enabled by default;
   it needs to be turned on explicitly. In addition, if built with FDB support, some compilers will
   require linking to be forced.

   .. code-block:: shell

   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install>
   -DENABLE_MULTIO_SERVER=ON -DECBUILD_EXE_LINKER_FLAGS=-Wl,--no-as-needed $srcdir

3. Compile, test and install

   .. code-block:: bash

   make -j10
   ctest
   make install


.. _`CMake`: https://cmake.org
.. _`ecbuild`: https://github.com/ecmwf/ecbuild
.. _`eckit`: https://github.com/ecmwf/eckit
.. _`metkit`: https://github.com/ecmwf/metkit
.. _`eccodes`: https://github.com/ecmwf/eccodes
.. _`fdb`: https://github.com/ecmwf/fdb
