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
* `eccodes`_
* `metkit`_
* `atlas`_

.. index:: Dependencies; Optional

Optional
~~~~~~~~

* MPI
* `fdb`_ (`-DENABLE_FDB5=ON`. Default: ON)
* `fckit`_ (if fortran is enabled `-DENABLE_FORTRAN=ON`. Default: ON)
* `atlas-orca`_ (to enable downloading (e)ORCA grids with atlas)
* `mir`_ (for interpolation `-DENABLE_MIR=ON`. Default: ON)
* `pgen`_ (internal, `-DENABLE_PGEN=ON`, Default: OFF)

.. index:: Build, Install
   :name: build-install

Build & Install
---------------

.. code-block:: shell

   # 1. Clone repository
   git clone https://github.com/ecmwf/multio

   # 2. Set up environment as appropriate
   srcdir=$(pwd)
   builddir=build
   installdir=$HOME/local

   # 3. Run Cmake/ecbuild. Note that the package is in active development and the I/O-server functionality is not enabled by default; it needs to be turned on explicitly.
   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install> -DENABLE_MULTIO_SERVER=ON $srcdir

   #  In addition, if built with FDB support, some compilers will require linking to be forced.
   ecbuild --prefix=$installdir -- -DCMAKE_PREFIX_PATH=<path/to/dependencies/install>
   -DENABLE_MULTIO_SERVER=ON -DECBUILD_EXE_LINKER_FLAGS=-Wl,--no-as-needed $srcdir

   # 4. Compile, test and install
   make -j10
   ctest
   make install


.. _`CMake`: https://cmake.org
.. _`ecbuild`: https://github.com/ecmwf/ecbuild
.. _`eckit`: https://github.com/ecmwf/eckit
.. _`eccodes`: https://github.com/ecmwf/eccodes
.. _`metkit`: https://github.com/ecmwf/metkit
.. _`atlas`: https://github.com/ecmwf/atlas
.. _`atlas-orca`: https://github.com/ecmwf/atlas-orca
.. _`fdb`: https://github.com/ecmwf/fdb
.. _`mir`: https://github.com/ecmwf/mir
.. _`pgen`: https://github.com/ecmwf/pgen
.. _`fckit`: https://github.com/ecmwf/fckit
