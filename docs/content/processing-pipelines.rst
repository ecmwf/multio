.. index:: processing-pipelines

Processing pipelines
====================

.. index:: Processing pipelines; Configuration

.. _`configuration`:

Configuration
-------------

The processing pipelines are configured via a yaml file. Each pipeline consists of a user-defined
list of actions. There are pipelines both on the model-side and on the server side. Some actions may
be defined either the model or the server, while some actions may only make sense to define for one
of those.

The path to the configuration file is defined by the environment variable
``MULTIO_SERVER_CONFIG_FILE``. If that is not set, **multio** will look for ``multio-server.yaml``
in ``MULTIO_SERVER_CONFIG_PATH``, if that is set, or in the local folder otherwise. The
configuration file typically consists of two main parts:

* A list of action plans on the model (``client``) side of multio, where they act on partial fields
  and the domain/grid properties of partial fields.
* A list of action plans on the server (e.g. ``server``) side, where they act on either
  partial fields or the aggregated fields depending on the action's position in the pipeline.

Every interface call that passes a message (data + metadata) to **multio** via the
:ref:`data-routing API <reference-data-routing>` will result in that message being passed to every
pipeline. Each action will examine its own configuration as well as checking certain keys in the
message's metadata to decide how to handle the message.

Here is a small example of a configuration with three pipelines (``plans``) on the model
(``client``) side and one on the server (``server``) side.

.. code-block:: yaml

   client:
     plans:

       - name : ocean-replay-grid-info-stream
         actions :
           - type : select
             match :
              - category : [ocean-domain-map, ocean-mask]

           - type : transport
             target : server

       - name : ocean-replay-test-stream1
         actions :
           - type : select
             match :
              - name: [ sst, ssv ]

           - type : transport
             target : server

       - name : ocean-replay-test-stream2
         actions :
           - type : select
             match :
              - field: [ ssu, ssw ]

           - type : transport
             target : server

   server:
     transport : mpi
     group : multio
     count : 2
     plans :

       - name : ocean-fields
         actions :
           - type : select
             match :
              - category : [ocean-2d]

           - type : aggregate

           - type : single-field-sink


Note that this example uses `mpi` as transport. Refer to :ref:`MPI Communicators` to learn how
MultIO is setting up MPI communicators.

Actions
-------

It is the intent in the design that actions should be able to be added and modified with a
relatively low development effort, based on new and changing requirements. **multio** currently
supports the following actions.


Select
~~~~~~

The ``select`` action is practically always the first action in a pipeline as it acts as a filter to
decide whether the incoming message is meant for being processed here. It can match either
* the message's field name, checking the value for the metadata key ``name``, or
* the message's category, checking the value for the metadata key ``category``.

In either case, a list of field names or a list categories need be provided for which the message is
passed to the next action.


Statistics
~~~~~~~~~~

This action computes pointwise, temporal statistics over a user-defined time interval.

* It currently supports five operations: ``average``, ``minimum``, ``maximum``, ``accumulate`` and
  ``instant``, with the last one essentially being a filtering operation.
* It supports time units ``hours``, ``days`` and, to a limited extent, ``months``.
* Output frequencies are defined as ``3h`` for three-hourly, ``10d`` for ten-daily or ``1m`` for
  monthly, etc.
* It requires the following keys to be set in the fields metadata: ``startDate``, ``startTime``,
  ``step``, ``timeStep``. The ``timeStep`` is the time-step size and is assumed to be in seconds.

=============  ===============  ======================
Key            Example Value    Interpretation
=============  ===============  ======================
``startDate``  ``20170906``     ``yyyymmhh``
``startTime``  ``120000``       ``hhmmss``
``step``       ``42``           ``numerical``
``timestep``   ``1200``         ``seconds``
=============  ===============  ======================


For example, the following action would compute five-daily averages and would pass the result on to
the next action.

.. code-block:: yaml

       - type : statistics
         output-frequency: 5d
         operations:
           - average


Transport
~~~~~~~~~

It only makes sense to define a ``transport`` action for a pipeline on the model (``client``) side,
and it designates the last action of that pipeline.

* It is responsible for forwarding messages to the I/O-server, so a ``target`` needs to be specified.
* It will ensure that the partial fields of the same global field will be sent to the same server
  process for aggregation.
* Transport layer MPI is supported and there is also limited support for sockets.


Aggregation
~~~~~~~~~~~

This will create global, aggregated fields from the partial fields and once that has been completed,
it destroys the partial messages and passes the new, aggregated message to the next action. It needs
to be defined on the server side.

There is no additional configuration option, but the action assumes that the domain-connectivity
information has been communicated at the beginning of the run, by calling the API function

.. code-block:: c

   int multio_write_domain(multio_handle_t* mio, multio_metadata_t* md, int* data, int size);


Mask
~~~~

This action will mask parts of the aggregated field, so it is designed to come after aggregation, if
included in the pipeline. It will allow parts of the domain to be ignored and thus reduce the size of
the stored message. It is particularly useful for ocean forecast data.

Similar to the ``aggregate`` action, it assumes that the mask was communicated at the beginning of
the run, by calling the API function

.. code-block:: c

   int multio_write_mask(multio_handle_t* mio, multio_metadata_t* md, const double* data, int size);

Encode
~~~~~~

This action will encode data in the required format and pass the new, encoded message to the next
action. Currently, the ``GRIB`` (edition 2) format is supported. It is also possible to specify the
format as ``raw``, in which case the data will be passed on unencoded.

For GRIB encoding, a template must also be provided. Most of the GRIB keys are already defined in
the template, so what GRIB template to use will depend on the types of data being produced.

.. code-block:: yaml

       - type : encode
         format : grib
         template : unstr_avg_fc.tmpl
         unstructured-grid-type : eORCA025


Encode-Tensogram
~~~~~~~~~~~~~~~~

This action encodes raw field data into the `Tensogram`_ N-dimensional tensor message format,
producing self-describing binary messages that preserve MARS metadata. This action is particularly
useful for producing compact, portable output that can be processed by external analysis tools.

MARS metadata from the input message is preserved in two ways:

* On the output Message itself (for downstream routing within multio pipelines)
* Embedded in the Tensogram payload under ``base[0].mars`` (for external tool interoperability)

The action supports configurable encoding (simple_packing for lossy compression), multiple
compression algorithms (szip, zstd, lz4), optional filtering (shuffle), and integrity
verification (xxh3 hashing).

**Note:** This action requires tensogram support to be enabled at build time with
``-DENABLE_TENSOGRAM=ON``. The tensogram library must be installed and available
(see `github.com/ecmwf/tensogram`_).

Configuration options:

====================  ========================  ====================  ============================================
Key                   Allowed Values            Default               Description
====================  ========================  ====================  ============================================
``encoding``          ``none``,                 ``simple_packing``    Encoding method: ``none`` (raw float64) or
                      ``simple_packing``                              ``simple_packing`` (quantized integers)
``compression``       ``none``, ``szip``,       ``szip``              Compression algorithm applied after
                      ``zstd``, ``lz4``                               encoding
``filter``            ``none``, ``shuffle``     ``none``              Pre-compression filter (shuffle improves
                                                                      compression ratio)
``hash``              ``xxh3``, (empty string)  ``xxh3``              Hash algorithm for integrity checking
                                                                      (empty string disables)
``bits-per-value``    Integer (1-64)            ``16``                Bits per value for simple_packing
                                                                      (higher = more precision)
``decimal-scale-      Integer                   ``0``                 Decimal scale factor for simple_packing
factor``                                                              (multiplier = 10^factor)
====================  ========================  ====================  ============================================

Example configurations:

**High compression** (suitable for visual analysis):

.. code-block:: yaml

       - type : encode-tensogram
         encoding : simple_packing
         compression : szip
         bits-per-value : 12
         filter : shuffle

**Lossless** (raw float64, no packing):

.. code-block:: yaml

       - type : encode-tensogram
         encoding : none
         compression : zstd

**Balanced** (default settings):

.. code-block:: yaml

       - type : encode-tensogram
         # Uses defaults: simple_packing, 16 bits, szip compression

**Complete pipeline example** (select → encode → sink):

.. code-block:: yaml

       - name : surface-to-tensogram
         actions :
           - type : select
             match :
               - levtype : [sfc]

           - type : encode-tensogram
             encoding : simple_packing
             compression : szip
             bits-per-value : 16
             hash : xxh3

           - type : sink
             sinks :
               - type : file
                 append : true
                 path : output.tgm

Output files can be validated and inspected using the tensogram command-line tools:

.. code-block:: bash

   # Validate message integrity
   tensogram validate output.tgm

   # Display metadata
   tensogram info output.tgm

   # List all messages
   tensogram ls output.tgm

   # Dump message contents
   tensogram dump output.tgm

Or processed in Python using the tensogram package:

.. code-block:: python

   import tensogram

   with tensogram.TensogramFile.open("output.tgm") as f:
       for msg in f:
           meta, objects = msg
           # Access MARS metadata
           mars = meta.base[0].get('mars', {})
           # Access data arrays
           desc, data = objects[0]
           print(f"Shape: {data.shape}, dtype: {data.dtype}")

.. _`Tensogram`: https://github.com/ecmwf/tensogram
.. _`github.com/ecmwf/tensogram`: https://github.com/ecmwf/tensogram


Sink
~~~~

This action is responsible for outputting data and is at the end of the pipeline. It typically
involves passing the data to specialised libraries for a filesystem, object store or some other
forms of middleware. Currently files and `fdb`_ are supported.

It is possible to define multio sinks as part of the same action. **multio** will then loop over the
list of sinks and pass data to each of them. The following examples outputs messages to file and FDB
simultaneously.

.. code-block:: yaml

       - type : sink
         sinks :

           - type : fdb5
             config : {}

           - type : file
             append : true
             per-server : true
             path : ocean-output-field.grib

The key ``path`` must be set for file output. If multiple server processes are run, setting
``per-server`` to ``true`` will avoid possible race conditions by ensuring that different processes
will not attempt to write to the same file. Then **multio** will create files where the ``path``
value is prefixed with hostname and process-id information,
e.g. ``multio-myhostname-18862-ocean-output-field.grib``.

.. _`fdb`: https://github.com/ecmwf/fdb
