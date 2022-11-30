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

           - type : aggregation

           - type : single-field-sink



MPI Communicators
-----------------

    *Note:
    MPI splitting is very inflexible. On failure applications usually have to restart, 
    as it is not possible for single nodes to reconnect.
    In future as an alternative MultIO might also support MPI via 
    `MPI_connect`/`MPI_accept` to allow more failure tolerant and dynamic setups.*

MultIO is trying to be purely messaged based and intents to use a general 
concept of transports with different and multiple backends.
Nevertheless MPI is a common and important player in HPC and also needs a 
little bit of special handling regarding communicators.

In scientific distributed applications, communication, specialization of nodes and 
distributing tasks is done by splitting communicators with `MPI_split` to create subgroups, 
i.e. new intra communicators containing only a subset of nodes.
For splitting, a unqiue *color* (i.e. a tag represented by an integer) is chosen and 
all nodes have to commit on a split at the same times within a given timeframe.
Accordingly applications usually perform all splittings on initialization and 
maintain the structure for their whole lifetime. 

In practice this implies that models may pass down parent communicators through an interface 
and have to start client as well as server nodes. Currently, in the Multio API this is the 
only exception for configurations that are passed through the API. 
Multio then splits a *client* or a *server* communicator and uses `MPI_Group` to compute 
ranks of servers and clients (`mpigroups`_).
**Important: The passed communicator are expected to be either a client or a server. 
Nodes performing other tasks should not be contained in the communicator.**

In the configuration example above, you can see that client plans refer to a server through a *transport*. In the server section the *transport* is then specified to be "mpi".
The key `group` then refers ta a MPI communicator used for communication. Using `eckit::mpi` named lookup the communicator is looked up --- i.e. using `eckit` or `fckit` communicators may be used to manage communicators. 
Otherwise, if the communicator is not existing, a recursive lookup mechanism is triggered to setup a communicator (`mpisetup`_).

.. _`mpisetup`:

MPI Groups
~~~~~~~~~~

MultIO MPI is working with an communicator containing all clients and server instances. 
Yet clients do not know the server ranks in the communicator. To retrieve this information, 
a furter MPI split is made: All clients split from the parent communicator to a 
separate client communicator and all servers split to a separate server communicator.
Now with `MPI_Group` we can retrieve the parent group and one child group for each instance.
By using `MPI_Group_difference` it is possible to compute the *opposite* child group, 
i.e. clients can compute the server group and server the client group.
Now by using `MPI_Group_translate_ranks`, it is very easy to compute all ranks of both 
groups *in the parent communicator*. This now allows clients to directly communicate to servers
by mainting a list of server ranks.

This approach is very flexible. The only assumption is that a communicator containing 
only server and client instances is passed to MultIO.
As MultIO will split off clients and servers, it is also recommended to split whole 
separate communicator for MultIO. Otherwise splitting colors have to be adjusted (`mpisetup`_).



.. _`mpisetup`:

MPI Setup
~~~~~~~~~

.. code-block:: yaml

TBD


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
* Transport layer MPI is support and there is also limited support for sockets.


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
included in the pipelie. It will allow parts of the domain to be ignored and thus reduce the size of
the stored message. It is particularly useful for ocean forecast data.

Similar to the ``aggregation`` action, it assumes that the mask was communicated at the beginning of
the run, by colling the API function

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
         grid-type : eORCA025


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
