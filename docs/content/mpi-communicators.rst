.. index:: mpi-communicators

.. _`MPI Communicators`:

MPI Communicators
=================

    Note:
    MPI splitting is very inflexible. In case of a single node failure, applications usually have to restart,
    as it is not possible for single nodes to reconnect.
    In future as an alternative MultIO might also support MPI via
    `MPI_connect`/`MPI_accept` to allow more failure tolerant and dynamic setups.

    If possible, it is recommended to perform MPI splitting completely through the configuration,
    without passing any further information through the API (:ref:`Configuration based MPI Splitting<mpiconfsplit>`). This allows choosing the transport layer in the configuration.
    However, if you replace an existing IO server with MultIO, you may be forced to adopt a specific scheme of handling MPI communicators.

MultIO is trying to be purely message based and intents to use a general
concept of transports with different and multiple backends.
Nevertheless MPI is a common and important player in HPC and also needs a
little bit of special handling regarding communicators.

In scientific distributed applications, communication, specialisation of nodes and
distributing tasks is done by splitting communicators with `MPI_split` to create subgroups,
i.e. new intra communicators containing only a subset of nodes.
For splitting, a unique *colour* (i.e. a tag represented by an integer) is chosen and
all nodes have to commit on a split at the same times within a given timeframe.
Accordingly applications usually perform all splittings on initialisation and
maintain the structure for their whole lifetime.

In practice this implies that models may pass down parent communicators through an interface
and have to start client as well as server nodes. Currently, in the MultIO API this is the
only exception for configurations that are passed through the API.
MultIO then splits a *client* or a *server* communicator and uses `MPI_Group` to compute
ranks of servers and clients (`MPI Groups`_).
**Important: The passed communicator is expected to be either a client or a server.
Nodes performing other tasks should not be contained in the communicator.**

In the configuration example above, you can see that client plans refer to a server through a *transport*.
In the server section the *transport* is then specified to be "MPI".
The key `group` then refers ta a MPI communicator used for communication.
Using `eckit::mpi` named lookup the communicator is looked up ---
i.e. using `eckit` or `fckit` communicators may be used to manage communicators.
Otherwise, if the communicator is not existing, a recursive lookup mechanism is triggered to setup a communicator (`MPI Setup`_).

.. _`MPI Groups`:

MPI Groups
~~~~~~~~~~

MultIO MPI is working with an communicator containing all clients and server instances.
Nevertheless clients do not know the server ranks in the communicator. To retrieve this information,
a further MPI split is made: All clients split from the parent communicator to a
separate client communicator and all servers split to a separate server communicator.
Now with `MPI_Group` we can retrieve the parent group and one child group (server or client group) for each instance.
By using `MPI_Group_difference` it is possible to compute the *opposite* child group,
i.e. clients can compute the server group and server the client group.
Finally by using `MPI_Group_translate_ranks`, it is very easy to compute all ranks of both
groups *in the parent communicator*. This now allows clients to directly communicate to servers
by maintain a list of server ranks.

This approach is very flexible.
The only assumption is that the communicator passed to MultIO is only containing server and client instances.
Hence it is also recommended to split whole separate communicator for MultIO.

MultIO allows describing communicator splitting in the configuration (`MPI Setup`_).



.. _`MPI Setup`:

MPI Setup
~~~~~~~~~

MultIO is setting up MPI communicators on a flexible way following a recursive pattern.
It is possible to pass parent communicators through the API, create them through `eckit` or `fckit` or simply by describing
a splitting from the world communicator with a unique colour.

The default behaviour is as follows:

.. code-block:: yaml

    # Default MPI setup (implicitly assumed)
    mpi-communicators:
        multio:
            type: passed
            default: world
        multio-clients:
            type: split
            color: 777
            parent: multio
        multio-servers:
            type: split
            color: 888
            parent: multio

    ...

    server:
      transport: mpi
      group: multio # implicit default
      client-group: multio-clients  # implicit default
      server-group: multio-servers  # implicit default
      plans :
        ...


To make this work, MULTIO will look up the `group` key in the server section or take the default value 'multio'.
Then it is chekced whether a communicator with the given name already exists in `eckit::mpi`.
If it exists, the communicator is assumed the be the parent communicator,
otherwise the global mapping `mpi-communicators` is looked up for a communicator description with that key.

Communicators described in `mpi-communicators` must specify a `type` of either 'passed' or 'split'.
When a communicator is described by type *'passed'*, the communicator passed through the API is taken.
If no communicator has been passed, the communicator specified by key 'default' is taken -
which also defaults to 'world' if not specified.
The default lookup then happens through the same scheme described here: First `eckit::mpi` then `mpi-communicators` are looked up.
(Note: Using default can be restricted through the configuration context in the API).

In contrast, a communicator specifying type *'split'* also needs to specify a key `colour` and `parent` to create a new
communicator by performing a `MPI_split`. The `parent` communicator is looked up recursively through the same scheme (i.e. `eckit::mpi` or `mpi-communicators`).
Once the communicator has been created, it is added with the same name to the `eckit::mpi` communicators. On that way future lookups will get the communicator
immediately through the `eckit::mpi` lookup.

Now after the parent communicator has been created or retrieved, MULTIO will also retrieve the child communicator.
Depending on executed on a client or server, the key `client-group` or `server-group` is looked up or generated by
taking the value of `group` and appending '-clients` or '-servers'.
Using this value the child communicator is lookup ed through the recursive scheme described above.

Depending on the configuration of `mpi-communicators`, the MPI splitting behaviour can be customised.
For example, if the parent communicator must not be passed within the application, it can be described completely
from the configuration:

.. _`mpiconfsplit`:

Configuration based MPI splitting
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: yaml

    # Default MPI setup (implicitly assumed)
    mpi-communicators:
        multio:
            type: split
            color: 42 # Replace this by a unique color within your application
            parent: world
        multio-clients:
            type: split
            color: 1
            parent: multio
        multio-servers:
            type: split
            color: 2
            parent: multio

Here a splitting is performed in the communicator 'multio' instead of taking the passed communicator.
Moreover instead of naming the 'multio', you may also give it a different name.

If your application is not insisting on splitting on its own and you are free
to choose one of the given approaches,
the easiest way is to use this approach and configure MPI splitting configuration based.
