Environment variables
=====================

Multio reads several environment variables at start-up to tune the behaviour
of the transport layer (how clients talk to the I/O servers), the in-memory
message queue, and the way fields are distributed over the available server
ranks. This page documents the variables that affect performance on modern
HPC systems, where they are read in the source tree, their defaults, how they
interact, and how to choose values as a function of the message (field) size,
which in turn is a function of the resolution of the underlying mesh.

Quick reference
---------------

.. list-table::
   :header-rows: 1
   :widths: 30 15 55

   * - Variable
     - Default
     - Scope / purpose
   * - ``MULTIO_SERVER_DISTRIBUTION``
     - ``hashed_to_single``
     - Strategy used by a client to choose which server receives a field.
   * - ``MULTIO_MPI_POOL_SIZE``
     - see below
     - Number of concurrent MPI send/receive buffers (clients *and* servers).
   * - ``MULTIO_SERVER_MPI_POOL_SIZE``
     - ``max(1, min(clients/2, 4))``
     - Same as ``MULTIO_MPI_POOL_SIZE`` but only on server ranks. Takes precedence.
   * - ``MULTIO_CLIENT_MPI_POOL_SIZE``
     - ``2 × #servers``
     - Same as ``MULTIO_MPI_POOL_SIZE`` but only on client ranks. Takes precedence.
   * - ``MULTIO_MPI_BUFFER_SIZE``
     - ``64 MiB``
     - Size (in bytes) of *each* MPI buffer in the pool, on clients and servers.
   * - ``MULTIO_SERVER_MPI_BUFFER_SIZE``
     - ``64 MiB``
     - Same as ``MULTIO_MPI_BUFFER_SIZE`` but only on server ranks. Takes precedence.
   * - ``MULTIO_CLIENT_MPI_BUFFER_SIZE``
     - ``64 MiB``
     - Same as ``MULTIO_MPI_BUFFER_SIZE`` but only on client ranks. Takes precedence.
   * - ``MULTIO_MESSAGE_QUEUE_SIZE``
     - 1,048,576 (server ``Listener``), 1024 (``ThreadTransport`` and ``multio-maestro-syphon``)
     - Maximum number of queued messages between the receiving thread and the dispatcher.
   * - ``MULTIO_USED_SERVERS``
     - 1
     - Number of servers a given client cycles through when
       ``MULTIO_SERVER_DISTRIBUTION=hashed_cyclic``.

Additional Multio environment variables found in the code base that are not
performance knobs (listed here for completeness):

* ``MULTIO_SERVER_CONFIG_PATH`` – base directory for Multio YAML plans.
* ``MULTIO_SERVER_CONFIG_FILE`` – file name of the active plan.
* ``MULTIO_CONFIG_TRIGGERS`` – JSON/YAML string with trigger definitions for
  the ``Trigger`` sink.
* ``MULTIO_SINKS`` – legacy; reported in start-up log messages.

Where each variable is read
---------------------------

* ``MULTIO_SERVER_DISTRIBUTION`` – read in
  ``src/multio/action/transport/Transport.cc`` inside
  ``Transport::distributionType()``.
* ``MULTIO_{,SERVER_,CLIENT_}MPI_POOL_SIZE`` – read in
  ``src/multio/transport/MpiTransport.cc`` inside
  ``MpiTransport::getMpiPoolSize()``.
* ``MULTIO_{,SERVER_,CLIENT_}MPI_BUFFER_SIZE`` – read in the same file in
  ``MpiTransport::getMpiBufferSize()``. The pool and buffer size together
  parameterise the ``StreamPool`` constructor in
  ``src/multio/transport/StreamPool.cc``.
* ``MULTIO_MESSAGE_QUEUE_SIZE`` – read via ``eckit::Resource`` in
  ``src/multio/server/Listener.cc``, ``src/multio/transport/ThreadTransport.cc``
  and ``src/multio/tools/multio-maestro-syphon.cc``.
* ``MULTIO_USED_SERVERS`` – read via ``eckit::Resource`` in
  ``src/multio/action/transport/Transport.cc`` (``Transport::Transport``).

All of the ``*_MPI_*`` variables follow the same precedence rule: the more
specific ``*_SERVER_*`` / ``*_CLIENT_*`` variable wins on the matching side;
if it is unset, the generic ``MULTIO_MPI_*`` is used; otherwise the compiled
in default applies.

Detailed description
--------------------

``MULTIO_SERVER_DISTRIBUTION``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Controls how a client decides to which server a given ``Message`` (a 2-D
field or slice) is sent. Three strategies are available:

``hashed_to_single`` (default)
    ``server_id = hash(metadata) % #servers``. All clients that produce a
    field with the same metadata key (e.g. same parameter and level) send it
    to the same server, which is required for correct aggregation of
    distributed fields on the server side. This is the right default for
    production runs.

``hashed_cyclic``
    Uses the client's own rank as the base server and walks forward through
    ``MULTIO_USED_SERVERS`` consecutive servers, picking one by hashing the
    field metadata. Useful to spread traffic when the number of distinct
    metadata keys is small compared to ``#servers``.

``even``
    Assigns each new metadata hash to the server with the current lowest
    counter, i.e. a pure load-balancer. It does *not* guarantee that clients
    producing the same field send it to the same server, and can therefore
    only be used with actions that do not require global aggregation on the
    server side.

**HPC guidance.** Keep ``hashed_to_single`` unless you have measured load
imbalance on the servers. If you switch to ``hashed_cyclic``, set
``MULTIO_USED_SERVERS`` to a value in ``[2, #servers]``; a good starting
point is ``min(#servers, ceil(#distinct-fields-per-step / #servers))``.

``MULTIO_MPI_POOL_SIZE`` / ``MULTIO_SERVER_MPI_POOL_SIZE`` / ``MULTIO_CLIENT_MPI_POOL_SIZE``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Number of MPI buffers held simultaneously by the ``StreamPool``. Each buffer
can hold one outstanding non-blocking send (client side) or receive (server
side). The pool determines how many peers a rank can be communicating with
concurrently and therefore how much send/receive parallelism the transport
can extract from the interconnect.

Constraints enforced at start-up by ``MpiTransport``:

* On a *client*, ``pool_size ≥ #servers`` — otherwise not all servers can
  hold an open stream simultaneously and the run aborts with a
  ``UserError`` naming ``MULTIO_CLIENT_MPI_POOL_SIZE`` /
  ``MULTIO_MPI_POOL_SIZE``.
* On a *server*, ``pool_size ≥ 1``.

Defaults:

* Server: ``max(1, min(#clients / 2, 4))`` (capped to 4).
* Client: ``2 × #servers``.

**HPC guidance.**

* Client side: the default of ``2 × #servers`` is a good lower bound. It
  lets a client have one buffer "in flight" to every server while it fills
  a second one. Increase it (``4 × #servers`` or more) when fields are
  small compared to ``MULTIO_CLIENT_MPI_BUFFER_SIZE`` and messages are
  packed into the same buffer — more slots reduce the probability that a
  client stalls because every stream has an in-flight send on the same
  destination. Do not exceed ``#servers`` by a large factor: each slot
  costs one ``MULTIO_CLIENT_MPI_BUFFER_SIZE`` worth of resident memory per
  client rank.
* Server side: the default is intentionally small. The server pool is used
  mainly for outbound statistics / control traffic, so 2–4 is almost
  always enough. Increase only if you see ``MPI_Wait`` time growing on the
  server.
* Memory budget per rank: ``pool_size × buffer_size``. For the defaults
  on a client with 8 servers that is ``16 × 64 MiB = 1 GiB`` of resident
  MPI buffer, so this is a real cost when clients are pinned to small
  partitions of a node.

``MULTIO_MPI_BUFFER_SIZE`` / ``MULTIO_SERVER_MPI_BUFFER_SIZE`` / ``MULTIO_CLIENT_MPI_BUFFER_SIZE``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Size in **bytes** of every buffer in the pool. Default ``64 × 1024 × 1024``
= 64 MiB (``defaultBufferSize`` in ``MpiTransport.cc``).

Multio packs consecutive serialised messages addressed to the same server
into the same buffer and flushes it as a single MPI send when either

* a new message would not fit in the remaining space, or
* a probabilistic criterion in ``StreamPool::getStream`` fires (more likely
  the fuller the buffer is).

For correctness, **one individual serialised message must fit inside one
buffer**. If a single field is larger than ``buffer_size``, the run will
fail. The *serialised* size of a field is approximately

.. code-block:: text

   msg_bytes ≈ N_points × bytes_per_value + header_bytes

with ``bytes_per_value = 8`` for double precision and
``header_bytes ≈ 512 − 2048`` bytes (metadata, fieldId, framing).

Reference points for atmospheric global grids (single field, ``float64``):

.. list-table::
   :header-rows: 1
   :widths: 25 20 25 30

   * - Resolution
     - ``N_points``
     - Single field size
     - Suggested buffer size
   * - TCo639 (~18 km)
     - ≈ 6.6 M
     - ~50 MiB
     - 128 MiB
   * - TCo1279 (~9 km)
     - ≈ 26 M
     - ~200 MiB
     - 256–512 MiB
   * - TCo2559 (~4 km)
     - ≈ 105 M
     - ~800 MiB
     - 1 GiB
   * - TCo3999 (~2.5 km)
     - ≈ 250 M
     - ~2 GiB
     - 2–4 GiB

**Rule of thumb.** Set ``buffer_size`` to roughly
``4 × max(single_field_bytes)``, rounded up to a power of two. A factor of
~4 leaves room for packing a few consecutive small fields (2-D surface
variables, coefficients) in the same send and amortises the per-message MPI
overhead, while the power-of-two rounding matches the page/NIC MTU
boundaries used by most HPC interconnects.

**Memory budget.** The total resident MPI memory per rank is
``pool_size × buffer_size``, doubled if you also raise the pool on both
sides. Always check this against the memory available to a single MPI
rank in your batch allocation, especially on the client side where the
pool is by default ``2 × #servers``.

**Asymmetry.** The server side value may be larger than the client side
value (a server typically receives from many clients and may want bigger
receive staging), but it must never be smaller than the size of the
largest packed payload a client will send it, otherwise the receive will
fail. The simplest safe configuration is to set
``MULTIO_MPI_BUFFER_SIZE`` once and let both sides inherit it.

``MULTIO_MESSAGE_QUEUE_SIZE``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Capacity of the in-process, lock-free ``eckit::Queue`` that sits between
the transport receive loop and the dispatcher thread on each server.
Incoming ``Message``\ s are pushed onto the queue by the listener and
popped by the dispatcher that then runs the action plan.

Defaults vary by entry point:

* ``server/Listener.cc`` (I/O servers) — ``1024 * 1024 = 1 048 576`` slots.
* ``transport/ThreadTransport.cc`` (in-process threaded transport used by
  tests and single-process runs) — ``1024`` slots.
* ``tools/multio-maestro-syphon.cc`` — ``1024`` slots.

A "slot" is a move-only ``Message`` wrapper; it does *not* by itself hold
the payload buffer, so the memory cost of a queue slot is small. The
server default of ~1M is therefore not expensive in RAM, but it is a
hard cap on how many unprocessed messages can pile up when the action
plan (encoding, GRIB writing, statistics, …) is slower than the incoming
rate.

**HPC guidance.**

* Servers: keep the default (~1M) unless you have a specific reason to
  shrink it. If the dispatcher falls behind and the queue fills, clients
  will eventually block in ``MPI_Send`` — lowering this value just moves
  the back-pressure earlier and rarely helps.
* ``ThreadTransport`` / ``multio-maestro-syphon``: raise to at least
  ``#clients × #fields-per-step`` if you see stalls in the receive loop
  during a time step.
* Memory note: the queue holds at most ``queue_size`` live messages; the
  *payload* memory used in the worst case is bounded by
  ``queue_size × avg_payload_bytes``. At very high resolutions it is
  therefore worth reducing the queue size (e.g. to ``64k`` on the server)
  to keep the worst-case memory footprint under control.

``MULTIO_USED_SERVERS``
~~~~~~~~~~~~~~~~~~~~~~~

Only relevant when ``MULTIO_SERVER_DISTRIBUTION=hashed_cyclic``. Default
``1``. It defines the window of consecutive servers that a given client
cycles through when placing fields. Larger values spread load over more
servers at the cost of less locality of reference on the server side.

Worked example for an HPC run
-----------------------------

Assume a global TCo1279 run (``N_points ≈ 26 M``, double precision, so one
field ≈ 200 MiB) using 4096 client ranks and 64 I/O server ranks. A
reasonable starting configuration is:

.. code-block:: bash

   # Field placement
   export MULTIO_SERVER_DISTRIBUTION=hashed_to_single

   # Size each MPI buffer to ~4× the largest field, rounded to a power of two
   export MULTIO_MPI_BUFFER_SIZE=$((1024 * 1024 * 1024))   # 1 GiB

   # Allow clients to have one outstanding send to every server and then some
   export MULTIO_CLIENT_MPI_POOL_SIZE=$((4 * 64))          # 256 → 256 GiB/client (!)
   # … which is unrealistic; drop back to the default (2× #servers) and instead
   # reduce the buffer size if memory is tight:
   unset MULTIO_CLIENT_MPI_POOL_SIZE
   export MULTIO_MPI_BUFFER_SIZE=$((512 * 1024 * 1024))    # 512 MiB → 64 GiB/client

   # Servers: default pool (≤4) is fine, but grow buffer to match clients
   export MULTIO_SERVER_MPI_BUFFER_SIZE=$((512 * 1024 * 1024))

   # Leave the server queue at its default (~1M) or cap it to bound memory
   export MULTIO_MESSAGE_QUEUE_SIZE=65536

The example shows the most important trade-off on modern supercomputers:
``pool_size × buffer_size`` is *the* resident memory bill of the transport,
and it is multiplied by the number of MPI ranks per node. Always size the
buffer to the field first, then pick the smallest pool that keeps the NIC
busy (``≥ #servers`` on the client, ``1–4`` on the server), and only then
increase either knob if profiling shows the transport is the bottleneck.

Tuning checklist
----------------

1. Compute the largest field size from the mesh:
   ``msg_bytes ≈ N_points × 8``.
2. Set ``MULTIO_MPI_BUFFER_SIZE`` to ``next_pow2(4 × msg_bytes)``.
3. Leave the pool sizes at their defaults
   (server ≤ 4, client = ``2 × #servers``).
4. Check ``pool × buffer`` fits in the per-rank memory budget; if not,
   halve the buffer before touching the pool — a smaller buffer only
   costs extra MPI messages, while a smaller pool can cost correctness
   (client-side lower bound ``#servers``) or throughput.
5. Keep ``MULTIO_SERVER_DISTRIBUTION=hashed_to_single`` unless a profiler
   shows server load imbalance; in that case switch to ``hashed_cyclic``
   and tune ``MULTIO_USED_SERVERS``.
6. Leave ``MULTIO_MESSAGE_QUEUE_SIZE`` at its default on servers;
   reduce it only to cap worst-case memory on very-high-resolution runs.
