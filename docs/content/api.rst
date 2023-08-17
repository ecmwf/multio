.. index:: API

API
===


.. index:: API; C Interface

.. _`c-interface`:

C Interface
-----------

The C API interface is designed to have a consistent approach to function call design, and to error
and argument handling.

Calling Convention
~~~~~~~~~~~~~~~~~~

The library is built around a collection of objects which handle functionality. As C does not expose
objects directly, the API uses pointers to two fundamental types: ``multio_handle_t`` and
``multio_metadata_t``). Objects are allocated internally and these pointers provide handles to the
metadata object and the multio object, respectively.

All functions return an integer return code, with the sole exception of ``multio_error_string``,
which obtains the details of a previous error. This is beneficial for :ref:`error-handling` as it
makes it possible to wrap every function in the same way.

A side effect of this design is that all inputs and outputs are passed as arguments to
functions. All output values are passed as pointers, and some functions accept a null pointers for
optional output values.

.. code-block:: c

   multio_handle_t* mio;
   int rc = multio_new_handle(multio_handle_t** mio);

   multio_metadata_t* md;
   rc = multio_new_metadata(multio_metadata_t** md);


.. _`error-handling`:

Error Handling
~~~~~~~~~~~~~~

All functions return a status code, which should be checked. In case of error, a human readable
message can be obtained using ``multio_error_string``.

The return code is always one of the following:

``MULTIO_SUCCESS``
   The function completed successfully.

``MULTIO_ERROR_ECKIT_EXCEPTION``
   A known ``eckit::Exception`` was encountered. Call ``multio_error_string()`` with the returned
   code for details.

``MULTIO_ERROR_GENERAL_EXCEPTION``
   A known ``std::exception`` was encountered. Call ``multio_error_string()`` with the returned code
   for details.

``MULTIO_ERROR_UNKNOWN_EXCEPTION``
   An unexpected and/or unrecognised error was encountered. Call ``multio_error_string()`` with the
   returned code for details.

.. code-block:: c

   multio_metadata_t* md;
   int rc = multio_new_metadata(multio_metadata_t** md);

   if (rc != MULTIO_SUCCESS) {
       // Error, retrieve message and print it.
       fprintf(stderr, "Failed to construct metadata: %s\n", multio_error_string(rc));
   }
   else {
       // Success, continue processing.
   }


.. note::

   Internally, **multio** is written in C++ and the error handling uses exceptions. All exceptions
   will be caught at the C/C++ boundary in the C API and an appropriate error code will be returned.


Failure Handler
~~~~~~~~~~~~~~~

In certain scenarios, it might be more appropriate to have a callback on error. Instead of checking
return code after each call, a handler function can be set that will be called back after an error
has occurred.

This approach is very useful when a specific clean-up procedure is needed, before the current
process is aborted.

.. code-block:: c

   void handle_failure(void* context, int error_code) {
       fprintf(stderr, "Error: %s\n", multio_error_string(error_code));
       clean_up();
       exit(1);
   }

   multio_set_failure_handler(handle_failure, NULL);


The ``context`` parameter is user-specified, and is defined as the second argument to
``multio_set_failure_handler``.


C++ Interface
-------------

The interface in C++ mainly exists as an underlying base for implementing :ref:`the C API
<c-interface>` which wraps it. It is only suitable to be used within an environment in which
`eckit`_ is being used. If this is not the case it is recommended to use the C API.

Python Interface
----------------

The python interface gives a users a pyhtonic way to interact with multio. The interface
implements the underlying C API through a Multio object. This object then takes a python dictionary
as a config which is used to create a handle. The user can then create metadata uing python
dictionaries and then use functionality associated with multio thorugh the Multio object such
as writing data. An example notebook can be found :doc:`multio:content/usage-examples/multiopython.ipynb`
which shows a basic example of the python interface.

.. _`eckit`: https://github.com/ecmwf/eckit
