Multio Example
==============

Shown below is an example of reading some fields from a file and writing them in C++ but using the C api.
The example reads a field from a grib file and then outputs this to a location based on the multio-server.yaml file.

.. literalinclude:: ../../../tests/multio/multio_example.cc
    :language: c++
    :class: copybutton

This example code using the following config file:

.. literalinclude:: ../../../tests/multio/multio-server.yaml
    :language: yaml
    :class: copybutton

