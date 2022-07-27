.. index:: Introduction

Introduction
============

**multio** provides C++ libraries for data routing from distributed meteorological and earth-system models. It supports
  * I/O multiplexing to produce multiple output data from the same input;
  * I/O-server functionality to create aggregated horizontal fields from distributed parallel models;
  * post-processing pipelines to calculate derived meteorological products, such as temporal
    pointwise statistics or interpolation onto a different grids.

The data-routing is driven by metadata attached to the data, together with a configuration defining
the post-processing pipelines.
