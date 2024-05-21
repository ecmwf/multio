#!/bin/bash

# (C) Copyright 2022 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


client_binary=${1:-multio-replay}
server_binary=${2:-multio-probe}
mpi_executable=${3:-mpiexec}
mpi_args=${4:- }


function launch_mpi {
    local cmd="$mpi_executable $mpi_args -n 7 $client_binary : -n 2 $server_binary --test"
    echo "Running MPI command: $cmd"
    eval $cmd
}

launch_mpi
