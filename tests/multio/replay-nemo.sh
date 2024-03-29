#!/bin/bash

client_binary=${1:-multio-replay}
server_binary=${2:-multio-probe}
mpi_executable=${3:-mpiexec}
mpi_args=${4:- }

function launch_mpi {
    local cmd="$mpi_executable $mpi_args -n 7 $client_binary : -n 2 $server_binary --test"
    eval $cmd
}

launch_mpi
