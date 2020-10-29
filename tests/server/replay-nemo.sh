#!/bin/bash

mpi_args=$1
client_binary=${2:-multio-replay}
server_binary=${3:-multio-probe}

function launch_mpi {
    local cmd="mpiexec $mpi_args -np 7 $client_binary : -np 1 $server_binary --test"
    eval $cmd
}

launch_mpi
