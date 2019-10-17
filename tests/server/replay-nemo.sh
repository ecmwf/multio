#!/bin/bash

client_binary=${1:-multio-replay}
server_binary=${2:-multio-probe}

function launch_mpi {
    local cmd="mpiexec -np 7 $client_binary --nbclients=7 --nbservers=1 : -np 1 $server_binary --test"
    eval $cmd
}

launch_mpi
