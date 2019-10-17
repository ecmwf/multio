#!/bin/bash

binary=${1:-multio-hammer}

function fork_tcp_transport {
    local cmd="$binary --transport=\"tcp\" --port=$@ --nbclients=5 --nbservers=3"
    eval $cmd
}

fork_tcp_transport 7771 &
fork_tcp_transport 7772 &
fork_tcp_transport 7773 &

sleep 0.05

fork_tcp_transport 4441 &
fork_tcp_transport 4442 &
fork_tcp_transport 4443 &
fork_tcp_transport 4444 &
fork_tcp_transport 4445
