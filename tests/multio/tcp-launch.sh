#!/bin/bash

binary=${1:-multio-hammer}

function fork_tcp_transport {
    local cmd="$binary --transport=\"tcp\" --port=$@ --nbclients=5 --nbservers=3"
    eval $cmd
}

pids=()

fork_tcp_transport 7771 & pids+=( "$!" )
fork_tcp_transport 7772 & pids+=( "$!" )
fork_tcp_transport 7773 & pids+=( "$!" )

sleep 0.05

fork_tcp_transport 4441 & pids+=( "$!" )
fork_tcp_transport 4442 & pids+=( "$!" )
fork_tcp_transport 4443 & pids+=( "$!" )
fork_tcp_transport 4444 & pids+=( "$!" )
fork_tcp_transport 4445 & pids+=( "$!" )

code=0
for pid in "${pids[@]}"
do
    wait $pid
    tmp=$?
    if [[ $tmp != 0 ]]; then code=$tmp; fi
done

echo "All clients and servers stopped... spawning test child"

# Finally run and check
cmd="$binary --transport=\"tcp\" --check-data-only=true"
eval $cmd
tmp=$?
code=0
if [[ $tmp != 0 ]]; then code=$tmp; fi

exit $code
