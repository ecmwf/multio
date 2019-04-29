
function fork_tcp_transport {
    local cmd="./multio/src/multio/server/multio-hammer --transport=\"tcp\" --port=$@"
    eval $cmd
}

fork_tcp_transport 7771 &
fork_tcp_transport 7772 &
fork_tcp_transport 7773 &

sleep 0.01

fork_tcp_transport 4441 &
fork_tcp_transport 4442 &
fork_tcp_transport 4443 &
fork_tcp_transport 4444 &
fork_tcp_transport 4445

sleep 1
