
#include <iostream>

#include "multio/sandbox/ThreadTransport.h"

using namespace multio;

int main(int argc, char** argv) {
    eckit::LocalConfiguration config;
    config.set("name", "multiple threads");
    config.set("no_clients", 7);
    config.set("no_servers", 3);

    std::unique_ptr<sandbox::Transport> transport{new sandbox::ThreadTransport{config}};

    std::cout << *transport << std::endl;
}
