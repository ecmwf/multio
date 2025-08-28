#!/bin/bash

timeout "${1}s" "${@:2}"
if [ $? -eq 124 ]; then
    exit 0
else
    exit 1
fi
