#!/bin/bash

# NOTE we need to strip here because vanilla multio is for some reason huge
# We don't strip in general since that has caused some linker problem downstream
# But eg in Atlas we also strip due to size reason and so far no issues

if [ "$(uname)" != "Darwin" ] ; then
    strip --strip-unneeded /tmp/multio/target/multio/lib64/lib*.so
else 
    strip -x /tmp/multio/target/multio/lib/lib*.dylib
fi
