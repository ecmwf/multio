#!/bin/bash

# Usage:
# ./test_multio_step_meter.sh <nServers> <expected> <firstClientSteps> <secondClientSteps> ... <lastClientSteps>
#
# Example
# ./test_multio_step_meter.sh 2 12 1,2,3,4,5,6,7,8,9,10,11,12 1,3,6,9,12
#
# Environment variables:
#   MPIEXEC_EXECUTABLE  - path to mpiexec (default: mpiexec)
#   MPI_ARGS            - extra mpiexec arguments (optional)
#   MULTIO_PROBE_PATH   - path to multio-probe binary (required)
#   TEST_BINARY_PATH    - path to test_multio_step_meter binary (required)

set -euo pipefail

mpi_executable=${MPIEXEC_EXECUTABLE:-mpiexec}
mpi_args=${MPI_ARGS:-}
probe_path=${MULTIO_PROBE_PATH:?MULTIO_PROBE_PATH must be set}
test_binary=${TEST_BINARY_PATH:?TEST_BINARY_PATH must be set}

nServers=$1
expected=$2
shift 2

meter_file="../multio_step.meter"
rm -f ${meter_file}

# Build the mpi command: one process per client, then nServers processes for the servers
cmd="${mpi_executable} ${mpi_args}"
for step_list in "$@"; do
    steps=${step_list//,/ }
    cmd="${cmd} -n 1 ${test_binary} ${steps} :"
done
cmd="${cmd} -n ${nServers} ${probe_path} --test"
echo "Running: ${cmd}"
eval ${cmd}

# Verify the meter file
if [[ ! -f "${meter_file}" ]]; then
    echo "ERROR: meter file '${meter_file}' not found"
    exit 1
fi

actual=$(cat "${meter_file}" | tr -d '[:space:]')
if [[ "${actual}" != "${expected}" ]]; then
    echo "ERROR: expected meter value '${expected}', got '${actual}'"
    exit 1
fi

echo "OK: meter value is ${actual}"
