#!/bin/bash

# General path
HERE=/ec/res4/hpcperm/mavm/multiom-rules
cd ${HERE}

# environment variables
export MULTIOM_INSTALL_DIR="${HERE}/install-debug-intel-2021.4.0-hpcx/share/multiom"
export BIN_DIR="${HERE}/install-debug-intel-2021.4.0-hpcx/bin"
export KNOWLEDGE_VERSION='49r2v9'
export MULTIO_LOG_PATH="${HERE}/"
export REF_FDB_HOME="/perm/mavm/ERAdata/dataset.00003/fc_init/ref"
export MAIN_CONFIG_FILE="${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml"


echo " + Running the scf filter"
${BIN_DIR}/grib_filter  "${MULTIOM_INSTALL_DIR}/bin/era6-sfc.filter"  ${REF_FDB_HOME}/root/**/*.data
