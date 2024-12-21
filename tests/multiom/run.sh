#!/bin/bash

# General path
HERE=/ec/res4/hpcperm/mavm/multiom-rules
cd ${HERE}

# environment variables
export MULTIO_INSTALL_DIR="${HERE}/install-debug-intel-2021.4.0-hpcx/share/multiom"
export KNOWLEDGE_VERSION='49r2v9'
export MULTIO_LOG_PATH="${HERE}/"


# Call the feed
${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -p 8

