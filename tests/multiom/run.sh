#!/bin/bash

# General path
HERE=/ec/res4/hpcperm/mavm/multiom-rules
cd ${HERE}

# environment variables
export MULTIO_INSTALL_DIR="${HERE}/install-debug-intel-2021.4.0-hpcx/share/multiom"
export KNOWLEDGE_VERSION='49r2v9'
export MULTIO_LOG_PATH="${HERE}/"


# Call the feed
case "${1}" in

"one")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -q 5 -p 8 -s 0
  ;;


"sfc")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -q 5 -z "[33, 49, 183, 201, 238, 228141, 228226 ]" -s "[0, 1, 2, 3, 4]"
  ;;

"ml")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -q 1 -s "[ 0, 4]"
  ;;

"pl")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -q 2 -s "[ 0]"
  ;;


"sh")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -r 2
  ;;

"spm")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -q "[1,2,5]" -z "[33, 49, 183, 201, 238, 228141, 228226 ]" -s 0
  ;;


"all")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "/perm/mavm/ERAdata/dataset.00002/fc_init/dump.02/" -y "${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml" -z "[33, 49, 183, 201, 238, 228141, 228226 ]" -s 0
  ;;

*)
  echo "ERROR: no test found"
esac

