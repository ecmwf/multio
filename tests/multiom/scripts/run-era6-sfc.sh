#!/bin/bash

# General path
HERE=/ec/res4/hpcperm/mavm/multiom-rules
cd ${HERE}

# environment variables
export MULTIO_INSTALL_DIR="${HERE}/install-debug-intel-2021.4.0-hpcx/share/multiom"
export KNOWLEDGE_VERSION='49r2v9'
export MULTIO_LOG_PATH="${HERE}/"
export DUMP_MAIN_DIR="/perm/mavm/ERAdata/dataset.00003/fc_init/dump/"
export MAIN_CONFIG_FILE="${MULTIO_INSTALL_DIR}/output_manager_cfg.yaml"

# Clean the old results
rm -f ${HERE}/*.grib
rm -f ${HERE}/*.log

# Call the feed
case "${1}" in

"sfc-list")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -d -q 5 -s 0
  ;;

"sfc-gg-entire-atmosphere-instant-base")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[59, 78, 79, 136, 137, 164, 206, 162059, 162060, 162061, 162062, 162063, 162071, 162072, 162093, 228044, 228050, 228052, 228088, 228089, 228090]" -s 0
  ;;

"src-gg-entire-atmosphere-chem")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[233032, 233033, 233034, 233035]" -s 0
  ;;

"hl-gg-height-above-ground-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228239, 228240, 228241, 228246, 228247, 228249]" -s 0
  ;;

"sfc-gg-height-above-ground-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[165, 166, 167, 168, 207, 174096, 228029, 228131, 228132]" -s 0
  ;;

"sfc-gg-cloud-base-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228023]" -s 0
  ;;

"sfc-ice-top-on-water-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228013]" -s 0
  ;;

"sfc-gg-lake-bottom-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228010]" -s 0
  ;;

"sfc-gg-mean-sea-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[151]" -s 0
  ;;

"sfc-gg-mixed-layer-parcel-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228231,228232,228233,228234]" -s 0
  ;;

"sfc-gg-mixing-layer-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228008,228009]" -s 0
  ;;

"sfc-gg-tropopause-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228045]" -s 0
  ;;

"sfc-gg-nominal-top-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[178, 179, 208, 209, 212]" -s 0
  ;;

"sfc-gg-most-unstable-parcel-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228235,228236,228237]" -s 0
  ;;

"sfc-gg-entire-lake-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[228007, 228011]" -s 0
  ;;

"sfc-gg-height-above-sea-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -p "[140245, 140249]" -s 1
  ;;

"sfcgg--wave-base-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 6 -s "1" -z "[228141, 228226, 33, 49, 201, 202, 238]"
  ;;


"sfc-gg-wave-period-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 6 -s "1" -z "[140114, 140115, 140116, 140117, 140118, 140119, 140120]"
  ;;

"sfc-gg-wave-spectra-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 6 -s "1" -z "[140251]"
  ;;

"sfc-instant")
  ${HERE}/install-debug-intel-2021.4.0-hpcx/bin/multiom-feed -t "GRIB-HEADER-TO-MULTIO" -i "${DUMP_MAIN_DIR}" -y "${MAIN_CONFIG_FILE}" -q 5 -s "0" -z "[228141, 228226, 33, 49, 201, 202, 238]"
  ;;

*)
  echo "ERROR: no test found"
esac
