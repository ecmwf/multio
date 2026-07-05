#!/usr/bin/env bash
set -Eeuo pipefail

DATE_ARG=""
TIME_ARG=""
TARGET_DIR=""

usage() {
    cat <<'EOF'
Usage:
  grib1-to-grib2-gather-lists.sh --date YYYYMMDD --time HHMM --target-dir /path/to/workdir

Required:
  --date YYYYMMDD       Date to query from FDB
  --time HHMM           Time to query from FDB
  --target-dir <path>   Target directory for generated input/split/chunk files

Optional:
  -h, --help, ?         Show this help
EOF
}

die() {
    echo "ERROR: $*" >&2
    exit 1
}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --date)
                [[ $# -ge 2 ]] || die "--date requires an argument"
                DATE_ARG="$2"
                shift 2
                ;;

            --time)
                [[ $# -ge 2 ]] || die "--time requires an argument"
                TIME_ARG="$2"
                shift 2
                ;;

            --target-dir)
                [[ $# -ge 2 ]] || die "--target-dir requires an argument"
                TARGET_DIR="$2"
                shift 2
                ;;

            -h|--help|\?)
                usage
                exit 0
                ;;

            *)
                die "Unknown argument: $1"
                ;;
        esac
    done
}

validate_args() {
    [[ -n "${DATE_ARG}" ]] || die "Missing required argument: --date"
    [[ -n "${TIME_ARG}" ]] || die "Missing required argument: --time"
    [[ -n "${TARGET_DIR}" ]] || die "Missing required argument: --target-dir"

    [[ "${DATE_ARG}" =~ ^[0-9]{8}$ ]] || die "--date must be YYYYMMDD: got '${DATE_ARG}'"
    [[ "${TIME_ARG}" =~ ^[0-9]{4}$ ]] || die "--time must be HHMM: got '${TIME_ARG}'"

    local hh
    local mm
    hh="${TIME_ARG:0:2}"
    mm="${TIME_ARG:2:2}"

    (( 10#$hh >= 0 && 10#$hh <= 23 )) || die "Invalid hour in --time: ${hh}"
    (( 10#$mm >= 0 && 10#$mm <= 59 )) || die "Invalid minute in --time: ${mm}"

    date -d "${DATE_ARG}" "+%Y%m%d" >/dev/null 2>&1 || die "Invalid --date: ${DATE_ARG}"
}

prepare_dirs() {
    mkdir -p "${TARGET_DIR}"
    TARGET_DIR="$(cd "${TARGET_DIR}" && pwd -P)"
    INPDIR="${TARGET_DIR}/input"
    SPLITDIR="${TARGET_DIR}/split"
    CHUNKDIR="${TARGET_DIR}/chunks"
    mkdir -p "${INPDIR}" "${SPLITDIR}" "${CHUNKDIR}"
}

where_file() {
    local stream="$1"
    printf '%s/od-0001-%s-%s-%s.where\n' "${INPDIR}" "${TIME_ARG}" "${stream}" "${DATE_ARG}"
}

raw_list_file() {
    local stream="$1"
    printf '%s/od-0001-%s-%s-%s.list\n' "${INPDIR}" "${TIME_ARG}" "${stream}" "${DATE_ARG}"
}

split_list_file() {
    local stream="$1"
    local product="$2"
    printf '%s/od-0001-%s-%s-%s-%s.list\n' "${SPLITDIR}" "${TIME_ARG}" "${stream}" "${product}" "${DATE_ARG}"
}

gather_where_for_stream() {
    local stream="$1"
    local out
    out="$(where_file "${stream}")"

    log "Running fdb where for stream=${stream}"
    fdb where expver=0001,class=od,time="${TIME_ARG}",stream="${stream}",date="${DATE_ARG}" > "${out}"
}

find_data_files_for_stream() {
    local stream="$1"
    local where
    local list
    local root

    where="$(where_file "${stream}")"
    list="$(raw_list_file "${stream}")"
    read -r root < "${where}"

    [[ -n "${root}" ]] || die "No root path found in ${where}"
    [[ -d "${root}" ]] || die "FDB root path does not exist or is not a directory: ${root}"

    log "Finding data files for stream=${stream}"
    find "${root}" -name '*data' -printf '%s\t%p\n' | sort -n | cut -f2- > "${list}"
}

split_ensemble_stream() {
    local stream="$1"
    local list
    local product
    local out

    list="$(raw_list_file "${stream}")"
    for product in pf em es ep icp efi efic sot gbf gwt pfc cm; do
        out="$(split_list_file "${stream}" "${product}")"
        grep -E "/${product}:" "${list}" > "${out}" || true
    done
}

build_final_input_list() {
    local final_list
    final_list="${CHUNKDIR}/od-0001-${TIME_ARG}-all-${DATE_ARG}.list"

    log "Building final input list: ${final_list}"
    cat \
        "$(raw_list_file oper)" \
        "$(raw_list_file wave)" \
        "$(split_list_file enfo pf)" \
        "$(split_list_file enfo em)" \
        "$(split_list_file enfo es)" \
        "$(split_list_file enfo ep)" \
        "$(split_list_file enfo icp)" \
        "$(split_list_file enfo gbf)" \
        "$(split_list_file enfo gwt)" \
        "$(split_list_file waef pf)" \
        "$(split_list_file waef em)" \
        "$(split_list_file waef es)" \
        "$(split_list_file waef ep)" \
        "$(split_list_file waef icp)" \
        "$(split_list_file waef gbf)" \
        "$(split_list_file waef gwt)" \
        > "${final_list}"

    log "Created ${final_list} with $(wc -l < "${final_list}") files"
}

main() {
    parse_args "$@"
    validate_args
    prepare_dirs

    log "Gathering lists of files for date=${DATE_ARG} time=${TIME_ARG} into ${TARGET_DIR}"

    local stream
    for stream in oper wave enfo waef; do
        gather_where_for_stream "${stream}"
        find_data_files_for_stream "${stream}"
    done

    split_ensemble_stream enfo
    split_ensemble_stream waef
    build_final_input_list
}

main "$@"
