#!/usr/bin/env bash
set -Eeuo pipefail

DATE_ARG=""
TARGET_DIR=""
STREAMS=(oper wave enfo waef)
TIMES=(0000 0300 0600 0900 1200 1500 1800 2100)
MATCH_STREAMS=()
IGNORE_STREAMS=()
MATCH_TIMES=()
IGNORE_TIMES=()
MATCH_TYPES=()
IGNORE_TYPES=()

usage() {
    cat <<'EOF'
Usage:
  mtg2-list.sh --date YYYYMMDD --target-dir /path/to/workdir [filters]

Required:
  --date YYYYMMDD             Date to query from FDB
  --target-dir <path>         Target directory for generated input/split/chunk files

Optional:
  --match-stream <list>       Include only these comma-separated streams
  --ignore-stream <list>      Exclude these comma-separated streams
  --match-time <list>         Include only these comma-separated times
  --ignore-time <list>        Exclude these comma-separated times
  --match-type <list>         Include only these comma-separated types
  --ignore-type <list>        Exclude these comma-separated types
  -h, --help, ?               Show this help

All stream/time combinations are queried. Filters apply only when building the
final list. Match filters are applied before ignore filters.
EOF
}

die() {
    echo "ERROR: $*" >&2
    exit 1
}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

set_csv_arg() {
    local value="$1"
    local option="$2"
    local destination="$3"
    local -n result="${destination}"

    [[ -n "${value}" && "${value}" != ,* && "${value}" != *, && "${value}" != *,,* ]] \
        || die "${option} requires a non-empty comma-separated list"
    IFS=',' read -r -a result <<< "${value}"
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --date)
                [[ $# -ge 2 ]] || die "--date requires an argument"
                DATE_ARG="$2"
                shift 2
                ;;

            --target-dir)
                [[ $# -ge 2 ]] || die "--target-dir requires an argument"
                TARGET_DIR="$2"
                shift 2
                ;;

            --match-stream|--ignore-stream|--match-time|--ignore-time|--match-type|--ignore-type)
                [[ $# -ge 2 ]] || die "$1 requires an argument"
                case "$1" in
                    --match-stream) set_csv_arg "$2" "$1" MATCH_STREAMS ;;
                    --ignore-stream) set_csv_arg "$2" "$1" IGNORE_STREAMS ;;
                    --match-time) set_csv_arg "$2" "$1" MATCH_TIMES ;;
                    --ignore-time) set_csv_arg "$2" "$1" IGNORE_TIMES ;;
                    --match-type) set_csv_arg "$2" "$1" MATCH_TYPES ;;
                    --ignore-type) set_csv_arg "$2" "$1" IGNORE_TYPES ;;
                esac
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

validate_filter_values() {
    local option="$1"
    local values_name="$2"
    local allowed_name="$3"
    local -n values="${values_name}"
    local -n allowed="${allowed_name}"
    local value
    local candidate
    local valid

    for value in "${values[@]}"; do
        valid=false
        for candidate in "${allowed[@]}"; do
            if [[ "${value}" == "${candidate}" ]]; then
                valid=true
                break
            fi
        done
        ${valid} || die "Invalid value for ${option}: ${value}"
    done
}

validate_args() {
    [[ -n "${DATE_ARG}" ]] || die "Missing required argument: --date"
    [[ -n "${TARGET_DIR}" ]] || die "Missing required argument: --target-dir"
    [[ -d "${TARGET_DIR}" ]] || die "Target directory does not exist: ${TARGET_DIR}"
    [[ -w "${TARGET_DIR}" ]] || die "Target directory is not writable: ${TARGET_DIR}"
    [[ -x "$(command -v fdb)" ]] || die "fdb command not found in PATH"
    [[ -x "$(command -v find)" ]] || die "find command not found in PATH"

    [[ "${DATE_ARG}" =~ ^[0-9]{8}$ ]] || die "--date must be YYYYMMDD: got '${DATE_ARG}'"
    date -d "${DATE_ARG}" "+%Y%m%d" >/dev/null 2>&1 || die "Invalid --date: ${DATE_ARG}"

    validate_filter_values --match-stream MATCH_STREAMS STREAMS
    validate_filter_values --ignore-stream IGNORE_STREAMS STREAMS
    validate_filter_values --match-time MATCH_TIMES TIMES
    validate_filter_values --ignore-time IGNORE_TIMES TIMES
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
    local time="$2"
    printf '%s/od-0001-%s-%s-%s.where\n' "${INPDIR}" "${time}" "${stream}" "${DATE_ARG}"
}

raw_list_file() {
    local stream="$1"
    local time="$2"
    printf '%s/od-0001-%s-%s-%s.list\n' "${INPDIR}" "${time}" "${stream}" "${DATE_ARG}"
}

gather_where() {
    local stream="$1"
    local time="$2"
    local out
    out="$(where_file "${stream}" "${time}")"

    log "Querying stream=${stream} time=${time}"
    if ! fdb where expver=0001,class=od,time="${time}",stream="${stream}",date="${DATE_ARG}" \
        > "${out}" 2>/dev/null; then
        log "Skipping unavailable stream=${stream} time=${time}"
        return 1
    fi
}

find_data_files() {
    local stream="$1"
    local time="$2"
    local where
    local list
    local root=""

    where="$(where_file "${stream}" "${time}")"
    list="$(raw_list_file "${stream}" "${time}")"
    : > "${list}"
    read -r root < "${where}" || true

    if [[ -z "${root}" || ! -d "${root}" ]]; then
        log "Skipping unavailable stream=${stream} time=${time}"
        return 0
    fi

    if ! { find "${root}" -name '*data' -printf '%s\t%p\n' | sort -n | cut -f2-; } \
        > "${list}" 2>/dev/null; then
        : > "${list}"
        log "Skipping unavailable stream=${stream} time=${time}"
    fi
}

retrieve_combination() {
    local stream="$1"
    local time="$2"
    local list

    list="$(raw_list_file "${stream}" "${time}")"
    if ! gather_where "${stream}" "${time}"; then
        : > "${list}"
        return
    fi
    find_data_files "${stream}" "${time}"
}

contains_value() {
    local expected="$1"
    local values_name="$2"
    local -n values="${values_name}"
    local value

    for value in "${values[@]}"; do
        [[ "${value}" == "${expected}" ]] && return 0
    done
    return 1
}

dimension_selected() {
    local value="$1"
    local matches_name="$2"
    local ignores_name="$3"
    local -n matches="${matches_name}"

    if (( ${#matches[@]} > 0 )) && ! contains_value "${value}" "${matches_name}"; then
        return 1
    fi
    ! contains_value "${value}" "${ignores_name}"
}

type_selected() {
    local path="$1"
    local type
    local matched=false

    if (( ${#MATCH_TYPES[@]} > 0 )); then
        for type in "${MATCH_TYPES[@]}"; do
            if [[ "${path}" == *"/${type}:"* ]]; then
                matched=true
                break
            fi
        done
        ${matched} || return 1
    fi

    for type in "${IGNORE_TYPES[@]}"; do
        [[ "${path}" == *"/${type}:"* ]] && return 1
    done
    return 0
}

build_final_input_list() {
    local final_list
    local stream
    local time
    local list
    local path
    final_list="${CHUNKDIR}/od-0001-all-all-${DATE_ARG}.list"

    log "Building final input list: ${final_list}"
    : > "${final_list}"
    for stream in "${STREAMS[@]}"; do
        dimension_selected "${stream}" MATCH_STREAMS IGNORE_STREAMS || continue
        for time in "${TIMES[@]}"; do
            dimension_selected "${time}" MATCH_TIMES IGNORE_TIMES || continue
            list="$(raw_list_file "${stream}" "${time}")"
            while IFS= read -r path || [[ -n "${path}" ]]; do
                [[ -n "${path}" ]] || continue
                type_selected "${path}" && printf '%s\n' "${path}" >> "${final_list}"
            done < "${list}"
        done
    done

    log "Created ${final_list} with $(wc -l < "${final_list}") files"
}

require_slurm_allocation() {
    local job_id="${SLURM_JOB_ID:-${SLURM_JOBID:-}}"

    if [[ -z "$job_id" ]]; then
        echo "ERROR: this command must be run inside a SLURM allocation/job." >&2
        exit 1
    fi

    echo "Running inside SLURM allocation: ${job_id}"
}

main() {

    parse_args "$@"
    validate_args
    require_slurm_allocation

    prepare_dirs

    log "Gathering lists of files for date=${DATE_ARG} into ${TARGET_DIR}"

    local stream
    local time
    for stream in "${STREAMS[@]}"; do
        for time in "${TIMES[@]}"; do
            retrieve_combination "${stream}" "${time}"
        done
    done

    build_final_input_list
}

main "$@"
