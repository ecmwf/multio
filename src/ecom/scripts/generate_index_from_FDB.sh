#!/bin/bash

usage(){
    echo "Usage: $0 -o output_path -f bin_path -b fdb_path [-v]"
    echo
    echo "Options:"
    echo "  -o output_path  Path to the directory where the output files will be written."
    echo "  -b bin_path     Path to the directory containing the binary files."
    echo "  -f fdb_path     Path to the directory containing the fdb database."
    echo "  -h              Print this message."
    echo
    echo "All paths must be directories that exist. The script will exit with an error if any path does not exist."
    echo "The -v option is optional and enables verbose mode."
}

# Initialize our own variables:
output_path=""
bin_path=""
fdb_path=""
verbose=0

# Parse command-line arguments
while getopts "o:f:b:h" opt; do
  case ${opt} in
    o)
      output_path="${OPTARG}"
      if ! [[ -d ${output_path} ]] ; then
          echo "Error: The output path is not a directory"
          exit 1
      fi
      ;;
    b)
      bin_path="${OPTARG}"
      if ! [[ -d ${bin_path} ]] ; then
          echo "Error: The binary path is not a directory"
          exit 1
      fi
      ;;
    f)
      fdb_path="${OPTARG}"
      if ! [[ -d ${fdb_path} ]] ; then
          echo "Error: The fdb path path is not a directory"
          exit 1
      fi
      ;;
    h)
      usage
      exit 1
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done

# Error handling
if !command -v ${bin_path}/grib_get &> /dev/null; then
    echo "grib_get is not in the PATH"
    exit 1
fi


# Dump the current input configuration
echo "Current input configuration:"
echo "Output Path: ${output_path}"
echo "Binary Path: ${bin_path}"
echo "FDB Path: ${fdb_path}"
echo "Verbose Mode: ${verbose}"


# Clear the index files
rm -f ${output_path}/index.unsorted.dat
rm -f ${output_path}/index.sorted.dat

# Create the index file
echo "Start creating the index file"
for fname in `find "${fdb_path}" -type f -iname '*.data'` ; do
    ${bin_path}/grib_get -p step,paramId,level,levtype,gridType "${fname}" | awk -v c1="${fname}"  '{ if ( $5 == "sh") {printf "%4d  %8d  %8d  %1d  %-12s  %s\n", $1, $2, $3, 1, $4, c1; } else { printf "%4d  %8d  %8d  %1d  %-12s  %s\n", $1, $2, $3, 0, $4, c1;  }; }'  >> ${output_path}//index.unsorted.dat;
done

# Sort the index file
sort -n -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 ${output_path}/index.unsorted.dat -o ${output_path}/index.sorted.dat

# Exit successfully
exit 0