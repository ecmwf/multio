#!/bin/bash

usage(){
    echo "Usage: $0 -o output_path -f bin_path -b fdb_path [-v]"
    echo
    echo "Options:"
    echo "  -o output_path  Path to the directory where the output files will be written."
    echo "  -b bin_path     Path to the directory containing the binary files."
    echo "  -f fname        Name of the input grib file."
    echo "  -h              Print this message."
    echo
    echo "All paths must be directories that exist. The script will exit with an error if any path does not exist."
    echo "The -v option is optional and enables verbose mode."
}

# Initialize our own variables:
output_path=""
bin_path=""
fname=""
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
      fname="${OPTARG}"
      if ! [[ -f ${fname} ]] ; then
          echo "Error: The input file don't exist"
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
echo "Current Input Configuration:"
echo "Output Path: ${output_path}"
echo "Binary Path: ${bin_path}"
echo "Input File: ${fname}"
echo "Verbose Mode: ${verbose}"

# Clear the index files
rm -f ${output_path}/index_file.unsorted.dat
rm -f ${output_path}/index_file.sorted.dat

# Create the index file
${bin_path}/grib_get -p step,paramId,level,levtype,gridType "${fname}" | awk '{ if ( $5 == "sh") {printf "%4d  %8d  %8d  %1d  %-12s\n", $1, $2, $3, 1, $4; } else { printf "%4d  %8d  %8d  %1d  %-12s\n", $1, $2, $3, 0, $4;  }; }'  >> ${output_path}/index_file.unsorted.dat;


# Sort the index file
sort -n -k1,1 -k2,2 -k3,3 -k4,4 -k5,5 ${output_path}/index_file.unsorted.dat -o ${output_path}/index_file.sorted.dat

# Exit successfully
exit 0