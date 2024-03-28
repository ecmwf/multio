#!/bin/bash

usage(){
    echo "Usage: $0 -p paramId -l level -s step -r representation -t levType -i indexFile"
    echo
    echo "Options:"
    echo "  -s step           Step."
    echo "  -p paramId        Parameter ID."
    echo "  -l level          Level."
    echo "  -i indexFile      Path to the input index file."
    echo "  -b binaryPath     Binary path."
    echo "  -h                Print this message."
    echo
    echo "All parameters must be provided. The script will exit with an error if any parameter is missing."
}


# Initialize our own variables:
paramId=-1
level=-1
step=-1
indexFile=""
binaryPath=""

# Parse command-line arguments
while getopts "s:p:l:i:b:h" opt; do
  case ${opt} in
    s) # step
      step="${OPTARG}"
      ;;
    p) # paramId
      paramId="${OPTARG}"
      ;;
    l) # level
      level="${OPTARG}"
      ;;
    i) # input index file
      indexFile="${OPTARG}"
      if [[ ! -f ${indexFile} ]] ; then
          echo "Error: The input index file does not exist"
          exit 1
      fi
      ;;
    b) # level
      binaryPath="${OPTARG}"
      if [[ ! -d ${binaryPath} ]] ; then
          echo "Error: The binary path does not exist"
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

# Check for missing parameters
if [ $paramId -eq -1 ] || [ $level -eq -1 ] || [ $step -eq -1 ] ; then
    echo "Error: Missing parameters"
    usage
    exit 1
fi

# Check if the required commands are in the PATH
if !command -v "${binaryPath}"/grib_copy &> /dev/null; then
    echo "grib_copy is not in the PATH"
    exit 1
fi

if !command -v "${binaryPath}"/grib_count &> /dev/null; then
    echo "grib_count is not in the PATH"
    exit 1
fi

# remove the reference file if it exists
rm -f reference.grib

# Dump the used configuration
echo "Used Configuration:"
echo "  - Step: $step"
echo "  - Parameter ID: $paramId"
echo "  - Level: $level"
echo "  - Index File: $indexFile"

# AWK script to print lines where all values in the first five columns meet the conditions
cnt=0
fname="undefined"
for i in `awk -v c1="${step}" -v c2="${paramId}" -v c3="${level}" '
{
    if ($1 == c1 && $2 == c2  && $3 == c3) {
        print $6
    }
}' index.sorted.dat` ; do
    cnt=$((${cnt}+1));
    if [[ ${cnt} -gt 1 ]] ; then
        echo "Error: More than one file found"
        exit 1
    fi
    fname="${i}"
done
echo " + File to be opened: " ${fname}

# Copy the grib message to be compared to the local directory
${binaryPath}/grib_copy -w paramId="${paramId}",step="${step}",level="${level}" "${fname}" reference.grib

if [[ $? -ne 0 ]]; then
    echo "Error: grib_copy failed"
    exit 1
fi

if ! [[ -f reference.grib ]]; then
    echo "Error: The reference file was not created"
    exit 1
fi


# Count the messages in the reference file
N=`grib_count reference.grib`
if [ $N -ne 1 ]; then
    echo "Error: More than one message found"
    exit 1
fi

# Exit successfully
exit 0