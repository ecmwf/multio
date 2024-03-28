#!/bin/bash

usage(){
    echo "Usage: $0 -p paramId -l level -s step -r representation -t levType -i indexFile"
    echo
    echo "Options:"
    echo "  -p paramId        Parameter ID."
    echo "  -l level          Level."
    echo "  -s step           Step."
    echo "  -r representation Representation."
    echo "  -t levType        Level type."
    echo "  -i indexFile      Path to the input index file."
    echo "  -h                Print this message."
    echo
    echo "All parameters must be provided. The script will exit with an error if any parameter is missing."
}

# Initialize our own variables:
paramId=-1
level=-1
step=-1
representation=-1
levType=-1
indexFile=""

# Parse command-line arguments
while getopts "p:l:s:r:t:i:h" opt; do
  case ${opt} in
    p) # paramId
      paramId="${OPTARG}"
      ;;
    l) # level
      level="${OPTARG}"
      ;;
    s) # step
      step="${OPTARG}"
      ;;
    r) # representation
      represetnation="${OPTARG}"
      ;;
    t) # levtype
      levType="${OPTARG}"
      ;;
    i) # input index file
      indexFile="${OPTARG}"
      if [[ ! -f ${indexFile} ]] ; then
          echo "Error: The input index file does not exist"
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
if [ $paramId -eq -1 ] || [ $level -eq -1 ] || [ $step -eq -1 ] || [ $representation -eq -1 ] || [ $levType -eq -1 ] || [ -z $indexFile ]; then
    echo "Error: Missing parameters"
    usage
    exit 1
fi

# Check if the required commands are in the PATH
if !command -v grib_copy &> /dev/null; then
    echo "grib_copy is not in the PATH"
    exit 1
fi

if !command -v grib_count &> /dev/null; then
    echo "grib_count is not in the PATH"
    exit 1
fi


# Check if the correct number of arguments is provided
if [ "$#" -ne 5 ]; then
    echo "Usage: $0 step paramId level isSphericalHarmonics levType"
    exit 1
fi

# Dump the used configuration
echo "Used Configuration:"
echo "  - Parameter ID: $paramId"
echo "  - Level: $level"
echo "  - Step: $step"
echo "  - Representation: $representation"
echo "  - Level Type: $levType"
echo "  - Index File: $indexFile"

# AWK script to print lines where all values in the first five columns meet the conditions
cnt=0
fname="undefined"
for i in `awk -v c1="${step}" -v c2="${paramId}" -v c3="${level}" -v c4="${representation}" -v c5="${levType}" '
{
    if ($1 == c1 && $2 == c2  && $3 == c3 && $4 == c4  && $5 == c5) {
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
grib_copy -w paramId="${2}",step="${1}",level="${3}" "${fname}" reference.grib

# Count the messages in the reference file
N=`grib_count reference.grib`
if [ $N -ne 1 ]; then
    echo "Error: More than one message found"
    exit 1
fi

# Exit successfully
exit 0