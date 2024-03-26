#!/bin/bash

#
usage(){
    echo "Usage: $0 -p paramId -l level -s step -d databaseFile -i inputDir -b binaryPath -c samplesPath"
    echo
    echo "Options:"
    echo "  -p paramId        Parameter ID."
    echo "  -l level          Level."
    echo "  -s step           Step."
    echo "  -d databaseFile   Path to the input index file."
    echo "  -i inputDir       Path to the directory containing the input files."
    echo "  -b binaryPath     Path to the directory containing the binary files."
    echo "  -c samplesPath    Path to the directory containing the samples."
    echo "  -y yamlConfig     Path to the yaml configuration file."
    echo "  -h                Print this message."
    echo
    echo "EXAMPLE: run.sh -y "/ec/res4/scratch/mavm/develop_v7/raps/multio_yaml/output-manager-config.yaml" -i "/ec/res4/scratch/mavm/develop_v7/runs/rundir/tco399l137/i0e9/hres/intel.hpcx/blq.intel.sp/h24.N8.T120xt8xh1+ioT8xt8xh0.iomode_od.i16r0w16.ORCA025_Z75.htco399-33824009" -p 77  -l 4 -s 0 -d "./index.sorted.dat" -b "/ec/res4/scratch/mavm/develop_v7/ifs-bundle/build/bin" -c "/ec/res4/scratch/mavm/develop_v7/ifs-bundle/source/eccodes/ifs_samples/grib1_mlgrib2""
    echo "All parameters must be provided. The script will exit with an error if any parameter is missing."
}

#
# Initialize our own variables:
paramId=-1
level=-1
step=-1
databaseFile=""
inputDir=""
binaryPath=""
samplesPath=""
yamlConfig=""
nProcs=1

#
# Parse command-line arguments
while getopts "p:l:s:d:i:b:c:y:n:h" opt; do
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
    d) # input index file
      databaseFile="${OPTARG}"
      if [[ ! -f ${databaseFile} ]] ; then
          echo "Error: The input index file does not exist"
          exit 1
      fi
      ;;
    i) # input index file
      inputDir="${OPTARG}"
      if [[ ! -d ${inputDir} ]] ; then
          echo "Error: The input directory does not exist"
          exit 1
      fi
      ;;
    b) # binary path
      binaryPath="${OPTARG}"
      if [[ ! -d ${binaryPath} ]] ; then
          echo "Error: The binary path does not exist"
          exit 1
      fi
      ;;
    c) # codes samples path
      samplesPath="${OPTARG}"
      if [[ ! -d ${samplesPath} ]] ; then
          echo "Error: The samples path does not exist"
          exit 1
      fi
      ;;
    y) # codes samples path
      yamlConfig="${OPTARG}"
      if [[ ! -f ${yamlConfig} ]] ; then
          echo "Error: The yaml configuration file does not exist: ${yamlConfig}"
          exit 1
      fi
      ;;
    n) # number of processors
      nProcs="${OPTARG}"
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

#
# Error handling
if !command -v ${binaryPath}/output_manager_tool.SP &> /dev/null; then
    echo "output manager tool is not in the path is not in the PATH"
    exit 1
fi

if !command -v "${binaryPath}"/grib_coompare &> /dev/null; then
    echo "grib_compare is not in the PATH"
    exit 1
fi

#
# Generate the report of the input configuration
echo "Input Configuration Report:"
echo "---------------------------"
echo "Parameter ID.........: $paramId"
echo "Level................: $level"
echo "Step.................: $step"
echo "Representation.......: $representation"
echo "Level Type...........: $levType"
echo "Input Index File.....: $databaseFile"
echo "Input Directory......: $inputDir"
echo "Binary Path..........: $binaryPath"
echo "Samples Path.........: $samplesPath"
echo "YAML Configuration...: $yamlConfig"

#
# Set the environment variables
export ECCODES_SAMPLES_PATH=${samplesPath}

${binaryPath}/codes_info

#
# Run the standalone tool
${binaryPath}/standalone-output-manager -t "GRIBX" -n 8 -i "${inputDir}" -p "${paramId}" -l "${level}" -s "${step}" -y "${yamlConfig}"

#
# Search and extract the reference message
sourceSearch01.sh -p "${paramId}" -l "${level}" -s "${step}" -i "${databaseFile}" -b "${binaryPath}"

#
# Check if the reference file was created
if ! [[ -f "reference.grib" ]]; then
    echo "Error: The reference file was not created"
    exit 1
fi

if ! [[ -f "allfields_00000001.grib" ]]; then
    echo "Error: The reference file was not created"
    exit 1
fi

#
# Compare the reference file with the fields file
${binaryPath}/grib_compare -H "allfields_00000001.grib" "reference.grib"

#
# Report comparison result
if [[ $? -ne 0 ]]; then
    echo "Error: The files are different"
    exit 1
else
    echo "The files are the same"
    exit 0
fi
