#!/bin/bash

usage(){
    echo "Usage: $0 -m <dirName> -s <[0-9]*> -l <[ml|pl|sfc|t|v]> -f <0|1> [-h]"
    echo
    echo "Options:"
    echo "  -m mainDir     Main directory where the databaase has been extracted/filtered."
    echo "  -s step        Step"
    echo "  -l levType     levType"
    echo "  -f isSperical  true if it is a spherical harmonics"
    echo "  -h             Print this message."
    echo
    echo "All paths must be directories that exist. The script will exit with an error if any path does not exist."
    echo "The -v option is optional and enables verbose mode."
}


#
# 
levType2enum(){
# echo "Input option:: " ${1}
case ${1} in
  ml)
    echo 1
    return;
    ;;
  pl) 
    echo 2
    return;
    ;;
  sfc)
    echo 5	  
    return;
    ;;
  t)
    echo 4
    return;
    ;;
  v)
    echo 3
    return;
    ;;
  *)
    echo "ERROR: unknown levType"
    exit 1
  esac
}


#
# Initialize our own variables:
mainDir="./compareDB.mavm"
step=4
cstep="step_0004"
levType="ml"
levTypeEnum=0
isSpehrical=0
executable="./secom"
reproducerFolder="./reproducer"
configurationFile="output_manager_config.yaml"

# Bold green text for [OK]
OK="\e[1;32mOK\e[0m"

# Bold red text for [KO]
KO="\e[1;31mKO\e[0m"

#
# Parse command-line arguments
while getopts "b:r:y:m:s:l:f:h" opt; do
  case ${opt} in
    b)
      executable=${OPTARG};
      if ! [[ -x ${executable} ]] ; then
        echo "ERROR: executable does not exist"
      fi
      ;;
    r)
      reproducerFolder=${OPTARG};
      if ! [[ -d ${reproducerFolder} ]] ; then
        echo "ERROR: reproducer folder does not exist"
      fi
      ;;
    y)
      configurationFile=${OPTARG};
      if ! [[ -f ${configurationFile} ]] ; then
        echo "ERROR: configuration file does not exist"
      fi
      ;;
    m)
      mainDir="${OPTARG}"
      ;;
    s)
      step="${OPTARG}"
      cstep=`printf "step_%4.4d" ${step}`
      ;;
    l)
      levType="${OPTARG}"
      levTypeEnum=`levType2enum  ${levType}`
      ;;
    f)
      isSpherical="${OPTARG}"
      if [[ ${isSpherical} != "0" ]] && [[ ${isSpherical} != "1" ]] ; then
        echo "ERROR: isSperical wrong value: ${isSpherical}"
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

#
# 
export ECCODES_SAMPLES_PATH=/home/valentini/ecmwf/ecom/multio-bundle/source/eccodes/ifs_samples/grib1_mlgrib2

#
# Create the main directory name
dirName=`printf "%s/step_%4.4d/%s_%d" ${mainDir} ${step} ${levType} ${isSpherical}`

#
# Error handling
if ! [[ -d ${mainDir} ]] ; then
  echo "Error: The main directory does not exist: ${mainDir}"
  exit 1
fi

if ! [[ -d ${mainDir}/${cstep} ]] ; then
  echo "Error: The main directory does not exist: ${mainDir}/${cstep}"
  exit 1
fi

if ! [[ -d ${dirName} ]] ; then
  echo "Error: The directory does not exist: ${dirName}"
  exit 1
fi


# Error handling
if !command -v ${bin_path}/grib_get &> /dev/null; then
    echo "grib_get is not in the PATH"
    exit 1
fi


# echo ${dirName}
cd ${dirName}
rm -f *.grib
rm -f *.log
rm -f *.report
rm -rf ./errors
mkdir ./errors
N=`find . -type f -iname 'msg_*' | wc -l`;
cnt=0;
err=0;
echo " - ${N} fields to check: ${dirName}"
for i in `find . -type f -iname 'msg_*'` ; do
  cnt=$((${cnt}+1))	
  tmp=`echo "scale=10; ${cnt}/${N}*100" | bc -l | sed -e 's/\./,/'`;
  adv=`printf "%6.2f" ${tmp}`;
  # Extract paramId and levelist
  # echo "Processing file: ", $i
  pt=`echo $i | sed -e '{s/_/ /g}' | awk '{print $2;}'`;
  l=`echo $i | sed -e '{s/_/ /g}' | awk '{print $3;}'`;
  if [[ ${levTypeEnum} -eq 2  ]] ; then
    l=$((${l}*100))
  fi
  p=`echo ${pt} | awk -F '.' '{ if( NF==2 ){ if ( $2 != 128 ) {printf "%3.3d%3.3d", $2, $1;} else { print $1; } } else { print $1; }  }'`;

  r=$((${isSpherical} + 1))
  # Checking the field
  command="${executable} -t GRIBX -i ${reproducerFolder}  -y ${configurationFile} -s ${step}  -q ${levTypeEnum} -r ${r} -p ${p} -l ${l} -n 8"
  echo -n "[${adv}%].........................................................."
  rm -f *.grib
  rm -f *.log
  rm -f *.report
  ${executable} -t "GRIBX" -i "${reproducerFolder}"  -y "${configurationFile}" -s "${step}"  -q "${levTypeEnum}" -r "${r}" -p "${p}" -l "${l}" -n "8"   > encoding.log
  if [[ -f ./allfields_00000001.grib  ]] ; then
    grib_compare -H "./allfields_00000001.grib" "${i}" > grib_compare.log 2>&1; 
    if [[ $? -ne 0 ]] ; then  
      # echo "ERROR: encoded file not match"
      # cat encoding.log
      # echo -e "["${KO}"]"
      echo -e "["${KO}"]"
      mkdir "./errors/${i}"
      echo ${command} > "./errors/${i}/command.log"
      mv -f *.report "./errors/${i}" 2&> /dev/null
      mv grib_compare.log "./errors/${i}"
      mv *.log ./errors/${i}/
      cp $i ./errors/${i}/
      mv *.grib ./errors/${i}/
      err=$((${err}+1))
    else
      echo -e "["${OK}"]"
    fi
  else
    # echo "ERROR: encoded file not generated"
    # cat encoding.log
    echo -e "["${KO}"]"
    mkdir "./errors/${i}"
    echo ${command} > "./errors/${i}/command.log"
    mv -f *.report "./errors/${i}" 2&> /dev/null
    mv grib_compare.log "./errors/${i}"
    mv *.log ./errors/${i}/
    cp $i ./errors/${i}/
    mv *.grib ./errors/${i}/
    err=$((${err}+1))
  fi
  rm -f *.grib
  rm -f *.log
  rm -f *.report
done

if [[ ${err} -gt 0 ]] ; then
  echo -e "\e[1;31m $err tests out of ${N} failed \e[0m"
else
  echo -e "\e[1;32m all tests passed\e[0m"
fi
# Exit on success
exit 0

