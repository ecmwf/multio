#!/bin/bash


for curr_fname in `find . -type f -name '*.F90'` ; do
  N1=`cat "${curr_fname}" | grep BLOCK | sed -e 's/\s*//g' | grep '^BLOCK$' | wc -l`;
  N2=`cat "${curr_fname}" | grep 'CHARACTER(LEN=:), ALLOCATABLE :: STR' | wc -l`;
  bkp_fname="${curr_fname}.bkp";
  mv "${curr_fname}" "${bkp_fname}";
  if [[ ${N1} -eq ${N2} ]] ; then
    sed -e '{s/CHARACTER(LEN=:), ALLOCATABLE :: STR/PP_DEBUG_PUSH_FRAME()/};{s/PP_DEBUG_CREATE_ERROR_MSG( STR,/PP_DEBUG_PUSH_MSG_TO_FRAME(/};{s/PP_DEBUG_ABORT(\s*STR\s*)/PP_DEBUG_ABORT()/}' "${bkp_fname}" > "${curr_fname}";
  else
    echo "${curr_fname} : NOT OK - ${N1} ${N2}";
  fi
done