# (C) Copyright 2005- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
# virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
#


#
# \brief Function used to run an interpolation test
#
# \param [in] BINDIR      Directory that contains the binary of the project
# \param [in] MARS_FOLDER Directory where the test data have been downloaded
# \param [in] SOURCE_NAME string used to generate the source test file name
# \param [in] RESULT_NAME string used to generate the result file name
# \param [in] TEST_FOLDER Folder where the test plan is stored 
# \param [in] TEST_NAME   Name of the test plan file
# \param [in] ARGS        Flag used to select the kind of check to be performed on the values
#                         If the value is "permissive" then a weak check is performed,
#                         otherwise a strict test is performed 
#
Interpolate(){

#
# Input parametrisation
local BINDIR=${1};
local MARS_FOLDER=${2};
local SOURCE_NAME=${3};
local RESULT_NAME=${4};
local TEST_FOLDER=${5};
local TEST_NAME=${6};
local ARGS=${7};

#
# Executables used to perform the test
TEST_EXE=${BINDIR}/bin/multio-feed
DIFF_TOOL=${BINDIR}/bin/grib_compare

#
# Data/Configuration files used to run the test
TEST_EXPECTED_RESULT=${BINDIR}/multio/tests/interpolate/data/${MARS_FOLDER}/mars-${RESULT_NAME}.grib
TEST_COMPUTED_RESULT=${BINDIR}/multio/tests/interpolate/${TEST_FOLDER}/multio-${RESULT_NAME}.grib
MULTIO_PLAN=${BINDIR}/multio/tests/interpolate/${TEST_FOLDER}/${TEST_NAME}.yaml
MULTIO_FEED=${BINDIR}/multio/tests/interpolate/data/${MARS_FOLDER}/mars-${SOURCE_NAME}.grib

#
# Test report and logging files
RUN_TEST=${BINDIR}/multio/tests/interpolate/${TEST_FOLDER}/${TEST_NAME}.sh
TEST_REPORT=${BINDIR}/multio/tests/interpolate/${TEST_FOLDER}/${TEST_NAME}.report
TEST_OUTLOG=${BINDIR}/multio/tests/interpolate/${TEST_FOLDER}/${TEST_NAME}.log

#
# Definition of the line padding (Logging purposes)
LINE_PADDING="..................................................";

#
# Initialize report file
echo " + Test Report"   > ${TEST_REPORT}
echo "   -----------"   >> ${TEST_REPORT}
echo "   - ${RUN_TEST}" >> ${TEST_REPORT}
echo " " >> ${TEST_REPORT}
echo " + Test configuration" >> ${TEST_REPORT}
echo "   ------------------" >> ${TEST_REPORT}
echo "   - BINDIR           :: " ${BINDIR}      >> ${TEST_REPORT}
echo "   - MARS_FOLDER      :: " ${MARS_FOLDER} >> ${TEST_REPORT}
echo "   - SOURCE_NAME      :: " ${SOURCE_NAME} >> ${TEST_REPORT}
echo "   - RESULT_NAME      :: " ${RESULT_NAME} >> ${TEST_REPORT}
echo "   - TEST_FOLDER      :: " ${TEST_FOLDER} >> ${TEST_REPORT}
echo "   - TEST_NAME        :: " ${TEST_NAME}   >> ${TEST_REPORT}
echo " " >> ${TEST_REPORT}
echo " + Local variables" >> ${TEST_REPORT}
echo "   ------------------" >> ${TEST_REPORT}
echo "   - TEST_EXE             :: "  ${TEST_EXE}             >> ${TEST_REPORT}
echo "   - DIFF_TOOL            :: "  ${DIFF_TOOL}            >> ${TEST_REPORT}
echo "   - TEST_EXPECTED_RESULT :: "  ${TEST_EXPECTED_RESULT} >> ${TEST_REPORT}
echo "   - TEST_COMPUTED_RESULT :: "  ${TEST_COMPUTED_RESULT} >> ${TEST_REPORT}
echo "   - MULTIO_PLAN          :: "  ${MULTIO_PLAN}          >> ${TEST_REPORT}
echo "   - MULTIO_FEED          :: "  ${MULTIO_FEED}          >> ${TEST_REPORT}
echo "   - TEST_REPORT          :: "  ${TEST_REPORT}          >> ${TEST_REPORT}
echo "   - TEST_OUTLOG          :: "  ${TEST_OUTLOG}          >> ${TEST_REPORT}
echo " " >> ${TEST_REPORT}
echo " + Test status" >> ${TEST_REPORT}
echo "   -----------" >> ${TEST_REPORT}

#
# Initialize logging file
echo "Test Log:    ${0}" > ${TEST_OUTLOG}

#
# Beginning of the test
MSG="Report start"
echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
MSG="Log start"
echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_OUTLOG}

# Check for existence of test tool "multio-feed"
if [[ -x ${TEST_EXE} ]] ; then
  MSG="Test executable exists";
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Test executable NOT exists";
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 1
fi

# Check for existence of test tool "grib_compare"
if [[ -x ${DIFF_TOOL} ]] ; then
  MSG="Diff tool exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Diff tool NOT exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 2
fi

# Check for existence of test plan
if [[ -e ${MULTIO_PLAN} ]] ; then
  MSG="Plan file exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Plan file NOT exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 3
fi

# Check for existence of input grib file
if [[ -e ${MULTIO_FEED} ]] ; then
  MSG="Input grib file exist"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Input grib file NOT exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 4
fi

# Check for existence of expected result
if [[ -e ${TEST_EXPECTED_RESULT} ]] ; then
  MSG="Expected result file exist"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Expected result file NOT exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 5
fi

# Remove old test results
if [[ -e ${TEST_COMPUTED_RESULT} ]] ; then
  rm ${TEST_COMPUTED_RESULT}
fi

#
# Run the test
${TEST_EXE} --decode --plans=${MULTIO_PLAN} ${MULTIO_FEED} 2>> ${TEST_OUTLOG}

#
# Check exit status for the test
if [[ $? == 0 ]] ; then
  MSG="Test executed with success"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Test execution failed"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 6
fi

#
# Check existence of the test output grib file with interpolated data 
if [[ -e ${TEST_COMPUTED_RESULT} ]] ; then
  MSG="Test ouptut file exists"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Test output file not found"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 7
fi

#
# Run the metadata comparison between field interpolate using mars and field interpolated using multio
${DIFF_TOOL} -H ${TEST_EXPECTED_RESULT} ${TEST_COMPUTED_RESULT} 2>> ${TEST_OUTLOG}

#
# Check exit status for the test
if [[ $? == 0 ]] ; then
  MSG="Metadata comparison executed with success"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Matadata comparison failed"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 8
fi

#
# Run the values comparison between field interpolate using mars and field interpolated using multio
# This is a value comaparison because the metadata have been already comared and are the same!!!
if [[ ${ARGS} == "permissive" ]] ; then
  #
  # It would be nice to be able to make the comparison only on a cropped subset of the field 
  # (i.e. avoid the poles due to the problems with different versions of mir). Not sure
  # that mir-compare supports this kind of comparisons. 
  #
  # TODO: improve mir-compare to allow cropped comparisons.
  # TODO: this comaprison is fake at the moment since the parameters (-P -T 1024) are too
  # width just to pass the test with the errors at the poles
  ${DIFF_TOOL} -P -T 1024 ${TEST_EXPECTED_RESULT} ${TEST_COMPUTED_RESULT} 2>> ${TEST_OUTLOG}
else
  ${DIFF_TOOL} ${TEST_EXPECTED_RESULT} ${TEST_COMPUTED_RESULT} 2>> ${TEST_OUTLOG}
fi

#
# Check exit status for the test
if [[ $? == 0 ]] ; then
  MSG="Values comparison executed with success"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
else
  MSG="Values comparison failed"
  echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[KO]" >> ${TEST_REPORT}
  cat ${TEST_OUTLOG}
  cat ${TEST_REPORT}
  echo "Test can be rerun manually using: " ${RUN_TEST}
  exit 9
fi

#
# Cleanup test tmp files
rm -f ${TEST_OUTLOG}
rm -f ${TEST_COMPUTED_RESULT}

#
# Exit on success
MSG="Test ended with success"
echo "   - "$(date)" :: "${MSG}${LINE_PADDING:${#MSG}}"[OK]" >> ${TEST_REPORT}
exit 0

}