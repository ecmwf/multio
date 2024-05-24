#!/bin/sh

# Tries to set a lot of GRIB2 keys to default values
#
# To run on all files
# > find . -type f -name "*.tmpl" -exec ./cleanGrib.sh {} \;

GETCMD=/Users/mapg/git/ifs-bundle/build/bin/grib_get
SETCMD=/Users/mapg/git/ifs-bundle/build/bin/grib_set

FILEIN=$1
FILEOUT="$1.new"

if [[ !  $($GETCMD -p edition $FILEIN) == "2" ]]; then
echo "Ignoring $FILEIN (GRIB1)"
else
echo "Cleaning $FILEIN"
if ! $($GETCMD -p typeOfStatisticalProcessing $FILEIN > /dev/null 2>&1);
then

$SETCMD -s \
discipline=0,\
centre=98,\
subCentre=0,\
year=0,\
month=0,\
day=0,\
hour=0,\
minute=0,\
second=0,\
productionStatusOfProcessedData=255,\
typeOfProcessedData=255,\
parameterCategory=0,\
parameterNumber=0,\
typeOfGeneratingProcess=255,\
backgroundProcess=255,\
generatingProcessIdentifier=255,\
hoursAfterDataCutoff=65535,\
minutesAfterDataCutoff=255,\
stepUnits=1,\
startStep=0,\
typeOfFirstFixedSurface=255,\
scaleFactorOfFirstFixedSurface=255,\
scaledValueOfFirstFixedSurface=4294967295,\
typeOfSecondFixedSurface=255,\
scaleFactorOfSecondFixedSurface=255,\
scaledValueOfSecondFixedSurface=4294967295,\
bitMapIndicator=255,\
bitmapPresent=0\
 $FILEIN $FILEOUT

 mv $FILEOUT $FILEIN
else

$SETCMD -s \
discipline=0,\
centre=98,\
subCentre=0,\
year=0,\
month=0,\
day=0,\
hour=0,\
minute=0,\
second=0,\
productionStatusOfProcessedData=255,\
typeOfProcessedData=255,\
parameterCategory=0,\
parameterNumber=0,\
typeOfGeneratingProcess=255,\
backgroundProcess=255,\
generatingProcessIdentifier=255,\
hoursAfterDataCutoff=65535,\
minutesAfterDataCutoff=255,\
stepUnits=1,\
startStep=0,\
typeOfFirstFixedSurface=255,\
scaleFactorOfFirstFixedSurface=255,\
scaledValueOfFirstFixedSurface=4294967295,\
typeOfSecondFixedSurface=255,\
scaleFactorOfSecondFixedSurface=255,\
scaledValueOfSecondFixedSurface=4294967295,\
yearOfEndOfOverallTimeInterval=0,\
monthOfEndOfOverallTimeInterval=0,\
dayOfEndOfOverallTimeInterval=0,\
hourOfEndOfOverallTimeInterval=0,\
minuteOfEndOfOverallTimeInterval=0,\
secondOfEndOfOverallTimeInterval=0,\
typeOfStatisticalProcessing=255,\
typeOfTimeIncrement=255,\
indicatorOfUnitForTimeRange=255,\
lengthOfTimeRange=0,\
indicatorOfUnitForTimeIncrement=255,\
timeIncrement=0,\
bitMapIndicator=255,\
bitmapPresent=0\
 $FILEIN $FILEOUT

 mv $FILEOUT $FILEIN
fi
fi
