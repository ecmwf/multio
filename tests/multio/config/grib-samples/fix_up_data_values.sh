#!/bin/sh

# Tries to set value to 273 for templates with paramId=130
#
# To run on all files
# > find . -type f -name "*.tmpl" -exec ./fix_up_data_values.sh {} \;

FILEIN=$1
FILEOUT="$1.new"

grib_set -w paramId=130 -d273 $FILEIN $FILEOUT

mv $FILEOUT $FILEIN
