#!/bin/bash
#
# create analogues of multio templates for a given target resolution (e.g. for O4000)
# from existing templates (e.g. for O1280)
#
# T. Rackow, ECMWF, 2023


TARGET_RES=O4000
BUNDLE_BIN=/scratch/project_465000454/sbeyer/ifs-bundle/build-fesom.lumi-g/bin/
TEMPLATES=/scratch/project_465000454/thorackow/RAPS_filter/multio_templates/
TEMPLATES_SAMPLE=${TEMPLATES}/templates.O1280/


##########################

# load the modules for the bundle/multio-generate-grib-template tool
source ${BUNDLE_BIN}/../../arch/eurohpc/lumi-g/default/env.sh

# create the new template folder
TEMPLATES_OUT=${TEMPLATES}/templates.${TARGET_RES}/
mkdir -p ${TEMPLATES_OUT}/

# copy the resolution-independent files
for file in ${TEMPLATES_SAMPLE}/regular_ll_*tmpl
do
	[ -f $file ] && cp $file ${TEMPLATES_OUT}/.
done
for file in ${TEMPLATES_SAMPLE}/sh*
do
	[ -f $file ] && cp $file ${TEMPLATES_OUT}/.
done

# create the resolution-dependent files
cd ${TEMPLATES_SAMPLE}/
for file in reduced_gg*tmpl reduced_ll*tmpl
do
	echo "Generating new file based on ${file} file..."
	${BUNDLE_BIN}/multio-generate-grib-template --grid=${TARGET_RES} --sample=${file} --output=${TEMPLATES_OUT}/${file}
done
cd -
