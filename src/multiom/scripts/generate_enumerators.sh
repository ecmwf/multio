#!/bin/bash

OUTDIR="/ec/res4/hpcperm/mavm/multio-bundle/enumerators"
rm -f "${OUTDIR}/*.txt"
rm -f "${OUTDIR}/**/*.txt"
rm -rf "${OUTDIR}/class"
rm -rf "${OUTDIR}/type"
rm -rf "${OUTDIR}/stream"

# Generate enumerators for the given enum class
mkdir "${OUTDIR}/class"
echo "select grib_code, mars_abbreviation, long_name from grib_class ;" | mysql -h webapps-db-prod -u ecmwf_ro -pecmwf_ro param | tr "\t" " " | sed '1s/.*/0 0 Unknown/' > "${OUTDIR}/class/class_from_db.txt"
cat "${OUTDIR}/class/class_from_db.txt" | awk '{ print "  INTEGER(KIND=JPIB_K), PARAMETER :: CLASS_"toupper($2)"_E="$1"_JPIB_K" }; END{print "  INTEGER(KIND=JPIB_K), PARAMETER :: N_CLASS="NR"_JPIB_K"}' > "${OUTDIR}/class/class_enum_declaration.txt"
cat "${OUTDIR}/class/class_from_db.txt" | awk '{ print "  PUBLIC :: CLASS_"toupper($2)"_E" }; END{print "  PUBLIC :: N_CLASS"}' > "${OUTDIR}/class/class_enum_visibility.txt"
cat "${OUTDIR}/class/class_from_db.txt" | awk '{ print "  CASE (CLASS_"toupper($2)"_E)"; print "    CCLASS = \047"tolower($2)"\047" }' > "${OUTDIR}/class/class_enum_to_string.txt"
cat "${OUTDIR}/class/class_from_db.txt" | awk '{ print "  CASE (\047"tolower($2)"\047)"; print "    ICLASS = CLASS_"toupper($2)"_E" }' > "${OUTDIR}/class/class_string_to_enum.txt"

# Generate enumerators for the given enum type
mkdir "${OUTDIR}/type"
echo "select grib_code, mars_abbreviation, long_name from grib_type ;" | mysql -h webapps-db-prod -u ecmwf_ro -pecmwf_ro param | tr "\t" " " | sed '1s/.*/0 0 Unknown/' > "${OUTDIR}/type/type_from_db.txt"
cat "${OUTDIR}/type/type_from_db.txt" | awk '{ print "  INTEGER(KIND=JPIB_K), PARAMETER :: TYPE_"toupper($2)"_E="$1"_JPIB_K" }; END{print "  INTEGER(KIND=JPIB_K), PARAMETER :: N_TYPE="NR"_JPIB_K"}' > "${OUTDIR}/type/type_enum_declaration.txt"
cat "${OUTDIR}/type/type_from_db.txt" | awk '{ print "  PUBLIC :: TYPE_"toupper($2)"_E" }; END{print "  PUBLIC :: N_TYPE"}' > "${OUTDIR}/type/type_enum_visibility.txt"
cat "${OUTDIR}/type/type_from_db.txt" | awk '{ print "  CASE (TYPE_"toupper($2)"_E)"; print "    CTYPE = \047"tolower($2)"\047" }' > "${OUTDIR}/type/type_enum_to_string.txt"
cat "${OUTDIR}/type/type_from_db.txt" | awk '{ print "  CASE (\047"tolower($2)"\047)"; print "    ITYPE = TYPE_"toupper($2)"_E" }' > "${OUTDIR}/type/type_string_to_enum.txt"

# Generate enumerators for the given enum stream
mkdir "${OUTDIR}/stream"
echo "select grib_code, mars_abbreviation, long_name from grib_stream ;" | mysql -h webapps-db-prod -u ecmwf_ro -pecmwf_ro param | tr "\t" " " | sed '1s/.*/0 0 Unknown/' > "${OUTDIR}/stream/stream_from_db.txt"
cat "${OUTDIR}/stream/stream_from_db.txt" | awk '{ print "  INTEGER(KIND=JPIB_K), PARAMETER :: STREAM_"toupper($2)"_E="$1"_JPIB_K" }; END{print "  INTEGER(KIND=JPIB_K), PARAMETER :: N_STREAM="NR"_JPIB_K"}' > "${OUTDIR}/stream/stream_enum_declaration.txt"
cat "${OUTDIR}/stream/stream_from_db.txt" | awk '{ print "  PUBLIC :: STREAM_"toupper($2)"_E" }; END{print "  PUBLIC :: N_STREAM"}' > "${OUTDIR}/stream/stream_enum_visibility.txt"
cat "${OUTDIR}/stream/stream_from_db.txt" | awk '{ print "  CASE (STREAM_"toupper($2)"_E)"; print "    CSTREAM = \047"tolower($2)"\047" }' > "${OUTDIR}/stream/stream_enum_to_string.txt"
cat "${OUTDIR}/stream/stream_from_db.txt" | awk '{ print "  CASE (\047"tolower($2)"\047)"; print "    ISTREAM = STREAM_"toupper($2)"_E" }' > "${OUTDIR}/stream/stream_string_to_enum.txt"

