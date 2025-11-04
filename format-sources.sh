#!/usr/bin/env bash

# Usage: find . \( -iname "*.h" -or -iname "*.c" -or -iname "*.cc" \) -exec ./format-sources.sh {} \;

for f in $@
do
    base="$(basename "$f")"
    ext="${base##*.}"
    if [ "${ext}" == "h" ] || [ "${ext}" == "c" ] || [ "${ext}" == "cc" ]
    then
        # apply clang-format file .clang-format in top dir
        clang-format -i -style=file -fallback-style=none $f

        # divider lines with 120 chars
        perl -pi -e 's#^//-+$#//----------------------------------------------------------------------------------------------------------------------#g' $f
        perl -pi -e 's#^//=+$#//----------------------------------------------------------------------------------------------------------------------#g' $f
    fi

    # remove trailing spaces
    perl -pi -e 's# +$##g' $f

    # remove trailing newlines
    perl -0777 -pi -e 's/\s*\z/\n/' $f
done
