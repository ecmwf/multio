if( HAVE_TENSOGRAM )

    ecbuild_add_library(
        TARGET multio-action-encode-tensogram

        TYPE SHARED  # Due to reliance on factory self-registration this library cannot be static

        SOURCES
            EncodeTensogram.cc
            EncodeTensogram.h

        PUBLIC_LIBS
            multio
            tensogram::tensogram
    )

endif()
