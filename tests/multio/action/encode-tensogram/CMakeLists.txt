if( HAVE_TENSOGRAM )

    ecbuild_add_test( TARGET    test_multio_encode_tensogram
                      SOURCES   test_multio_encode_tensogram.cc
                      NO_AS_NEEDED
                      LIBS      multio
                                multio-action-encode-tensogram
                                multio-action-debug-sink
                                tensogram::tensogram )

endif()
