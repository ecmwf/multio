include_directories( ${ECKIT_INCLUDE_DIRS} )

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_journal
                  SOURCES   test_multio_journal.cc
                  LIBS      multio )
