include_directories( ${ECKIT_INCLUDE_DIRS} )
include_directories( ${ATLAS_INCLUDE_DIRS} )

ecbuild_add_test( TARGET    test_multio_datasink_factory
                  SOURCES   test_multio_datasink_factory.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_multio
                  SOURCES   test_multio_multio.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_file_sink
                  SOURCES   test_multio_file_sink.cc
                  LIBS      multio )

ecbuild_add_test( TARGET    test_multio_journal
                  SOURCES   test_multio_journal.cc
                  LIBS      multio )

add_subdirectory(sandbox)
add_subdirectory(server)
