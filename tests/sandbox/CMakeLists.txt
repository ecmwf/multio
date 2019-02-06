include_directories( ${ECKIT_INCLUDE_DIRS} )
include_directories( ${ATLAS_INCLUDE_DIRS} )

ecbuild_add_test( TARGET   test_multio_sandbox_simple_transport
                  SOURCES  test_multio_sandbox_simple_transport.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS     multio multio-sandbox)

ecbuild_add_test( TARGET   test_multio_sandbox_listener
                  SOURCES  test_multio_sandbox_listener.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS     multio multio-sandbox)

ecbuild_add_test( TARGET   test_multio_sandbox_thread_transport
                  SOURCES  test_multio_sandbox_thread_transport.cc
                  CONDITION HAVE_MULTIO_SERVER
                  LIBS     multio multio-sandbox)
