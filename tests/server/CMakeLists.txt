if(HAVE_MULTIO_SERVER)

include_directories( ${ECKIT_INCLUDE_DIRS} )
include_directories( ${ATLAS_INCLUDE_DIRS} )

ecbuild_add_test( TARGET   test_multio_message
                  SOURCES  test_multio_message.cc
                  MPI      2
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_transport
                  SOURCES  test_multio_transport.cc
                  MPI      2
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_atlas_field
                  SOURCES  test_multio_atlas_field.cc
                  MPI      8
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_dispatcher
                  SOURCES  test_multio_dispatcher.cc
                  MPI      3
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_io_server_prototype
                  SOURCES  test_multio_io_server_prototype.cc
                  MPI      8
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_actions
                  SOURCES  test_multio_actions.cc
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_plan
                  SOURCES  test_multio_plan.cc
                  LIBS     multio multio-server)

ecbuild_add_test( TARGET   test_multio_plan_assembler
                  SOURCES  test_multio_plan_assembler.cc
                  LIBS     multio multio-server)

endif(HAVE_MULTIO_SERVER)
