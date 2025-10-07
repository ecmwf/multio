
set ( PREFIX test_multio_maestro )

ecbuild_add_test(
    TARGET    ${PREFIX}_api
    SOURCES   test_maestro_api.cc
    LIBS      multio-maestro
)
