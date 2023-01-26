
list( APPEND _action_ifs_test_data
    "ifs_test_templates.tar.gz"
)

list( APPEND _action_nextGEMS_test_data
    "nextgems_test_templates.tar.gz"
)

ecbuild_get_test_multidata( EXTRACT TARGET multio_action_ifs_get_test_data DIRNAME multio/tests/actions NAMES ${_action_ifs_test_data} )
ecbuild_get_test_multidata( EXTRACT TARGET multio_action_nextGEMS_get_test_data DIRNAME multio/tests/actions NAMES ${_action_nextGEMS_test_data} )


if( HAVE_MIR )
add_subdirectory(interpolate)
endif( HAVE_MIR )
