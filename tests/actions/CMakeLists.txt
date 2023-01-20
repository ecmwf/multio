
list( APPEND _actions_ifs_test_data
    "ifs_test_templates.tar.gz"
)

list( APPEND _actions_nextGEMS_test_data
    "nextgems_test_templates.tar.gz"
)

ecbuild_get_test_multidata( EXTRACT TARGET multio_actions_ifs_get_test_data DIRNAME multio/tests/actions NAMES ${_actions_ifs_test_data} )
ecbuild_get_test_multidata( EXTRACT TARGET multio_actions_nextGEMS_get_test_data DIRNAME multio/tests/actions NAMES ${_actions_nextGEMS_test_data} )


if( HAVE_MIR )
add_subdirectory(interpolate)
endif( HAVE_MIR )
