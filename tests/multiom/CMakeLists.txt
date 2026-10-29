add_subdirectory(scripts)

ecbuild_add_test(
    TARGET multiom_test_prefix_to_levtype
    SOURCES test_prefix_to_levtype.F90
    LIBS multiom-core
    LINKER_LANGUAGE Fortran
)
