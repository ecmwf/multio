add_subdirectory(scripts)

ecbuild_add_test(
    TARGET multiom_test_clte_datetime
    SOURCES clte_datetime.F90
    LIBS multiom
)
