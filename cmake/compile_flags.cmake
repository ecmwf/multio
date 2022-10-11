
# Force linking if FDB is a dependency
if(HAVE_FDB5)

  set(ECBUILD_EXE_LINKER_FLAGS "-Wl,--no-as-needed")

endif()
