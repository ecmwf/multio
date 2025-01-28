# Get the current source directory
set(ROOT_FOLDER ${CMAKE_CURRENT_SOURCE_DIR})

# Find all subdirectories in the current source directory
file(GLOB SUBDIRS RELATIVE ${ROOT_FOLDER} ${ROOT_FOLDER}/*)

# Loop through each entry and add it as a subdirectory if it's a directory
foreach(SUBDIR ${SUBDIRS})
    if(IS_DIRECTORY ${ROOT_FOLDER}/${SUBDIR})
        set(KNOWLEDGE_VERSION ${SUBDIR})
        add_subdirectory(${SUBDIR})
    endif()
endforeach()
