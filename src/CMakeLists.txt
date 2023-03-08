message(STATUS "Generating App library")

file(GLOB PROJECT_INCLUDE_FILES *.h)
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.f90)

#----- Non ompi version
add_library(App STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_dependencies(App ${PROJECT_NAME}_build_info)
target_include_directories(App PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(App PROPERTIES
   VERSION ${PROJECT_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
   POSITION_INDEPENDENT_CODE  ON)
target_include_directories(App INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include)

if(rmn_FOUND)
   target_link_libraries(App PRIVATE rmn)
endif()

#----- ompi version
add_library(App-ompi STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_dependencies(App-ompi ${PROJECT_NAME}_build_info)
target_include_directories(App-ompi PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(App-ompi PROPERTIES
   VERSION ${PROJECT_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/ompi
   COMPILE_FLAGS "${MPI_COMPILE_FLAGS}"
   LINK_FLAGS "${MPI_LINK_FLAGS}"
   POSITION_INDEPENDENT_CODE  ON)
target_include_directories(App-ompi INTERFACE ${CMAKE_CURRENT_BINARY_DIR}/include/ompi)

if(rmn_FOUND)
   target_link_libraries(App-ompi PRIVATE rmn)
endif()

#----- Specific target dependencies
target_compile_definitions(App-ompi PUBLIC HAVE_MPI HAVE_OPENMP)
target_link_libraries(App-ompi PUBLIC MPI::MPI_C OpenMP::OpenMP_C)

install(TARGETS App App-ompi 
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   PUBLIC_HEADER DESTINATION include)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/app.mod TYPE INCLUDE)
