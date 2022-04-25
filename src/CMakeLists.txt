message(STATUS "Generating libApp librairie")

file(GLOB PROJECT_INCLUDE_FILES *.h)
file(GLOB PROJECT_C_FILES *.c)
#file(GLOB PROJECT_F_FILES f90/*.f90 )

add_library(App$ENV{OMPI} SHARED ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})

set_target_properties(App$ENV{OMPI} PROPERTIES 
   VERSION ${PROJECT_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   POSITION_INDEPENDENT_CODE  ON
   COMPILE_FLAGS "${MPI_COMPILE_FLAGS}"
   LINK_FLAGS "${MPI_LINK_FLAGS}")

add_dependencies(App$ENV{OMPI} ${PROJECT_NAME}_build_info)

#target_link_libraries(App$ENV{OMPI} ${LIBXML2_LIBRARIES})
#target_link_libraries(App$ENV{OMPI} rmn)
#target_link_libraries(App$ENV{OMPI} MPI::MPI_C OpenMP::OpenMP_C)

install(TARGETS App$ENV{OMPI} 
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   PUBLIC_HEADER DESTINATION include)
   