message(STATUS "Generating App library")

file(GLOB PROJECT_INCLUDE_FILES *.h)
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.f90)

#----- Non ompi version
set(targets App)

add_library(App STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_dependencies(App ${PROJECT_NAME}_build_info)
target_include_directories(App PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
set_target_properties(App PROPERTIES
   VERSION ${PROJECT_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   INTERFACE_COMPILE_DEFINITIONS HAVE_APP=${PROJECT_VERSION}
   Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
   POSITION_INDEPENDENT_CODE  ON)
target_include_directories(App INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>)
add_library(App::App ALIAS App)

if(rmn_FOUND)
   target_link_libraries(App PRIVATE rmn)
endif()

if(WITH_OMPI AND OpenMP_FOUND AND MPI_FOUND) 
   message(STATUS "Generating App OMPI library")

   #----- ompi version
   set(targets App App-ompi)

   add_library(App-ompi STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
   add_dependencies(App-ompi ${PROJECT_NAME}_build_info)
   target_include_directories(App-ompi PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
   set_target_properties(App-ompi PROPERTIES
      VERSION ${PROJECT_VERSION}
      PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
      INTERFACE_COMPILE_DEFINITIONS HAVE_APP=${PROJECT_VERSION}
      Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/ompi
      POSITION_INDEPENDENT_CODE  ON)
   target_include_directories(App-ompi INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/ompi>)

   if(rmn_FOUND)
      target_link_libraries(App-ompi PRIVATE rmn)
   endif()

   #----- Specific target dependencies
   target_compile_definitions(App-ompi PUBLIC HAVE_MPI HAVE_OPENMP)
   target_link_libraries(App-ompi PUBLIC MPI::MPI_C OpenMP::OpenMP_C)
   add_library(App::App-ompi ALIAS App-ompi)
endif()

install(TARGETS ${targets}
   EXPORT ${PROJECT_NAME}-targets
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   PUBLIC_HEADER DESTINATION include
   INCLUDES DESTINATION include) # Causes auto-generated targets to have ${_IMPORT_PREFIX}/include in INTERFACE_INCLUDE_DIRECTORIES
   
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/app.mod TYPE INCLUDE)
