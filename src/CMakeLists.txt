message(STATUS "Generating App library")

file(GLOB PROJECT_INCLUDE_FILES *.h)
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.f90)

#----- Non ompi version
set(targets App App-shared)
set(shared_targets App-shared)
set(static_targets App)

add_library(App STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
target_include_directories(App PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
target_include_directories(App INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>)

add_library(App-shared SHARED $<TARGET_OBJECTS:App>)
target_include_directories(App-shared PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
target_include_directories(App-shared INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>)
 
add_dependencies(App ${PROJECT_NAME}_build_info)
set_target_properties(App App-shared PROPERTIES
   VERSION ${PROJECT_VERSION}
   PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
   INTERFACE_COMPILE_DEFINITIONS HAVE_APP="${PROJECT_VERSION}"
   Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
   OUTPUT_NAME App
   POSITION_INDEPENDENT_CODE  ON)

if(BUILD_SHARED_LIBS)
    add_library(App::App ALIAS App-shared)
else()
    add_library(App::App ALIAS App)
endif()

if(rmn_FOUND)
   target_link_libraries(App PRIVATE rmn)
   target_link_libraries(App-shared PRIVATE rmn)
endif()

if(WITH_OMPI AND OpenMP_FOUND AND MPI_FOUND) 
   message(STATUS "Generating App ompi library")

   #----- ompi version
   list(APPEND targets App-ompi App-ompi-shared)
   list(APPEND shared_targets App-ompi-shared)
   list(APPEND static_targets App-ompi)

   add_library(App-ompi STATIC ${PROJECT_INCLUDE_FILES} ${PROJECT_C_FILES} ${PROJECT_F_FILES})
   add_dependencies(App-ompi ${PROJECT_NAME}_build_info)
   target_include_directories(App-ompi PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
   target_include_directories(App-ompi INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/ompi>)
   target_compile_definitions(App-ompi PUBLIC HAVE_MPI HAVE_OPENMP)
   target_link_libraries(App-ompi PUBLIC MPI::MPI_C OpenMP::OpenMP_C)

   add_library(App-ompi-shared SHARED $<TARGET_OBJECTS:App-ompi>)
   target_include_directories(App-ompi-shared PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
   target_include_directories(App-ompi-shared INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include/ompi>)
   target_compile_definitions(App-ompi-shared PUBLIC HAVE_MPI HAVE_OPENMP)
   target_link_libraries(App-ompi-shared PUBLIC MPI::MPI_C OpenMP::OpenMP_C)

   set_target_properties(App-ompi App-ompi-shared PROPERTIES
      VERSION ${PROJECT_VERSION}
      PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
      INTERFACE_COMPILE_DEFINITIONS HAVE_APP="${PROJECT_VERSION}"
      Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/ompi
      OUTPUT_NAME App-ompi
      POSITION_INDEPENDENT_CODE  ON)

   if(rmn_FOUND)
      target_link_libraries(App-ompi PRIVATE rmn)
      target_link_libraries(App-ompi-shared PRIVATE rmn)
   endif()

   if(BUILD_SHARED_LIBS)
      add_library(App::App-ompi ALIAS App-shared)
   else()
      add_library(App::App-ompi ALIAS App)
   endif()
endif()

install(TARGETS ${shared_targets}
   EXPORT ${PROJECT_NAME}-shared-targets
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   INCLUDES DESTINATION include
   PUBLIC_HEADER DESTINATION include)

install(TARGETS ${static_targets}
   EXPORT ${PROJECT_NAME}-static-targets
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   INCLUDES DESTINATION include
   PUBLIC_HEADER DESTINATION include)  

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/app.mod TYPE INCLUDE)
