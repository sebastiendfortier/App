if(BUILD_TESTING)
    set(CMAKE_Fortran_PREPROCESS ON)
    add_executable(test_app_loglevel_noinit test_app_loglevel_noinit.f90)
    add_test(
        NAME test_app_loglevel_noinit
        COMMAND $<TARGET_FILE:test_app_loglevel_noinit>
    )
    set_tests_properties(test_app_loglevel_noinit
        PROPERTIES PASS_REGULAR_EXPRESSION "INFO")
    target_link_libraries(test_app_loglevel_noinit App)
    add_dependencies(check test_app_loglevel_noinit)
endif()
