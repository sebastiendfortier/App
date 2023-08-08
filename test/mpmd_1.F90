
program mpmd_1
    use app_mpmd
    use app_test_mpmd_helper
    implicit none

    type(mpmd_context) :: context
    integer :: comm_12, comm_123, comm_13, comm_14, comm_124, comm_134

    call context % init(MPMD_TEST1_ID)

    call validate_comm_size(context % get_own_comm(), NUM_PROCS_TEST1, '(1)')

    if (context % has_component(MPMD_TEST2_ID)) then
        comm_12 = context % get_shared_comm([MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_12, NUM_PROCS_TEST1 + NUM_PROCS_TEST2, '(1, 2)')
    end if

    if (context % has_component(MPMD_TEST2_ID) .and. context % has_component(MPMD_TEST3_ID)) then
        comm_123 = context % get_shared_comm([MPMD_TEST3_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_123, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST3, '(1, 2, 3)')
    end if

    if (context % has_component(MPMD_TEST3_ID)) then
        comm_13 = context % get_shared_comm([MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_13, NUM_PROCS_TEST1 + NUM_PROCS_TEST3, '(1, 3)')
    end if

    if (context % has_component(MPMD_TEST4_ID)) then
        comm_14 = context % get_shared_comm([MPMD_TEST1_ID, MPMD_TEST4_ID])
        call validate_comm_size(comm_14, NUM_PROCS_TEST1 + NUM_PROCS_TEST4, '(1, 4)')
    end if

    if (context % has_component(MPMD_TEST2_ID) .and. context % has_component(MPMD_TEST4_ID)) then
        comm_124 = context % get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_124, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST4, '(1, 2, 4)')
    end if

    if (context % has_component(MPMD_TEST3_ID) .and. context % has_component(MPMD_TEST4_ID)) then
        comm_134 = context % get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_134, NUM_PROCS_TEST1 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(1, 3, 4)')
    end if

    call context % finalize()

end program mpmd_1
