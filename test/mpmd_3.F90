
program mpmd_3
    use app_mpmd
    use app_test_mpmd_helper
    implicit none

    type(mpmd_context) :: context
    integer :: comm_12, comm_123, comm_13, comm_234, comm_134, comm_34

    call context % init(MPMD_TEST3_ID)

    call validate_comm_size(context % get_own_comm(), NUM_PROCS_TEST3, '(3)')

    if (.not. (context % has_component(MPMD_TEST1_ID) .and. context % has_component(MPMD_TEST2_ID))) then
        print *, 'ERROR MPMD_3 can only be launched if MPMD_1 and MPMD_2 are present!'
        error stop 1
    end if

    comm_12 = context % get_shared_comm([MPMD_TEST2_ID, MPMD_TEST1_ID])
    call validate_comm_size(comm_12, 0, '(3)')

    comm_123 = context % get_shared_comm([MPMD_TEST2_ID, MPMD_TEST3_ID, MPMD_TEST1_ID])
    call validate_comm_size(comm_123, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST3, '(3, 1, 2)')

    comm_13 = context % get_shared_comm([MPMD_TEST1_ID, MPMD_TEST3_ID])
    call validate_comm_size(comm_13, NUM_PROCS_TEST1 + NUM_PROCS_TEST3, '(3, 1)')

    if (context % has_component(MPMD_TEST4_ID)) then
        comm_234 = context % get_shared_comm([MPMD_TEST3_ID, MPMD_TEST4_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_234, NUM_PROCS_TEST2 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 2, 4)')
    end if

    if (context % has_component(MPMD_TEST4_ID)) then
        comm_134 = context % get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_134, NUM_PROCS_TEST1 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 1, 4)')

        comm_34 = context % get_shared_comm([MPMD_TEST3_ID, MPMD_TEST4_ID])
        call validate_comm_size(comm_34, NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 4)')
    end if

    call context % finalize()

end program mpmd_3

