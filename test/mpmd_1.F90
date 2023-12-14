
program mpmd_1
    use app
    use app_mpmd
    use app_test_mpmd_helper
    implicit none

    integer :: comm_12, comm_123, comm_13, comm_14, comm_124, comm_134
    integer :: comm_15
    integer :: return_status

    call Mpmd_Init(MPMD_TEST1_ID)
    ! call mpmd_end_test()
    ! stop

    call validate_comm_size(Mpmd_get_own_comm(), NUM_PROCS_TEST1, '(1)')

    if (Mpmd_has_component(MPMD_TEST2_ID)) then
        comm_12 = Mpmd_get_shared_comm([MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_12, NUM_PROCS_TEST1 + NUM_PROCS_TEST2, '(1, 2)')
    end if

    if (Mpmd_has_component(MPMD_TEST2_ID) .and. Mpmd_has_component(MPMD_TEST3_ID)) then
        comm_123 = Mpmd_get_shared_comm([MPMD_TEST3_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_123, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST3, '(1, 2, 3)')
    end if

    if (Mpmd_has_component(MPMD_TEST3_ID)) then
        comm_13 = Mpmd_get_shared_comm([MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_13, NUM_PROCS_TEST1 + NUM_PROCS_TEST3, '(1, 3)')
    end if

    if (Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_14 = Mpmd_get_shared_comm([MPMD_TEST1_ID, MPMD_TEST4_ID])
        call validate_comm_size(comm_14, NUM_PROCS_TEST1 + NUM_PROCS_TEST4, '(1, 4)')
    end if

    if (Mpmd_has_component(MPMD_TEST2_ID) .and. Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_124 = Mpmd_get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_124, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST4, '(1, 2, 4)')
    end if

    if (Mpmd_has_component(MPMD_TEST3_ID) .and. Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_134 = Mpmd_get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_134, NUM_PROCS_TEST1 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(1, 3, 4)')
    end if

    if (Mpmd_has_component(MPMD_TEST5_ID)) then
        comm_15 = Mpmd_get_shared_comm([MPMD_TEST5_ID, MPMD_TEST1_ID])
        call validate_comm_size(comm_15, NUM_PROCS_TEST1 + NUM_PROCS_TEST5, '(1, 5)')
    end if

    call mpmd_end_test()

end program mpmd_1
