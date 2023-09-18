
program mpmd_2
    use app_mpmd
    use app_test_mpmd_helper
    implicit none

    integer :: comm_12, comm_123, comm_234, comm_124, comm_24
    integer :: return_status

    call Mpmd_init(MPMD_TEST2_ID)

    call validate_comm_size(Mpmd_get_own_comm(), NUM_PROCS_TEST2, '(2)')

    if (.not. (Mpmd_has_component(MPMD_TEST1_ID))) then
        print *, 'ERROR MPMD_2 can only be launched if MPMD_1 is present!'
        error stop 1
    end if

    call App_Log(APP_INFO, 'Getting shared 12 (2)')
    comm_12 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST1_ID])
    call App_Log(APP_INFO, 'Got shared 12 (2)')
    call validate_comm_size(comm_12, NUM_PROCS_TEST1 + NUM_PROCS_TEST2, '(2, 1)')

    ! Get it again, with inverted IDs. This should *not* be a collective call (mpmd_1 does not do it)
    comm_12 = Mpmd_get_shared_comm([MPMD_TEST1_ID, MPMD_TEST2_ID])
    call validate_comm_size(comm_12, NUM_PROCS_TEST1 + NUM_PROCS_TEST2, '(2, 1)')

    if (Mpmd_has_component(MPMD_TEST3_ID)) then
        comm_123 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST3_ID, MPMD_TEST1_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_123, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST3, '(2, 1, 3)')
    end if

    if (Mpmd_has_component(MPMD_TEST3_ID) .and. Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_234 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST4_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_234, NUM_PROCS_TEST2 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(2, 3, 4)')
    end if

    if (Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_124 = Mpmd_get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_124, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST4, '(2, 1, 4)')

        comm_24 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST4_ID])
        call validate_comm_size(comm_24, NUM_PROCS_TEST2 + NUM_PROCS_TEST4, '(2, 4)')
    end if

    call mpmd_end_test()

end program mpmd_2
