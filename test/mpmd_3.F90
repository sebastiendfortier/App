
program mpmd_3
    use app_mpmd
    use app_test_mpmd_helper
    implicit none

    integer :: comm_12, comm_123, comm_13, comm_234, comm_134, comm_34
    integer :: return_status

    call Mpmd_init(MPMD_TEST3_ID)

    call validate_comm_size(Mpmd_get_own_comm(), NUM_PROCS_TEST3, '(3)')

    if (.not. (Mpmd_has_component(MPMD_TEST1_ID) .and. Mpmd_has_component(MPMD_TEST2_ID))) then
        print *, 'ERROR MPMD_3 can only be launched if MPMD_1 and MPMD_2 are present!'
        error stop 1
    end if

    call App_Log(APP_WARNING, 'Expecting failure to retrieve shared comm')
    comm_12 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST1_ID])
    call validate_comm_size(comm_12, 0, '(3)')

    comm_123 = Mpmd_get_shared_comm([MPMD_TEST2_ID, MPMD_TEST3_ID, MPMD_TEST1_ID])
    call validate_comm_size(comm_123, NUM_PROCS_TEST1 + NUM_PROCS_TEST2 + NUM_PROCS_TEST3, '(3, 1, 2)')

    comm_13 = Mpmd_get_shared_comm([MPMD_TEST1_ID, MPMD_TEST3_ID])
    call validate_comm_size(comm_13, NUM_PROCS_TEST1 + NUM_PROCS_TEST3, '(3, 1)')

    if (Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_234 = Mpmd_get_shared_comm([MPMD_TEST3_ID, MPMD_TEST4_ID, MPMD_TEST2_ID])
        call validate_comm_size(comm_234, NUM_PROCS_TEST2 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 2, 4)')
    end if

    if (Mpmd_has_component(MPMD_TEST4_ID)) then
        comm_134 = Mpmd_get_shared_comm([MPMD_TEST4_ID, MPMD_TEST1_ID, MPMD_TEST3_ID])
        call validate_comm_size(comm_134, NUM_PROCS_TEST1 + NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 1, 4)')

        comm_34 = Mpmd_get_shared_comm([MPMD_TEST3_ID, MPMD_TEST4_ID])
        call validate_comm_size(comm_34, NUM_PROCS_TEST3 + NUM_PROCS_TEST4, '(3, 4)')
    end if

    call mpmd_end_test()

end program mpmd_3

