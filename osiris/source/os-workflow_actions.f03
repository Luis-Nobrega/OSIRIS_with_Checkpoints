module m_workflow_actions

    use m_workflow, only: set_workflow_step
    use m_workflow_reader, only: get_value
    use m_simulation 
    use m_parameters
    use m_restart
    use m_logprof
    use m_time_step       ! Para a função n()
    use m_node_conf       ! Para a função comm()
     
    implicit none
    private

    public :: check_and_execute

    integer, parameter :: MAX_KEYS = 3
    character(len=20), parameter :: keys(MAX_KEYS) = &
        ["checkpoint        ", &
         "restart           ", &
         "steering_step     "]  

contains

    subroutine check_and_execute(sim)
        class( t_simulation ), intent(inout) :: sim
        character(len=:), allocatable :: val
        integer :: i, new_step  ! Mudamos o nome da variável para evitar conflito

        do i = 1, MAX_KEYS
            val = get_value(trim(keys(i)))
            if (len_trim(val) == 0) then
                print *, "DEBUG - No command found for ", trim(keys(i))
                cycle
            end if

            select case (trim(keys(i)))
                case ("checkpoint")
                    call write_restart(sim)
                    print*, "DEBUG - Checkpoint command executed"

                case ("restart")
                    print*, "DEBUG - Restart command found"
                    ! Implementar lógica de reinício

                case ("steering_step") 
                    read(val, *) new_step  ! Usamos variável diferente
                    call set_workflow_step(new_step)
                    print*, "DEBUG - Steering step changed to ", new_step

                case default
                    print *, "Unknown command"
            end select
        end do
    end subroutine check_and_execute

    subroutine write_restart(sim)
        class( t_simulation ), intent(inout) :: sim
        type( t_restart_handle ) :: restart_handle

        call begin_event(restart_write_ev)

        if ( mpi_node() == 0 ) then
            print *, ''
            print *, ' Writing checkpoint information for timestep n =', n(sim%tstep)
            print *, ''
        endif

#ifdef __RST_SION__
        restart_handle%if_acc_size = .true.
        restart_handle%data_size = 0
        call sim%write_checkpoint( restart_handle )
        restart_handle%if_acc_size = .false.
#endif

        call restart_write_open( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                               n(sim%tstep), ndump(sim%tstep), &
                               file_id_rst, restart_handle )

        call sim%write_checkpoint( restart_handle )

        call restart_write_close( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                                n(sim%tstep), restart_handle )

        call end_event(restart_write_ev)
    end subroutine write_restart

end module m_workflow_actions