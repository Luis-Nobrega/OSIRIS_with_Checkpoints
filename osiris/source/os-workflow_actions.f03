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

    subroutine check_and_execute(sim, can_checkpoint)
        class( t_simulation ), intent(inout) :: sim
        character(len=:), allocatable :: val
        integer :: i, new_step  ! Mudamos o nome da variável para evitar conflito
        logical :: is_checkpoint_step
        logical , intent(inout) :: can_checkpoint

        ! Check if current step is a checkpoint step
        is_checkpoint_step = if_restart_write( sim%restart, n(sim%tstep), ndump(sim%tstep), &
                                        comm(sim%no_co), sim%no_co%ngp_id() )

        do i = 1, MAX_KEYS
            val = get_value(trim(keys(i)))
            if (len_trim(val) == 0) then
                if ( mpi_node() == 0 ) then
                    print *, "DEBUG - No value found for ", trim(keys(i))
                end if
                ! Skip iteration if no value is found
                cycle
            end if

            select case (trim(keys(i)))
                case ("checkpoint")
                    if  (.not. is_checkpoint_step) then
                        can_checkpoint = .true.               
                    else
                        can_checkpoint = .false.
                        if ( mpi_node() == 0 ) then
                            print*, "DEBUG - Checkpoint command skipped due to existing checkpoint"
                        end if
                    end if

                case ("restart")
                    if ( mpi_node() == 0 ) then
                        print*, "DEBUG - Restart command found"
                    end if
                    ! Implementar lógica de reinício

                case ("steering_step") 
                    read(val, *) new_step  ! Usamos variável diferente
                    call set_workflow_step(new_step)
                    if ( mpi_node() == 0 ) then
                        print*, "DEBUG - Steering step changed to ", new_step
                    end if

                case default
                    if ( mpi_node() == 0 ) then
                        print *, "Unknown command"
                    end if
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