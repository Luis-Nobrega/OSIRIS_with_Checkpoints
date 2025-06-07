module m_workflow !*!

    use m_workflow_reader
    use m_node_conf
    use mpi
    use m_simulation, only: t_simulation  ! Adiciona definição da simulação

    use m_simulation !^!
    use m_parameters !^!
    use m_restart !^!
    use m_logprof !^!
    use m_time_step       !^! Para a função n()
    use m_node_conf       !^! Para a função comm()

    implicit none

    private
    public :: check_workflow_step, set_workflow_step

    integer :: iteration_counter = 0
    integer :: workflow_step
    integer :: ierr, n_entries

    character(len=*), parameter :: filename = steering_filename
    character(len=256) :: used_filename = "steering_input_deck.used" ! Novo nome para o ficheiro renomeado

contains

    subroutine set_workflow_step(step, sim)
        implicit none
        integer, intent(in) :: step
        class(t_simulation), intent(inout) :: sim

        if (step > 0) then
            workflow_step = step
            if (root(sim%no_co)) then
                print*, "Workflow step set to: ", workflow_step
            end if
        else
            if (root(sim%no_co)) then
                print*, "Steering step not defined or invalid: ", step
                print*, "Use 'steering_step = value' Must be greater than 0."
                print*, "Won't be checking steering checkpoints."
            end if
        end if
    end subroutine set_workflow_step

    subroutine check_file_exists(file_ok)
        implicit none
        logical, intent(out) :: file_ok
        logical :: exists
        integer :: iunit, ierr

        inquire(file=filename, exist=exists)

        if (exists) then
            open(newunit=iunit, file=filename, status='old', iostat=ierr)
            if (ierr == 0) then
                close(iunit)
                file_ok = .true.
                print*, "File exists and is readable"
            else
                file_ok = .false.
                print*, "File could not be opened. I/O error code: ", ierr
            end if
        else
            file_ok = .false.
        end if
    end subroutine check_file_exists

    subroutine rename_file(old_name, new_name, success)
        implicit none
        character(len=*), intent(in) :: old_name
        character(len=*), intent(in) :: new_name
        logical, intent(out) :: success
        integer :: status
        character(len=512) :: command

        command = 'mv "' // trim(old_name) // '" "' // trim(new_name) // '"'
        call execute_command_line(trim(command), exitstat=status)
        success = (status == 0)

        if (.not. success) then
            print *, "Error: Failed to rename file using shell command"
            print *, "Command: ", trim(command)
            print *, "Exit status: ", status
        end if
    end subroutine rename_file

    subroutine check_workflow_step(file_ok, sim, no_co, steering_exit)
        implicit none
        logical, intent(out) :: file_ok, steering_exit
        type(t_simulation), intent(inout) :: sim
        type(t_node_conf), intent(in) :: no_co
        logical :: file_exists_now, success

        if (mod(iteration_counter, workflow_step) == 0 .and. iteration_counter /= 0) then
            if (root(no_co)) call check_file_exists(file_exists_now)
            call MPI_BCAST(file_exists_now, 1, MPI_LOGICAL, 0, no_co%comm, ierr)

            if (file_exists_now) then
                call read_steering_file(no_co, ierr)
                !val = get_value("simulation_name") ! DEBUG
                !print *, "DEBUG - simulation_name value: ", trim(val) ! DEBUG
                if (ierr == 0) then
                    !print *, "DEBUG - Leu com sucesso"
                    if (root(no_co)) then
                        call rename_file(filename, used_filename, success)
                        !if (success) print *, "DEBUG - Rename com sucesso"
                    end if
                    file_ok = .true.
                    call check_and_execute(sim, steering_exit)
                else
                    file_ok = .false.
                end if
            else
                file_ok = .false.
            end if
        else
            file_ok = .false.
        end if

        iteration_counter = iteration_counter + 1

        !!! VER se vale a pena meter aqui uma flag 
        ! que manda fazer barrier sse o workflow_actions 
        ! Fizer alguma alteração à simulação 
    end subroutine check_workflow_step

    ! <-------------------------------->!
    ! <--- Change of parameters --->!
    ! <-------------------------------->!

    subroutine check_and_execute(sim, steering_exit)
        class(t_simulation), intent(inout) :: sim
        character(len=:), allocatable :: val
        integer :: i, new_step
        logical :: is_checkpoint_step
        logical, intent(out) :: steering_exit
        character(len=:), allocatable :: keys(:)
        logical :: is_diagnostic

        ! Diagnostic variables
        integer :: diagnostic_ierr
        character(len=:), allocatable :: diagnostic_name
        character(len=:), allocatable :: identifier
        character(len=:), allocatable :: diag_command
        character(len=:), allocatable :: diag_data(:)

        ! Check checkpoint status
        is_checkpoint_step = if_restart_write(sim%restart, n(sim%tstep), ndump(sim%tstep), &
                            comm(sim%no_co), sim%no_co%ngp_id())

        keys = get_keys()

        do i = 1, size(keys)
            val = get_value(trim(keys(i)))
            if (len_trim(val) == 0) then
                if (mpi_node() == 0) print *, "DEBUG - No value for ", trim(keys(i))
                cycle
            end if

            is_diagnostic = .false.

            select case (trim(keys(i)))
                case ("checkpoint")
                    if (.not. is_checkpoint_step .and. val == "1") then
                        call write_restart(sim)
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Checkpoint skipped (existing checkpoint)"
                    end if

                case ("restart")
                    if (mpi_node() == 0 .and. val == "1") &
                        print*, "DEBUG - Restart command found"

                case ("steering_step") 
                    read(val, *) new_step
                    call set_workflow_step(new_step, sim)
                    if (mpi_node() == 0) print*, "DEBUG - Steering step changed to ", new_step
                
                case ("stop")
                    if (.not. is_checkpoint_step .and. val == "1") then
                        call write_restart(sim)
                        steering_exit = .true.
                        exit
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Stop command skipped"
                    end if
                
                case ("abort")
                    if (val == "1") then 
                        steering_exit = .true.
                        exit
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Abort command skipped"
                    end if

                case default
                    call trim_diagnostic(trim(keys(i)), diagnostic_name, diagnostic_ierr)
                    if (diagnostic_ierr == 0) then
                        select case (diagnostic_name)
                            case ("diag_current")
                                call parse_workflow_diagnostic(val, identifier, diag_command, diag_data, diagnostic_ierr)
                                if (mpi_node() == 0) then
                                    print *, "DEBUG - identifier = ", identifier
                                    if (allocated(diag_command)) print *, "DEBUG - command = ", trim(diag_command)
                                    if (allocated(diag_data) .and. size(diag_data) >= 3) &
                                        print *,  diag_data(1)
                                        print *,  diag_data(2)
                                        print *,  diag_data(3)
                                end if
                            
                            case ("diag_emf", "diag_neutral", "diag_species")
                                if (mpi_node() == 0) &
                                    print *, "DEBUG - ", trim(diagnostic_name), " command found"
                                    
                            case default
                                if (mpi_node() == 0) &
                                    print *, "Unknown command: ", trim(diagnostic_name)
                        end select
                    end if
            end select
        end do

        ! Cleanup -> Be careful to deallocare only after assigning value 
        if (allocated(val)) deallocate(val)
        if (allocated(keys)) deallocate(keys)
        if (allocated(diag_command)) deallocate(diag_command)
        if (allocated(diag_data)) deallocate(diag_data)
        if (allocated(identifier)) deallocate(identifier)
        if (allocated(diagnostic_name)) deallocate(diagnostic_name)
    end subroutine check_and_execute

    ! <-------------------------------->! DUPLICATED ROUTINE FROM MAIN MODULE

    subroutine write_restart( sim ) !^!

        implicit none

        class ( t_simulation ), intent(inout) :: sim

        type( t_restart_handle )    ::  restart_handle

        call begin_event(restart_write_ev)

        if ( mpi_node() == 0 ) then
            print *, ''
            print *, ' Writing checkpoint information for timestep n =', n(sim%tstep)
            print *, ''
        endif

        #ifdef __RST_SION__

        ! Measure expected file size, only required by the sion library
        restart_handle%if_acc_size = .true.
        restart_handle%data_size = 0
        call sim%write_checkpoint( restart_handle )
        restart_handle%if_acc_size = .false.

        #endif

        ! Open checkpoint files
        call restart_write_open( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                                n(sim%tstep), ndump(sim%tstep), &
                                file_id_rst, restart_handle )

        ! Write checkpoint data
        call sim%write_checkpoint( restart_handle )

        ! Close checkpoint files
        call restart_write_close( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                                    n(sim%tstep), restart_handle )

        call end_event(restart_write_ev)

        end subroutine write_restart

end module m_workflow
