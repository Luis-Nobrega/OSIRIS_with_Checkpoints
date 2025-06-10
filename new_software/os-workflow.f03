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
    use m_time !^! Para a função t()
    use m_node_conf       !^! Para a função comm()
    use stringutil !^! Para converter strings para integers

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

    subroutine check_file_exists(filename, file_exists, file_size, file_content)
        implicit none
        character(len=*), intent(in) :: filename
        logical, intent(out) :: file_exists
        integer, intent(out) :: file_size
        character(:), allocatable, intent(out) :: file_content  ! Will hold raw bytes
        integer :: iunit, ierr

        ! Check if file exists and get its size
        inquire(file=filename, exist=file_exists, size=file_size)

        if (file_exists) then
            ! Allocate buffer to hold file content
            allocate(character(file_size) :: file_content)

            ! Open file in binary/stream mode to read raw bytes
            open(newunit=iunit, file=filename, access='stream', form='unformatted', &
                status='old', action='read', iostat=ierr)

            if (ierr == 0) then
                ! Read entire file into the buffer
                read(iunit, iostat=ierr) file_content
                close(iunit)

                if (ierr /= 0) then
                    deallocate(file_content)
                    file_exists = .false.
                    file_size = -1
                    print*, "Error reading file content. I/O error code: ", ierr
                else
                    print*, "Successfully read file. Size (bytes): ", file_size
                end if
            else
                deallocate(file_content)
                file_exists = .false.
                file_size = -1
                print*, "File could not be opened. I/O error code: ", ierr
            end if
        else
            file_size = -1
            print*, "File does not exist: ", trim(filename)
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
        integer :: file_size, ierr
        character(:), allocatable :: file_content  ! Removed intent(out) as it's local
        
        if (mod(iteration_counter, workflow_step) == 0 .and. iteration_counter /= 0) then
            ! if root, read the steering file and get the size in bytes
            if (root(no_co)) then
                call check_file_exists(filename, file_exists_now, file_size, file_content)
            end if
            
            ! Broadcast file existence status and the file size
            call MPI_BCAST(file_exists_now, 1, MPI_LOGICAL, 0, no_co%comm, ierr) 
            call MPI_BCAST(file_size, 1, MPI_INTEGER, 0, no_co%comm, ierr) 

            if (file_exists_now .and. file_size > 0) then
                ! Allocate space for content on non-root processes
                if (.not. root(no_co)) allocate(character(len=file_size) :: file_content)
                
                ! Broadcast file content
                if (file_size > 0) then
                    call MPI_BCAST(file_content, file_size, MPI_CHARACTER, 0, no_co%comm, ierr)
                    if (ierr /= 0) then
                        file_ok = .false.
                        if (allocated(file_content)) deallocate(file_content)
                        return
                    end if
                end if
                
                ! parse the file content
                call read_steering_file(no_co, file_content, ierr)
                
                if (ierr == 0) then
                    if (root(no_co)) then
                        call rename_file(filename, used_filename, success)
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

        if (allocated(file_content)) deallocate(file_content)
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
        integer :: ierr

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

            ierr = 0

            select case (trim(keys(i)))
                case ("checkpoint")
                    if (.not. is_checkpoint_step .and. val == "1") then
                        call write_restart(sim)
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Checkpoint skipped (existing checkpoint)"
                    end if

                 case ("tmax")
                    ! Change the simulation maximum time
                    call set_max_time(sim, get_value(trim(keys(i))) , ierr)

                case ("restart")
                    ! IN PROGRESS ????????????????????????????????????'''
                    if (mpi_node() == 0 .and. val == "1") &
                        print*, "DEBUG - Restart command found"

                case ("steering_step") 
                    ! Change the steering step. This is absolute frequency! 
                    read(val, *) new_step
                    call set_workflow_step(new_step, sim)
                    if (mpi_node() == 0) print*, "DEBUG - Steering step changed to ", new_step
                
                case ("stop")
                    ! Output restart files and stop the simulation
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
                    ! Handle diagnostics 
                    call trim_diagnostic(trim(keys(i)), diagnostic_name, diagnostic_ierr)
                    if (diagnostic_ierr == 0) then
                        select case (diagnostic_name)

                            ! THIS PART IS CURRENTLY USELESS
                            case ("diag_current")
                                call parse_workflow_diagnostic(val, identifier, diag_command, diag_data, diagnostic_ierr)
                                if (mpi_node() == 0) then
                                    print *, "DEBUG - identifier = ", identifier
                                end if
                            
                            case ("diag_emf", "diag_neutral", "diag_species")
                                if (mpi_node() == 0) &
                                    print *, "DEBUG - ", trim(diagnostic_name), " command found"
                                    
                            case default
                                if (mpi_node() == 0) &
                                    print *, "Unknown command: ", trim(diagnostic_name)
                            ! THIS PART IS CURRENTLY USELESS
                        end select
                    end if
            end select

            ! SEE WHETHER TO BCAST IERR FOR SYNCRONIZARION ??????????????????????????????????????

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
        
        !<-------------------------------->! 

        subroutine set_max_time(sim, val, ierr)
        implicit none

        class(t_simulation), intent(inout) :: sim
        character(len=*), intent(in) :: val
        integer, intent(out) :: ierr

        real(p_double) :: updated_tmax
        integer :: conv_ierr

        ierr = 0

        updated_tmax = strtodouble(val, conv_ierr)

        if (updated_tmax == 0.0d0 .or. updated_tmax < 0.0d0) then
            ierr = 1
            if (mpi_node() == 0) then
                print*, "DEBUG - Tmax setting skipped", trim(val)
            end if
            return
        end if

        if (conv_ierr == 0) then
            ! Use a função pública para obter o tempo atual
            if (updated_tmax > t(sim%time)) then
                call set_tmax(sim%time, updated_tmax)  ! Você precisa implementar essa rotina
                if (mpi_node() == 0) then
                    print*, "DEBUG - Updated tmax to ", tmax(sim%time)
                end if
            else if (mpi_node() == 0) then
                print*, "DEBUG - tmax not updated, new value ", updated_tmax, &
                        " is not larger than current time ", t(sim%time)
            end if
        else
            ierr = 1
            if (mpi_node() == 0) then
                print*, "DEBUG - Error converting tmax value: ", trim(val)
            end if
        end if

    end subroutine set_max_time

    !<-------------------------------->! 

    ! CONVERTER CASE DEFAULT DE CIMA PARA UMA SUBROTINA POR FAVOR
    subroutine process_diagnostics()
        implicit none
        print*, "DEBUG - TEST"
        
    end subroutine process_diagnostics

end module m_workflow
