module m_workflow  
    implicit none 

    private
    public :: check_workflow_step, set_workflow_step  ! Making the subroutine public

    integer :: iteration_counter = 0
    integer :: workflow_step = 50  
    
    character(len=*), parameter :: steering_filename = 'steering_input_deck'

contains      

    subroutine set_workflow_step(step)
        ! This subroutine sets the workflow step
        ! Input argument:
        ! step = the new workflow step value
        implicit none
        integer, intent(in) :: step

        if (step > 0) then
            workflow_step = step
            print*, "Workflow step set to: ", workflow_step
        else
            print*, "Invalid workflow step. Must be greater than 0."
            print*, "Defaulting to: ", workflow_step
        end if
    end subroutine set_workflow_step

    !<------------------------------------------->!

    subroutine check_file_exists(file_ok)
        ! This subroutine checks if the file exists and is readable
        ! Output argument:
        !   file_ok = .true. if file exists and can be opened, .false. otherwise
        implicit none
        logical, intent(out) :: file_ok
        logical :: exists
        integer :: iunit, ierr

        inquire(file=steering_filename, exist=exists)

        if (exists) then
            open(newunit=iunit, file=steering_filename, status='old', iostat=ierr)
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

    !<------------------------------------------->!

    subroutine rename_file(old_name, new_name, success)
        ! Renames a file using a shell command
        implicit none
        character(len=*), intent(in)  :: old_name
        character(len=*), intent(in)  :: new_name
        logical, intent(out) :: success
        integer :: status
        character(len=512) :: command

        ! Construct shell command (works for Unix-like systems)
        command = 'mv "' // trim(old_name) // '" "' // trim(new_name) // '"'
        call execute_command_line(trim(command), exitstat=status)
        success = (status == 0)
        
        if (.not. success) then
            print *, "Error: Failed to rename file using shell command"
            print *, "Command: ", trim(command)
            print *, "Exit status: ", status
        end if
    end subroutine rename_file
    
    !<------------------------------------------->!

    subroutine check_workflow_step(file_ok)
        ! Verifies if step is valid for reading the new file 
        implicit none
        logical, intent(out) :: file_ok
        character(len=256) :: used_filename
        logical :: file_exists_now
        
        ! First do the cheap modulo check
        if (iand(iteration_counter, workflow_step - 1) == 0) then  ! Faster alternative to MOD when workflow_step is power of 2
        ! Alternative: if (mod(iteration_counter, workflow_step) == 0) then  ! Standard version
        
            ! Only check file if we passed the modulo test
            call check_file_exists(file_exists_now)
            
            if (file_exists_now) then
                print*, "Reading workflow file at iteration: ", iteration_counter

                used_filename = trim(steering_filename) // '_used'
                call rename_file(steering_filename, used_filename, file_ok)
            else
                file_ok = .false.
            end if
        else
            file_ok = .false.
        end if
        
        iteration_counter = iteration_counter + 1
    end subroutine check_workflow_step

end module m_workflow