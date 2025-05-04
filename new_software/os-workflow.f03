module m_workflow  
    implicit none 

    private
    public :: file_exists  ! Explicitly make it available

    character(len=*), parameter :: steering_filename = 'steering_input_deck'
   
contains      

    logical function file_exists()
    ! This function checks if the steering file exists and is readable
    ! It returns .true. if the file exists and is readable, .false. otherwise
        implicit none
        logical :: exists
        integer :: iunit, ierr
        
        inquire(file=steering_filename, exist=exists)
        
        if (exists) then
            open(newunit=iunit, file=steering_filename, status='old', iostat=ierr)
            if (ierr == 0) then
                close(iunit)
                file_exists = .true.
                print*, "File exists and is readable"
            else
                file_exists = .false.
                print*, "File does not exist and is readable"
                print*, "Error opening file: ", steering_filename
                print*, "I/O error code: ", ierr
            end if
        else
            file_exists = .false.
        end if
    end function file_exists


end module m_workflow
