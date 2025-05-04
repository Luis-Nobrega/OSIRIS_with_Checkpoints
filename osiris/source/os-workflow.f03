module m_workflow  
    implicit none 

    private
    public :: check_file_exists  ! Tornando a subrotina pública

    character(len=*), parameter :: steering_filename = 'steering_input_deck'

contains      

    subroutine check_file_exists(file_ok)
    ! Esta subrotina verifica se o arquivo existe e é legível
    ! Argumento de saída:
    !   file_ok = .true. se o arquivo existe e pode ser aberto, .false. caso contrário
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
                print*, "File exists but could not be opened"
                print*, "Error opening file: ", steering_filename
                print*, "I/O error code: ", ierr
            end if
        else
            file_ok = .false.
            print*, "File does not exist"
        end if
    end subroutine check_file_exists

end module m_workflow
