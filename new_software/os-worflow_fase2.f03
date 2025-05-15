module m_workflow_reader
  use m_input_file        ! brings in t_input_file, open_input, get_namelist
  use mpi                 ! for MPI_COMM_WORLD
  implicit none
  private

  public :: read_workflow_deck
  public :: steering_file

  type(t_input_file) :: steering_file   ! module‐scope buffer

contains

  subroutine read_workflow_deck(filename)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: stat

    ! 1) Read entire input into our buffer
    call open_input(steering_file, filename, MPI_COMM_WORLD)
    ! 2) Attempt to extract the "laser" block
    steering_file%pos = 0
    call get_namelist( steering_file, "laser", stat )
    ! 3) Output whatever raw text we got (if any)
    if (stat == 0) then
      print *, "=== laser namelist block ==="
      print *, trim(steering_file%nml_text)
    else
      print *, "No 'laser' block found in " // trim(filename)
    end if

    ! (Optional) close later with: call close_input(steering_file)
  end subroutine read_workflow_deck

end module m_workflow_reader
