program workflow_test
    use m_workflow_reader, only: read_steering_file, get_value, get_size, steering_filename
    implicit none
    
    integer :: i, iostat, n_entries
    character(len=:), allocatable :: val
    
    print *, "Attempting to read steering file: ", steering_filename
    
    call read_steering_file(iostat)
    
    if (iostat /= 0) then
        print *, "Error reading steering file! iostat = ", iostat
        stop 1
    end if
    
    n_entries = get_size()
    print *, "Number of key-value pairs found: ", n_entries
    
    if (n_entries > 0) then
        print *, "----------------------------------------"
        print *, "Contents of steering file:"
        
        ! Test some keys
        call test_key("simulation_name")
        call test_key("time_steps")
        call test_key("output_frequency")
        call test_key("initial_condition")
        call test_key("output_format")
        
        print *, "----------------------------------------"
    else
        print *, "Warning: Steering file appears to be empty or no valid pairs found"
    end if
    
contains
    
    subroutine test_key(key)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: val
        
        val = get_value(key)
        if (len(val) > 0) then
            print *, "'", trim(key), "' = '", trim(val), "'"
        else
            print *, "Key '", trim(key), "' not found or has empty value"
        end if
    end subroutine test_key

end program workflow_test