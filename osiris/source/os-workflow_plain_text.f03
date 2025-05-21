module m_workflow_reader
    use, intrinsic :: iso_fortran_env, only: iostat_end
    implicit none
    private
    
    ! Public interfaces
    public :: read_steering_file, get_value, get_size, steering_filename
    
    ! Internal storage type
    type :: term_value_pair
        character(len=:), allocatable :: term
        character(len=:), allocatable :: value
    end type term_value_pair
    
    type :: term_value_collection
        type(term_value_pair), allocatable :: pairs(:)
        integer :: count = 0
    contains
        procedure :: add => add_or_update_term
        procedure :: get => get_term_value
        procedure :: size => collection_size
    end type term_value_collection
    
    ! Single collection instance
    type(term_value_collection), private :: collection
    character(len=256), parameter :: steering_filename = 'steering_input_deck' ! Name of the steering file

contains

    subroutine read_steering_file(iostat)
        integer, intent(out), optional :: iostat
        integer :: unit, io_stat
        
        open(newunit=unit, file=steering_filename, status='old', action='read', iostat=io_stat)
        if (present(iostat)) iostat = io_stat
        if (io_stat /= 0) then
            print *, "File didn't open properly!", trim(steering_filename), "iostat=", io_stat
            return
        end if
        
        call read_key_value_lines(unit, process_pair, io_stat)
        close(unit)
        
        if (present(iostat)) iostat = io_stat
    end subroutine read_steering_file

    function get_value(key) result(value)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: value
        value = collection%get(key)
    end function get_value

    function get_size() result(n)
        integer :: n
        n = collection%size()
    end function get_size

    subroutine read_key_value_lines(file_unit, processor, iostat)
        integer, intent(in) :: file_unit
        interface
            subroutine processor(key, value, success)
                character(len=*), intent(in) :: key
                character(len=*), intent(in) :: value
                logical, intent(out) :: success
            end subroutine
        end interface
        integer, intent(out), optional :: iostat
        character(len=256) :: line, key, value
        integer :: io_stat, eq_pos, comment_pos
        logical :: success
        
        if (present(iostat)) iostat = 0
        
        do
            read(file_unit, '(A)', iostat=io_stat) line
            if (io_stat /= 0) exit
            
            ! Skip empty lines
            if (len_trim(line) == 0) cycle
            
            ! Find comment marker (!) and ignore everything after it
            comment_pos = index(line, '!')
            if (comment_pos > 0) then
                line = line(1:comment_pos-1)
            end if
            
            ! Skip if line is empty after comment removal
            if (len_trim(line) == 0) cycle
            
            ! Find equals sign
            eq_pos = index(line, '=')
            if (eq_pos == 0) cycle  ! Skip lines without equals sign
            
            ! Extract key (left of equals) and value (right of equals)
            key = adjustl(line(1:eq_pos-1))
            value = adjustl(line(eq_pos+1:))
            
            ! Remove any remaining whitespace
            key = trim(key)
            value = trim(value)
            
            ! Skip if key is empty
            if (len_trim(key) == 0) cycle
            
            print *, "DEBUG - Found pair: '", trim(key), "' = '", trim(value), "'"
            call processor(key, value, success)
            if (.not. success .and. present(iostat)) iostat = -2
        end do
        
        if (present(iostat)) then
            if (io_stat == iostat_end) then
                iostat = 0  ! Normal end-of-file
            else
                iostat = io_stat  ! Propagate other errors
            end if
        end if
    end subroutine read_key_value_lines

    subroutine process_pair(key, value, success)
        character(len=*), intent(in) :: key, value
        logical, intent(out) :: success
        call collection%add(key, value, success)
    end subroutine process_pair

    subroutine add_or_update_term(this, key, value, success)
        class(term_value_collection), intent(inout) :: this
        character(len=*), intent(in) :: key, value
        logical, intent(out) :: success
        integer :: i
        
        do i = 1, this%count
            if (this%pairs(i)%term == key) then
                this%pairs(i)%value = value
                success = .true.
                return
            end if
        end do
        
        if (.not. allocated(this%pairs)) allocate(this%pairs(10))
        if (this%count == size(this%pairs)) then
            block
                type(term_value_pair), allocatable :: temp(:)
                allocate(temp(2*size(this%pairs)))
                temp(1:this%count) = this%pairs
                call move_alloc(temp, this%pairs)
            end block
        end if
        
        this%count = this%count + 1
        this%pairs(this%count)%term = key
        this%pairs(this%count)%value = value
        success = .true.
    end subroutine add_or_update_term

    function get_term_value(this, term) result(value)
        class(term_value_collection), intent(in) :: this
        character(len=*), intent(in) :: term
        character(len=:), allocatable :: value
        integer :: i
        
        value = ""
        do i = 1, this%count
            if (this%pairs(i)%term == term) then
                value = this%pairs(i)%value
                exit
            end if
        end do
    end function get_term_value

    function collection_size(this) result(n)
        class(term_value_collection), intent(in) :: this
        integer :: n
        n = this%count
    end function collection_size

end module m_workflow_reader