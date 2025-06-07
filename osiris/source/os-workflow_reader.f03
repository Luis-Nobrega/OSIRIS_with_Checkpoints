module m_workflow_reader !*!

    use, intrinsic :: iso_fortran_env, only: iostat_end
    use mpi, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_CHARACTER, MPI_BCAST, MPI_BARRIER
    use m_node_conf, only: t_node_conf, root
    implicit none
    private

    ! Public interfaces
    public :: read_steering_file, get_value, get_size, steering_filename, get_keys
    public :: parse_workflow_diagnostic, trim_diagnostic

    ! Internal storage type
    type :: term_value_pair
        character(len=:), allocatable :: term
        character(len=:), allocatable :: value
    end type term_value_pair

    type :: term_value_collection
        type(term_value_pair), allocatable :: pairs(:)
        integer :: count = 0
        type(t_node_conf), pointer :: no_co => null()
    contains
        procedure :: add => add_or_update_term
        procedure :: get => get_term_value
        procedure :: size => collection_size
    end type term_value_collection

    ! Single collection instance
    type(term_value_collection), private :: collection
    character(len=256), parameter :: steering_filename = 'steering_input_deck'

contains

    ! New version that takes raw file content
    subroutine read_steering_file(no_co, file_content, iostat)
        type(t_node_conf), intent(in), target :: no_co
        character(len=*), intent(in) :: file_content  ! Raw file content
        integer, intent(out), optional :: iostat
        integer :: io_stat = 0
        
        collection%no_co => no_co
        
        ! Reset collection before parsing new content
        if (allocated(collection%pairs)) deallocate(collection%pairs)
        collection%count = 0
        
        ! All processes parse the file content independently
        call parse_steering_data(file_content, io_stat)
        
        if (present(iostat)) iostat = io_stat
    end subroutine read_steering_file

    subroutine parse_steering_data(file_content, iostat)
        character(len=*), intent(in) :: file_content
        integer, intent(out) :: iostat
        integer :: pos, eq_pos, line_end, line_num
        character(len=:), allocatable :: line, key, value
        logical :: success

        iostat = 0
        pos = 1
        line_num = 0

        do while (pos <= len(file_content))
            ! Find end of line
            line_end = index(file_content(pos:), new_line('A'))
            if (line_end == 0) line_end = len(file_content) - pos + 2
            
            ! Extract line
            line = file_content(pos:pos+line_end-2)
            pos = pos + line_end
            line_num = line_num + 1

            ! Skip empty lines and comments
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '!') cycle

            ! Find key-value pair
            eq_pos = index(line, '=')
            if (eq_pos == 0) then
                ! ignore line if no '=' found
                cycle
            endif

            key = adjustl(trim(line(1:eq_pos-1)))
            value = adjustl(trim(line(eq_pos+1:)))

            ! Store in collection
            call collection%add(key, value)
        end do
    end subroutine parse_steering_data

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
        integer :: success_count  ! NOVO
        logical :: success

        if (present(iostat)) iostat = 0
        success_count = 0  ! Inicializa contador de entradas válidas

        do
            read(file_unit, '(A)', iostat=io_stat) line
            if (io_stat /= 0) exit

            if (len_trim(line) == 0) cycle

            comment_pos = index(line, '!')
            if (comment_pos > 0) line = line(1:comment_pos-1)
            if (len_trim(line) == 0) cycle

            eq_pos = index(line, '=')
            if (eq_pos <= 0) cycle

            key = adjustl(trim(line(1:eq_pos-1)))
            value = adjustl(trim(line(eq_pos+1:)))

            call processor(key, value, success)
            if (success) then
                success_count = success_count + 1
            else
                print *, "DEBUG - Falha ao processar linha: ", trim(line)
            endif
        end do

        if (present(iostat)) iostat = io_stat

        ! DEBUG: mostrar quantos pares foram processados com sucesso
        !print *, "DEBUG - Linhas processadas com sucesso: ", success_count

        ! Define erro se nenhum par foi processado com sucesso
        if (present(iostat) .and. success_count == 0) iostat = 1

        if (present(iostat)) then
            if (io_stat == -1) then
                iostat = 0  ! EOF não é erro
            else
                iostat = io_stat
            endif
        endif
    end subroutine read_key_value_lines

    subroutine process_pair(key, value, success)
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        logical, intent(out) :: success

        call collection%add(key, value)
        success = .true.
    end subroutine process_pair

    subroutine add_or_update_term(this, key, value)
        class(term_value_collection), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer :: i
        type(term_value_pair), allocatable :: tmp(:)

        do i = 1, this%count
            if (trim(this%pairs(i)%term) == trim(key)) then
                this%pairs(i)%value = value
                return
            endif
        end do

        if (.not. allocated(this%pairs)) then
            allocate(this%pairs(1))
        else
            allocate(tmp(this%count + 1))
            tmp(1:this%count) = this%pairs
            call move_alloc(tmp, this%pairs)
        endif

        this%count = this%count + 1
        this%pairs(this%count)%term = key
        this%pairs(this%count)%value = value
    end subroutine add_or_update_term

    function get_term_value(this, key) result(val)
        class(term_value_collection), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: val
        integer :: i

        val = ""
        do i = 1, this%count
            if (trim(this%pairs(i)%term) == trim(key)) then
                val = this%pairs(i)%value
                return
            endif
        end do
    end function get_term_value

    function collection_size(this) result(n)
        class(term_value_collection), intent(in) :: this
        integer :: n
        n = this%count
    end function collection_size

    function get_keys() result(keys)    
        character(len=:), allocatable :: keys(:)
        integer :: i, max_len

        ! Primeiro determinar o comprimento máximo necessário
        max_len = 0
        do i = 1, collection%count
            max_len = max(max_len, len_trim(collection%pairs(i)%term))
        end do

        ! Alocar o array com comprimento fixo suficiente
        allocate(character(len=max_len) :: keys(collection%count))

        ! Copiar as chaves
        do i = 1, collection%count
            keys(i) = trim(collection%pairs(i)%term)
        end do
    end function get_keys


    ! <-------------------------------->!
    ! <--- Read diagnostic commands --->!
    ! <-------------------------------->!


    subroutine parse_workflow_diagnostic(string, identifier, command, data, ierr)
        ! Subroutine parses strings for workflow diagnostics.
        ! Format: [identifier, command, value1, value2, ...]
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: identifier, command
        character(len=:), allocatable, intent(out) :: data(:)
        integer, intent(out) :: ierr
        
        character(len=len(string)) :: working_string, temp_string
        character(len=len(string)), allocatable :: temp_data(:)
        integer :: i, n, pos, count
        
        ! Initialize outputs
        ierr = 1
        if (allocated(identifier)) deallocate(identifier)
        if (allocated(command)) deallocate(command)
        if (allocated(data)) deallocate(data)
        
        ! Remove whitespace and brackets
        working_string = adjustl(string)
        working_string = trim(working_string)
        if (len_trim(working_string) == 0) return
        
        ! Remove brackets if present
        if (working_string(1:1) == '[' .and. working_string(len_trim(working_string):len_trim(working_string)) == ']') then
            working_string = working_string(2:len_trim(working_string)-1)
            working_string = trim(adjustl(working_string))
        end if
        
        ! Count commas to determine number of elements
        count = 1
        do i = 1, len_trim(working_string)
            if (working_string(i:i) == ',') count = count + 1
        end do
        
        ! Need at least identifier, command, and one data value
        if (count < 3) return
        
        ! First parse identifier
        pos = index(working_string, ',')
        if (pos == 0) return
        identifier = trim(adjustl(working_string(1:pos-1)))
        
        ! Then parse command
        temp_string = working_string(pos+1:)
        pos = index(temp_string, ',')
        if (pos == 0) return
        command = trim(adjustl(temp_string(1:pos-1)))
        
        ! Finally parse data values
        temp_string = temp_string(pos+1:)
        count = 1
        do i = 1, len_trim(temp_string)
            if (temp_string(i:i) == ',') count = count + 1
        end do
        
        ! Temporary fixed-length storage
        allocate(character(len=len(temp_string)) :: temp_data(count))
        
        do i = 1, count
            pos = index(temp_string, ',')
            if (pos == 0) then
                temp_data(i) = trim(adjustl(temp_string))
                exit
            else
                temp_data(i) = trim(adjustl(temp_string(1:pos-1)))
                temp_string = temp_string(pos+1:)
            end if
        end do
        
        ! Convert to deferred-length strings
        allocate(character(len=maxval(len_trim(temp_data))) :: data(count))
        do i = 1, count
            data(i) = trim(temp_data(i))
        end do
        
        ierr = 0
    end subroutine parse_workflow_diagnostic

   subroutine trim_diagnostic(string, trimmed_name, ierr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: trimmed_name
        integer, intent(out) :: ierr

        integer :: first_underscore, second_underscore
        character(len=:), allocatable :: temp

        ! Initialize outputs
        trimmed_name = ''
        ierr = 1

        ! Find first underscore
        first_underscore = index(string, '_')
        if (first_underscore == 0) return

        ! Find second underscore (after first one)
        second_underscore = index(string(first_underscore+1:), '_')

        ! Extract portion of string
        if (second_underscore > 0) then
            temp = string(1:first_underscore + second_underscore - 1)
        else
            temp = string
        end if

        ! Remove whitespace and assign
        trimmed_name = trim(adjustl(temp))

        if (len_trim(trimmed_name) > 0) ierr = 0
    end subroutine trim_diagnostic

end module m_workflow_reader
