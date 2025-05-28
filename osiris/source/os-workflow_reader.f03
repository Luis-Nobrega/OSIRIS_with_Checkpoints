module m_workflow_reader !*!

    use, intrinsic :: iso_fortran_env, only: iostat_end
    use mpi, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_CHARACTER, MPI_BCAST, MPI_BARRIER
    use m_node_conf, only: t_node_conf, root
    implicit none
    private

    ! Public interfaces
    public :: read_steering_file, get_value, get_size, steering_filename, get_keys ! for reading the steering file
    public :: parse_workflow_diagnostic, trim_diagnostic ! for diagnostics

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
        procedure :: broadcast => broadcast_collection
    end type term_value_collection

    ! Single collection instance
    type(term_value_collection), private :: collection
    character(len=256), parameter :: steering_filename = 'steering_input_deck'

contains

    subroutine read_steering_file(no_co, iostat)
        type(t_node_conf), intent(in), target :: no_co
        integer, intent(out), optional :: iostat
        integer :: unit, io_stat, ierr

        collection%no_co => no_co

        if (root(no_co)) then
            open(newunit=unit, file=steering_filename, status='old', action='read', iostat=io_stat)
            if (io_stat /= 0) then
                print *, "Error opening steering file: ", trim(steering_filename), " iostat=", io_stat
                if (present(iostat)) iostat = io_stat
                return
            endif

            call read_key_value_lines(unit, process_pair, io_stat)
            !print *, "DEBUG - io_stat do read_key_value_lines: ", io_stat
            close(unit)  ! Close after reading is done
        endif

        ! Broadcast the read status to all processes
        call MPI_BCAST(io_stat, 1, MPI_INTEGER, 0, no_co%comm, ierr)

        ! Only broadcast the collection if the read was successful
        if (io_stat == 0) then
            call collection%broadcast(no_co%comm)
            !print *, "DEBUG - Broadcasted collection"
        endif

        if (present(iostat)) iostat = io_stat
    end subroutine read_steering_file

    subroutine broadcast_collection(this, comm)
        class(term_value_collection), intent(inout) :: this
        integer, intent(in) :: comm
        integer :: i, term_len, value_len, ierr

        call MPI_BCAST(this%count, 1, MPI_INTEGER, 0, comm, ierr)

        if (.not. root(this%no_co) .and. .not. allocated(this%pairs)) then
            allocate(this%pairs(this%count))
        endif

        do i = 1, this%count
            if (root(this%no_co)) then
                term_len = len(this%pairs(i)%term)
                value_len = len(this%pairs(i)%value)
            endif
            call MPI_BCAST(term_len, 1, MPI_INTEGER, 0, comm, ierr)
            call MPI_BCAST(value_len, 1, MPI_INTEGER, 0, comm, ierr)

            if (.not. root(this%no_co)) then
                this%pairs(i)%term  = repeat(' ', term_len)
                this%pairs(i)%value = repeat(' ', value_len)
            endif

            call MPI_BCAST(this%pairs(i)%term, term_len, MPI_CHARACTER, 0, comm, ierr)
            call MPI_BCAST(this%pairs(i)%value, value_len, MPI_CHARACTER, 0, comm, ierr)
        end do
    end subroutine broadcast_collection

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
        logical, intent(out) :: ierr
        
        character(len=len(string)) :: working_string, temp_string
        character(len=len(string)), allocatable :: temp_data(:)
        integer :: i, n, pos, count
        
        ! Initialize outputs
        ierr = .false.
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
        
        ierr = .true.
    end subroutine parse_workflow_diagnostic

    subroutine trim_diagnostic(string, trimmed_name, ierr)
        ! Subroutine to extract base diagnostic name (removes trailing _N)
        ! Input format: "diag_type_N" → returns "diag_type"
        character(len=*), intent(in) :: string
        character(len=*), intent(out) :: trimmed_name
        logical, intent(out) :: ierr
        
        integer :: first_underscore, second_underscore
        
        ! Initialize outputs
        trimmed_name = ''
        ierr = .false.
        
        ! Find first underscore
        first_underscore = index(string, '_')
        if (first_underscore == 0) return  ! No underscores found
        
        ! Find second underscore (after first one)
        second_underscore = index(string(first_underscore+1:), '_')
        
        ! Extract appropriate portion of string
        if (second_underscore > 0) then
            trimmed_name = string(1:first_underscore + second_underscore - 1)
        else
            trimmed_name = trim(string)
        end if
        
        ! Remove any leading/trailing whitespace
        trimmed_name = trim(adjustl(trimmed_name))
        
        ! Set success flag if we got a non-empty result
        if (len_trim(trimmed_name) > 0) ierr = .true.
    end subroutine trim_diagnostic

end module m_workflow_reader
