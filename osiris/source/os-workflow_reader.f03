module m_workflow_reader !*!

    use, intrinsic :: iso_fortran_env, only: iostat_end
    use mpi, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_CHARACTER, MPI_BCAST, MPI_BARRIER
    use m_node_conf, only: t_node_conf, root
    use stringutil
    implicit none
    private

    integer, parameter :: p_double = kind(1.0d0)  ! Standard double precision

    ! Public interfaces
    public :: read_steering_file, get_value, get_size, steering_filename, get_keys
    public :: parse_workflow_diagnostic, trim_diagnostic, parse_bracketed_pair
    public :: str_array_to_int,str_array_to_real, str_array_to_logical, determine_array_type

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


    subroutine parse_workflow_diagnostic(string, name, identifier, command, data, add, ierr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: name, identifier, command, data(:)
        logical, intent(out) :: add
        integer, intent(out) :: ierr
        character(len=len(string)) :: working_string, temp_string
        character(len=len(string)), allocatable :: temp_data(:)
        integer :: i, pos, count, start_idx, end_idx

        ! Inicialização e limpeza
        ierr = 1
        add = .false.
        if (allocated(name)) deallocate(name)
        if (allocated(identifier)) deallocate(identifier)
        if (allocated(command)) deallocate(command)
        if (allocated(data)) deallocate(data)

        working_string = trim(adjustl(string))
        if (len_trim(working_string) == 0) then
            print*, "STEERING ERROR: Empty input string"
            return
        end if

        ! Remove colchetes
        if (working_string(1:1) == '[' .and. working_string(len_trim(working_string):) == ']') then
            working_string = working_string(2:len_trim(working_string)-1)
            working_string = trim(adjustl(working_string))
        end if

        ! Contar elementos baseados em ponto-e-vírgula
        count = 1
        do i = 1, len_trim(working_string)
            if (working_string(i:i) == ';') count = count + 1
        end do

        ! Verificar número mínimo de elementos
        if (count < 4) then
            print*, "STEERING ERROR: Expected 4+ elements, got", count
            return
        end if

        ! Parse NAME (primeiro campo)
        pos = index(working_string, ';')
        if (pos <= 0) then
            print*, "STEERING ERROR: Missing first ';'"
            return
        end if
        name = trim(adjustl(working_string(1:pos-1)))
        working_string = trim(adjustl(working_string(pos+1:)))

        ! Verificar e processar sinal '+'
        if (index(name, '+') > 0) then
            add = .true.
            name = name(1:index(name, '+')-1)
        end if

        ! Parse IDENTIFIER (segundo campo)
        pos = index(working_string, ';')
        if (pos <= 0) then
            print*, "STEERING ERROR: Missing second ';'"
            return
        end if
        identifier = trim(adjustl(working_string(1:pos-1)))
        working_string = trim(adjustl(working_string(pos+1:)))

        ! Parse COMMAND (terceiro campo)
        pos = index(working_string, ';')
        if (pos <= 0) then
            print*, "STEERING ERROR: Missing third ';'"
            return
        end if
        command = trim(adjustl(working_string(1:pos-1)))
        
        ! O restante são os DADOS (pode ter múltiplos valores)
        temp_string = trim(adjustl(working_string(pos+1:)))
        
        ! Contar valores de dados
        count = 1
        do i = 1, len_trim(temp_string)
            if (temp_string(i:i) == ';') count = count + 1
        end do

        ! Extrair valores de dados
        allocate(character(len=len(temp_string)) :: temp_data(count))
        do i = 1, count
            pos = index(temp_string, ';')
            if (pos == 0) then
                temp_data(i) = trim(adjustl(temp_string))
                exit
            else
                temp_data(i) = trim(adjustl(temp_string(1:pos-1)))
                temp_string = trim(adjustl(temp_string(pos+1:)))
            end if
        end do

        ! Converter para strings de comprimento variável
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

    ! <-------------------------------->!
    ! <--- Read other namelists --->!
    ! <-------------------------------->!

    subroutine parse_bracketed_pair(string, data1, data2, ierr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: data1, data2
        integer, intent(out) :: ierr
        
        character(len=len(string)) :: working_string
        integer :: start_idx, end_idx, semicolon_pos
        
        ! Initialize
        ierr = 1
        if (allocated(data1)) deallocate(data1)
        if (allocated(data2)) deallocate(data2)
        
        working_string = adjustl(string)
        
        ! Check for brackets
        start_idx = index(working_string, '[')
        end_idx = index(working_string, ']')
        
        if (start_idx == 0 .or. end_idx == 0 .or. start_idx >= end_idx) then
            print*, "ERROR: Missing or malformed brackets"
            return
        endif
        
        ! Extract content between brackets
        working_string = working_string(start_idx+1:end_idx-1)
        working_string = adjustl(working_string)  ! Trim leading spaces
        
        ! Find semicolon separator
        semicolon_pos = index(working_string, ';')
        if (semicolon_pos == 0) then
            print*, "ERROR: Missing semicolon separator"
            return
        endif
        
        ! Extract and trim both data items
        data1 = trim(adjustl(working_string(1:semicolon_pos-1)))
        data2 = trim(adjustl(working_string(semicolon_pos+1:)))
        
        ! Validate we got both items
        if (len_trim(data1) == 0 .or. len_trim(data2) == 0) then
            print*, "ERROR: Empty data item(s)"
            return
        endif
        
        ierr = 0  ! Success
    end subroutine parse_bracketed_pair

    ! <-------------------------------->!
    ! <--- Parse arrays of strings  --->!
    ! <-------------------------------->!

    subroutine str_array_to_int(str_array, int_array, ierr)
        implicit none
        ! Inputs
        character(len=:), allocatable, intent(in)  :: str_array(:)
        ! Outputs
        integer, allocatable, intent(out)          :: int_array(:)
        integer, intent(out)                       :: ierr

        ! Locals
        integer :: i, n, conv_ierr

        n = size(str_array)
        allocate(int_array(n))

        do i = 1, n
            ! Handle empty strings explicitly
            if (len_trim(str_array(i)) == 0) then
                ierr = 1
                !print *, "STR_ARRAY_TO_INT ERROR: Empty string at position ", i
                return
            end if
            
            int_array(i) = strtoint(trim(str_array(i)), conv_ierr)
            if (conv_ierr /= 0) then
                ierr = conv_ierr
                !print *, "STR_ARRAY_TO_INT ERROR: Cannot convert '", trim(str_array(i)), "' to integer"
                return
            end if
        end do

        ierr = 0
    end subroutine str_array_to_int

    subroutine str_array_to_real(str_array, real_array, ierr)
        implicit none
        ! Inputs
        character(len=:), allocatable, intent(in)  :: str_array(:)
        ! Outputs
        real(p_double), allocatable, intent(out)   :: real_array(:)
        integer, intent(out)                       :: ierr

        ! Locals
        integer :: i, n, conv_ierr

        n = size(str_array)
        allocate(real_array(n))

        do i = 1, n
            ! Handle empty strings explicitly
            if (len_trim(str_array(i)) == 0) then
                ierr = 1
                !print *, "STR_ARRAY_TO_REAL ERROR: Empty string at position ", i
                return
            end if
            
            real_array(i) = strtodouble(trim(str_array(i)), conv_ierr)
            if (conv_ierr /= 0) then
                ierr = conv_ierr
                !print *, "STR_ARRAY_TO_REAL ERROR: Cannot convert '", trim(str_array(i)), "' to double"
                return
            end if
        end do

        ierr = 0
    end subroutine str_array_to_real

    subroutine str_array_to_logical(str_array, logical_array, ierr)
        implicit none
        ! Inputs
        character(len=:), allocatable, intent(in)  :: str_array(:)
        ! Outputs
        logical, allocatable, intent(out)          :: logical_array(:)
        integer, intent(out)                       :: ierr

        ! Locals
        integer :: i, n

        n = size(str_array)
        allocate(logical_array(n))

        do i = 1, n
            ! Handle empty strings explicitly
            if (len_trim(str_array(i)) == 0) then
                ierr = 1
                !print *, "STR_ARRAY_TO_LOGICAL ERROR: Empty string at position ", i
                return
            end if
            
            if (trim(str_array(i)) == ".true.") then
                logical_array(i) = .true.
            else if (trim(str_array(i)) == ".false.") then 
                logical_array(i) = .false.
            else 
                ierr = 1
                !print *, "STR_ARRAY_TO_LOGICAL ERROR: Cannot convert '", trim(str_array(i)), "' to logical"
                return
            end if
        end do

        ierr = 0
    end subroutine str_array_to_logical

    subroutine determine_array_type(str_array, int_array, real_array, logical_array, ierr)
        implicit none
        ! Inputs
        character(len=:), allocatable, intent(in)  :: str_array(:)
        ! Outputs
        integer, allocatable, intent(out)          :: int_array(:)
        real(p_double), allocatable, intent(out)   :: real_array(:)
        logical, allocatable, intent(out)          :: logical_array(:)
        integer, intent(out)                       :: ierr

        ! Locals
        character(len=:), allocatable :: first_str
        integer :: conv_ierr
        integer :: dummy_int  ! To capture integer function result
        real(p_double) :: dummy_real  ! To capture real function result

        ! Initialize all output arrays as unallocated
        if (allocated(int_array)) deallocate(int_array)
        if (allocated(real_array)) deallocate(real_array)
        if (allocated(logical_array)) deallocate(logical_array)

        ! Check for empty input array
        if (size(str_array) == 0) then
            ierr = 1
            return
        end if

        ! Get first non-empty string if available
        first_str = trim(str_array(1))

        ! Check if first string is empty
        if (len_trim(first_str) == 0) then
            ierr = 1
            return
        end if

        ! Determine type based on first element
        if (first_str == ".true." .or. first_str == ".false.") then
            ! Convert to logical
            call str_array_to_logical(str_array, logical_array, ierr)
        else
            ! Try integer conversion - assign to dummy variable
            dummy_int = strtoint(first_str, conv_ierr)
            if (conv_ierr == 0) then
                call str_array_to_int(str_array, int_array, ierr)
            else
                ! Try real conversion - assign to dummy variable
                dummy_real = strtodouble(first_str, conv_ierr)
                if (conv_ierr == 0) then
                    call str_array_to_real(str_array, real_array, ierr)
                else
                    ! None of the types matched
                    ierr = 1
                end if
            end if
        end if
    end subroutine determine_array_type
   

end module m_workflow_reader
