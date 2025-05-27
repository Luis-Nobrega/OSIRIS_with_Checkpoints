module m_workflow_reader !*!

    use, intrinsic :: iso_fortran_env, only: iostat_end
    use mpi, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_CHARACTER, MPI_BCAST, MPI_BARRIER
    use m_node_conf, only: t_node_conf, root
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

end module m_workflow_reader
