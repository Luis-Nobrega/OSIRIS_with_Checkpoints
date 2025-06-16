module m_workflow !*!

    use m_workflow_reader
    use m_node_conf
    use mpi
    use m_simulation, only: t_simulation  ! Adiciona definição da simulação

    use m_simulation !^!
    use m_parameters !^!
    use m_restart !^!
    use m_logprof !^!
    use m_time_step       !^! Para a função n()
    use m_time !^! Para a função t()
    use m_node_conf       !^! Para a função comm()
    use stringutil !^! Para converter strings para integers

    ! Imports for diagnostics 

    use m_vdf_report  
    use m_vdf_define  
    use m_emf_diag
    use m_current_define
    use m_current_diag
    use m_neutral
    use m_diag_neutral
    use m_particles_define
    use m_particles
    use m_species_define

    use m_emf_define ! para o p_extfld_none
    


    implicit none

    private
    public :: check_workflow_step, set_workflow_step

    integer :: iteration_counter = 0
    integer :: workflow_step
    integer :: ierr, n_entries

    character(len=*), parameter :: filename = steering_filename
    character(len=256) :: used_filename = "steering_input_deck.used" ! Novo nome para o ficheiro renomeado

contains

    subroutine set_workflow_step(step, sim)
        implicit none
        integer, intent(in) :: step
        class(t_simulation), intent(inout) :: sim

        if (step > 0) then
            workflow_step = step
            if (root(sim%no_co)) then
                print*, "Workflow step set to: ", workflow_step
            end if
        else
            if (root(sim%no_co)) then
                print*, "Steering step not defined or invalid: ", step
                print*, "Use 'steering_step = value' Must be greater than 0."
                print*, "Won't be checking steering checkpoints."
            end if
        end if
    end subroutine set_workflow_step

    subroutine check_file_exists(filename, file_exists, file_size, file_content)
        implicit none
        character(len=*), intent(in) :: filename
        logical, intent(out) :: file_exists
        integer, intent(out) :: file_size
        character(:), allocatable, intent(out) :: file_content  ! Will hold raw bytes
        integer :: iunit, ierr

        ! Check if file exists and get its size
        inquire(file=filename, exist=file_exists, size=file_size)

        if (file_exists) then
            ! Allocate buffer to hold file content
            allocate(character(file_size) :: file_content)

            ! Open file in binary/stream mode to read raw bytes
            open(newunit=iunit, file=filename, access='stream', form='unformatted', &
                status='old', action='read', iostat=ierr)

            if (ierr == 0) then
                ! Read entire file into the buffer
                read(iunit, iostat=ierr) file_content
                close(iunit)

                if (ierr /= 0) then
                    deallocate(file_content)
                    file_exists = .false.
                    file_size = -1
                    print*, "Error reading file content. I/O error code: ", ierr
                else
                    print*, "Successfully read file. Size (bytes): ", file_size
                end if
            else
                deallocate(file_content)
                file_exists = .false.
                file_size = -1
                print*, "File could not be opened. I/O error code: ", ierr
            end if
        else
            file_size = -1
            print*, "File does not exist: ", trim(filename)
        end if
    end subroutine check_file_exists

    subroutine rename_file(old_name, new_name, success)
        implicit none
        character(len=*), intent(in) :: old_name
        character(len=*), intent(in) :: new_name
        logical, intent(out) :: success
        integer :: status
        character(len=512) :: command

        command = 'mv "' // trim(old_name) // '" "' // trim(new_name) // '"'
        call execute_command_line(trim(command), exitstat=status)
        success = (status == 0)

        if (.not. success) then
            print *, "Error: Failed to rename file using shell command"
            print *, "Command: ", trim(command)
            print *, "Exit status: ", status
        end if
    end subroutine rename_file

    subroutine check_workflow_step(file_ok, sim, no_co, steering_exit)
        implicit none
        logical, intent(out) :: file_ok, steering_exit
        type(t_simulation), intent(inout) :: sim
        type(t_node_conf), intent(in) :: no_co
        logical :: file_exists_now, success
        integer :: file_size, ierr
        character(:), allocatable :: file_content  ! Removed intent(out) as it's local
        
        if (mod(iteration_counter, workflow_step) == 0 .and. iteration_counter /= 0) then
            ! if root, read the steering file and get the size in bytes
            if (root(no_co)) then
                call check_file_exists(filename, file_exists_now, file_size, file_content)
            end if
            
            ! Broadcast file existence status and the file size
            call MPI_BCAST(file_exists_now, 1, MPI_LOGICAL, 0, no_co%comm, ierr) 
            call MPI_BCAST(file_size, 1, MPI_INTEGER, 0, no_co%comm, ierr) 

            if (file_exists_now .and. file_size > 0) then
                ! Allocate space for content on non-root processes
                if (.not. root(no_co)) allocate(character(len=file_size) :: file_content)
                
                ! Broadcast file content
                if (file_size > 0) then
                    call MPI_BCAST(file_content, file_size, MPI_CHARACTER, 0, no_co%comm, ierr)
                    if (ierr /= 0) then
                        file_ok = .false.
                        if (allocated(file_content)) deallocate(file_content)
                        return
                    end if
                end if
                
                ! parse the file content
                call read_steering_file(no_co, file_content, ierr)
                
                if (ierr == 0) then
                    if (root(no_co)) then
                        call rename_file(filename, used_filename, success)
                    end if
                    file_ok = .true.
                    call check_and_execute(sim, steering_exit)
                else
                    file_ok = .false.
                end if
            else
                file_ok = .false.
            end if
        else
            file_ok = .false.
        end if

        iteration_counter = iteration_counter + 1

        if (allocated(file_content)) deallocate(file_content)
    end subroutine check_workflow_step

    ! <-------------------------------->!
    ! <--- Change of parameters --->!
    ! <-------------------------------->!

    subroutine check_and_execute(sim, steering_exit)
        class(t_simulation), intent(inout) :: sim
        character(len=:), allocatable :: val
        integer :: i, new_step
        logical :: is_checkpoint_step
        logical, intent(out) :: steering_exit
        character(len=:), allocatable :: keys(:)
        integer :: ierr

        ! Diagnostic variables
        integer :: diagnostic_ierr
        character(len=:), allocatable :: diagnostic_name
        character(len=:), allocatable :: identifier
        character(len=:), allocatable :: diag_command
        character(len=:), allocatable :: diag_data(:)
        integer, allocatable :: diag_data_int(:)

        ! Check checkpoint status
        is_checkpoint_step = if_restart_write(sim%restart, n(sim%tstep), ndump(sim%tstep), &
                            comm(sim%no_co), sim%no_co%ngp_id())

        keys = get_keys()

        do i = 1, size(keys)
            val = get_value(trim(keys(i)))
            if (len_trim(val) == 0) then
                if (mpi_node() == 0) print *, "DEBUG - No value for ", trim(keys(i))
                cycle
            end if

            ierr = 0

            select case (trim(keys(i)))
                case ("checkpoint")
                    if (.not. is_checkpoint_step .and. val == "1") then
                        call write_restart(sim)
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Checkpoint skipped (existing checkpoint)"
                    end if

                 case ("tmax")
                    ! Change the simulation maximum time
                    call set_max_time(sim, get_value(trim(keys(i))) , ierr)

                case ("restart")
                    ! IN PROGRESS ????????????????????????????????????
                    if (mpi_node() == 0 .and. val == "1") &
                        print*, "DEBUG - Restart command found"

                case ("steering_step") 
                    ! Change the steering step. This is absolute frequency! 
                    read(val, *) new_step
                    call set_workflow_step(new_step, sim)
                    if (mpi_node() == 0) print*, "DEBUG - Steering step changed to ", new_step
                
                case ("stop")
                    ! Output restart files and stop the simulation
                    if (.not. is_checkpoint_step .and. val == "1") then
                        call write_restart(sim)
                        steering_exit = .true.
                        exit
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Stop command skipped"
                    end if
                
                case ("abort")
                    if (val == "1") then 
                        steering_exit = .true.
                        exit
                    else if (mpi_node() == 0) then
                        print*, "DEBUG - Abort command skipped"
                    end if

                case default
                    ! Handle diagnostics 
                    call trim_diagnostic(trim(keys(i)), diagnostic_name, diagnostic_ierr)
                    if (diagnostic_ierr == 0) then
                        select case (diagnostic_name)

                            ! THIS PART IS CURRENTLY USELESS
                            case ("diag_current")
                                call parse_workflow_diagnostic(val, identifier, diag_command, diag_data, diagnostic_ierr)

                                if (diagnostic_ierr == 0) then
                                    ! Convert string array to integer array 
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                    if (ierr == 0) then
                                        call add_current_report(sim, trim(identifier))
                                        call steering_current_diag(sim, trim(identifier), trim(diag_command), diag_data_int, ierr)
                                    end if
                                end if
            
                            ! Desalocar apenas se alocado
                            if (allocated(diag_data_int)) deallocate(diag_data_int)

                            case ("diag_emf")

                                call parse_workflow_diagnostic(val, identifier, diag_command, diag_data, diagnostic_ierr)

                                if (diagnostic_ierr == 0) then
                                    ! Convert string array to integer array 
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                    if (ierr == 0) then
                                        call add_emf_report(sim, trim(identifier))
                                        call steering_emf_diag(sim, trim(identifier), trim(diag_command), diag_data_int, ierr)
                                    end if
                                end if
            
                            ! Desalocar apenas se alocado
                            if (allocated(diag_data_int)) deallocate(diag_data_int)

                            case ("diag_neutral")

                                call parse_workflow_diagnostic(val, identifier, diag_command, diag_data, diagnostic_ierr)

                                    if (diagnostic_ierr == 0) then
                                        ! Convert string array to integer array 
                                        call str_array_to_int(diag_data, diag_data_int, ierr)
                                        if (ierr == 0) then
                                            !!!!!!!!!!!!!! MUDAR ISTO
                                            call steering_neutral_diag(sim, trim(identifier), "Neutral", trim(diag_command), diag_data_int, ierr)
                                        end if
                                    end if

                            case ("diag_species")
                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                if (mpi_node() == 0) &
                                    print *, "DEBUG - ", trim(diagnostic_name), " command found"

                            case ("diag_particles")
                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                    
                            case default
                                if (mpi_node() == 0) &
                                    print *, "Unknown command: ", trim(diagnostic_name)
                            ! THIS PART IS CURRENTLY USELESS
                        end select
                    end if
            end select

            ! SEE WHETHER TO BCAST IERR FOR SYNCRONIZARION ??????????????????????????????????????

        end do

        ! Cleanup -> Be careful to deallocate only after assigning value 
        if (allocated(val)) deallocate(val)
        if (allocated(keys)) deallocate(keys)
        if (allocated(diag_command)) deallocate(diag_command)
        if (allocated(diag_data)) deallocate(diag_data)
        if (allocated(identifier)) deallocate(identifier)
        if (allocated(diagnostic_name)) deallocate(diagnostic_name)
        if (allocated(diag_data_int)) deallocate(diag_data_int)
    end subroutine check_and_execute

    ! <-------------------------------->! DUPLICATED ROUTINE FROM MAIN MODULE

    subroutine write_restart( sim ) !^!

        implicit none

        class ( t_simulation ), intent(inout) :: sim

        type( t_restart_handle )    ::  restart_handle

        call begin_event(restart_write_ev)

        if ( mpi_node() == 0 ) then
            print *, ''
            print *, ' Writing checkpoint information for timestep n =', n(sim%tstep)
            print *, ''
        endif

        #ifdef __RST_SION__

        ! Measure expected file size, only required by the sion library
        restart_handle%if_acc_size = .true.
        restart_handle%data_size = 0
        call sim%write_checkpoint( restart_handle )
        restart_handle%if_acc_size = .false.

        #endif

        ! Open checkpoint files
        call restart_write_open( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                                n(sim%tstep), ndump(sim%tstep), &
                                file_id_rst, restart_handle )

        ! Write checkpoint data
        call sim%write_checkpoint( restart_handle )

        ! Close checkpoint files
        call restart_write_close( sim%restart, comm(sim%no_co), sim%no_co%ngp_id(), &
                                    n(sim%tstep), restart_handle )

        call end_event(restart_write_ev)

        end subroutine write_restart
        
        !<-------------------------------->! 

        subroutine set_max_time(sim, val, ierr)
        implicit none

        class(t_simulation), intent(inout) :: sim
        character(len=*), intent(in) :: val
        integer, intent(out) :: ierr

        real(p_double) :: updated_tmax
        integer :: conv_ierr

        ierr = 0

        updated_tmax = strtodouble(val, conv_ierr)

        if (updated_tmax == 0.0d0 .or. updated_tmax < 0.0d0) then
            ierr = 1
            if (mpi_node() == 0) then
                print*, "DEBUG - Tmax setting skipped", trim(val)
            end if
            return
        end if

        if (conv_ierr == 0) then
            ! Use a função pública para obter o tempo atual
            if (updated_tmax > t(sim%time)) then
                call set_tmax(sim%time, updated_tmax)  ! Você precisa implementar essa rotina
                if (mpi_node() == 0) then
                    print*, "DEBUG - Updated tmax to ", tmax(sim%time)
                end if
            else if (mpi_node() == 0) then
                print*, "DEBUG - tmax not updated, new value ", updated_tmax, &
                        " is not larger than current time ", t(sim%time)
            end if
        else
            ierr = 1
            if (mpi_node() == 0) then
                print*, "DEBUG - Error converting tmax value: ", trim(val)
            end if
        end if

    end subroutine set_max_time

    !<------------------------------------------------------->!

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
            int_array(i) = strtoint(str_array(i), conv_ierr)
            if (conv_ierr /= 0) then
                ierr = conv_ierr
                return
            end if
        end do

        ierr = 0
    end subroutine str_array_to_int

    !<------------------------------------------------------->! 
    !                 DIAGNOSTICS HANDLER
    ! All functions to handle diagnostics will be here
    !               May God have mercy on us
    !<------------------------------------------------------->! 

    !<----------------------EMF------------------------------>! 

    !-----------------------------------------------------------------------------------------
    !       Change emf parameters through steering file
    !-----------------------------------------------------------------------------------------
    subroutine steering_emf_diag(sim, report_spec, command_name, new_value, ierr)      
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "e3", "e2, line, x1, 64, 96")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_diag_emf), pointer :: diag_emf
        type(t_vdf_report), pointer :: report
        type(t_vdf_report_item), pointer :: item
        character(len=:), allocatable :: quant, details
        integer :: pos, item_type, direction, id
        logical :: found, is_tavg
        integer, dimension(2) :: gipos
        integer ::  n_dimensions 
        
        ! Inicializa ponteiros e variáveis
        ierr = 0
        diag_emf => sim%emf%diag
        report => diag_emf%reports

        n_dimensions = sim%g_space%x_dim ! Sets number of dimensions for spacial averages
        
        ! Passo 1: Parse da especificação do relatório
        pos = index(report_spec, ',')
        if (pos > 0) then
            quant = report_spec(1:pos-1)        ! Assign substring first
            quant = trim(adjustl(quant))        ! Then trim/adjust
            details = report_spec(pos+1:)       ! Assign substring first
            details = trim(adjustl(details))    ! Then trim/adjust
        else
            quant = trim(adjustl(report_spec))
            details = ""
        end if
        
        ! Passo 2: Encontrar o relatório principal (quantidade)
        found = .false.
        do while (associated(report) .and. .not. found)
            if (root(sim%no_co)) then
                print *, "DEBUG - Found: ", report%name
            endif
            if (trim(report%name) == quant) then
                found = .true.
                exit
            end if
            report => report%next
        end do
        
        if (.not. found) then
            ierr = -1  ! Relatório não encontrado
            return
        end if
        
        ! Passo 3: Parse dos detalhes (se existirem)
        item_type = -1
        direction = -1
        gipos = -1
        is_tavg = .false.
        id = -1
        
        if (len_trim(details) > 0) then
            ! Parse dos componentes
            if (index(details, 'tavg') > 0) is_tavg = .true.
            if (index(details, 'savg') > 0) item_type = p_savg
            if (index(details, 'senv') > 0) item_type = p_senv
            if (index(details, 'line') > 0) item_type = p_line
            if (index(details, 'slice') > 0) item_type = p_slice
            
            ! Parse de direção e posições
            if (index(details, 'x1') > 0) direction = 1
            if (index(details, 'x2') > 0) direction = 2
            if (index(details, 'x3') > 0) direction = 3
            
            ! Extrair posições (ex: "64, 96")
            ! (Implementação simplificada - em produção usar split mais robusto)
            if (item_type == p_line .or. item_type == p_slice) then
                pos = index(details, ',', back=.true.)
                if (pos > 0) then
                    read(details(pos+1:), *) gipos(1)
                    if (item_type == p_line) then
                        pos = index(details(1:pos-1), ',', back=.true.)
                        if (pos > 0) read(details(pos+1:), *) gipos(2)
                    end if
                end if
            end if
        end if
        
       ! Passo 4: Encontrar item específico
        if (item_type > 0) then
            found = .false.
            item => report%list
            do while (associated(item) .and. .not. found)
                if (item%type == item_type) then
                    if (item_type == p_line .or. item_type == p_slice) then
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%direction == direction .and. &
                            all(item%gipos == gipos) .and. &
                            item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    else
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    end if
                end if
                item => item%next
            end do
        end if
        
        ! Passo 5: Aplicar modificações conforme o comando
        select case (trim(command_name))
            ! Comandos globais (afetam todo o relatório)
            case ("ndump_fac")
                report%ndump(p_full) = new_value(1)
            case ("ndump_fac_ave")
                report%ndump(p_savg) = new_value(1)
                report%ndump(p_senv) = new_value(1)
            case ("ndump_fac_lineout")
                report%ndump(p_line) = new_value(1)
                report%ndump(p_slice) = new_value(1)
            
            ! Comandos para itens específicos
            case ("n_ave")
                if (size(new_value) >= n_dimensions) then
                    report%n_ave(1:n_dimensions) = new_value(1:n_dimensions)
                else
                    ierr = -3  ! Tamanho inválido
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid size for n_ave in steering_emf_diag"
                    endif
                end if
            case ("n_tavg")
                report%n_tavg = new_value(1)
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &       ! Dimensão espacial
                        1, &                    ! f_dim
                        sim%grid%g_nx, &        ! nx
                        sim%emf%e%gc_num(), &   ! gc_num (CORRECTED)
                        sim%emf%e%dx(), &       ! dx
                        .true. &                ! zero
                    )
                endif
            case ("gipos")
                if (associated(item) .and. size(new_value) >= size(item%gipos)) then
                    item%gipos = new_value(1:size(item%gipos))
                else
                    ierr = -4  ! Item inválido ou tamanho incorreto
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid item or size mismatch for gipos in steering_emf_diag"
                    endif
                end if
            
            ! Comandos para parâmetros globais do diag_emf
            case ("ndump_fac_ene_int")
                if (diag_emf%ndump_fac_ene_int > 0) then
                    ! This quantity can't be initialized during the simulation
                    diag_emf%ndump_fac_ene_int = new_value(1)
                else
                    if (mpi_node() == 0) then
                        print *, "DEBUG - ndump_fac_ene_int not set, skipping"
                    endif
                endif
                
            case ("ndump_fac_charge_cons")
                ! WARNING: This parameter currently doesn't work 
                ! Check if flag is set 
                if ( sim % emf % if_charge_cons( sim%tstep ) ) then
                     diag_emf%ndump_fac_charge_cons = new_value(1)
                else
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Charge conservation diagnostic is temporarily disabled"
                    endif
                    
                endif

            case ("prec")
                diag_emf%prec = new_value(1)
                report%prec = new_value(1)  ! Atualiza precisão do relatório
            
            case default
                ierr = -5  ! Comando desconhecido
        end select
        
    end subroutine steering_emf_diag

    !-----------------------------------------------------------------------------------------
    !       Add emf reports
    !-----------------------------------------------------------------------------------------
    subroutine add_emf_report(sim, input_string)
        character(len=*), intent(in) :: input_string
        class(t_simulation), intent(inout) :: sim
        integer :: ierr
        type(t_diag_emf), pointer :: diag_emf
        type(t_vdf_report), pointer :: report

        ! Get the emf and diag_emf structures from the simulation
        diag_emf => sim%emf%diag

        ! Use the existing add_report routine to add the new report
        call add_report( diag_emf%reports, trim(input_string), diag_emf%report_quants, &
                        sim%g_space%x_dim, ierr )

        ! Initialize the report if it was added successfully
        if (ierr == 0) then
            ! WARNING THIS MAY CAUSE PROBLEMS AND IS EXPERIMENTAL
            call sim % emf % diag % init( sim % emf %ext_fld == p_extfld_none, &
                sim % emf %part_fld_alloc, interpolation( sim%part ))

            ! Inicializa tavg_data se necessário
            report => diag_emf%reports
            do while (associated(report))
                if (trim(report%name) == trim(input_string)) then
                    if (report%n_tavg > 0) then
                        ! Inicializa tavg_data com parâmetros da grade
                        call report%tavg_data%new( &
                            sim%grid%x_dim, &       ! Dimensão espacial
                            1, &                     ! f_dim (campo escalar)
                            sim%grid%g_nx, &         ! Tamanho da grade global
                            sim%emf%e%gc_num(), &    ! Células guarda
                            sim%emf%e%dx(), &        ! Tamanho do célula
                            .true. &                 ! Zero o campo
                        )
                    endif
                    exit
                endif
                report => report%next
            enddo
        end if

        if (ierr /= 0) then
            if (mpi_node() == 0) then
                print *, "Error adding EMF report: ", trim(input_string)
            endif
        else
            if (mpi_node() == 0) then
                print *, "Added new EMF report: ", trim(input_string)
            endif
        endif
    end subroutine add_emf_report

    !<----------------------CURRENT------------------------------>! 

    !-----------------------------------------------------------------------------------------
    !       Change current parameters through steering file
    !-----------------------------------------------------------------------------------------
    subroutine steering_current_diag(sim, report_spec, command_name, new_value, ierr)      
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "j1", "j2", "j3")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_current_diag), pointer :: diag_current
        type(t_vdf_report), pointer :: report
        type(t_vdf_report_item), pointer :: item
        character(len=:), allocatable :: quant, details
        integer :: pos, item_type, direction, id
        logical :: found, is_tavg
        integer, dimension(2) :: gipos
        integer ::  n_dimensions
        
        ! Inicializa ponteiros e variáveis
        ierr = 0
        diag_current => sim%jay%diag
        report => diag_current%reports

        n_dimensions = sim%g_space%x_dim ! Sets number of dimensions for spacial averages
        
        ! Passo 1: Parse da especificação do relatório
        pos = index(report_spec, ',')
        if (pos > 0) then
            quant = report_spec(1:pos-1)        ! Assign substring first
            quant = trim(adjustl(quant))        ! Then trim/adjust
            details = report_spec(pos+1:)       ! Assign substring first
            details = trim(adjustl(details))    ! Then trim/adjust
        else
            quant = trim(adjustl(report_spec))
            details = ""
        end if
        
        ! Passo 2: Encontrar o relatório principal (quantidade)
        found = .false.
        do while (associated(report) .and. .not. found)
            if (trim(report%name) == quant) then
                found = .true.
                exit
            end if
            report => report%next
        end do
        
        if (.not. found) then
            ierr = -1  ! Relatório não encontrado
            return
        end if
        
        ! Passo 3: Parse dos detalhes (se existirem)
        item_type = -1
        direction = -1
        gipos = -1
        is_tavg = .false.
        id = -1
        
        if (len_trim(details) > 0) then
            ! Parse dos componentes
            if (index(details, 'tavg') > 0) is_tavg = .true.
            if (index(details, 'savg') > 0) item_type = p_savg
            if (index(details, 'senv') > 0) item_type = p_senv
            if (index(details, 'line') > 0) item_type = p_line
            if (index(details, 'slice') > 0) item_type = p_slice
            
            ! Parse de direção e posições
            if (index(details, 'x1') > 0) direction = 1
            if (index(details, 'x2') > 0) direction = 2
            if (index(details, 'x3') > 0) direction = 3
            
            ! Extrair posições (ex: "64, 96")
            ! (Implementação simplificada - em produção usar split mais robusto)
            if (item_type == p_line .or. item_type == p_slice) then
                pos = index(details, ',', back=.true.)
                if (pos > 0) then
                    read(details(pos+1:), *) gipos(1)
                    if (item_type == p_line) then
                        pos = index(details(1:pos-1), ',', back=.true.)
                        if (pos > 0) read(details(pos+1:), *) gipos(2)
                    end if
                end if
            end if
        end if
        
       ! Passo 4: Encontrar item específico
        if (item_type > 0) then
            found = .false.
            item => report%list
            do while (associated(item) .and. .not. found)
                if (item%type == item_type) then
                    if (item_type == p_line .or. item_type == p_slice) then
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%direction == direction .and. &
                            all(item%gipos == gipos) .and. &
                            item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    else
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    end if
                end if
                item => item%next
            end do
        end if
        
        ! Passo 5: Aplicar modificações conforme o comando
        select case (trim(command_name))
            ! Comandos globais (afetam todo o relatório)
            case ("ndump_fac")
                report%ndump(p_full) = new_value(1)
            case ("ndump_fac_ave")
                report%ndump(p_savg) = new_value(1)
                report%ndump(p_senv) = new_value(1)
            case ("ndump_fac_lineout")
                report%ndump(p_line) = new_value(1)
                report%ndump(p_slice) = new_value(1)
            
            ! Comandos para itens específicos
            case ("n_ave")
                if (size(new_value) >= n_dimensions) then
                    report%n_ave(1:n_dimensions) = new_value(1:n_dimensions)
                else
                    ierr = -3  ! Tamanho inválido
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid size for n_ave in steering_current_diag"
                    endif 
                end if
            case ("n_tavg")
                report%n_tavg = new_value(1)
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &      ! x_dim
                        1, &                   ! f_dim
                        sim%grid%g_nx, &       ! nx
                        sim%emf%e%gc_num(), &  ! gc_num (CORRECTED)
                        sim%emf%e%dx(), &      ! dx -> will be using emf for easy data access
                        .true. &               ! zero
                    )
                endif

                ! Re-inicializa tavg_data se necessário
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &   ! x_dim
                        1, &                ! f_dim
                        sim%grid%g_nx, &    ! nx
                        sim%emf%e%gc_num(), &  ! gc_num
                        sim%emf%e%dx(), &      ! dx
                        .true. &            ! zero
                    )
                endif
            case ("gipos")
                if (associated(item) .and. size(new_value) >= size(item%gipos)) then
                    item%gipos = new_value(1:size(item%gipos))
                else
                    ierr = -4  ! Item inválido ou tamanho incorreto
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid item or size mismatch for gipos in steering_current_diag"
                    endif 
                end if
            
            case ("prec")
                report%prec = new_value(1)
            
            case default
                ierr = -5  ! Comando desconhecido
        end select
        
    end subroutine steering_current_diag

    !-----------------------------------------------------------------------------------------
    !       Add current reports
    !-----------------------------------------------------------------------------------------
    subroutine add_current_report(sim, input_string)
        character(len=*), intent(in) :: input_string
        class(t_simulation), intent(inout) :: sim
        integer :: ierr
        type(t_current_diag), pointer :: diag_current
        type(t_vdf_report), pointer :: report

        ! Get the emf and diag_emf structures from the simulation
        diag_current=> sim%jay%diag

        ! Use the existing add_report routine to add the new report
        call add_report( diag_current%reports, trim(input_string), diag_current%report_quants, &
                        sim%g_space%x_dim, ierr )

        ! Initialize the report if it was added successfully
        if (ierr == 0) then
            ! WARNING THIS MAY CAUSE PROBLEMS AND IS EXPERIMENTAL
            call sim % jay % diag % init(interpolation( sim%part ))

            report => diag_current%reports
            do while (associated(report))
                if (trim(report%name) == trim(input_string)) then
                    if (report%n_tavg > 0) then
                        ! Inicializa tavg_data com parâmetros da grade
                        call report%tavg_data%new( &
                            sim%grid%x_dim, &       ! Dimensão espacial
                            1, &                     ! f_dim (campo escalar)
                            sim%grid%g_nx, &         ! Tamanho da grade global
                            sim%emf%e%gc_num(), &     ! Células guarda
                            sim%emf%e%dx(), &           ! Tamanho do célula
                            .true. &                 ! Zero o campo
                        )
                    endif
                    exit
                endif
                report => report%next
            enddo

        end if

        if (ierr /= 0) then
            if (mpi_node() == 0) then
                print *, "Error adding CURRENT report: ", trim(input_string)
            endif
        else
            if (mpi_node() == 0) then
                print *, "Added new CURRENT report: ", trim(input_string)
            endif
        endif

    end subroutine add_current_report

    !<------------------------------------------------------->! 
    !                 PARTICLE DIAGNOSTICS HANDLER
    !        Both charged and neutrals are defined here          
    !<------------------------------------------------------->! 

    !<----------------------NEUTRALS------------------------------->! 

    !-----------------------------------------------------------------------------------------
    !       Change neutral parameters through steering file
    !-----------------------------------------------------------------------------------------
    subroutine steering_neutral_diag(sim, report_spec, particle_name, command_name, new_value, ierr)      
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim
        character(*), intent(in) :: particle_name      ! Nome da partíula ex: sodium, etc.
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "e3", "e2, line, x1, 64, 96")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_neutral), pointer :: diag_neutral
        type(t_vdf_report), pointer :: report
        type(t_vdf_report_item), pointer :: item
        character(len=:), allocatable :: quant, details
        integer :: pos, item_type, direction, id
        logical :: found, is_tavg
        integer, dimension(2) :: gipos
        integer ::  n_dimensions 
        logical :: found_name
        
        ! Inicializa ponteiros e variáveis
        ierr = 0
        found_name = .false.

        do id = 1, size(sim%part%neutral)
            if (trim(sim%part%neutral(id)%name) == trim(particle_name)) then
                diag_neutral => sim%part%neutral(id)
                found_name = .true.
                exit
            end if
        end do

        if (.not. found_name) then
            ierr = 1
            if (mpi_node() == 0) then
                print *, "DEBUG - Neutral particle id not found: ", trim(particle_name)
            end if
            return
        end if

        report => diag_neutral%diag%reports

        n_dimensions = sim%g_space%x_dim ! Sets number of dimensions for spacial averages
        
        ! Passo 1: Parse da especificação do relatório
        pos = index(report_spec, ',')
        if (pos > 0) then
            quant = report_spec(1:pos-1)        ! Assign substring first
            quant = trim(adjustl(quant))        ! Then trim/adjust
            details = report_spec(pos+1:)       ! Assign substring first
            details = trim(adjustl(details))    ! Then trim/adjust
        else
            quant = trim(adjustl(report_spec))
            details = ""
        end if
        
        ! Passo 2: Encontrar o relatório principal (quantidade)
        found = .false.
        do while (associated(report) .and. .not. found)
            if (root(sim%no_co)) then
                print *, "DEBUG - Found: ", report%name
            endif
            if (trim(report%name) == quant) then
                found = .true.
                exit
            end if
            report => report%next
        end do
        
        if (.not. found) then
            ierr = -1  ! Relatório não encontrado
            return
        end if
        
        ! Passo 3: Parse dos detalhes (se existirem)
        item_type = -1
        direction = -1
        gipos = -1
        is_tavg = .false.
        id = -1
        
        if (len_trim(details) > 0) then
            ! Parse dos componentes
            if (index(details, 'tavg') > 0) is_tavg = .true.
            if (index(details, 'savg') > 0) item_type = p_savg
            if (index(details, 'senv') > 0) item_type = p_senv
            if (index(details, 'line') > 0) item_type = p_line
            if (index(details, 'slice') > 0) item_type = p_slice
            
            ! Parse de direção e posições
            if (index(details, 'x1') > 0) direction = 1
            if (index(details, 'x2') > 0) direction = 2
            if (index(details, 'x3') > 0) direction = 3
            
            ! Extrair posições (ex: "64, 96")
            ! (Implementação simplificada - em produção usar split mais robusto)
            if (item_type == p_line .or. item_type == p_slice) then
                pos = index(details, ',', back=.true.)
                if (pos > 0) then
                    read(details(pos+1:), *) gipos(1)
                    if (item_type == p_line) then
                        pos = index(details(1:pos-1), ',', back=.true.)
                        if (pos > 0) read(details(pos+1:), *) gipos(2)
                    end if
                end if
            end if
        end if
        
       ! Passo 4: Encontrar item específico
        if (item_type > 0) then
            found = .false.
            item => report%list
            do while (associated(item) .and. .not. found)
                if (item%type == item_type) then
                    if (item_type == p_line .or. item_type == p_slice) then
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%direction == direction .and. &
                            all(item%gipos == gipos) .and. &
                            item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    else
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    end if
                end if
                item => item%next
            end do
        end if
        
        ! Passo 5: Aplicar modificações conforme o comando
        select case (trim(command_name))
            ! Comandos globais (afetam todo o relatório)
            case ("ndump_fac")
                report%ndump(p_full) = new_value(1)
            case ("ndump_fac_ave")
                report%ndump(p_savg) = new_value(1)
                report%ndump(p_senv) = new_value(1)
            case ("ndump_fac_lineout")
                report%ndump(p_line) = new_value(1)
                report%ndump(p_slice) = new_value(1)
            
            ! Comandos para itens específicos
            case ("n_ave")
                if (size(new_value) >= n_dimensions) then
                    report%n_ave(1:n_dimensions) = new_value(1:n_dimensions)
                else
                    ierr = -3  ! Tamanho inválido
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid size for n_ave in steering_emf_diag"
                    endif
                end if
            case ("n_tavg")
                report%n_tavg = new_value(1)
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &       ! Dimensão espacial
                        1, &                    ! f_dim
                        sim%grid%g_nx, &        ! nx
                        sim%emf%e%gc_num(), &   ! gc_num (CORRECTED)
                        sim%emf%e%dx(), &       ! dx
                        .true. &                ! zero
                    )
                endif

            case ("gipos")
                if (associated(item) .and. size(new_value) >= size(item%gipos)) then
                    item%gipos = new_value(1:size(item%gipos))
                else
                    ierr = -4  ! Item inválido ou tamanho incorreto
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid item or size mismatch for gipos in steering_emf_diag"
                    endif
                end if                

            case ("prec")
                report%prec = new_value(1)  ! Atualiza precisão do relatório
            
            case default
                ierr = -5  ! Comando desconhecido
        end select
        
    end subroutine steering_neutral_diag

    !-----------------------------------------------------------------------------------------
    !       Add neutral reports
    !-----------------------------------------------------------------------------------------
    subroutine add_neutral_report()
        ! For now does nothing 
    end subroutine add_neutral_report


    !<----------------------SPECIES------------------------------>! 

     !-----------------------------------------------------------------------------------------
    !       Change neutral parameters through steering file
    !-----------------------------------------------------------------------------------------
    subroutine steering_species_diag(sim, report_spec, particle_name, command_name, new_value, ierr)      
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim
        character(*), intent(in) :: particle_name      ! Nome da partíula ex: sodium, etc.
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "e3", "e2, line, x1, 64, 96")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_species), pointer :: diag_species
        type(t_vdf_report), pointer :: report
        type(t_vdf_report_item), pointer :: item
        character(len=:), allocatable :: quant, details
        integer :: pos, item_type, direction, id
        logical :: found, is_tavg
        integer, dimension(2) :: gipos
        integer ::  n_dimensions 
        logical :: found_name
        
        ! Inicializa ponteiros e variáveis
        ierr = 0
        found_name = .false.

        diag_species => sim%part%species  ! Start with first species in list

        do while (associated(diag_species))
            if (trim(diag_species%name) == trim(particle_name)) then
                found_name = .true.
                exit
            end if
            diag_species => diag_species%next  ! Move to next species in list
        end do

        if (.not. found_name) then
            ierr = 1
            if (mpi_node() == 0) then
                print *, "DEBUG - Species particle id not found: ", trim(particle_name)
            end if
            return
        end if

        report => diag_species%diag%reports

        n_dimensions = sim%g_space%x_dim ! Sets number of dimensions for spacial averages
        
        ! Passo 1: Parse da especificação do relatório
        pos = index(report_spec, ',')
        if (pos > 0) then
            quant = report_spec(1:pos-1)        ! Assign substring first
            quant = trim(adjustl(quant))        ! Then trim/adjust
            details = report_spec(pos+1:)       ! Assign substring first
            details = trim(adjustl(details))    ! Then trim/adjust
        else
            quant = trim(adjustl(report_spec))
            details = ""
        end if
        
        ! Passo 2: Encontrar o relatório principal (quantidade)
        found = .false.
        do while (associated(report) .and. .not. found)
            if (root(sim%no_co)) then
                print *, "DEBUG - Found: ", report%name
            endif
            if (trim(report%name) == quant) then
                found = .true.
                exit
            end if
            report => report%next
        end do
        
        if (.not. found) then
            ierr = -1  ! Relatório não encontrado
            return
        end if
        
        ! Passo 3: Parse dos detalhes (se existirem)
        item_type = -1
        direction = -1
        gipos = -1
        is_tavg = .false.
        id = -1
        
        if (len_trim(details) > 0) then
            ! Parse dos componentes
            if (index(details, 'tavg') > 0) is_tavg = .true.
            if (index(details, 'savg') > 0) item_type = p_savg
            if (index(details, 'senv') > 0) item_type = p_senv
            if (index(details, 'line') > 0) item_type = p_line
            if (index(details, 'slice') > 0) item_type = p_slice
            
            ! Parse de direção e posições
            if (index(details, 'x1') > 0) direction = 1
            if (index(details, 'x2') > 0) direction = 2
            if (index(details, 'x3') > 0) direction = 3
            
            ! Extrair posições (ex: "64, 96")
            ! (Implementação simplificada - em produção usar split mais robusto)
            if (item_type == p_line .or. item_type == p_slice) then
                pos = index(details, ',', back=.true.)
                if (pos > 0) then
                    read(details(pos+1:), *) gipos(1)
                    if (item_type == p_line) then
                        pos = index(details(1:pos-1), ',', back=.true.)
                        if (pos > 0) read(details(pos+1:), *) gipos(2)
                    end if
                end if
            end if
        end if
        
       ! Passo 4: Encontrar item específico
        if (item_type > 0) then
            found = .false.
            item => report%list
            do while (associated(item) .and. .not. found)
                if (item%type == item_type) then
                    if (item_type == p_line .or. item_type == p_slice) then
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%direction == direction .and. &
                            all(item%gipos == gipos) .and. &
                            item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    else
                        ! Corrigido: usar .eqv. para lógicos
                        if (item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    end if
                end if
                item => item%next
            end do
        end if
        
        ! Passo 5: Aplicar modificações conforme o comando
        select case (trim(command_name))
            ! Comandos globais (afetam todo o relatório)
            case ("ndump_fac")
                report%ndump(p_full) = new_value(1)
            case ("ndump_fac_ave")
                report%ndump(p_savg) = new_value(1)
                report%ndump(p_senv) = new_value(1)
            case ("ndump_fac_lineout")
                report%ndump(p_line) = new_value(1)
                report%ndump(p_slice) = new_value(1)
            
            ! Outras frequências de dump

            case ("ndump_fac_ene")
                diag_species%diag%ndump_fac_ene  = new_value(1)
            case ("ndump_fac_heatflux")
                diag_species%diag%ndump_fac_heatflux = new_value(1)
            case ("ndump_fac_temp")
                diag_species%diag%ndump_fac_temp = new_value(1)
            case ("ndump_fac_raw")  
                diag_species%diag%ndump_fac_raw = new_value(1)

            ! Comandos para itens específicos

            case ("n_ave")
                if (size(new_value) >= n_dimensions) then
                    report%n_ave(1:n_dimensions) = new_value(1:n_dimensions)
                else
                    ierr = -3  ! Tamanho inválido
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid size for n_ave in steering_emf_diag"
                    endif
                end if
            case ("n_tavg")
                report%n_tavg = new_value(1)
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &       ! Dimensão espacial
                        1, &                    ! f_dim
                        sim%grid%g_nx, &        ! nx
                        sim%emf%e%gc_num(), &   ! gc_num (CORRECTED)
                        sim%emf%e%dx(), &       ! dx
                        .true. &                ! zero
                    )
                endif

            case ("gipos")
                if (associated(item) .and. size(new_value) >= size(item%gipos)) then
                    item%gipos = new_value(1:size(item%gipos))
                else
                    ierr = -4  ! Item inválido ou tamanho incorreto
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid item or size mismatch for gipos in steering_emf_diag"
                    endif
                end if                

            case ("prec")
                report%prec = new_value(1)  ! Atualiza precisão do relatório

            ! Add here code for other commands 

            case ("raw_gamma_limit")
                diag_species%diag%raw_gamma_limit = new_value(1)
            case ("raw_fraction")
                diag_species%diag%raw_fraction = new_value(1)
            case ("ndump_fac_tracks")
                diag_species%diag%ndump_fac_tracks = new_value(1)
            case ("n_start_tracks")
                diag_species%diag%n_start_tracks = new_value(1)

            case default
                ierr = -5  ! Comando desconhecido
        end select
        
    end subroutine steering_species_diag

    !-----------------------------------------------------------------------------------------
    !       Add species reports
    !-----------------------------------------------------------------------------------------
    subroutine add_species_report()
        ! For now does nothing 
    end subroutine add_species_report

end module m_workflow
