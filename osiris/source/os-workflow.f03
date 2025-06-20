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
    use m_species_diagnostics
    use m_particles
    use m_species_define
    use m_restart, only: t_restart_handle

    #include "os-config.h"
    #include "os-preprocess.fpp"

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
        character(len=:), allocatable :: name
        character(len=:), allocatable :: diag_command
        character(len=:), allocatable :: diag_data(:)
        integer, allocatable :: diag_data_int(:)
        logical :: add_rep

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
            add_rep = .false.

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

                case ("omega_p0")
                    ! Change the plasma frequency
                    call set_omega_p0(sim, get_value(trim(keys(i))), ierr)

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
                                call parse_workflow_diagnostic(val, name, identifier, diag_command, diag_data, add_rep, diagnostic_ierr)

                                if (diagnostic_ierr == 0) then
                                    ! Convert string array to integer array 
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                    if (ierr == 0) then
                                        call add_current_report(sim, trim(identifier))
                                        call steering_current_diag(sim, trim(identifier), trim(diag_command), diag_data_int, ierr)
                                    end if
                                end if

                            case ("diag_emf")

                                call parse_workflow_diagnostic(val, name, identifier, diag_command, diag_data, add_rep, diagnostic_ierr)
                                
                                if (diagnostic_ierr == 0) then                                  
                                    ! Convert string array to integer array 
                                    if (allocated(diag_data_int)) deallocate(diag_data_int)
                                    call str_array_to_int(diag_data, diag_data_int, ierr)

                                    if (ierr == 0) then
                                        call add_emf_report(sim, trim(identifier))
                                        call steering_emf_diag(sim, trim(identifier), trim(diag_command), diag_data_int, ierr)
                                    end if
                                end if
            

                            case ("diag_neutral")

                                call parse_workflow_diagnostic(val, name, identifier, diag_command, diag_data, add_rep, diagnostic_ierr)
                                    if (diagnostic_ierr == 0) then
                                        ! Convert string array to integer array 
                                    if (allocated(diag_data_int)) deallocate(diag_data_int)
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                        if (ierr == 0) then
                                            !!!!!!!!!!!!!! MUDAR ISTO
                                            call add_neutral_report(sim, trim(identifier), name)
                                            call steering_neutral_diag(sim, trim(identifier), name, trim(diag_command), diag_data_int, ierr)
                                        end if
                                    end if

                            case ("diag_species")
                                call parse_workflow_diagnostic(val, name, identifier, diag_command, diag_data, add_rep, diagnostic_ierr)

                                    if (diagnostic_ierr == 0) then
                                        ! Convert string array to integer array 
                                    if (allocated(diag_data_int)) deallocate(diag_data_int)
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                        if (ierr == 0) then
                                            ! ------------------------------------------------------ !
                                            ! WARNING: This is a temporary solution
                                            ! I need to find a way to differentiate 
                                            ! between species diagnostics - Contact Ricardo bt this
                                            ! ------------------------------------------------------ !
                                            call add_species_report(sim, trim(identifier), name, add_rep)
                                            call steering_species_diag(sim, trim(identifier), name, trim(diag_command), diag_data_int, add_rep, ierr)
                                        end if
                                    end if

                            case ("diag_particles")
                                call parse_workflow_diagnostic(val, name, identifier, diag_command, diag_data, add_rep, diagnostic_ierr)

                                    if (diagnostic_ierr == 0) then
                                        ! Convert string array to integer array 
                                    if (allocated(diag_data_int)) deallocate(diag_data_int)
                                    call str_array_to_int(diag_data, diag_data_int, ierr)
                                        if (ierr == 0) then
                                            !DIABLED DUE TO STRUCTURE PROBLEM
                                            print *, "DEBUG - Particles diagnostics are disabled for now"
                                            !call add_particles_report(sim, trim(identifier))
                                            !call steering_particles_diag(sim, trim(identifier), trim(diag_command), diag_data_int, ierr)
                                        end if
                                    end if
                                    
                            case default
                                if (mpi_node() == 0) &
                                    print *, "Unknown command: ", trim(diagnostic_name)
                        end select
                    end if
            end select

            ! ------------------------------ ! 
            ! should I add a barrier here?
            ! ------------------------------ ! 

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
        integer :: pos, item_type, direction
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
        call parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        
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
            ! WARNING THIS MAY CAUSE PROBLEMS AND IS EXPERIMENTAL - Reinitialize the emf diag structure
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
        integer :: pos, item_type, direction
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
        call parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        
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
            if (allocated(quant)) deallocate(quant)
            if (allocated(details)) deallocate(details)
            return
        end if
        
        ! Passo 3: Parse dos detalhes (se existirem)
        call parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        
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

        if (allocated(quant)) deallocate(quant)
        if (allocated(details)) deallocate(details)
        
    end subroutine steering_neutral_diag

    !-----------------------------------------------------------------------------------------
    !       Add neutral reports
    !-----------------------------------------------------------------------------------------
    subroutine add_neutral_report(sim, input_string, particle_name)
        character(len=*), intent(in) :: input_string
        class(t_simulation), intent(inout) :: sim
        integer :: ierr, id
        character(len=*), intent(in) :: particle_name ! Nome da partícula neutra (ex: sodium, etc.)
        type(t_diag_neutral), pointer :: diag_neutral
        type(t_vdf_report), pointer :: report
        logical :: found_name

        found_name = .false.

        ! Find which neutral particle to add the report for
        do id = 1, size(sim%part%neutral)
            print *, "DEBUG - ", particle_name, " Name: ", trim(sim%part%neutral(id)%name)
            
            if (trim(sim%part%neutral(id)%name) == trim(particle_name)) then
                diag_neutral => sim%part%neutral(id)%diag ! define diag neutral 
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

        ! Use the existing add_report routine to add the new report
        call add_report( diag_neutral%reports, trim(input_string), diag_neutral%report_quants, &
                        sim%g_space%x_dim, ierr )

        ! Initialize the report if it was added successfully
        if (ierr == 0) then
            ! WARNING THIS MAY CAUSE PROBLEMS AND IS EXPERIMENTAL - Reinitialize the init diag structure
            call setup(sim % part % neutral(id) % diag, particle_name, interpolation(sim%part))
            !call sim % part % neutral(id) % diag % setup( particle_name, interpolation( sim%part ))

            ! Inicializa tavg_data se necessário
            report => diag_neutral%reports
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
                print *, "Error adding NEUTRAL report: ", trim(input_string)
            endif
        else
            if (mpi_node() == 0) then
                print *, "Added new NEUTRAL report: ", trim(input_string)
            endif
        endif
        
    end subroutine add_neutral_report

    !<----------------------SPECIES------------------------------>! 

    !-----------------------------------------------------------------------------------------
    !       Change neutral parameters through steering file
    !-----------------------------------------------------------------------------------------
    subroutine steering_species_diag(sim, report_spec, particle_name, command_name, new_value, averaged_quant, ierr)  

        use m_species_diagnostics, only: p_report_quants, p_rep_udist ! get report quantifier    
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim
        character(*), intent(in) :: particle_name      ! Nome da partícula ex: sodium, etc.
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "e3", "e2, line, x1, 64, 96")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        logical, intent(in) :: averaged_quant           ! Indica se é uma quantidade média (para n_ave)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_species), pointer :: diag_species
        type(t_vdf_report), pointer :: report
        type(t_vdf_report_item), pointer :: item
        character(len=:), allocatable :: quant, details
        integer :: pos, item_type, direction, id
        logical :: found, is_tavg
        integer, dimension(2) :: gipos
        integer :: n_dimensions 
        logical :: found_name
        
        ! Inicializa ponteiros e variáveis
        ierr = 0
        found_name = .false.

        diag_species => sim%part%species  ! Start with first species in list

        ! Find the species with the given name
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

        n_dimensions = sim%g_space%x_dim ! Sets number of dimensions for spacial averages
        
        ! Step 1: Parse report specification
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
        
        ! Step 2: Find the appropriate report list based on quant
        found = .false.
        
        if (averaged_quant) then         
            report => diag_species%diag%rep_cell_avg
        else 
            report => diag_species%diag%reports
        end if
        
        do while (associated(report) .and. .not. found)
            if (trim(report%name) == quant) then
                found = .true.
                exit
            end if
            report => report%next
        end do
        
        ! If still not found, check udist reports
        if (.not. found) then
            report => diag_species%diag%rep_udist
            do while (associated(report) .and. .not. found)
                if (trim(report%name) == quant) then
                    found = .true.
                    exit
                end if
                report => report%next
            end do
        end if
        
        if (.not. found) then
            ierr = -1  ! Relatório não encontrado
            return
        end if
        
        ! Step 3: Parse details (if they exist)
        call parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        
        ! Step 4: Find specific item
        if (item_type > 0) then
            found = .false.
            item => report%list
            do while (associated(item) .and. .not. found)
                if (item%type == item_type) then
                    if (item_type == p_line .or. item_type == p_slice) then
                        if (item%direction == direction .and. &
                            all(item%gipos == gipos) .and. &
                            item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    else
                        if (item%tavg .eqv. is_tavg) then
                            found = .true.
                            exit
                        end if
                    end if
                end if
                item => item%next
            end do
        end if
        
        ! Step 5: Apply modifications according to command
        select case (trim(command_name))
            ! Global commands (affect whole report)
            case ("ndump_fac")
                report%ndump(p_full) = new_value(1)
            case ("ndump_fac_ave")
                report%ndump(p_savg) = new_value(1)
                report%ndump(p_senv) = new_value(1)
            case ("ndump_fac_lineout")
                report%ndump(p_line) = new_value(1)
                report%ndump(p_slice) = new_value(1)
            
            ! Other dump frequencies
            case ("ndump_fac_ene")
                diag_species%diag%ndump_fac_ene = new_value(1)
            case ("ndump_fac_heatflux")
                diag_species%diag%ndump_fac_heatflux = new_value(1)
            case ("ndump_fac_temp")
                diag_species%diag%ndump_fac_temp = new_value(1)
            case ("ndump_fac_raw")  
                diag_species%diag%ndump_fac_raw = new_value(1)

            ! Commands for specific items
            case ("n_ave")
                if (size(new_value) >= n_dimensions) then
                    report%n_ave(1:n_dimensions) = new_value(1:n_dimensions)
                else
                    ierr = -3  ! Invalid size
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid size for n_ave in steering_species_diag"
                    endif
                end if
            case ("n_tavg")
                report%n_tavg = new_value(1)
                if (report%n_tavg > 0 .and. .not. associated(report%tavg_data%f1)) then
                    call report%tavg_data%new( &
                        sim%grid%x_dim, &       ! Spatial dimension
                        1, &                     ! f_dim
                        sim%grid%g_nx, &         ! Global grid size
                        sim%emf%e%gc_num(), &    ! Guard cells
                        sim%emf%e%dx(), &        ! Cell size
                        .true. &                 ! Zero the field
                    )
                endif

            case ("gipos")
                if (associated(item) .and. size(new_value) >= size(item%gipos)) then
                    item%gipos = new_value(1:size(item%gipos))
                else
                    ierr = -4  ! Invalid item or size mismatch
                    if (mpi_node() == 0) then
                        print *, "DEBUG - Invalid item or size mismatch for gipos in steering_species_diag"
                    endif
                end if                

            case ("prec")
                report%prec = new_value(1)  ! Update report precision

            ! Other commands
            case ("raw_gamma_limit")
                diag_species%diag%raw_gamma_limit = new_value(1)
            case ("raw_fraction")
                diag_species%diag%raw_fraction = new_value(1)
            case ("ndump_fac_tracks")
                diag_species%diag%ndump_fac_tracks = new_value(1)
            case ("n_start_tracks")
                diag_species%diag%n_start_tracks = new_value(1)

            case default
                ierr = -5  ! Unknown command
        end select
        
    end subroutine steering_species_diag

    !-----------------------------------------------------------------------------------------
    !       Add species reports
    !-----------------------------------------------------------------------------------------
    subroutine add_species_report(sim, input_string, name, averaged_quant)

        use m_species_diagnostics, only: p_report_quants, p_rep_udist ! get report quantifiers

        implicit none

        type(t_restart_handle) :: restart_handle
        character(len=*), intent(in) :: input_string
        class(t_simulation), intent(inout) :: sim
        logical, intent(in) :: averaged_quant  ! If true, report is averaged
        integer :: ierr, report_id, i, idx
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: parsed_string
        type(t_species), pointer :: type_species
        type(t_diag_species), pointer :: diag_species
        type(t_vdf_report), pointer :: report
        logical :: found_name

        ! Inicializa ponteiros e variáveis
        ierr = 0
        found_name = .false.
        report_id = 0 ! default value 

        type_species => sim%part%species  ! Start with first species in list

        ! As there is a linked list of species, we need to find the one with the given name
        do while (associated(type_species))
            if (trim(type_species%name) == trim(name)) then
                found_name = .true.
                exit
            end if
            type_species => type_species%next  ! Move to next species in list
        end do

        if (.not. found_name) then
            ierr = 1
            if (mpi_node() == 0) then
                print *, "DEBUG - Species particle id not found: ", trim(name)
            end if
            if (allocated(parsed_string)) deallocate(parsed_string)
            return
        end if

        ! Get the diag_species structure from the species
        diag_species => type_species%diag

        idx = index(input_string, ",")
        if (idx > 0) then
            parsed_string = input_string(1:idx-1)  ! Get the part before the comma
            parsed_string = trim(adjustl(parsed_string))  ! Trim leading spaces
        else
            parsed_string = input_string  ! No comma found, use the whole string
        end if

        ! Indentify which report type to add based on the input string

        do i = 1, size(p_report_quants)
            if (trim(p_report_quants(i)) == parsed_string) then
                if (averaged_quant) then
                    report_id = 2  ! 2 for averaged reports
                else
                    report_id = 1  ! 1 for full reports
                end if
                exit
            end if
        end do

        if (report_id == 0) then 
            do i = 1, size(p_rep_udist)
                if (trim(p_rep_udist(i)) == parsed_string) then
                    report_id = 3  ! 3 for udist reports
                    exit
                end if
            end do
        end if 

        if (allocated(parsed_string)) deallocate(parsed_string)

        ! Select the appropriate report list based on report_type
        select case (report_id)
            case (1)
                call add_report(diag_species%reports, trim(input_string), diag_species%report_quants, &
                            sim%g_space%x_dim, ierr)
                report => diag_species%reports
            case (2)
                call add_report(diag_species%rep_cell_avg, trim(input_string), diag_species%report_quants, &
                            sim%g_space%x_dim, ierr)
                report => diag_species%rep_cell_avg
            case (3)
                call add_report(diag_species%rep_udist, trim(input_string), diag_species%report_quants, &
                            sim%g_space%x_dim, ierr)
                report => diag_species%rep_udist
            case default
                ierr = 1
                if (mpi_node() == 0) then
                    print *, "Invalid report type: ", report_id
                endif
                return
        end select

        ! Initialize the report if it was added successfully
        if (ierr == 0) then
            ! Initialize ALL diagnostics for this species
            call diag_species % init( &
                trim(type_species%name), &          ! spec_name
                sim%g_space%x_dim, &                ! n_x_dim
                0, &                                ! ndump_fac (default value)
                interpolation( sim%part ), &        ! interpolation
                .false., &                          ! restart
                restart_handle )                    ! restart_handle

            ! Find the newly added report and initialize time averaging if needed
            do while (associated(report))
                if (trim(report%name) == trim(input_string)) then
                    if (report%n_tavg > 0) then
                        ! Initialize tavg_data with grid parameters
                        call report%tavg_data%new( &
                            sim%grid%x_dim, &       ! Spatial dimension
                            1, &                     ! f_dim (scalar field)
                            sim%grid%g_nx, &         ! Global grid size
                            sim%emf%e%gc_num(), &    ! Guard cells
                            sim%emf%e%dx(), &        ! Cell size
                            .true. &                 ! Zero the field
                        )
                    endif
                    exit
                endif
                report => report%next
            enddo
        end if

        ! Report status
        if (ierr /= 0) then
            if (mpi_node() == 0) then
                print *, "Error adding report: ", report_id
            endif
        else
            if (mpi_node() == 0) then
                print *, "Added new report: ", report_id
            endif
        endif

    end subroutine add_species_report

    !<----------------------PARTICLES------------------------------>! 

    !-----------------------------------------------------------------------------------------
    !       Change PARTICLE parameters through steering file - DISABLED FOR NOW !!!!!!!
    !-----------------------------------------------------------------------------------------
    subroutine steering_particles_diag(sim, report_spec, command_name, new_value, ierr)      
        implicit none
        
        ! Argumentos
        class(t_simulation), intent(inout) :: sim      
        character(*), intent(in) :: command_name      ! Nome do comando (ex: "ndump_fac", "n_ave", etc.)
        character(*), intent(in) :: report_spec       ! Especificação do relatório (ex: "e3", "e2, line, x1, 64, 96")
        integer, dimension(:), intent(in) :: new_value ! Novo valor (pode ser escalar ou array)
        integer, intent(out) :: ierr                  ! Código de erro (0 = sucesso)
        
        ! Variáveis locais
        type(t_particles), pointer :: diag_particles
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

        diag_particles => sim%part  ! Start with particle 

        report => diag_particles%reports

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
        call parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        
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

            case ("low_jay_roundoff")
                diag_particles%low_jay_roundoff = (new_value(1) /= 0) ! true 1 /false 0
            
            case ("ndump_fac_ene")
                diag_particles%ndump_fac_ene = new_value(1)

            case default
                ierr = -5  ! Comando desconhecido
        end select
        
    end subroutine steering_particles_diag

    !-----------------------------------------------------------------------------------------
    !       Add particles reports 
    !-----------------------------------------------------------------------------------------
    subroutine add_particles_report()
        ! DISABLED FOR NOW
    end subroutine add_particles_report

    !<------------------------------------------------------->! 
    !                 Auxiliary Functions
    !            To encapsulate common tasks       
    !<------------------------------------------------------->! 

    !-----------------------------------------------------------------------------------------
    !       Parses the report specification string
    !-----------------------------------------------------------------------------------------
    subroutine parse_report_spec(report_spec, quant, details, item_type, direction, gipos, is_tavg)
        character(*), intent(in) :: report_spec
        character(:), allocatable, intent(out) :: quant, details
        integer, intent(out) :: item_type, direction
        integer, dimension(2), intent(out) :: gipos
        logical, intent(out) :: is_tavg
        
        integer :: pos, pos2, ierr
        character(len=256) :: token
        logical :: has_direction
        
        ! Inicialização
        item_type = -1; direction = -1; gipos = -1; is_tavg = .false.
        
        ! 1. Separa quantidade e detalhes
        pos = index(report_spec, ',')
        if (pos > 0) then
            quant = trim(adjustl(report_spec(1:pos-1)))
            details = trim(adjustl(report_spec(pos+1:)))
        else
            quant = trim(adjustl(report_spec))
            details = ""
        end if
        
        ! 2. Verifica se tem 'tavg' em qualquer lugar
        is_tavg = (index(details, 'tavg') > 0)
        
        ! 3. Determina o tipo de item
        if (index(details, 'savg') > 0) item_type = p_savg
        if (index(details, 'senv') > 0) item_type = p_senv
        if (index(details, 'line') > 0) item_type = p_line
        if (index(details, 'slice') > 0) item_type = p_slice
        
        ! Default para full report se nenhum tipo especificado
        if (item_type == -1) item_type = p_full
        
        ! 4. Extrai direção (se aplicável)
        has_direction = .false.
        if (index(details, 'x1') > 0) then
            direction = 1; has_direction = .true.
        else if (index(details, 'x2') > 0) then
            direction = 2; has_direction = .true.
        else if (index(details, 'x3') > 0) then
            direction = 3; has_direction = .true.
        end if
        
        ! 5. Processa posições (gipos) - abordagem robusta
        gipos = -1  ! Inicializa com valor inválido
        if (item_type == p_line .or. item_type == p_slice) then
            ! Encontra o último bloco numérico nos detalhes
            pos = index(details, ',', back=.true.)
            
            if (pos > 0) then
                ! Tenta ler primeiro valor (após última vírgula)
                token = trim(adjustl(details(pos+1:)))
                read(token, *, iostat=ierr) gipos(1)
                
                ! Para lineouts, tenta ler segundo valor (se existir)
                if (item_type == p_line .and. ierr == 0) then
                    ! Procura vírgula anterior
                    pos2 = index(details(1:pos-1), ',', back=.true.)
                    if (pos2 > 0) then
                        token = trim(adjustl(details(pos2+1:pos-1)))
                        read(token, *, iostat=ierr) gipos(2)
                    end if
                end if
            end if
        end if
    end subroutine parse_report_spec


    !-----------------------------------------------------------------------------------------
    !       Checks if the addition of a report is valid to avoid crashes 
    !       It will check if n_ave and n_tavg are set correctly
    !-----------------------------------------------------------------------------------------
    subroutine valid_report_add(sim, identifier, diag_command, report, ierr)
        ! --------------------- !
        ! NOT USED FOR NOW 
        ! --------------------- !
        implicit none

        class(t_simulation), intent(inout) :: sim
        character(len=*), intent(in) :: identifier
        character(len=*), intent(in) :: diag_command
        type(t_vdf_report), pointer, intent(inout) :: report
        integer, intent(out) :: ierr

        ierr = 0

        ! 1) Check if the report exists
        if (.not. associated(report)) then
            ! Report not initialized
            if (index(identifier, ',') > 0) then
                ! Complex identifier is invalid if report doesn't exist
                ierr = 2
                return
            else
                ! Basic command, allow creation
                return
            end if
        else
            ! Report exists
            if (index(identifier, ',') > 0) then
                ! Identifier is complex → check if valid tags are included

                if (index(identifier, 'tavg') > 0) then
                    print*, "Debug - n_tavg ", report%n_tavg
                    if (report%n_tavg < 2) then
                        ierr = 3
                        if (root(sim%no_co)) then
                            print *, "DEBUG - Ignoring tavg. Ident: ", trim(identifier), "Cmd: ", trim(diag_command)
                        end if
                        return
                    end if
                end if

                if (index(identifier, 'savg') > 0) then
                    if ((any(report%n_ave(1:sim%g_space%x_dim) < 1))) then 
                        ierr = 3
                        if (root(sim%no_co)) then
                            print *, "DEBUG - Invalid n_ave for savg. Ident: ", trim(identifier), "Cmd: ", trim(diag_command)
                        end if
                       return
                    end if
                end if

                if (index(identifier, 'senv') > 0) then
                    if ((any(report%n_ave(1:sim%g_space%x_dim) < 1))) then 
                        ierr = 3
                        if (root(sim%no_co)) then
                            print *, "DEBUG - Invalid n_ave for savg. Ident: ", trim(identifier), "Cmd: ", trim(diag_command)
                        end if
                       return
                    end if
                end if

                if (index(identifier, 'line') > 0) then
                    if (sim%g_space%x_dim < 2) then
                        ierr = 3
                        if (root(sim%no_co)) then
                            print *, "DEBUG - Ignoring line. Ident: ", trim(identifier), "Cmd: ", trim(diag_command)
                        end if
                        return
                    end if
                end if

                if (index(identifier, 'slice') > 0) then
                    if (sim%g_space%x_dim < 3) then
                        ierr = 3
                        if (root(sim%no_co)) then
                            print *, "DEBUG - Ignoring slice. Ident: ", trim(identifier), "Cmd: ", trim(diag_command)
                        end if
                        return
                    end if
                end if

            else
                return
            end if
        end if

    end subroutine valid_report_add
    
    !-----------------------------------------------------------------------------------------
    !       Orders a restart write -> Copied from main file
    !-----------------------------------------------------------------------------------------
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
        
    
    !----------------------------------------------------------------------------------------
    !       Changes the maximum time of the simulation
    !----------------------------------------------------------------------------------------

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

    subroutine set_omega_p0(sim, val, ierr)
        implicit none

        class(t_simulation), intent(inout) :: sim
        character(len=*), intent(in) :: val
        integer, intent(out) :: ierr

        real(p_double) :: updated_tmax
        integer :: conv_ierr

        ierr = 0

        ! Check if the value is empty or invalid
        if (len_trim(val) == 0) then
            ierr = 1
            if (mpi_node() == 0) then
                print *, "DEBUG - Omega_p0 setting skipped due to empty value"
            end if
            return
        end if

        ! Convert the value to a double precision real
        updated_tmax = strtodouble(val, conv_ierr)
        ! Change the parameter 
        if (updated_tmax > 0) then
            sim%options%omega_p0 = updated_tmax
            if (mpi_node() == 0) then
                print *, "DEBUG - Updated omega_p0 to ", sim%options%omega_p0
            end if
        else
            ierr = 1
            if (mpi_node() == 0) then
                print *, "DEBUG - Omega_p0 not updated, new value ", updated_tmax, &
                        " is not larger than zero"
            end if
        end if
    end subroutine set_omega_p0

    !----------------------------------------------------------------------------------------
    !       Converts an array of strings to integers
    !       WARNING: This funcion is temporary. The array will be substitured 
    !       by a list in the near future to acommodate reals, bools and strings 
    !----------------------------------------------------------------------------------------

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
                if (mpi_node() == 0) then
                    print *, "STR_ARRAY_TO_INT ERROR: Empty string at position ", i
                end if
                return
            end if
            
            int_array(i) = strtoint(trim(str_array(i)), conv_ierr)
            if (conv_ierr /= 0) then
                ierr = conv_ierr
                if (mpi_node() == 0) then
                    print *, "STR_ARRAY_TO_INT ERROR: Cannot convert '", trim(str_array(i)), "' to integer"
                end if
                return
            end if
        end do

        ierr = 0
    end subroutine str_array_to_int

end module m_workflow