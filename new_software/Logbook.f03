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


! -------------------- !
! Doesn't WORK -> need to use 
! -------------------- !

subroutine access_psource_components(src)
    class(t_psource), intent(in) :: src  ! Polymorphic variable
    
    select type (src)
    type is (t_psource_std)
        ! Now you can access t_psource_std specific components
        print *, "Accessing t_psource_std components:"
        print *, "some_integer = ", src%some_integer
        print *, "some_real = ", src%some_real
        ! ... access other components ...
        
    class default
        print *, "Unknown or unsupported type extension"
    end select
end subroutine access_psource_components

subroutine modify_profile_params (sim, name, command, value, ierr)   
        ! -------------------- !
        ! UNDER DEVELOPMENT 
        ! -------------------- !
        class(t_simulation), intent(inout) :: sim
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: command
        character(len=*), intent(in) :: value
        integer, intent(out) :: ierr

        ! Local variables
        type(t_species), pointer :: species
        class(t_psource), pointer :: src
        real(p_double) :: numeric_double
        integer :: numeric_int
        logical :: found_name

        ierr = 0
        found_name = .false.
        
        ! Start with first species in list
        species => sim%part%species

        ! Traverse species linked list
        do while (associated(species))
            if (trim(species%name) == trim(name)) then
                found_name = .true.
                exit
            end if
            species => species%next
        end do

        if (.not. found_name) then
            ierr = 1
            if (mpi_node() == 0) then
                write(0,*) "Species not found: ", trim(name)
            end if
            return
        end if

        ! Access the source object
        src => species%source
        if (.not. associated(src)) then
            ierr = 2
            if (mpi_node() == 0) then
                write(0,*) "No source defined for species: ", trim(name)
            end if
            return
        end if

        select case (trim(command))
            case ("density")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0 ) then 
                    src%density = numeric_double
                endif 

            case ("den_min")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0 ) then
                    src%den_min = numeric_double
                endif 

            case ("num_x")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0 ) then
                    src%num_x = numeric_double
                endif 

            case ("gauss_center")

            case ("gauss_n_sigma")
                
            case ("channel_dir") !???????????????????????????''
                numeric_int = strtoint(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if ((numeric_int < 1 .or. numeric_int > 3) .and. mpi_node() == 0) then 
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 ) then
                    src%channel_dir = numeric_int
                endif 

            case ("channel_r0")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%channel_r0 = numeric_double
                endif 

            case ("channel_depth")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%channel_depth = numeric_double
                endif 

            case ("channel_size")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%channel_size = numeric_double
                endif 
            
            case ("channel_center")

            case ("channel_wall")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%channel_wall = numeric_double
                endif 

            case ("channel_pos")

            case ("channel_bottom")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for setting density"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%channel_bottom = numeric_double
                endif 

            case ("sphere_center")

            case ("sphere_radius")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for sphere_radius"
                else if (ierr == 0 .and. numeric_double >= 0) then
                    src%sphere_radius = numeric_double
                endif 
            
            ! Constant charge add check to see if t_psource_constq
            case ("sample_rate")
                numeric_int = strtoint(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for sample_rate"
                else if (ierr == 0 .and. numeric_int > 0) then
                    src%channel_dir = numeric_int
                endif 

            ! Beam specific quantities Add check to see if type t_psource_beam
            case ("focal_dist")

            case ("alpha")

            case ("uth")

            case ("gamma")
                numeric_double = strtodouble(value, ierr)
                if (ierr /= 0 .and. mpi_node() == 0) then
                    print *, "DEBUG : Invalid value for gamma"
                else if (ierr == 0 .and. numeric_double >= 1) then
                    src% = numeric_double
                endif 

            ! Raw specific quantities Add check to see if  t_psource_file
            case("file_name")
                

            case("x1_offset")

            case ("q_mult")


    end subroutine modify_profile_params