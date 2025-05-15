module m_laser
  implicit none
  public :: t_laser, laser_params, nl_laser  ! Adicione nl_laser aos públicos

  type :: t_laser
    real :: wavelength  ! Nome deve bater exatamente com o namelist
    real :: peak_power  ! Incluindo capitalização
  end type

  type(t_laser) :: laser_params

  ! Namelist deve usar as mesmas variáveis que estão no tipo
  namelist /nl_laser/ laser_params  ! Note que usamos a estrutura completa

end module


module m_workflow_reader
  use m_input_file
  use m_laser, only: t_laser, laser_params, nl_laser  ! Importe explicitamente
  use mpi
  implicit none

contains

  subroutine read_workflow_deck(filename)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: stat, ierr

    ! 1) Ler o arquivo
    call open_input(steering_file, filename, MPI_COMM_WORLD)

    ! Debug 1: Verificar se o buffer foi carregado
    if (steering_file%data_size == 0) then
      print *, "Erro: Buffer vazio após open_input!"
      return
    else
      print *, "Debug: Tamanho do buffer = ", steering_file%data_size
    endif

    ! 2) Procurar pelo bloco "nl_laser", substituindo o nome para "laser"
    steering_file%pos = 0
    call get_namelist(steering_file, 'nl_laser', stat, nml_output='laser')

    ! Debug 2: Verificar resultado da busca
    print *, "Debug: stat após get_namelist = ", stat

    if (stat == 0) then
      ! Debug 3: Mostrar conteúdo bruto do namelist
      print *, "Conteúdo do namelist 'laser':"
      print *, "'", trim(steering_file%nml_text), "'"

      ! 3) Ler os parâmetros
      read(steering_file%nml_text, nml=nl_laser, iostat=ierr)

      ! Debug 4: Verificar erro na leitura
      if (ierr /= 0) then
        print *, "Erro na leitura do namelist! Código:", ierr
      else
        print *, "Leitura bem-sucedida!"
      endif
    else
      print *, "Erro: Seção 'nl_laser' não encontrada."
    endif
  end subroutine


end module