#!/bin/bash

# Current directory
current_dir=$(pwd)

# --------------------------------------------#
#               PARAMETHERS ZONE
#              Change values here 
# --------------------------------------------#
osiris_executable_name="osiris-8002fe2-dirty-2D.e"
input_deck_name="input_weibel"
restart_file_name="RESTART"
restart_dir="/RE"  # Diretório de restart
osiris_finished=0
max_number_of_restarts=5
restart_count=0
n_processes=4  # Número de processos MPI

# --------------------------------------------#
#               Source code
# --------------------------------------------#

echo "==> Removendo ficheiros .out e .err "

rm -f *.out.* 2>/dev/null
rm -f *.err.* 2>/dev/null

echo "==> Iniciando execuções com até $max_number_of_restarts restarts..."

while [[ $osiris_finished -eq 0 && $restart_count -lt $max_number_of_restarts ]]; do
    echo "==> Submissão nº $((restart_count + 1)) via sbatch"

    mpiexec -n $n_processes $osiris_executable_name $input_deck_name
    sbatch $submit_job_name

    # Espera até o job terminar
    echo "Aguardando job SLURM terminar..."
    while squeue -u $USER | grep -q $submit_job_name; do
        sleep 10
    done

    # Verifica se o Osiris terminou corretamente
    if grep -q "Osiris run completed normally" slurm-*.out*; then
        echo "==> Osiris terminou corretamente."
        osiris_finished=1
    else
        echo "==> Osiris NÃO terminou corretamente. Verificando possibilidade de restart..."
        
        # Verifica se o diretório de restart existe e não está vazio
        if [[ -d "$restart_dir" ]] && [[ -n "$(ls -A "$restart_dir")" ]]; then
            ((restart_count++))
            echo "==> Restart possível. Número atual de restarts: $restart_count"
            
            # Modifica o comando no script de submissão para adicionar -r
            mpiexec -n $n_processes $osiris_executable_name "-r" $input_deck_name
            echo "==> Comando modificado para: $(grep "$osiris_executable_name" $submit_job_name)"
        else
            echo "==> Diretório de restart vazio ou não encontrado. Abortando."
            break
        fi
    fi
done

if [[ $osiris_finished -eq 0 ]]; then
    echo "Número máximo de restarts alcançado ou erro fatal."
else
    echo "Execução finalizada com sucesso após $restart_count restarts."
fi