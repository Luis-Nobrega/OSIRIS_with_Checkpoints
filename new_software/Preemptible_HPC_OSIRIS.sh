#!/bin/bash

# Current directory
current_dir=$(pwd)

# --------------------------------------------#
#               PARAMETHERS ZONE              #
# --------------------------------------------#
osiris_executable_name="osiris-8002fe2-dirty-2D.e"
input_deck_name="input_weibel"
submit_job_name="submitjob"  
restart_dir="/RE"  
osiris_finished=0
max_number_of_restarts=5
restart_count=0
max_queue_time=120 #(in seconds)

# --------------------------------------------#
#               Source code                   #
# --------------------------------------------#
job_state=""
search_files=0
started=1

echo "==> Removendo ficheiros .out e .err "
rm -f *.out.* 2>/dev/null
rm -f *.err.* 2>/dev/null

echo "==> Iniciando execuções com até $max_number_of_restarts restarts..."

while [[ $osiris_finished -eq 0 && $restart_count -lt $max_number_of_restarts ]]; do
    echo "==> Submissão nº $((restart_count + 1)) via sbatch"
    JOBID=$(sbatch --parsable $submit_job_name)
    echo "==> Job ID: $JOBID"

    # Wait for job to start/finish
    while [ $search_files -eq 0 ]; do
        sleep 10
        job_state=$(squeue -j "$JOBID" -h -o "%t" 2>/dev/null)
        
        # Job no longer in queue
        if [ -z "$job_state" ]; then
            echo "==> Job concluído ou removido da fila"
            started=0
            break
        fi
        
        echo "==> Estado do job: $job_state"

        if [[ "$job_state" == "PD" ]]; then
            echo "Job está pendente (PD)"
        elif [[ "$job_state" == "R" ]]; then
            echo "Job está a correr (R)"
            # Job started - exit monitoring loop
            search_files=1
        else
            echo "Estado desconhecido: $job_state"
            started=0
            break
        fi
    done

    if [[ $started -eq 0 ]]; then
        echo "==> Job não iniciado corretamente. Abortando."
        break
    fi

    # Find log files using JOBID
    err_file=$(find . -type f -name "*.err.${JOBID}" | head -n 1)
    out_file=$(find . -type f -name "*.out.${JOBID}" | head -n 1)
    
    # Wait for job completion
    while squeue -j "$JOBID" &>/dev/null; do
        sleep 10
    done

    echo "==> Rotina terminada, verificando ficheiros de log..."
    error_detected=0

    # Check error file
    if [[ -s "$err_file" ]]; then
        echo "==> ERRO: Ficheiro $err_file não está vazio!"
        if grep -q "CANCELLED" "$err_file"; then
            echo "==> ERRO: Job foi CANCELADO!"
            error_detected=1
        else 
            echo "==> ERRO desconhecido em $err_file"
            error_detected=1
        fi
    fi

    # Process results
    if [[ $error_detected -eq 1 ]]; then
        echo "==> Processo interrompido devido a erro."
        # Rename log files
        [[ -f "$err_file" ]] && mv "$err_file" "${err_file}.$restart_count"
        [[ -f "$out_file" ]] && mv "$out_file" "${out_file}.$restart_count"
        
        # Check restart feasibility
        if [[ -d "$restart_dir" && -n "$(ls -A "$restart_dir")" ]]; then
            ((restart_count++))
            echo "==> Restart possível. Total: $restart_count"
            sed -i "s|\($osiris_executable_name\)|\1 -r|" $submit_job_name
        else
            echo "==> Diretório de restart inválido. Abortando."
            break
        fi
    else
        # Check normal completion
        if grep -q "Osiris run completed normally" "$out_file" 2>/dev/null; then
            echo "==> Osiris terminou corretamente."
            osiris_finished=1
        else
            echo "==> Osiris NÃO terminou corretamente."
            # Rename logs for debugging
            [[ -f "$err_file" ]] && mv "$err_file" "${err_file}.$restart_count"
            [[ -f "$out_file" ]] && mv "$out_file" "${out_file}.$restart_count"
            
            if [[ -d "$restart_dir" && -n "$(ls -A "$restart_dir")" ]]; then
                ((restart_count++))
                echo "==> Restart possível. Total: $restart_count"
                sed -i "s|\($osiris_executable_name\)|\1 -r|" $submit_job_name
            else
                echo "==> Diretório de restart inválido. Abortando."
                break
            fi
        fi
    fi
    
    # Reset for next iteration
    search_files=0
    started=1
done

if [[ $osiris_finished -eq 0 ]]; then
    echo "Número máximo de restarts alcançado."
else
    echo "Execução bem-sucedida após $restart_count restarts."
fi