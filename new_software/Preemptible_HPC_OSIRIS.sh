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

# --------------------------------------------#
#               Source code                   #
# --------------------------------------------#
echo "==> Removendo ficheiros .out e .err "
rm -f *.out.* 2>/dev/null
rm -f *.err.* 2>/dev/null

echo "==> Iniciando execuções com até $max_number_of_restarts restarts..."

while [[ $osiris_finished -eq 0 && $restart_count -lt $max_number_of_restarts ]]; do
    echo "==> Submissão nº $((restart_count + 1)) via sbatch"
    JOBID=$(sbatch --parsable $submit_job_name)
    echo "==> Job ID: $JOBID"

    # Ficheiros de log esperados
    err_file="slurm-${JOBID}.err"
    out_file="slurm-${JOBID}.out"
    
    # Monitoriza continuamente o ficheiro .err
    error_detected=0
    while squeue -j "$JOBID" -h >/dev/null; do
        sleep 10
        
        # Verifica se o .err existe e tem conteúdo
        if [[ -s "$err_file" ]]; then
            echo "==> ERRO: Ficheiro $err_file não está vazio!"
            error_detected=1
            scancel "$JOBID"  # Cancela o job imediatamente
            break
        fi
    done

    # Processa o estado após o job terminar
    if [[ $error_detected -eq 1 ]]; then
        echo "==> Processo interrompido devido a erros."
        echo "==> Aguardando 30 segundos antes do restart..."
        sleep 30
        
        # Renomeia os ficheiros para evitar nova leitura
        if [[ -f "$err_file" ]]; then
            mv "$err_file" "${err_file}.$restart_count"
        fi
        if [[ -f "$out_file" ]]; then
            mv "$out_file" "${out_file}.$restart_count"
        fi
        
        # Verifica restart
        if [[ -d "$restart_dir" ]] && [[ -n "$(ls -A "$restart_dir")" ]]; then
            ((restart_count++))
            echo "==> Restart possível. Restarts: $restart_count"
            sed -i "s|\($osiris_executable_name\)|\1 -r|" $submit_job_name
        else
            echo "==> Diretório de restart inválido. Abortando."
            break
        fi
    else
        # Verifica conclusão normal
        if grep -q "Osiris run completed normally" "$out_file" 2>/dev/null; then
            echo "==> Osiris terminou corretamente."
            osiris_finished=1
        else
            echo "==> Osiris NÃO terminou corretamente."
            
            # Renomeia os ficheiros mesmo em falha sem erro explícito
            [[ -f "$err_file" ]] && mv "$err_file" "${err_file}.$restart_count"
            [[ -f "$out_file" ]] && mv "$out_file" "${out_file}.$restart_count"
            
            if [[ -d "$restart_dir" ]] && [[ -n "$(ls -A "$restart_dir")" ]]; then
                ((restart_count++))
                echo "==> Restart possível. Restarts: $restart_count"
                sed -i "s|\($osiris_executable_name\)|\1 -r|" $submit_job_name
            else
                echo "==> Diretório de restart inválido. Abortando."
                break
            fi
        fi
    fi
done

if [[ $osiris_finished -eq 0 ]]; then
    echo "Número máximo de restarts alcançado."
else
    echo "Execução bem-sucedida após $restart_count restarts."
fi