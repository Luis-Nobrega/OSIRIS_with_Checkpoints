#!/bin/bash

# Diretório atual
current_dir=$(pwd)

# Parâmetros
osiris_executable_name="osiris-8002fe2-dirty-2D.e"
input_deck_name="input_weibel"
submit_job_name="submit_job"
restart_file_name="RESTART"
osiris_finished=0
max_number_of_restarts=5
restart_count=0

echo "==> Iniciando execuções com até $max_number_of_restarts restarts..."

while [[ $osiris_finished -eq 0 && $restart_count -lt $max_number_of_restarts ]]; do
    echo "==> Submissão nº $((restart_count + 1)) via sbatch"

    sbatch $submit_job_name

    # Espera até o job terminar (ajuste conforme sua fila SLURM)
    echo "Aguardando job SLURM terminar..."
    while squeue -u $USER | grep -q $submit_job_name; do
        sleep 10
    done

    # Verifica se o Osiris terminou corretamente (por exemplo, via mensagem no arquivo de saída)
    if grep -q "Osiris run completed normally" slurm-*; then
        echo "==> Osiris terminou corretamente."
        osiris_finished=1
    else
        echo "==> Osiris NÃO terminou corretamente. Verificando possibilidade de restart..."

        if [[ -f "$restart_file_name" ]]; then
            ((restart_count++))
            echo "==> Restart possível. Número atual de restarts: $restart_count"
        else
            echo "==> Arquivo de restart NÃO encontrado. Abortando."
            break
        fi
    fi
done

if [[ $osiris_finished -eq 0 ]]; then
    echo "⚠️ Número máximo de restarts alcançado ou erro fatal."
else
    echo "✅ Execução finalizada com sucesso após $restart_count restarts."
fi
