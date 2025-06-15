#!/bin/bash

current_dir=$(pwd)
osiris_executable_name= "osiris-8002fe2-dirty-2D.e"
input_deck_name= 
submit_job_name= "submit_job"
restart_file_name=
osiris_finished=0
max_number_of_restarts=5

# check error file to continue ?
# Osiris run completed normally
# mpirun -n 4 /home/lnobrega/osiris_tests/osiris--2D-.e input_weibel

# $ mpiexec -np 16 ../bin/osiris-2D.e -r lwfa.2d
# mpiexec -np 4 osiris-8002fe2-dirty-2D.e -r input_weibel