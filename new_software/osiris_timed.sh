
command="mpiexec -np 4 osiris-9ba60c0-dirty-2D.e input_weibel_test"

# Clear environment
./cleaner_osiris.sh

# Get start time (seconds since epoch with nanosecond precision)
start=$(date +%s.%N)

# Execute the command
$command

# Calculate duration using bash arithmetic
end=$(date +%s.%N)
duration=$(awk "BEGIN {print $end - $start}")

# Format the output
printf "Execution time: %.3f seconds\n" $duration