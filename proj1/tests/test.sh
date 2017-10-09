#!/bin/bash
#Tests the program with input/outpu files in the same folder

program="../build/proj1"
perf_file="perf_results.dat"
max_vertices=100000


for inputfile in *.in; do
	echo -------$inputfile--------
	diff <(eval $program <$inputfile) ${inputfile%.*}.out
done

#--- Performance mesure ---
if [ "$1" == "-perf" ]; then
	echo "#### Testing Performance ####"
	total_test="$2"
	echo "---- Generating Test Cases ----"
	# generate random graphs
	while (( total_test > 0 )); do
		python randomgraphgenerator.py $max_vertices >t${total_test}.random.in 
		((total_test--))
	done

	echo "---- Checking Runtime ----"
	for inputfile in *.random.in; do
		# test instructions used
		instr=$(perf stat -x, -e instructions:u $program < $inputfile 2>&1 >/dev/null)

		#prints .dat file with the V+E <-> Intructions correspondence
		graph_info=$(head -n 1 $inputfile)
		graph_info_array=($graph_info)
		echo $((${graph_info_array// /+})) ${instr%,,*} >> $perf_file
	done

	rm *.random.in
fi


# @copyright Grupo 70