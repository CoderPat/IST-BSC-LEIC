#!/bin/bash
#Tests the program with input/outpu files in the same folder

program="../build/proj1"
max_vertices=10000


for inputfile in *.in; do
	echo -------$inputfile--------
	diff <(eval $program <$inputfile) ${inputfile%.*}.out
done

#--- Performance mesure ---
if [ "$1" == "-perf" ]; then
	echo "#### Testing Performance ####"
	total_test="$2"
	echo "---- Generating Test Cases ----"
	while (( total_test > 0 )); do
		python randomgraphgenerator.py $max_vertices >t${total_test}.random.in 
		((total_test--))
	done

	for inputfile in *.random.in; do
		perf stat -x, -e instructions:u $program < $inputfile >/dev/null
	done

	rm *.random.in
fi


# @copyright Grupo 70
