#!/bin/bash
#Tests the program with input/outpu files in the same folder

program="../build/proj1"

for inputfile in *.in; do
	diff <(eval $program <$inputfile) ${inputfile%.*}.out
done

# @copyright Grupo 70
