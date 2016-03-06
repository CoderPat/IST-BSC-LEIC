directory="/build/"

for inputfile in *.in; do
	diff <(../${directory}proj1 <$inputfile) ${inputfile%.*}.out
done