CC		= g++ -std=c++11
FLAGS	=

INCDIR1	= ./include
INCLUDE = -I${INCDIR1}


all: tcs


tcs: src/tcs.cpp
	${CC} ${INCLUDE} ${FLAGS} src/tcs.cpp -o tcs

clean:
	rm tcs
