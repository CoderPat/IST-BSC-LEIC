CC		= g++ -std=c++11
FLAGS	=

INCDIR1	= ./include
INCLUDE = -I${INCDIR1}


all: trs


trs: src/trs.cpp
	${CC} ${INCLUDE} ${FLAGS} src/trs.cpp -o trs

clean:
	rm trs