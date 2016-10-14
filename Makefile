CC		= g++ -std=c++11
#Nota a -fdiagnostics-color=auto so foi introduzida no gcc 4.9. O ubuntu so tem nos repos oficiais o gcc 4.8
FLAGS	= 

INCDIR1	= ./include
INCLUDE = -I${INCDIR1}

.phony: all clean sandbox_user sandbox_english engfrench

all: user trs tcs

utils.o: include/utils.hpp src/utils.cpp
	${CC} ${INCLUDE} ${FLAGS} src/utils.cpp -c -o utils.o

tcs: src/tcs.cpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/tcs.cpp utils.o -o tcs

trs: src/trs.cpp include/tcplib.hpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/trs.cpp utils.o -o trs

user: src/user_main.cpp include/tcplib.hpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/user_main.cpp utils.o -o user
	


clean:
	rm trs user tcs utils.o
