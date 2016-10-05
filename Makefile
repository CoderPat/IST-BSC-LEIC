CC		= g++ -std=c++11
#Nota a -fdiagnostics-color=auto so foi introduzida no gcc 4.9. O ubuntu so tem nos repos oficiais o gcc 4.8
FLAGS	= 

INCDIR1	= ./include
INCLUDE = -I${INCDIR1}


all: user trs

trs: src/trs.cpp include/tcplib.hpp include/udplib.hpp
	${CC} ${INCLUDE} ${FLAGS} src/trs.cpp -o trs

user: src/user_main.cpp include/tcplib.hpp include/udplib.hpp
	${CC} ${INCLUDE} ${FLAGS} src/user_main.cpp -o user

clean:
	rm trs user