CC		= g++ -std=c++11
#Nota a -fdiagnostics-color=auto so foi introduzida no gcc 4.9. O ubuntu so tem nos repos oficiais o gcc 4.8
FLAGS	= 

INCDIR1	= ./include
INCLUDE = -I${INCDIR1}

.phony: all clean sandbox sandbox_user sandbox_english sandbox_french

all: user trs tcs

utils.o: include/utils.hpp src/utils.cpp
	${CC} ${INCLUDE} ${FLAGS} src/utils.cpp -c -o utils.o

tcs: src/tcs.cpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/tcs.cpp utils.o -o tcs

trs: src/trs.cpp include/tcplib.hpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/trs.cpp utils.o -o trs

user: src/user_main.cpp include/tcplib.hpp include/udplib.hpp utils.o
	${CC} ${INCLUDE} ${FLAGS} src/user_main.cpp utils.o -o user
	
sandbox: sandbox_user sandbox_english sandbox_french

sandbox_user: user
	rm -rf ./sandbox/user
	mkdir -p ./sandbox/user
	cp ./auxiliary/english/*.jpg ./sandbox/user/
	cp ./auxiliary/french/*.jpg ./sandbox/user/
	cp ./user ./sandbox/user/user

sandbox_english: trs
	rm -rf ./sandbox/english
	mkdir -p ./sandbox/english
	cp ./auxiliary/english/*.txt ./sandbox/english/
	cp ./auxiliary/*.jpg ./sandbox/english/
	cp ./trs ./sandbox/english/trs

sandbox_french: trs
	rm -rf ./sandbox/french
	mkdir -p ./sandbox/french
	cp ./auxiliary/french/*.txt ./sandbox/french/
	cp ./auxiliary/*.jpg ./sandbox/french/
	cp ./trs ./sandbox/french/trs

clean:
	rm -rf ./sandbox/user ./sandbox/english ./sandbox/french
	rm -r trs user tcs utils.o
