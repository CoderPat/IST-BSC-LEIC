# RC Translate
RC Translate is project that creates a Server/Client interface and lets the user translate words and file from a server. To compile, run the makefile and it will create 3 executables: trs, tcs and user

## How to use
After compiling copy one of trs executable to the english folder and another to the french folder (they contain the respective file and text translation dictionaries and copies of the translation images) to simulate two translation servers. After that just run the executables like in the program specification.

## Compiling
To compile simply run make on the root directory
`make`

## Running
### TCS Server
To run the tcs server on the default port (58007) just run:
`./tcs`
If you intend to specify a port use althe -p argument. For instance to listen on port 12345, run:
`./tcs -p 12345`

### TRS Server
To run the trs server with the default port (59007) and with the de
