# RC Translate
RC Translate is project that creates a Server/Client interface and lets the user translate words and file from a server. To compile, run the makefile and it will create 3 executables: trs, tcs and user

## How to use
After compiling copy one of trs executable to the english folder and another to the french folder (they contain the respective file and text translation dictionaries and copies of the translation images) to simulate two translation servers. After that just run the executables like in the program specification.

## Compiling
To compile simply run make on the root directory
```
make
```

We also recommend that you create a sandbox environment to soothe testing, since it avoids file conflicts with similar names.
To do so, use:
```
make sandbox
```
It will create 3 folders inside sandbox:

> sandbox/user - a folder with all our example files and with the user executable (ie, the client executable)
>
> sandbox/english - a folder configured to be used as an english TRS, it contains the trs executable and the files needed to run it
>
> sandbox/french - a folder configured to be used as a french TRS, it contains the trs executable and the files needed to run it
>

## Running


### TCS Server
To run the tcs server on the default port (58007) just run:
```
./tcs
```
If you intend to specify a port use althe -p argument. For instance, to listen on port 12345, run:
```
./tcs -p 12345
```

### TRS Server
To run the trs server with the default port (59007) and with the de
In order to run the tcs server you need to be in a folder with the following files: tcs (executable), file_translation.txt, text_translation.txt, and all the images referenced in file_translation.txt.
We provide two setup folders on sandbox/english and sandbox/french, if you ran the make `sandbox command`
Then just run inside the folder:
```
./trs <language>
```
ie:
```
cd ./sandbox/english
./trs english
```
This runs the server with the default parameters, you can specify the hostname and port of the tcs server with -n and -e respectively, and your own port with -p.
For instante to run a french server on port 59007 connecting to a tcs server on 107.6.108.7 port 80, run:
```
cd ./sandbox/french
./trs french -p 59007 -n 107.6.108.7 -p 80 -e 59007
```

### Client
To run the client with the default tcs server address and port (localhost, 50007):
```
./user
```
to run connecting to a server on a different address and port use the -n and -p parameters, respectively. For instance, to connect to a tcs running on 166.62.117.116 port 80, use:
 ```
./user -n 31.192.120.36 -p 80
```
