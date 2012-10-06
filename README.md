# Mintee

Mintee (minimal tee) is simple program to execute a command and write its output to a file and the standard output.
It kinda works like the Unix tee program, with the exception that only one file can be used to write the output.

## Build instructions

The FPC compiler (2.6+) is necessary to compile the source code - it can be downloaded from [http://www.freepascal.org/download.var](http://www.freepascal.org/download.var).

To compile mintee, open a command-line window and change the directory to the one where mintee.lpr is located.
Then give:

    fpc mintee.lpr
    
This should produce the mintee executable.

## Usage

    mintee [OPTIONS] [FILE]
    Copy standard input to FILE, and also to standard output.');
    
        -a, --append              append instead of overwriting files');
        -c <cmd>, --command=<cmd> execute command <cmd> and read from its output instead of stdin');
        --no-stdout               do not write to stdout');
    

## License

Distributed under the terms of the MIT license.
Copyright © 2012 Georgios Migdos.

