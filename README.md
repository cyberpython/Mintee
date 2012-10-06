# Mintee

Mintee is simple program to execute a command and write its output to a file and the standard output.

## Build instructions

The FPC compiler (2.6+) is necessary to compile the source code - it can be downloaded from [http://www.freepascal.org/download.var](http://www.freepascal.org/download.var).

To compile mintee, open a command-line window and change the directory to the one where mintee.lpr is located.
Then give:

    fpc mintee.lpr
    
This should produce the mintee executable.

## Usage

    mintee [OPTIONS] <CMD>
    Execute command <CMD> and copy the command's output to a file as well as to the standard output.
    
      -a, --append            append instead of overwriting files
      -o <file>, --out=<file> copy from the command's stdout to <file>
      --no-stdout             do not write to stdout

## License

Distributed under the terms of the MIT license.
Copyright © 2012 Georgios Migdos.

