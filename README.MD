# RUSE abstract machine architecture

[![Build Status](https://travis-ci.org/sylvarant/RUSE-AM.svg?branch=master)](https://travis-ci.org/sylvarant/RUSE-AM)

Academic research project that aimed to develop 
abstract machines by duplicating them across a secure and insecure low-level memory space. 
Halted, due to lack of publishable results.


## Directory structure

- test/ : test cases
- test/log : detailed logs of all the tests
- front-end/ : ml and scheme parsers
- cesk/ : implementation of the CESK machine
- buildsrc/ : all files used to build the executable contains the Makefile
- bin/ : the executables produced through compilation
- lib/ : the libraries produced as attack targets for low level attacks
- attacks/ : attacks to be explored
- tools/ : helpful scripts

## Executing

./ruse [-slpdfwc] sourcefile
    -a :: creat the vm as an api
    -s :: produce sancus executable
    -h :: hardware (fpga) oriented build
    -n :: native mode - compile the bytecode into the vm
    -l :: produce a log of the debug and build information in test/log
    -d :: activate debug mode, outputs machine trace on stderr
    -f :: Fides support - (needs to be redone !!)
    -p :: Preprocess the input file, outputs the results and aborts
    -c :: Compile the input file, no execution
    -w :: Compile with warnings

## Testing 

Configure setup using :
    ./tools/update
Execute tests using :
    ./tools/testruse

## Specification
see lang-spec/

