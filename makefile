anim: all
	./growNetworkExample.exe
	python networkAnimation.py

all: growNetworkExample.exe

growNetworkExample.exe: Network.o growNetworkExample.o ArrayOperations.o ErrorHandler.o
	gfortran -g Network.o growNetworkExample.o ArrayOperations.o ErrorHandler.o -o growNetworkExample.exe

Network.o: Network.f03 ArrayOperations.o ErrorHandler.o
	gfortran -g -Wall -c Network.f03

growNetworkExample.o: growNetworkExample.f03
	gfortran -g -Wall -c growNetworkExample.f03

ErrorHandler.o: ErrorHandler.f03
	gfortran -g -Wall -c ErrorHandler.f03

ArrayOperations.o: ArrayOperations.f03
	gfortran -g -Wall -c ArrayOperations.f03

clean:
	rm *o *.exe
	funit --clean

test:
	make -f maketest

testclean: all
	funit --clean
