all: growNetwork.exe

growNetwork.exe: Network.o growNetwork.o ArrayOperations.o ErrorHandler.o
	gfortran -g Network.o growNetwork.o ArrayOperations.o ErrorHandler.o -o growNetwork.exe
	
Network.o: Network.f90 ArrayOperations.o ErrorHandler.o
	gfortran -g -Wall -c Network.f90
	
growNetwork.o: growNetwork.f90
	gfortran -g -Wall -c growNetwork.f90
	
ErrorHandler.o: ErrorHandler.f90
	gfortran -g -Wall -c ErrorHandler.f90

ArrayOperations.o: ArrayOperations.f90
	gfortran -g -Wall -c ArrayOperations.f90

clean:
	rm *o *.exe
	funit --clean
	
test:
	make -f maketest
	
testclean: all
	funit --clean
