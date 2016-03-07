all: growNetwork.exe

growNetwork.exe: Network.o growNetwork.o ArrayOperations.o ErrorHandler.o
	gfortran -g Network.o growNetwork.o ArrayOperations.o ErrorHandler.o -o growNetwork.exe
	
Network.o: Network.f03 ArrayOperations.o ErrorHandler.o
	gfortran -g -Wall -c Network.f03
	
growNetwork.o: growNetwork.f03
	gfortran -g -Wall -c growNetwork.f03
	
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
