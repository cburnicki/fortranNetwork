all: growNetwork.exe

testdebug:  test.f90 ArrayOperations.f90 ArrayOperations.o test.o
	gfortran -g ArrayOperations.o test.o -o test.exe	

growNetwork.exe: Network.o growNetwork.o
	gfortran Network.o growNetwork.o -o growNetwork.exe
	
Network.o: Network.f90 ArrayOperations.o
	gfortran -c Network.f90
	
growNetwork.o: growNetwork.f90 ArrayOperations.o
	gfortran -c growNetwork.f90
		
test: test.exe

test.exe: ArrayOperations.o test.o
	gfortran ArrayOperations.o test.o -o test.exe
	
test.o: test.f90 ArrayOperations.o
	gfortran -c test.f90

ArrayOperations.o: ArrayOperations.f90
	gfortran -c ArrayOperations.f90

clean:
	rm *o growNetwork.exe
