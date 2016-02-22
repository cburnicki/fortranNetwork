all: growNetwork.exe

testdebug:  test.f90 ArrayOperations.f90 ArrayOperations.o test.o
	gfortran -g ArrayOperations.o test.o -o test.exe	

growNetwork.exe: network.o growNetwork.o
	gfortran network.o growNetwork.o -o growNetwork.exe
	
network.o: arrayOperations.o
	gfortran -c network.f90
	
growNetwork.o: arrayOperations.o
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
