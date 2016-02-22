program test
    use ArrayOperations
    
    implicit none
    
    integer, dimension(:), allocatable :: intArray
    integer, dimension(4) :: intArray2
    
    intArray2(1) = 1
    intArray2(2) = 2
    intArray2(3) = 3
    intArray2(4) = 4
    
    call appendIntArray(intArray, intArray2)
    call appendIntArray(intArray, intArray2)
    
    write(*,*) intArray(1)
    write(*,*) intArray(2)
    write(*,*) intArray(3)
    write(*,*) intArray(4)
    write(*,*) intArray(5)
    write(*,*) intArray(6)
    write(*,*) intArray(7)
    write(*,*) intArray(8)
    
end program test
    
    
