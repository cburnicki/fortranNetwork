module ArrayOperations
    
    implicit none
    
    contains
    
    ! appends an integer to a 1D array of integers
    subroutine appendInt(array, intgr)
        implicit none
        
        integer, dimension(:), allocatable, intent(inout) :: array
        integer, intent(in) :: intgr
        integer, dimension(:), allocatable :: newArray
        integer :: arraySize, i
        
        if (allocated(array)) then
            arraySize = size(array)
            allocate(newArray(arraySize + 1))
            
            do i = 1, arraySize
                newArray(i) = array(i)
            end do
            
            newArray(arraySize + 1) = intgr
            
            deallocate(array)            
            call move_alloc(newArray, array)
            
        else
            allocate(array(1))
            array(1) = intgr
        
        end if
        
    end subroutine appendInt
    
    ! append an array of integers to an array of integers
    subroutine appendIntArray(array, intArray)
        implicit none
        
        integer, dimension(:), allocatable, intent(inout) :: array
        integer, intent(inout) :: intArray(:)
        integer, dimension(:), allocatable :: newArray
        integer :: arraySize, intArraySize
        
        if (allocated(array)) then
            arraySize = size(array)
            intArraySize = size(intArray)
            allocate(newArray(arraySize + intArraySize))
            
            newArray(1:arraySize) = array(:)
            newArray(arraySize+1 : arraySize+intArraySize) = intArray(:)
            
            deallocate(array)
            
            call move_alloc(newArray, array)
            
        else            
            intArraySize = size(intArray)
            allocate(array(intArraySize))
            array(:) = intArray(:)
            
        end if
        
    end subroutine appendIntArray
            
            
        
end module arrayOperations
