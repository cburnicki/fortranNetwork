module ErrorHandler
    implicit none
    
    contains
    
    ! prints the error message and stops the program
    subroutine criticalError(message)
        implicit none
        
        character(len=*) :: message
        
        write(*,*) 'CRITICAL: ' // message
        stop
    end subroutine criticalError
    
    ! prints the error message
    subroutine error(message)
        implicit none
        
        character(len=*) :: message
        
        write(*,*) 'ERROR: ' // message
    end subroutine error
    
    ! prints the warning message
    subroutine warning(message)
        implicit none
        
        character(len=*) :: message
        
        write(*,*) 'WARNING: ' // message
    end subroutine warning  
    
end module ErrorHandler
