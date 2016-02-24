program test
    implicit none
    
    integer, allocatable :: a(:), b(:)
    logical, allocatable :: res(:)
    
    allocate(a(3))
    allocate(b(3))
    allocate(res(3))
        
    a = (/1,2,3/)
    b = (/1,3,1/)
    
    res = a==b
    
    res = .not. res
    
    write(*,*) res    
    

end program test
