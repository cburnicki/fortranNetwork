program growNetwork
    use network
    
    implicit none
    
    call addEdge(1,2)
    call addEdge(4,2)
    
    call addEdge(1,3)
    
    write(*,*) edges
    
end program growNetwork
