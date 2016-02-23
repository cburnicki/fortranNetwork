program growNetwork
    use network
    
    implicit none
    
    call addEdge(1,2)
    call addEdge(4,2)
    
    write(*,*) edges
    
end program growNetwork
