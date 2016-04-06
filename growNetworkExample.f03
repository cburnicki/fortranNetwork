program growNetwork
    use Network
    
    implicit none
    
    integer :: rounds, newNodesPerRound, newEdgesPerRound, i, j
    character(len=14) :: filename
    
    call srand(int(secnds(0.0)))
    
    rounds = 6
    newNodesPerRound = 4
    newEdgesPerRound = 4
    
    call addNode(1)
    
    do i = 1, rounds
    
        do j = 1, newNodesPerRound
        
            call addNode(maxval(nodes)+1)
        end do
    
        do j = 1, newEdgesPerRound
        
            call addRandomEdge()
        end do
        
        write(filename,'(a,i4.4,a)') "data/t",i+1000,".txt"
        call exportEdges(filename)
    end do 
    
    contains 
    
    subroutine addRandomEdge()
        implicit none
        
        integer :: N, a, b
        
        N = size(nodes)
        
        
        a = int(N*rand(0))
        b = int(N*rand(0))
        
        call addEdge(a, b)
        
    end subroutine addRandomEdge
    
end program growNetwork
