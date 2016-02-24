module Network
    
    use ArrayOperations
    use ErrorHandler
    
    implicit none

    integer, dimension(:,:), allocatable :: edges
    integer, dimension(:), allocatable :: nodes
    
    contains
    
    ! Adds an edge if the edge doesn't already exists
    ! If an edge is added, the corresponding nodes will also be added
    ! if they don't exist
    subroutine addEdge(a, b)
        implicit none
        
        integer, intent(in) :: a, b
        integer :: n_edges
        integer, allocatable :: new_edges(:,:)
        
        if ( .not. isValidNode(a) .or. .not. isValidNode(b) ) then
            return
        end if
        
        if ( a == b ) then
            return
        end if
        
        ! do not add the edge if it already exists
        if (hasEdge(a, b)) then
            return
        end if
        
        if (allocated(edges)) then
            n_edges = size(edges, 1)
            allocate(new_edges(n_edges +  1, 2))
            
            new_edges(1:n_edges, :) = edges(:,:)
            new_edges(n_edges + 1, :) = (/a, b/)
        
            deallocate(edges)
            call move_alloc(new_edges, edges)
            
        else
            allocate(edges(1,2))
            edges(1,:) = (/a, b/)
            
        end if
        
        ! add the nodes
        call addNode(a)
        call addNode(b)
        
    end subroutine addEdge
    
    ! Checks if an edge is already in edges
    logical function hasEdge(a, b)
        implicit none
        
        integer, intent(in) :: a, b
        integer :: n_edges, i
        
        if (allocated(edges)) then
            n_edges = size(edges, 1)
            
            do i = 1, n_edges
                if (edges(i,1) == a .and. edges(i,2) == b) then
                    hasEdge = .TRUE.
                    return
                end if
            end do
                    
        end if
        hasEdge = .FALSE.
        
    end function hasEdge
    
    ! Checks if a node already exists
    logical function hasNode(node)
        implicit none
        
        integer, intent(in) :: node
        
        hasNode = intInArray(nodes, node)
        
        return
    end function hasNode
    
    ! adds a node if it doesn't already exist
    subroutine addNode(node)
        implicit none
        
        integer, intent(in) :: node
        
        if ( .not. isValidNode(node)) then
            return
        end if
        
        if (hasNode(node) .eqv. .TRUE.) then
            return
        end if
        
        call appendInt(nodes, node)
    end subroutine
    
    ! delete an edge
    subroutine deleteEdge(a,b)
        implicit none
        
        integer, intent(in) :: a, b
        integer, dimension(:,:), allocatable :: new_edges
        ! counts how often (a,b) was in edges
        integer :: n_edges, i, j
        
        if ( .not. isValidNode(a) .or. .not. isValidNode(b) ) then
            return
        end if
        
        if (allocated(edges) .eqv. .FALSE.) then
            return
        end if
        
        ! j is the running index for the new array
        j = 1     
        
        n_edges = size(edges, 1)
        ! if the node exists, the new array will have exactly 1 entry less
        allocate(new_edges(n_edges - 1, 2))
        do i=1, n_edges
            ! copy entries that don't match the deleted edge to the new array
            if (edges(i,1) /= a .or. edges(i,2) /= b) then
                new_edges(j, :) = edges(i, :)
                j = j + 1
                
                if (j == n_edges + 1) then
                    ! all edges have been copied, edge to delete wasn't found
                    deallocate(new_edges)
                    return
                end if
            end if            
        end do
        ! Too few edges have been copied, hence more than one edge
        ! matched the deletion
        if ( j < i - 1 ) then
            call error('deleteEdge: edge was found more than once.')
        end if
        
        ! move new_edges to edges
        deallocate(edges)
        call move_alloc(new_edges, edges)
    end subroutine deleteEdge
    
    ! delete a node
    subroutine deleteNode(node)
        implicit none
        
        integer, intent(in) :: node
        integer, allocatable :: new_nodes(:)
        logical, allocatable :: compare_result(:)
        integer :: n_nodes
        
        if ( .not. isValidNode(node) ) then
            return
        end if
        
        if (.not. allocated(nodes)) then
            return
        end if
        
        n_nodes = size(nodes)
        
        allocate(compare_result(n_nodes))
                
        compare_result = nodes == node
                
        if ( count(compare_result) == 0 ) then
            ! node not in nodes
            return
        
        else if ( count(compare_result) == 1 ) then
            ! copy everything else in a new nodes array
            allocate(new_nodes(n_nodes - 1))
            new_nodes = 0
            new_nodes = pack(nodes, .not. compare_result, new_nodes)
            
            deallocate(nodes)
            deallocate(compare_result)
            call move_alloc(new_nodes, nodes)
            
            ! delete all edges containing the deleted node
            call deleteEdgesWithNode(node)
            
        else
            ! this should not happen
            call error('deleteNode: node existed more than once.')
            
        end if
    end subroutine deleteNode
    
    ! checks if the value for the node is valid
    logical function isValidNode(node)
        implicit none
    
        integer, intent(in) :: node
        
        isValidNode = node > 0
        return
    end function isValidNode
    
    ! deletes all edges containing a node
    subroutine deleteEdgesWithNode(node)
        implicit none
        
        integer, intent(in) :: node
        integer, dimension(:,:), allocatable :: new_edges
        ! counts how often (a,b) was in edges
        integer :: n_edges, i, j
        
        if ( .not. isValidNode(node)) then
            return
        end if
        
        if (allocated(edges) .eqv. .FALSE.) then
            return
        end if
        
        ! j is the running index for the new array
        j = 1     
        
        n_edges = size(edges, 1)
        ! if the node exists, the new array will have exactly 1 entry less
        allocate(new_edges(n_edges - 1, 2))
        do i=1, n_edges
            ! copy entries that don't match to the new array
            if (edges(i,1) /= node .and. edges(i,2) /= node) then
                new_edges(j, :) = edges(i, :)
                j = j + 1
            end if            
        end do
                
        ! move new_edges to edges
        deallocate(edges)
        call move_alloc(new_edges, edges)
        
    end subroutine deleteEdgesWithNode
            
            
end module Network
