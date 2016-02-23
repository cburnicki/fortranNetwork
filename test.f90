program test
    logical l
    l = any((/.true., .true., .true./))
    print *, l
    call section
    contains
      subroutine section
        integer a(6), b(6)
        logical c(6)
        a = (/1,2,1,3,2,4/)
        b = 4
        print *, b
        c = a .eq. b
        print *, c
        d = pack(a, c)
        print *, d
      end subroutine section
end program test
