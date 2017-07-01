module arrayTest
        contains
                subroutine helpUnits(units,repj,lineOverlord)
                !prints all units that are available in a module
                implicit none

                character(32), dimension(:,:,:), intent(in) :: units
                integer :: repj
                integer :: n
                integer :: lineOverlord

                        print *
                        print*,"List of all units in the module:"
                        print *,'-----------------------------------------------'
                        do n = 1,lineOverlord
                        if (units(repj,1,n) == '0')exit
                        print  '(2X,A12,A)',units(repj,1,n),units(repj,2,n)
                        end do
                        print *
                end subroutine helpUnits
end module arrayTest
