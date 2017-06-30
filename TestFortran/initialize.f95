program convertersetup
implicit none

character(32) :: filename               
character(8) :: again
character(32) :: txt,txtx
character(16), allocatable, dimension(:,:) :: units

real, allocatable, dimension(:,:) :: factor

integer :: statOpen
integer :: statRead
integer :: repj         !number of attempts to load modules
integer :: repk
integer :: n,nn         !continuous variables
integer :: line
integer :: lineOverlord

!IDEAS: decadic exponents, make a subroutine that identifies the module with the highest number of units
!       make a readme with a quick overview

!--------------------------------------------------------------------------------------------------
!--------------------------------SETTINGS----------------------------------------------------------
                                
lineOverlord = 8                !Specifies the maximum number of units
filename = 'list'               !Specifies the file containing the modules - the general list

!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
!-------------------------------HEADER-------------------------------------------------------------
print *,"===================================================================================================="
print *,"                            C0NV3R510N pre-0.1 - by Michael Haefner"
print *,"===================================================================================================="
!--------------------------------------------------------------------------------------------------

!-------------------------------Program Loop-------------------------------------------------------
do

 
        call Listing(filename,repj)             !opens the initialization subroutine for the general list 
                
        allocate(units(repj,lineOverlord))      !allocates the arrays for the units and the conversion factors
        allocate(factor(repj,lineOverlord))     !MISSING: might add the decadic exponents someday (kilo, Mega, Giga, Tera, Peta...)
       
        do n = 1,repj                           !Clears the arrays
        do nn=1,lineOverlord
        units(n,nn)='0'
        factor(n,nn)=0
        end do
        end do

        repk = repj + 1                         !HACK 1: the list-file begins with a header line. This recognizes this header line
        do n = 1,repk
                open(11,file=filename,status='old',action='read')                       !opens the general list
                read(11,*) txt
                if (txt == '#')then                                                     !ignores the first line
                else                                                                    !opens the modules
                        call countingLines(line,txt)                                    !gets the number of units
                        innerLoop: do nn = 1,20000                                      !ignores everything up to the marker '***'
                        open(12,file=txt,status='old')
                        read(12,*,iostat=statRead) txtx                                 !MISSING: a useful debugger for malfunctioning modules
                        if(txtx == '***' .or. statRead /= 0)exit innerLoop
                        end do innerLoop

                        do nn = 1,line                                                  !gets the units and conversion factors
                        read(12,*,iostat=statRead) units(n-1,nn),factor(n-1,nn)         !into the respective arrays
                        if (statRead /= 0)exit                                          !and ends at the file end
                        end do                                                          !HACK 1: n-1 recognizes the header line as well!
                        close(12)                                                       !closes the module
                end if
               ! if (txt == '#')
               ! else
               !         open(12,file=txt,status='old')
               !         read(12,*) units,
        end do
        close(11)                                                                       !closes the general list
        
        do n = 1,repj
        print *,units(n,:)
        print *
        print *,factor(n,:)
        end do

        print *,"To cancel the process, type x, q, end or quit."
        read '(A)',again
        if (again == 'x' .or. again == 'q' .or. again == 'end' .or. again == 'quit')exit        !exits the loop
        deallocate(units)
        deallocate(factor)

end do

end program convertersetup
!------------------------------------------------------------------------
subroutine Listing(filename,repj)
!initializes the listing of available modules
implicit none

character(32) :: filename
character(32) :: txt

integer :: statOpen                    !status while opening the file
integer :: statRead                    !status while reading the file
integer :: line                        !number of the current line
integer :: n                           !maximum of lines

integer :: repj

open(11,file=filename,status='old',action='read', iostat=statOpen)

!Checks the initialization file's integrity, counts the modules and names them

if (statOpen == 0)then
         line = -1
         do n = 1,20000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '***')exit
                
                line = line + 1

        end do

        rewind(11)

  if (statRead > 0)then
         print *,'An error occured while reading line ',line + 1,'.'

  else
          print *
          !print *,'The end of the file was reached.'
          print *,'The initializer has found ',line,' modules in the initialization file.'
          print *,"The modules are:"
          
         do n = 1,20000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '#')exit
         end do
        
         do n = 1,20000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '***')exit
                print *,txt
        
         end do
         print *,"Attempting to load the modules now..."

  end if

  else
          print *,"Couldn't open the file. The system error #",statOpen," occured!"

end if

repj = line 
close (11)
end subroutine Listing
!--------------------------------------------------------------------------------------------------
subroutine countingLines(line,txt)
!counts the lines of a module to define the corresponding array sizes
implicit none

character(32) :: txt
character(32) :: units

integer :: statOpen                    !status while opening the file
integer :: statRead                    !status while reading the file
integer :: line                        !number of the current line
integer :: n                           !maximum of lines

integer :: repj

line = 0

open(12,file=txt,status='old',action='read', iostat=statOpen)

!Checks the initialization file's integrity and counts the lines

if (statOpen == 0)then
         do n = 1,20000
                read(12,*,iostat=statRead) units
                if (statRead /= 0 .or. units == '***')exit
        end do
        
        do n = 1,20000
                read(12,*,iostat=statRead) units
                if (statRead /= 0)exit

                line = line + 1
        end do

  if (statRead > 0)then
         print *,'An error occured while reading line ',line + 1,'.'
         print *,"Make sure that you've got *** to signalize the beginning of the data!"

  else
  end if
else
        print *,"Couldn't open the file. The system error ##",statOpen," occured!"
end if
close(12)
end subroutine countingLines
