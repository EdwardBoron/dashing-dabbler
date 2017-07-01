program convertersetup

use arraytest

implicit none

character(32) :: filename               
character(8) :: again
character(8) :: help
character(12) :: txt,txtx
character(32), allocatable, dimension(:,:,:) :: units
character(12), dimension(2,32) :: modules

real*8, allocatable, dimension(:,:) :: factor
real*8, allocatable, dimension(:,:,:) :: table            !the final conversion table

integer :: statOpen
integer :: statRead
integer :: repj         !number of attempts to load modules
integer :: repk
integer :: n,nn,nnn     !continuous variables
integer :: l            !variables for looping
integer :: line
integer :: lineOverlord

real*8 :: input
character(8) :: inputUnit
real*8 :: output
character(8) :: outputUnit

!IDEAS: decadic exponents, make a subroutine that identifies the module with the highest number of units
!       make a readme with a quick overview
!       Careful! The program only supports as many lines as specified in the loops with n = 1,(*)!
!       Currently: (*) = 2'000'000

!--------------------------------------------------------------------------------------------------
!--------------------------------SETTINGS----------------------------------------------------------
                                
lineOverlord = 32                !Specifies the maximum number of units
!lineUnderlord = 4               !Specifies the maximum number of modules
filename = 'modules.dat'         !Specifies the file containing the modules - the general list

!--------------------------------------------------------------------------------------------------                                
!--------------------------------------------------------------------------------------------------
!-------------------------------HEADER-------------------------------------------------------------
print *,"===================================================================================================="
print *,"                            C0NV3R510N pre-0.1 - by Michael Haefner"
print *,"===================================================================================================="
!--------------------------------------------------------------------------------------------------
                                
!-------------------------------Program Loop-------------------------------------------------------
do

!-------------------------------Initialization-----------------------------------------------------
        do n = 1,2
        do nn = 1,32
        modules(n,nn)='0'
        end do
        end do
        
        call Listing(filename,repj,modules)     !opens the initialization subroutine for the general list 
                
        allocate(units(repj,2,lineOverlord))      !allocates the arrays for the units and the conversion factors
        allocate(factor(repj,lineOverlord))     !MISSING: might add the decadic exponents someday (kilo, Mega, Giga, Tera, Peta...)
        allocate(table(repj,lineOverlord,lineOverlord))
       
        do n = 1,repj                           !Clears the arrays
        do nn=1,lineOverlord
        units(n,1,nn)='0'
        units(n,2,nn)='0'
        factor(n,nn)=0
        do nnn=1,lineOverlord
        table(n,nn,nnn)=0.0
        end do
        end do
        end do


        repk = repj + 1                         !HACK 1: the list-file begins with a header line. This recognizes this header line
        do n = 1,repk
                open(11,file=filename,status='old',action='read')                       !opens the general list
                read(11,*) txt
                if (txt == '#')then                                                     !ignores the first line
                else                                                                    !opens the modules
                        call countingLines(line,txt)                                    !gets the number of units
                        innerLoop: do nn = 1,2000000                                    !ignores everything up to the marker '***'
                        open(12,file=txt,status='old',iostat = statOpen)
                        if (statOpen == 0)then
                                read(12,*,iostat=statRead) txtx                                 !MISSING: a useful debugger for malfunctioning modules
                                if(txtx == '***' .or. statRead /= 0)exit innerLoop
                        else                                                            !Ignores missing modules
                                modules(1,n-1) = '0'
                                modules(2,n-1) = '0'
                                exit
                        end if
                        end do innerLoop

                                do nn = 1,line                                                  !gets the units and conversion factors
                                read(12,*,iostat=statRead) units(n-1,1,nn),factor(n-1,nn),units(n-1,2,nn)         !into the respective arrays
                                if (statRead /= 0)exit                                          !and ends at the file end
                                end do                                                          !HACK 1: n-1 recognizes the header line as well!
                                if (statOpen == 0)then
                                        print *,"The module ",txt,"has been read successfully."
                                else
                                end if
                        close(12)                                                       !closes the module
                end if
               ! if (txt == '#')
               ! else
               !         open(12,file=txt,status='old')
               !         read(12,*) units,
        end do
        close(11)                                                                       !closes the general list

        do n = 1,repj                                                                   !sets up the conversion table
        do nn = 1,lineOverlord
        do nnn = 1,lineOverlord
        if (factor(n,nnn) == 0)exit
        table(n,nn,nnn) = factor(n,nn) / factor(n,nnn)
        end do
        end do
        end do

        print *,'The conversion table is set up!'

!-------------------------------End of the initialization------------------------------------------

        !call helpModules(modules)                                                       !calls for help
        
        l = 0
       do                                                                               !loops everything!!!
        call helpModules(modules)
        !print *,modules(2,:)


        do while (l < 1)                                                                !loops the module input
        print *,"Please input the module for conversion. To see the list again, enter h. To exit, enter x."
        read (*,*,iostat = statRead) help
        if (help == 'x')exit                                                            !quick exit
        if (help == 'h')then                                                            !calls for help
                call helpModules(modules)
                l = -1
        else
        end if

        do n = 1,lineOverlord                                                           !loops through the list to find the input
        if (help == modules(1,n))then
                repj = n
                l = 1
        else
        end if
        end do
        if (l == 0)then                                                                 !gives an error when finding nothing
                print *,"Your input isn't available. Please choose another option."
        else
        end if
        end do
       
        if (help == 'x')exit                                                            !Exits if exiting out of the module selection

        do                                                                              !loops inside the module
         l = 0
         
         call helpUnits(units,repj,lineOverlord)

         do while (l < 1)
         l = 0
         print *,"Input the value and the unit. If the unit contains / or ', put it into quotation marks."
         read (*,*,iostat = statRead) input,inputUnit                                                     
         if (statRead /= 0)then
                 print *,"Please input a number as value."
                 l = -1
         else
                if (inputUnit /= '0')then
                        do n = 1,lineOverlord
                        if (inputUnit == units(repj,1,n))then
                                nn = n
                                l = 1
                        else
                        end if
                        end do
                else
                end if
         end if
         if (l == 0)then
                 print *
                 print *,"This unit isn't available. Please choose another option."
         else
         end if
         end do
         
         l = 0
         do while (l < 1)
         print *,"Into which unit should the value be converted?"
        print *," If the unit contains / or ', put it into quotation marks. Abort with x."
         read (*,*,iostat = statRead) outputUnit
         
         if (outputUnit == 'x')exit
         if (outputUnit /= '0')then
         do n = 1,lineOverlord
         if (outputUnit == units(repj,1,n))then
                 nnn = n
                 l = 1
         else
         end if
         end do
         else
         end if
         if (l == 0)then
                 print *
                 print *,"This unit isn't available. Please choose another option."
         else
         end if
         end do
         
         output = input * table(repj,nn,nnn)
         print *,"================"
         print *
         print *,input,inputUnit
         print *
         print *,"==>",output,outputUnit
         print *
         print *,"================"

         print *,"Press enter for another conversion. To exit, enter x."
         read '(A)',help
         if (help == 'x')exit

        end do

        !write (*,'(G8.0,x,A)') output,outputUnit


       ! if (help == 'x')exit                                                            !everything to exit the loop or continue
               ! call helpModules(modules)
        print *,"To choose another module, press enter. To exit, enter x."
        read '(A)',help
        if (help == 'x')exit
        l = 0
       end do



!-------------------------------Testing Output-----------------------------------------------------
                                                                       
       ! do n = 1,repj           !prints the arrays for the units and conversion factors
       ! print *,units(n,:)
       ! print *
       ! print *,table(1,n,:)
       ! end do
        
!-------------------------------Exit---------------------------------------------------------------
        if (help == 'x')exit                       
        !print *,"To cancel the process, type x, q, end or quit."                                !asks the user
        !read '(A)',again
        !if (again == 'x' .or. again == 'q' .or. again == 'end' .or. again == 'quit')exit        !exits the loop
        deallocate(units)                                                                       !cleans up
        deallocate(factor)
        deallocate(table)

end do

end program convertersetup
!--------------------------------------------------------------------------------------------------
!-------------------------------SUBROUTINES--------------------------------------------------------
!--------------------------------------------------------------------------------------------------
subroutine Listing(filename,repj,modules)
!initializes the listing of available modules - iterates through the general list and looks for mistakes
implicit none

character(32) :: filename
character(32) :: txt
character(12),dimension(2,32) :: modules

integer :: statOpen                                                     !status while opening the file
integer :: statRead                                                     !status while reading the file
integer :: scratch
integer :: line                                                         !number of the current line
character(8) :: lineT
integer :: n                                                            !maximum of lines
integer :: nn

integer :: repj

lineT = '0'

open(13,file='temp',status='unknown')
do nn = 0,9                            !HACK 1:again accounting for the first line
write(13,'(I1)') nn
end do
do nn = 10,99
write(13,'(I2)')nn
end do
close(13)


open(11,file=filename,status='old',action='read', iostat=statOpen)      !opens the general list

if (statOpen == 0)then
         line = -1                                                      !HACK 1:again accounting for the first line

         do n = 1,2000000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '***')exit                !reads the file until it hits the end or '***'
                
                line = line + 1                                         !counts the lines
                !lineT = achar(line + 48)                                !builds the module array

                open(13,file='temp',status='unknown')
                read(13,*,iostat = scratch) lineT
                if (scratch /= 0)exit
                modules(1,line) = lineT                                 !the respective keys
                modules(2,line) = txt                                   !the respective modules

        end do
        close(13,status='delete')
        rewind(11)                                                      !back to the beginning

  if (statRead > 0)then
         print *,'An error occured while reading line ',line + 1,'.'    !Data reading error... shouldn't occur with character

  else
          print *
          !print *,'The end of the file was reached.'
          print *,'The initializer has found',line,'modules in the initialization file.'
          print *,"The modules are:"                                    !lists the modules between '#' and '***'
                                                                        !modules after '***' are omitted
         do n = 1,2000000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '#')exit
         end do
        
         do n = 1,2000000
                read(11,*,iostat=statRead) txt
                if (statRead /= 0 .or. txt == '***')exit
                print *,txt
        
         end do
         print *,"Attempting to load the modules now..."                !Flavor text

  end if

  else
          print *,"Couldn't open the file. The system error ",statOpen,"occured during general initialization!"
          print *,"Check your general list!"                            !Error in the general list

end if

repj = line                                                             !Gets the number of modules for the unit and factor arrays
close (11)                                                              !Closes the general list
end subroutine Listing
!--------------------------------------------------------------------------------------------------
subroutine countingLines(line,txt)
!counts the lines of a module to get the number of units and factors
implicit none

character(12) :: txt
character(32) :: units

integer :: statOpen                                                     !status while opening the file
integer :: statRead                                                     !status while reading the file
integer :: line                                                         !number of the current line
integer :: n                                                            !maximum of lines

integer :: repj

line = 0

open(12,file=txt,status='old',action='read', iostat=statOpen)           !opens the module file

if (statOpen == 0)then                                                  !omits the lines up to '***'
         do n = 1,2000000
                read(12,*,iostat=statRead) units
                if (statRead /= 0 .or. units == '***')exit
        end do
        
        do n = 1,2000000                                                !counts the lines containing units and factors
        read(12,*,iostat=statRead) units                                !stops at the end of the file or '***'
                if (statRead /= 0 .or. units == '***')exit

                line = line + 1
        end do

  if (statRead > 0)then                                                 !checks the data integrity of the module files
         print *,'An error occured while reading line ',line + 1,'.'
         print *,"Make sure that you've got *** to signalize the beginning of the data!"

  else
  end if
else                                                                    !reports errors while opening the module files
        print *,"Couldn't open the file ",txt ," The system error ",statOpen,"occured!"
        print *,"Check the module files!"
end if
!if (statOpen == 0)then
        close(12)                                                               !CLOSES the module files. Always close everything to prevent
!else
!end if
                                                                        !hilarious bugs...
end subroutine countingLines
!--------------------------------------------------------------------------------------------------
subroutine helpModules(modules)
!is called to inform about the installed modules
implicit none

character(12),dimension(2,32) :: modules

integer :: n

        print *
        print *,'  Key         ','Module'
        print *,'------------------------------'
        do n = 1,32
        if (modules(1,n) == '0')then
                cycle
        end if
        print *,'  ',modules(1,n),modules(2,n)
        end do
        print * 
end subroutine helpModules
!--------------------------------------------------------------------------------------------------
