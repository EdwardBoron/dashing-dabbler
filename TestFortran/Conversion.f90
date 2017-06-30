!The following program is a simple unit converter, which is currently only able to convert kcal and kJ.

program convert
implicit none           !This doesn't specify the variables without being asked first
character :: unit*10    !Sets up the initial unit to be used
character :: result*10  !Sets up the resulting unit
character :: again*3    !Used to start the program over again.

real :: numIn   !The input value specified by the user
real :: numOut  !The output value
real :: factor  !The conversion factor

integer :: i,j         !used for the while loops

i = 0    !Can't be too careful
j = 0

do while (i == 0)                                !The big loop for restarting the program
 do while (j == 0)                               !The loop for specifying the initial unit
  print *,'Input a unit to start with:'
  read '(a)',unit

  !evaluate the unit

  if (unit == 'kcal' .or. unit == 'kj')then
   j = 1

  else if (unit == 'x')then
   j = 1 
 
  else
   print *,"I don't recognize this unit. To stop, input x"

  end if
 end do

 j = 0    !Saving on variables :)

 do while (j == 0)                                !The loop for the final unit
  print *,'Input a unit to calculate:'
  read '(a)',result
  
  !evaluate the other unit

  if (unit == 'kcal' .and. result == 'kj')then
   j = 1
   factor = 4.184

  else if (unit == 'kj' .and. result == 'kcal')then
   j = 1
   factor = 0.239006

  else if (result == 'x')then
   j = 1

  else
   print *,"I don't recognize this unit. To stop, input x"

  end if
 end do
 
 !There should be a part that registers the input x to stop doing calculations.
 !Best to put it after every of the two unit loops -- don't want to cause inconvenience
 
 print *,'Input a value'
 read *,numIn
 numOut = numIn * factor

 !Gives out the result
 print *,'The result is ',numOut,' ',result
 
 print *,'Do another conversion? (Yes/No)'
 read *,again
 if (again == 'No')then
  i = 1
 else
 end if
 factor = 0    !resetting a whole bunch of variables
 numIn = 0
 numOut = 0
 j = 0
end do

end program convert
