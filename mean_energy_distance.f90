program mean_energy_distance
! Evaluates the energy for each point in a distance region
! and calculates the mean energy in that region.
implicit none
integer :: i
integer, parameter :: nat = 6, ndivision = 6
real(8), dimension(ndivision), parameter :: COdistance = (/100,50,20,10,5,1/)
real(8), parameter :: autoA = 0.52917721092e0

character(len=1), dimension(nat) :: atname
real(8), dimension(nat*3) :: xyz, der
real(8), dimension(ndivision) :: mean_ener
integer, dimension(ndivision) :: npoints
real(8) :: ener, dCO, s

mean_ener = 0e0
npoints = 0
do
  read(*,*,end=999) 
  read(*,*)
  do i=1,nat
     read(*,*) atname(i), xyz(3*(i-1)+1), xyz(3*(i-1)+2), xyz(3*(i-1)+3)
  enddo
  xyz = xyz / autoA

  ! Evaluate energy at that point.
  call potXYZ(xyz, ener, der)

  ! Evaluate dCO distance at the point.
  call dCO_dis(xyz, dCO)

  ! Add energy to energy range count and add one to the number of points 
  ! in that range.
  do i=1,ndivision-1
     if (dCO <= COdistance(i) .and. dCO > COdistance(i+1)) then
        mean_ener(i) = mean_ener(i) + ener
		npoints(i) = npoints(i) + 1
		exit
	 endif
  enddo

  ! Calculate IRC coordinate. If greater than zero exit.
  call IRC_coor(xyz, s)
  if (s>0) exit

enddo
999 continue

write(*,*) mean_ener, npoints
endprogram


subroutine dCO_dis(xyz, dCO)
implicit none
integer :: i
real(8), dimension(6*3), intent(in) :: xyz
real(8), intent(out) :: dCO

dCO = 0e0

do i=1,3
   dCO = dCO + (xyz(i)-xyz(12+i)) * (xyz(i)-xyz(12+i))
enddo

dCO = sqrt(dCO)
endsubroutine

subroutine IRC_coor(xyz,s)
implicit none
integer :: i
real(8), dimension(6*3) :: xyz
real(8) :: s1, s2, s, dCH1, dCH2, dOH1, dOH2

dCH1 = 0e0
dCH2 = 0e0
dOH1 = 0e0
dOH2 = 0e0

do i=1, 3
   dCH1 = dCH1 + (xyz(i) - xyz(6 + i))**2
   dCH2 = dCH2 + (xyz(i) - xyz(9 + i))**2

   dOH1 = dOH1 + (xyz(12 + i) - xyz(6 + i))**2
   dOH2 = dOH2 + (xyz(12 + i) - xyz(9 + i))**2
enddo

dCH1 = sqrt(dCH1)
dCH2 = sqrt(dCH2)

dOH1 = sqrt(dOH1)
dOH2 = sqrt(dOH2)

s1 = dCH1 - dOH1
s2 = dCH2 - dOH2
if (s1 .gt. s2) then
   s = s1
else
   s = s2
endif
endsubroutine
