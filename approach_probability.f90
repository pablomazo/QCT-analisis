program approach_probability
use xyzfiles
! Given an xyz file with a trajectory, the code evaluates
! the probability of the system reaching different distances.

! It is useful to determine if there is something on the PES
! that causes unexpected rebounds.

implicit none
integer, parameter :: nat = 6, ndis = 6, ngreater = 10
integer :: idis, igreater
real(8), dimension(nat,3) :: xyz
character(len=1), dimension(nat) :: atname
real(8), dimension(ndis), parameter :: reac_dis = (/100e0, 50e0, 20e0, 10e0, 9e0,5e0/)
integer, dimension(ndis) :: n_in_dis
logical :: end_file

real(8) :: dCO, dCO_prev

n_in_dis = 0
igreater = 0e0

end_file = .FALSE.
dCO_prev = 1000e0
do while (.not. end_file)
  ! Read geometry from file.
  call readxyz(5, nat, atname, xyz, end_file)

  ! Calculate C-O distance.
  call COdis(xyz, dCO)

  do idis=1, ndis
     if (dCO <= reac_dis(idis) .and. dCO_prev > reac_dis(idis)) then
	     ! If the distance is lower than the specified read_dis distance
		 ! and the previous distance was greater, add one to the list.
         n_in_dis(idis) = 1
         !n_in_dis(idis) = n_in_dis(idis) + 1
	 endif
  enddo

  if (dCO > dCO_prev) then
     ! If the reactants are separating for more than 10 steps
	 ! stop evaluating the distances.
     igreater = igreater + 1
	 if  (igreater > ngreater) then
         end_file = .TRUE.
	 endif
  else
     igreater = 0
  endif

  if (dCO < reac_dis(ndis)) then
     ! If the minimum value of distance was reached, exit reading the trajectory.
     end_file = .TRUE.
  endif

  dCO_prev = dCO
enddo

write(*,*) n_in_dis
endprogram

subroutine COdis(xyz, dCO)
implicit none
integer :: i
real(8), dimension(6,3), intent(in) :: xyz
real(8), intent(out) :: dCO

dCO = 0e0

do i=1,3
   dCO = dCO + (xyz(1,i) - xyz(5,i)) * (xyz(1,i) - xyz(5,i))
enddo

dCO = sqrt(dCO)

endsubroutine
