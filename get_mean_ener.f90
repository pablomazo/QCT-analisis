program get_probabilities
implicit none
integer, parameter :: ndivision = 6 
integer :: idis, i
real(8), dimension(ndivision), parameter :: COdistance = (/100,50,20,10,5,1/)
real(8), dimension(ndivision) :: ener_tmp, ener 
integer, dimension(ndivision) :: npoints, npoints_tmp
integer(8) :: total_traj

ener = 0e0
npoints = 0
do
  read(*,*,end=999) ener_tmp, npoints_tmp 
  ener = ener + ener_tmp
  npoints = npoints + npoints_tmp
enddo
999 continue
ener = ener / dble(npoints)
write(*,*) '# Distance range, mean energy/au, number of points'
do i=1, ndivision-1
   write(*,*) COdistance(i), COdistance(i+1), ener(i), npoints(i)
enddo
endprogram
