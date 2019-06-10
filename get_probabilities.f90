program get_probabilities
implicit none
integer, parameter :: ndis = 6
integer :: idis
real(8), dimension(ndis), parameter :: reac_dis = (/100e0, 50e0, 20e0, 10e0, 9e0,5e0/)
integer(8), dimension(ndis) :: suma, tmp
integer(8) :: total_traj

suma = 0
total_traj = 0
do
  read(*,*,end=999) tmp
  suma = suma + tmp
  total_traj = total_traj + 1
enddo
999 continue
write(*,*) 'Total # of trajs', total_traj
do idis=1, ndis
   write(*,'(A8,3X,F8.3,2X,A6,3X,F5.3)') 'Dis / A:',reac_dis(idis), 'prob:', dble(suma(idis))/dble(total_traj)
enddo
endprogram
