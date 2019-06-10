program make_approach_line
use xyzfiles
implicit none
integer, parameter :: nA = 4, nB = 2, nat = nA + nB
integer, parameter :: ngreater = 20 
integer :: i, igreater
real(8), dimension(nA*3) :: xyzA
real(8), dimension(nB*3) :: xyzB
real(8), dimension(nat,3) :: xyz, minxyz
real(8) :: dCO, dCO_prev
character(len=1), dimension(nA), parameter :: atnameA=(/'C','O','H','H'/)
character(len=1), dimension(nB), parameter :: atnameB=(/'O','H'/)
character(len=1), dimension(nat) :: atname
character(len=1) :: dumchar 
logical :: end_file

real(8),parameter :: autoA = 0.52917721092d0

igreater = 0
end_file = .FALSE.
dCO_prev = 1e6
do while (.not. end_file)
   ! Read geometry from file.
   call readxyz(5, nat, atname, xyz, end_file)

   ! Calculate dCO distance
   call COdis(xyz, dCO)

   ! If dCO is increasing add one to igreater
   if (dCO > dCO_prev) then
      ! It has been incresing for more than ngreater steps, so print approacing line
      if (igreater > ngreater) then
         ! Make approaching line:
		 do i=1,nA
            xyzA(3*(i-1)+1) = minxyz(i,1)
            xyzA(3*(i-1)+2) = minxyz(i,2)
            xyzA(3*(i-1)+3) = minxyz(i,3)
		 enddo

		 do i=1,nB
            xyzB(3*(i-1)+1) = minxyz(i+nA,1)
            xyzB(3*(i-1)+2) = minxyz(i+nA,2)
            xyzB(3*(i-1)+3) = minxyz(i+nA,3)
		 enddo
         call makeline(1, nA, nB, xyzA, xyzB, atnameA, atnameB)
		 end_file = .TRUE.
	  endif
      igreater = igreater + 1 
   else
	 minxyz = xyz
   endif
   dCO_prev = dCO
enddo
endprogram

subroutine makeline(id, nA, nB, xyzA, xyzB, atnameA, atnameB)
implicit none
integer :: i,j
integer, parameter :: npoints = 100
integer, intent(in) :: id, nA, nB
real(8), dimension(nA*3), intent(in) :: xyzA
real(8), dimension(nB*3), intent(in) :: xyzB
real(8), dimension((nA+nB)*3) :: xyz
character(len=1), dimension(nA), intent(in) :: atnameA
character(len=1), dimension(nB), intent(in) :: atnameB
character(len=15) :: file_name

real(8), dimension(3) :: cmA, cmB, R

real(8), parameter :: autoA = 0.52917721092e0
real(8), parameter :: factor = 5e-1

write(file_name,'(A8,I3.3,A4)') 'approach_', id,'.xyz'
open(100, file=file_name, status='replace')

call get_CM(nA, xyzA, atnameA, cmA)
call get_CM(nB, xyzB, atnameB, cmB)

R = cmB - cmA
call normalize(R)

do i=1,nA
   xyz(3*(i-1)+1) = xyzA(3*(i-1)+1) - cmA(1)
   xyz(3*(i-1)+2) = xyzA(3*(i-1)+2) - cmA(2)
   xyz(3*(i-1)+3) = xyzA(3*(i-1)+3) - cmA(3)
enddo

do i=1,npoints
   write(100,*) nA+nB
   write(100,*)
   do j=1,nA
      write(100,*) atnameA(j), xyz(3*(j-1)+1), xyz(3*(j-1)+2), xyz(3*(j-1)+3)
   enddo

   do j=1,nB
      xyz(3*((nA+j)-1)+1) = xyzB(3*(j-1)+1) - cmB(1) + factor * i * R(1)
      xyz(3*((nA+j)-1)+2) = xyzB(3*(j-1)+2) - cmB(2) + factor * i * R(2)
      xyz(3*((nA+j)-1)+3) = xyzB(3*(j-1)+3) - cmB(3) + factor * i * R(3)

      write(100,*) atnameB(j), xyz(3*((nA+j)-1)+1), xyz(3*((nA+j)-1)+2), xyz(3*((nA+j)-1)+3)
   enddo
enddo
endsubroutine

subroutine normalize(vec)
implicit none
integer :: i
real(8), dimension(3), intent(inout) :: vec
real(8) :: vec_norm

vec_norm = 0e0
do i=1,3
   vec_norm = vec_norm + vec(i) * vec(i)
enddo

vec_norm = sqrt(vec_norm)

vec = vec / vec_norm
endsubroutine

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
