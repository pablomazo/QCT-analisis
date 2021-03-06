program make_approach_line
use xyzfiles
use io
implicit none
integer :: i
real(8), dimension(:), allocatable :: xyzA, xyzB
real(8), dimension(:,:), allocatable :: xyz
character(len=1), dimension(:), allocatable :: atname
character(len=1) :: dumchar 
logical :: end_file

real(8),parameter :: autoA = 0.52917721092d0

call read_input

! Allocate memory
allocate(xyzA(nA*3), xyzB(nB*3), xyz(nat,3), atname(nat))

! Open file with geometry.
open(10, file=file_name, status='old')

! Read file with whole geometry from standard input.
call readxyz(10, nat, atname, xyz, end_file)

! Make approaching line:
do i=1,nA
   xyzA(3*(i-1)+1) = xyz(i,1)
   xyzA(3*(i-1)+2) = xyz(i,2)
   xyzA(3*(i-1)+3) = xyz(i,3)
enddo

do i=1,nB
   xyzB(3*(i-1)+1) = xyz(i+nA,1)
   xyzB(3*(i-1)+2) = xyz(i+nA,2)
   xyzB(3*(i-1)+3) = xyz(i+nA,3)
enddo
call makeline(1, nA, nB, xyzA, xyzB, atnameA, atnameB)
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
