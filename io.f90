module io
implicit none
integer :: nA, nB, nat
character(len=1), dimension(:), allocatable :: atnameA, atnameB
character(len=50) :: file_name

contains

subroutine read_input
implicit none
namelist/atoms/nA, nB
namelist/system_parameters/ atnameA, atnameB, file_name

read(*,nml=atoms)
nat = nA + nB

allocate(atnameA(nA), atnameB(nB))
read(*,nml=system_parameters)

endsubroutine
endmodule
