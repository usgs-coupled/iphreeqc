module Subs
  integer    (kind=4), dimension(7) :: vt
  real       (kind=8), dimension(7) :: dv
  character (len=100), dimension(7) :: sv 
  integer                           :: Id
  contains
  
  subroutine ExtractWrite(cell)
    include "IPhreeqc.f90.inc"
    integer    (kind=4), intent(in) :: cell
    do j = 1, 7
      ! Headings are on row 0
      Ierr = GetSelectedOutputValue(Id,1,j,vt(j),dv(j),sv(j))
      if(Ierr .ne. IPQ_OK) call EHandler()
    enddo
    write(*,"(a,i2/2(5x,a,f7.2))") "Cell",cell,"pH:",dv(6),"SR(calcite):",dv(7) 
  end subroutine ExtractWrite
  
  subroutine EHandler()
    include "IPhreeqc.f90.inc"
    call OutputErrorString(Id)
    stop
  end subroutine EHandler    
end module Subs
program Advect
  use Subs
  include "IPhreeqc.f90.inc"
  character(len=1024) Istring 
  
!Create module, load database, define initial conditions and selected output
  Id = CreateIPhreeqc()
  if (LoadDatabase(Id, "phreeqc.dat") .ne. 0) call EHandler()
  If (RunFile(Id, "ic") .ne. 0) call EHandler()

!Run cell 1, extract/write result
  if (RunString(Id, "RUN_CELLS; -cells; 1; END") .ne. 0) call EHandler()
  call ExtractWrite(1)

!Advect cell 1 solution to cell 2, run cell 2, extract/write results
  Ierr = AccumulateLine(Id, "SOLUTION_MODIFY 2")    
  Ierr = AccumulateLine(Id, "   -cb      " // sv(1))
  Ierr = AccumulateLine(Id, "   -total_h " // sv(2))
  Ierr = AccumulateLine(Id, "   -total_o " // sv(3))
  Ierr = AccumulateLine(Id, "   -totals  ")
  Ierr = AccumulateLine(Id, "      C     " // sv(4))
  Ierr = AccumulateLine(Id, "      Ca    " // sv(5))
  Ierr = AccumulateLine(Id, "RUN_CELLS; -cells; 2; END")
  if (RunAccumulated(Id) .ne. 0) call EHandler()
  call ExtractWrite(2)
  
 !Destroy module 
  if (DestroyIPhreeqc(Id) .ne. 0) call EHandler()
end program Advect