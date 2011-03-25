    implicit none
    include "IPhreeqc.f90.inc"
    integer Errors, i, iresult, vtype
    character(len=1024) Istring
    character(len=30) svalue
    double precision, dimension(0:6) :: dvalue 

    if (LoadDatabase("phreeqc.dat") .ne. 0) call ErrorMessage(Errors)
    ! Initial conditions
    write(Istring,"(a/)") &
        "Solution 1-2; END", &
        "EQUILIBRIUM_PHASES 1; CO2(g) -1.5 10", &
        "EQUILIBRIUM_PHASES 2; Calcite 0   10", &
        "END"
     Errors = AccumulateLine(Istring)
     If (Run(.FALSE.,.FALSE.,.FALSE.,.FALSE.) .ne. 0) call ErrorMessage(Errors)
    !If (runstring(Istring, .FALSE.,.FALSE.,.FALSE.,.FALSE.) .ne. 0) call ErrorMessage(Errors)

    ! Define SELECTED_OUTPUT and run cell 1
    write(Istring,"(a/)") &
        "SELECTED_OUTPUT; -reset false", &
        "USER_PUNCH", &
        "   -Heading  charge    H   O   C   Ca  pH  SR(calcite)", &
        "10 w = TOT(""water"")", &
        "20 PUNCH charge_balance, TOT(""H"")*w, TOT(""O"")*w, TOT(""C"")*w, TOT(""Ca"")*w", &
        "30 PUNCH -LA(""H+""), SR(""calcite"")", &
        "RUN_CELLS; -cells; 1; END"
    Errors = AccumulateLine(Istring)    
    if (Run(.FALSE.,.FALSE.,.FALSE.,.FALSE.) .ne. 0) call ErrorMessage(Errors)
    !call WriteResults(1, Ostring, GetSelectedOutputArray())

    ! Transfer to cell 2 and run cell 2
    do i = 0, 6
        iresult = GetSelectedOutputValue(1, i, vtype, dvalue(i), svalue)
    enddo
    write(Istring, "(a/3(a,e20.14/),a/2(a,e20.14/),a)") &
        "SOLUTION_MODIFY 2",                          &
        "   -cb      ", dvalue(0), &
        "   -total_h ", dvalue(1), &
        "   -total_o ", dvalue(2), &
        "   -totals ",             &
        "      C     ", dvalue(3), &
        "      Ca    ", dvalue(4), &
        "RUN_CELLS; -cells; 2; END"
    Errors = AccumulateLine(Istring)     
    if (Run(.FALSE.,.FALSE.,.FALSE.,.FALSE.) .ne. 0) call ErrorMessage(Errors)
    !call WriteResults(2, Ostring, GetSelectedOutputArray())

    end
    
    subroutine ErrorMessage(Errors)
    integer Errors
    write(*,*) "Errors in PHREEQC module: ", Errors
    end subroutine ErrorMessage
    
    subroutine WriteResults(cell)
    integer cell
    double precision ph, sr
    ph = GetSelectedOutputValue(1, 5)
    sr = GetSelectedOutputValue(1, 6)
    write(*,*) "Cell: ", cell 
    write(*,*) "    pH: ", pH, "    SR(calcite): ", sr
    End subroutine WriteResults
