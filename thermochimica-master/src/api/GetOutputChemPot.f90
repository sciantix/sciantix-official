
!-------------------------------------------------------------------------------
!
!> \file    GetOutputChemPot.f90
!> \brief   Get specific thermodynamic output.
!> \author  M.H.A. Piro
!> \date    Sept. 16, 2015
!
!
! Revisions:
! ==========
!
!   Date            Programmer          Description of change
!   ----            ----------          ---------------------
!   09/16/2015      M.H.A. Piro         Original code
!
!
! Purpose:
! ========
!
!> \details The purpose of this subroutine is to get specific thermodynamic
!! output from an equilibrium calculation.
!
!
!
! Pertinent variables:
! ====================
!
!> \param[inout]  cElementNameRequest   A three letter character string
!!                                       representing the name of a chemical
!!                                       element.
!> \param[out]    dElementChemPot       A double real scalar representing the
!!                                       chemical potential of this element.
!> \param[out]    INFO                  An integer scalar indicating a successful
!!                                       exit (== 0) or an error (/= 0).
!
!-------------------------------------------------------------------------------


subroutine GetOutputChemPot(cElementNameRequest, dElementChemPot, INFO)

    USE ModuleThermo
    USE ModuleThermoIO

    implicit none

    integer,      intent(out)   :: INFO
    integer                     :: i, j
    real(8),      intent(out)   :: dElementChemPot
    character(*), intent(in)    :: cElementNameRequest
    character(3)                :: cElementNameUse


    ! Initialize variables:
    INFO            = 0
    dElementChemPot = 0D0

    ! Only proceed if Thermochimica solved successfully:
    if (INFOThermo == 0) then

        ! Remove trailing blanks:
        cElementNameUse = cElementNameRequest
        cElementNameUse = TRIM(cElementNameUse)

        ! Loop through elements to find the one corresponding to the element
        ! being requested:
        j = 0
        LOOP_A: do i = 1, nElements
            if (cElementNameUse == cElementName(i)) then
                j = i
                exit LOOP_A
            end if
        end do LOOP_A

        ! Check to make sure that the element was found:
        if (j /= 0) then
            ! The element was found in the list.  Store the chemical potential
            ! of this element and convert back to units of J/g-at:
            dElementChemPot = dElementPotential(j) * dIdealConstant * dTemperature
        else
            ! This element was not found.  Report an error:
            INFO = 1
        end if

    else
        ! Record an error with INFO if INFOThermo /= 0.
        INFO = -1
    end if

    return

end subroutine GetOutputChemPot
