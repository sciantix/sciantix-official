

    !-------------------------------------------------------------------------------------------------------------
    !
    !> \file    ParseCSDataBlock.f90
    !> \brief   Parse the data block section of a ChemSage data-file.
    !> \author  M.H.A. Piro
    !> \date    Mar. 4, 2018
    !> \sa      ParseCSDataFile.f90
    !> \sa      ParseCSDataBlockGibbs.f90
    !> \sa      ParseCSDataBlockQKTO.f90
    !> \sa      ParseCSDataBlockRKMP.f90
    !> \sa      ParseCSDataBlockSUBL.f90
    !
    !
    ! DISCLAIMER
    ! ==========
    !
    ! All of the programming herein is original unless otherwise specified and is completely
    ! independent of ChemApp and related products, including Solgas, Solgasmix, Fact, FactSage
    ! and ChemSage.
    !
    !
    ! Revisions:
    ! ==========
    !
    !   Date            Programmer      Description of change
    !   ----            ----------      ---------------------
    !   10/06/2011      M.H.A. Piro     Original code
    !   12/20/2012      M.H.A. Piro     Check that the solution phase type is supported.
    !   03/04/2018      M.H.A. Piro     Added capability to handle SUBG phases.
    !
    !
    ! Purpose:
    ! ========
    !
    !> \details The purpose of this subroutine is to parse the "data block" section of a ChemSage data-file.
    !! The "data block" section of this data-file contains the thermodynamic data of all species and
    !! phases in the system, including the stoichiometry coefficients of all species and coefficients
    !! of the standard Gibbs energy equations.
    !!
    !! First, all solution phases are parsed, then all pure condensed phases and finally the fictive phases
    !! are parsed.  Within the solution phase loop, the solution phase name and type are first parsed, then
    !! the Gibbs energy equations of the pure species, and finally the mixing parameters are parsed.  Since
    !! the excess mixing parameters are model dependent, individual subroutines are assigned for each solution
    !! phase model.
    !
    !
    ! Pertinent variables:
    ! ====================
    !
    ! INFO                      A scalar integer that indicates a successful exit or identifies an error.
    ! nSpeciesCS                Number of species in the system (combined solution species and pure
    !                            separate phases).
    ! nGibbsEqSpecies           Number of Gibbs energy equations for a particular species.
    ! iSpeciesAtomsCS           Integer matrix representing the number of atoms of a particular
    !                            elements in a species.
    ! iParticlesPerMoleCS       An integer vector containing the number of particles per mole of the
    !                            constituent species formula mass.  The default value is 1.
    ! cSolnPhaseNameCS          The name of a solution phase.
    ! cSolnPhaseTypeCS          The type of a solution phase.
    ! cSolnPhaseTypeSupport     A character array representing solution phase types that are supported.
    !
    !-------------------------------------------------------------------------------------------------------------


subroutine ParseCSDataBlock

    USE ModuleParseCS

    implicit none

    integer               :: i, j, k, iCounterGibbsEqn
    real(8)               :: dDummy
    real(8),dimension(15) :: dTempVec


    ! Intialize variables:
    iCounterGibbsEqn    = 0
    iParticlesPerMoleCS = 1
    nParamCS            = 0
    nMagParamCS         = 0
    dTempVec            = 0D0
    nParamPhaseCS       = 0
    nMagParamPhaseCS    = 0
    dGibbsMagneticCS    = 0D0
    iMiscSUBI           = 0

    ! Loop through all solution phases:
    LOOP_SolnPhases: do i = 1, nSolnPhasesSysCS

        ! Entry 1: Read name of phase:
        read (1,111,IOSTAT = INFO) cSolnPhaseNameCS(i)
        111 FORMAT (A25)
        cSolnPhaseNameCS(i) = TRIM(ADJUSTL(cSolnPhaseNameCS(i)))

        if (INFO /= 0) then
            INFO = 1100 + i
            return
        end if

        ! Entry 2: Read model name (i.e., type of solution phase):
        read (1,*,IOSTAT = INFO) cSolnPhaseTypeCS(i)

        if (INFO /= 0) then
            INFO = 1200 + i
            return
        end if

        ! Check to make sure that this solution phase type is supported:
        INFO = 123
        LOOP_SolnTypeSupport: do j = 1, nSolnTypeSupport
            if (cSolnPhaseTypeCS(i) == cSolnPhaseTypeSupport(j)) then
                ! This solution phase type is supported:
                INFO = 0
                exit LOOP_SolnTypeSupport
            end if
        end do LOOP_SolnTypeSupport

        ! Report an error if this solution phase type is not supported and return:
        if (INFO == 123) then
            INFO = 17
            return
        end if

        ! Count sublattice phases
        if ((cSolnPhaseTypeCS(i) == 'SUBL').OR.(cSolnPhaseTypeCS(i) == 'SUBLM').OR. &
             (cSolnPhaseTypeCS(i) == 'SUBG').OR.(cSolnPhaseTypeCS(i) == 'SUBQ').OR. &
             (cSolnPhaseTypeCS(i) == 'SUBI').OR.(cSolnPhaseTypeCS(i) == 'SUBM')) then
             nCountSublatticeCS = nCountSublatticeCS + 1
             iPhaseSublatticeCS(i) = nCountSublatticeCS
        end if

        ! Check if the solution phase contains magnetic ordering terms, and if so,
        ! read in the terms:
        if ((cSolnPhaseTypeCS(i) == 'RKMPM').OR.(cSolnPhaseTypeCS(i) == 'SUBLM')) then
            ! Magnetic ordering terms are present.

            read (1,*,IOSTAT = INFO) dTempVec(1:2)
            do j = nSpeciesPhaseCS(i-1) + 1, nSpeciesPhaseCS(i)
                dGibbsMagneticCS(j,3:4) = dTempVec(1:2)
            end do

            ! Record an error if necessesary:
            if (INFO /= 0) then
                INFO = 1100 + i
                return
            end if

        elseif (cSolnPhaseTypeCS(i) == 'SUBG') then

            ! Read zeta (FNN/SNN ratio)
            ! In SUBG zeta is the same for every FNN pair
            ! NOTE: This method for keeping track of zeta is not very safe,
            ! relies on having a predictable FNN pair order in the dat file
            read (1,*,IOSTAT = INFO) dDummy
            do j = 1, nMaxSpeciesPhaseCS
                dZetaSpeciesCS(nCountSublatticeCS,j) = dDummy
            end do

            ! Read in two integers representing the number of species and the number of pairs:
            read (1,*,IOSTAT = INFO) nPairsSROCS(nCountSublatticeCS,1:2)

            if (INFO /= 0) then
              INFO = 1600 + i;
              return
            end if

        elseif (cSolnPhaseTypeCS(i) == 'SUBQ') then
            ! Do I need to do this?
            ! The SUBQ phase data files seems to not have the magnetic term so skipping this part.

            ! Read in two integers representing the number of species and the number of pairs:
            read (1,*,IOSTAT = INFO) nPairsSROCS(nCountSublatticeCS,1:2)

        end if

        ! Loop through species in solution phase:
        LOOP_SpeciesInSolnPhase: do j = nSpeciesPhaseCS(i-1) + 1, nSpeciesPhaseCS(i)

            ! SUBG and SUBQ phases contain a certain number of species, which are necessarily less
            ! than the number of pair fractions. The # of species indicated in the
            ! header file actually represents the number of pairs. Therefore, there are
            ! fewer species listed than what has been allocated.
            if (cSolnPhaseTypeCS(i) == 'SUBG' .OR. cSolnPhaseTypeCS(i) == 'SUBQ') then
                if (j >= nSpeciesPhaseCS(i-1) + 1 + nPairsSROCS(nCountSublatticeCS,1) ) then
                    exit LOOP_SpeciesInSolnPhase
                end if
            end if

            ! Store the magnetic ordering terms for each solution:
            k = nSpeciesPhaseCS(i-1) + 1
            dGibbsMagneticCS(j,3:4) = dGibbsMagneticCS(k,3:4)

            ! Store the phase index corresponding to the current species:
            iPhaseCS(j) = i

            if ((cSolnPhaseTypeCS(i) == 'SUBG') .OR. (cSolnPhaseTypeCS(i) == 'SUBQ')) then
                ! The following subroutine parses the Gibbs energy equations (entries 3-5):
                call ParseCSDataBlockGibbs(i,j,iCounterGibbsEqn)

                ! Get pair stoichiometry in terms of constituents
                read (1,*,IOSTAT = INFO) dConstituentCoefficientsCS(nCountSublatticeCS,j - nSpeciesPhaseCS(i-1),1:5)

                if (cSolnPhaseTypeCS(i) == 'SUBQ') then
                    ! Read zeta (FNN/SNN ratio)
                    ! In SUBQ zeta can differ for each FNN pair
                    read (1,*,IOSTAT = INFO) dZetaSpeciesCS(nCountSublatticeCS,j - nSpeciesPhaseCS(i-1))
                end if
            else if (cSolnPhaseTypeCS(i) == 'SUBM') then
                ! The following subroutine parses the Gibbs energy equations (entries 3-5):
                call ParseCSDataBlockGibbs(i,j,iCounterGibbsEqn)

                ! Get pair stoichiometry in terms of constituents
                read (1,*,IOSTAT = INFO) dConstituentCoefficientsCS(nCountSublatticeCS,j - nSpeciesPhaseCS(i-1),1:2)
            else
                ! The following subroutine parses the Gibbs energy equations (entries 3-5):
                call ParseCSDataBlockGibbs(i,j,iCounterGibbsEqn)
            endif

            ! Mole/equivalent fraction conversion and group number:
            if (cSolnPhaseTypeCS(i) == 'QKTO') then
                read (1,*,IOSTAT = INFO) dQKTOParamsCS(j,1:2)
            end if

            if (INFO /= 0) return

        end do LOOP_SpeciesInSolnPhase

        ! Check the type of solution phase to interpret mixing parameters:
        select case (cSolnPhaseTypeCS(i))
            ! Ideal mixture model
            case ('IDMX')
                ! Do nothing.
            ! Quasichemical Kohler-Toop model
            case ('QKTO')
                call ParseCSDataBlockQKTO(i)
            ! Redlich-Kister-Muggiano-Polynomial model
            case ('RKMP', 'RKMPM')
                call ParseCSDataBlockRKMP(i)
            ! Compound Energy Formalism (sublattice) model:
            case ('SUBL', 'SUBLM')
                call ParseCSDataBlockSUBL(i)
            ! Quadruplet quasichemical model:
            case ('SUBG')
                ! Parse the data-block section for SUBG phases:
                call ParseCSDataBlockSUBG(i)
            ! Quadruplet quasichemical model:
            case ('SUBQ')
                ! Parse the data-block section for SUBQ phases:
                call ParseCSDataBlockSUBG(i)
            ! Ionic Liquid Model:
            case ('SUBI')
                call ParseCSDataBlockSUBI(i)
            ! Two-sublattice Model:
            case ('SUBM')
                call ParseCSDataBlockSUBM(i)
            case default
                ! The solution phase type is not supported. Report an error.
                INFO = 17
                return
        end select ! End checking the type of solution phase.

        ! Return if an error has been recorded:
        if (INFO /= 0) return

        ! Record the index of the mixing parameter for this phase:
        nParamPhaseCS(i) = nParamCS
        nMagParamPhaseCS(i) = nMagParamCS

    end do LOOP_SolnPhases      ! Variable i

    allocate(iParamPassCS(nParamCS),iMagParamPassCS(nMagParamCS))
    iParamPassCS = 0
    iMagParamPassCS = 0

    ! Begin parsing pure condensed phases:
    LOOP_PureConPhase: do j = nSpeciesPhaseCS(nSolnPhasesSysCS) + 1, nSpeciesCS
        ! The phase index of a pure separate phase is set to zero:
        iPhaseCS(j) = 0

        call ParseCSDataBlockGibbs(iPhaseCS(j),j,iCounterGibbsEqn)

        if (INFO /= 0) exit LOOP_PureConPhase

    end do LOOP_PureConPhase

    return

end subroutine ParseCSDataBlock
