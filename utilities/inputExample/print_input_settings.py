"""
This is a python script to write the SCIANTIX default input file "input_settings.txt".
@author G. Zullo

"""

with open('input_settings.txt', 'w') as file:
    file.write('1    #    iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))\n')
    file.write('1    #    iFissionGasDiffusivity (0= constant value, 1= Turnbull et al. (1988))\n')
    file.write('1    #    iDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis)\n')
    file.write('1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al. (2018))\n')
    file.write('1    #    iResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000), 3= thermal resolution, Cognini et al. (2021))\n')
    file.write('1    #    iTrappingRate (0= constant value, 1= Ham (1958))\n')
    file.write('1    #    iNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))\n')
    file.write('1    #    iOutput (1= default output files)\n')
    file.write('1    #    iGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= White (2004))\n')
    file.write('1    #    iGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))\n')
    file.write('1    #    iGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017))\n')
    file.write('0    #    iFuelMatrix (0= UO2, 1= UO2 + HBS)\n')
    file.write('0    #    iGrainBoundaryVenting (0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE)\n')
    file.write('0    #    iRadioactiveFissionGas (0= not considered)\n')
    file.write('0    #    iHelium (0= not considered)\n')
    file.write('0    #    iHeDiffusivity (0= null value, 1= limited lattice damage, Luzzi et al. (2018), 2= significant lattice damage, Luzzi et al. (2018))\n')
    file.write('0    #    iGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)\n')
    file.write('0    #    iHighBurnupStructureFormation (0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020))\n')
    file.write('0    #    iHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)\n')
    file.write('0    #    iHeliumProductionRate (0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR))\n')
    file.write('0    #    iStoichiometryDeviation (0= not considered, 1= Cox et al. 1986, 2= Bittel et al. 1969, 3= Abrefah et al. 1994, 4= Imamura et al. 1997, 5= Langmuir-based approach)\n')
    file.write('0    #    iBubbleDiffusivity (0= not considered, 1= volume diffusivity)')
