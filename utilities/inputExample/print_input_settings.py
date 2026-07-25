"""
This is a python script to write the SCIANTIX default input file "input_settings.txt".
@author G. Zullo

Two constraints are enforced by src/file_manager/InputReading.C and are checked below:

1. The settings are positional: they are written in the order in which InputReading.C
   fills Sciantix_options[0..24]. Adding, removing or reordering an entry here without
   doing the same there silently shifts every following option.

2. ReadOneSetting() skips the inline comment with ignore(256, '\\n'), so the text after
   '#' must stay shorter than 256 characters. A longer comment leaves its tail in the
   stream, the next extraction fails, and every remaining option is silently read as 0.
"""

# ReadOneSetting() skips at most 256 characters after '#'; keep a margin.
MAX_COMMENT_LENGTH = 240

# (default value, option name, available models)
SETTINGS = [
    (1, 'iGrainGrowth',
     '0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013)'),
    (1, 'iFissionGasDiffusivity',
     '0= constant value, 1= Turnbull (1988), 2= Matzke (1980), 3= Turnbull (2010), 4= Ronchi (2007), '
     '5= UO2-HBS, 6= stoichiometry-dependent, 7-8-10= UO2-Cr, 9= Cooper (2021), 90= GPR-updated, 99= null value'),
    (1, 'iDiffusionSolver',
     '1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis, '
     '3= SDA with three coupled equations (UO2 + HBS)'),
    (1, 'iIntraGranularBubbleBehavior',
     '0= constant concentration and radius, 1= Pizzocri et al. (2018), 2= White and Tucker (1983), '
     '3= annealing / helium similarity ratio, 99= no intragranular bubbles'),
    (1, 'iResolutionRate',
     '0= constant value, 1= Turnbull (1971), 2= Losonen (2002), '
     '3= thermal resolution, Cognini et al. (2021), 99= null value'),
    (1, 'iTrappingRate',
     '0= constant value, 1= Ham (1958), 99= null value'),
    (1, 'iNucleationRate',
     '0= constant value, 1= Olander, Wongsawaeng (2006), 99= null value'),
    (1, 'iOutput',
     '1= default output files, 2= output with all the variables'),
    (1, 'iGrainBoundaryVacancyDiffusivity',
     '0= constant value, 1= Reynolds and Burton (1979), 2= White (2004), 5= HBS case, Barani et al. (2022)'),
    (1, 'iGrainBoundaryBehaviour',
     '0= no grain boundary bubbles, 1= Pastore et al. (2013)'),
    (1, 'iGrainBoundaryMicroCracking',
     '0= no model considered, 1= Barani et al. (2017), 2= Cappellari et al. (2025)'),
    (0, 'iFuelMatrix',
     '0= UO2, 1= UO2 + HBS'),
    (0, 'iGrainBoundaryVenting',
     '0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE, '
     '2= Claisse and Van Uffelen (2015), 3= Pagani et al. (2025)'),
    (0, 'iRadioactiveFissionGas',
     '0= not considered, 1= considered (Xe133, Kr85m)'),
    (0, 'iHelium',
     '0= not considered, 1= considered'),
    (0, 'iHeDiffusivity',
     '0= constant value, 1= limited lattice damage, Luzzi et al. (2018), '
     '2= significant lattice damage, Luzzi et al. (2018), 3= Talip et al. (2014), 99= null value'),
    (0, 'iGrainBoundarySweeping',
     '0= no model considered, 1= TRANSURANUS swept volume model'),
    (0, 'iHighBurnupStructureFormation',
     '0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020)'),
    (0, 'iHighBurnupStructurePorosity',
     '0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data'),
    (0, 'iHeliumProductionRate',
     '0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR), '
     '3= constant production rate'),
    (0, 'iStoichiometryDeviation',
     '0= not considered, 1= Cox et al. (1986), 2= Bittel et al. (1969), 3= Abrefah et al. (1994), '
     '4= Imamura and Une (1997), 5= Langmuir-based approach, Massih (2018), 6= as 5, with fixed S/V'),
    (0, 'iBubbleDiffusivity',
     '0= not considered, 1= volume diffusivity'),
    (0, 'iChromiumSolubility',
     '0= Riglet-Martial et al. (2014), 1= optimised coefficients'),
    (0, 'iDensification',
     '0= not considered, 1= fit from Van Uffelen, P. (2002), PhD thesis'),
    (0, 'iReleaseMode',
     '0= White (2004) coalescence, saturation threshold from Pastore et al. (2013), '
     '1= Pastore et al. (2013) coalescence, Cappellari et al. (2025) release, '
     '2= White (2004) coalescence, Cappellari et al. (2025) release'),
]

lines = [f'{value}    #    {name} ({models})\n' for value, name, models in SETTINGS]

# Refuse to write a file the parser would misread (see note 2 in the docstring).
for line in lines:
    comment = line[line.index('#') + 1:].rstrip('\n')
    if len(comment) > MAX_COMMENT_LENGTH:
        raise ValueError(
            f'comment is {len(comment)} characters, above the {MAX_COMMENT_LENGTH} limit '
            f'imposed by ReadOneSetting(): {line.split()[2]}'
        )

with open('input_settings.txt', 'w') as file:
    file.writelines(lines)
