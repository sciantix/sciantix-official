# Simulation of the NESTOR-3 experiment
217 fuel pin (n110 here except for the FGR) irradiated in PHENIX reactor.
Fuel material: fertile pellet (hollow, UO2) at the bottom + longer fissile column (MOX, Pu/M = 22.43%)
Cladding material: austenitic stainless steel 15-15Ti, 100 NRT-dpa

Design parameters
Mean LHR at ppn: 25.7 kW/m
EOL peak burnup: 13.28 at.%
Pellet outer radius: 2.719 mm
Pellet EOL inner radius: 0.8 mm (~1.6 mm central hole diameter), NESTOR-3 measured value, D7.3
Radial gap 0.106 m              % not used
Grain diameter 10 um            % assumption
TD 94.72%TD                     % where TD = 11159 kg/m3  see notebook mox_crystal_properties.ipynb
U/M = 0.78
Pu/M = 0.22  --> q = 0.22
O/M = 1.975  --> -0.025 = initial fuel stoichiometry deviation (\) = 1.975 - 2

initial (Pu238, Pu239, Pu240, Pu241, Pu242) % of Pu atoms = (1.3, 60.4, 23.4, 10.4, 4.5) From the SUPERFACT-1 experiment (irradiated in Phenix)

--> Fuel density:
        rho = 0.9472 * 11159
            = 10569.8 kg/m3
--> Assuming 200 MeV released per fission, fission rate density:
        F_dot_V = LHR / (E_fission*A)
                = 3.45e19 fissions/m3/s
--> Irradiation time: 
--> Irradiation time:
        M_Pu = 0.013*238 + 0.604*239 + 0.234*240 + 0.104*241 + 0.045*242
            = 239.56 g/mol
        M_M = 0.78*238 + 0.22*239.56
            = 238.34 g/mol
        M_MOX = M_M + 1.975*15.999
            = 269.94 g/mol
        Heavy-metal atom density:
        N_M = rho / M_MOX * N_A
            = 10569.8 / 0.26994 * 6.022e23
            = 2.36e28 atoms/m3
        Total fission density at EOL:
        F_EOL = Burnup at EOL * N_M = 3.13e27 fissions/m3
        Irradiation time:
        t = F_EOL / F_dot_V
        = 3.13e27 / 3.45e19
        = 9.07e7 s
        = 1050 days
        = 25200 h

Source if not otherwise indicated: 
    INSPYRE D7.3-Results of the benchmark between pre-and post-INSPYRE code versions on selected experimental cases (Public)

## Simulation results

GERMINAL after 420 days of irradiation sees a decrease in temperature 
    linked to gap reopening, jog formation and therefore improved gap conductance
    after that the gap size follows the jog thickness evolution.
Central hole in the first days.
FCT 1500°C-2000°C with TRANSURANUS providing the highest temperature regimes. 
    --> Fuel temperature ranging 800 to 2200K
The fission gas release increases rapidly after the high-power cycles at the beginning 
    of irradiation and after approximately 330 days. 
EOL Pressure estimated by GERMINAL: 70 bar.
    --> Pressure ranging 1 to 70 bar

## Measured

Fission gas released @stp for Xe, Kr = 401 cm3 
EOL He volume @stp = 38 cm3
Fuel elongation = 0.4%
Clad elongation = 0.3%