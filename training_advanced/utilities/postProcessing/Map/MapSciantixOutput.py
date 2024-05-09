import subprocess
import os
import re
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import shutil

class sciantix_simulation():
    def __init__(self):
        pass

    def set_script(self):
       self.path = os.getcwd()

    def writeInputSettings(self, value):
        with open('input_settings.txt', 'w') as file:
            file.write('1\t#\tiGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))\n')
            file.write('1\t#\tiFGDiffusionCoefficient (0= constant value, 1= Turnbull et al. (1988))\n')
            file.write('1\t#\tiDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis)\n')
            file.write('1\t#\tiIntraGranularBubbleEvolution (1= Pizzocri et al. (2018))\n')
            file.write('1\t#\tiResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000), 3= thermal resolution, Cognini et al. (2021))\n')
            file.write('1\t#\tiTrappingRate (0= constant value, 1= Ham (1958))\n')
            file.write('1\t#\tiNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))\n')
            file.write('1\t#\tiOutput (1= default output files)\n')
            file.write('2\t#\tiGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= White (2004))\n')
            file.write('1\t#\tiGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))\n')
            file.write('1\t#\tiGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017))\n')
            file.write('0\t#\tiFuelMatrix (0= UO2, 1= UO2 + HBS)\n')
            file.write(f'{value}\t#\tiGrainBoundaryVenting (0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE)\n')
            file.write('0\t#\tiRadioactiveFissionGas (0= not considered)\n')
            file.write('0\t#\tiHelium (0= not considered)\n')
            file.write('0\t#\tiHeDiffusivity (0= null value, 1= limited lattice damage, Luzzi et al. (2018), 2= significant lattice damage, Luzzi et al. (2018))\n')
            file.write('0\t#\tiGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)\n')
            file.write('0\t#\tiHighBurnupStructureFormation (0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020))\n')
            file.write('0\t#\tiHBS_FGDiffusionCoefficient (0= constant value)\n')
            file.write('0\t#\tiHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)\n')
            file.write('0\t#\tiHeliumProductionRate (0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR))\n')
            file.write('0\t#\tiStoichiometryDeviation (0= not considered, 1= Cox et al. 1986, 2= Bittel et al. 1969, 3= Abrefah et al. 1994, 4= Imamura et al. 1997, 5= Langmuir-based approach)\n')
            file.write('0\t#\tiBubbleDiffusivity (0= not considered, 1= volume diffusivity)')

    def writeInputHistory(self, t_0, t_end, T, F, sigma_hyd):
        input_history_1 = [t_0  ,'\t', T,'\t', F,'\t', sigma_hyd, '\n']
        input_history_2 = [t_end,'\t', T,'\t', F,'\t', sigma_hyd]
        result_string1 = ''.join(str(item) for item in input_history_1)
        result_string2 = ''.join(str(item) for item in input_history_2)

        history_path = self.path + '/input_history.txt'

        with open(history_path, 'w') as file:
            file.write(result_string1)
            file.write(result_string2)

        print(f"..writing input_history.txt:\n")
        print(result_string1)
        print(result_string2)
        print("-------------------------------")
        
    def run_sciantix(self, t_0, t_end, T_0, T_end, F, sigma_hyd, step, variable_name, mapped_value):

        shutil.copy("../../../bin/sciantix.x", os.getcwd())

        threshold_temperature = np.empty(shape=(1,1), dtype=float)
        threshold_burnup = np.empty(shape=(1,1), dtype=float)

        # loop over temperature values
        T = T_0
        for T in range(T_0, T_end, step):
            print(f"Test at temperature {T}\n")
            self.writeInputHistory(t_0, t_end, T, F, sigma_hyd)
            subprocess.run(self.path + "/sciantix.x")

            # finding calculated sciantix variable
            data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
            i,j = np.where(data == variable_name)
            values = np.array(data[1:, j], dtype=float)

            # looking for value to map
            # i,j = np.where(values == mapped_value)
            i,j = np.where(np.isclose(values, mapped_value, atol=mapped_value/10))
            i = np.array(i, dtype=int)

            try:
                threshold = np.min(i)
                print("Threshold found!")

                i,j = np.where(data == 'Temperature (K)')
                values = np.array(data[1:, j], dtype=float)

                threshold_temperature = np.append(threshold_temperature, values[threshold])

                i,j = np.where(data == 'Burnup (MWd/kgUO2)')
                values = np.array(data[1:, j], dtype=float)

                threshold_burnup = np.append(threshold_burnup, values[threshold])

            except:
                print("WARNING: Threshold not found!")

            print("\n")

        print(f"Threshold temperature = {threshold_temperature[1:]}")
        print(f"Threshold burnup = {threshold_burnup[1:]}")
    
        return threshold_temperature[1:], threshold_burnup[1:]

# beginning of the mapping script
map_saturation_fractional_coverage = sciantix_simulation()
map_saturation_fractional_coverage.set_script()

# irradiation parameters
temperature_step = 50
final_time = 50000
fission_rate = 3e18
sigma_hyd = 0

# name="Intergranular fractional coverage (/)"
name = "Fission gas release (/)"
fgr_mapp = 0.02

# map - no athermal release
map_saturation_fractional_coverage.writeInputSettings(0)
temperature0, burnup0 = map_saturation_fractional_coverage.run_sciantix(0, final_time, 650, 1800, fission_rate, sigma_hyd, step=temperature_step, variable_name=name, mapped_value=fgr_mapp)

# map - no athermal release (vented fraction)
map_saturation_fractional_coverage.writeInputSettings(1)
temperature1, burnup1 = map_saturation_fractional_coverage.run_sciantix(0, final_time, 650, 1800, fission_rate, sigma_hyd, step=temperature_step, variable_name=name, mapped_value=fgr_mapp)

# map - athermal release from open porosity (claisse)
map_saturation_fractional_coverage.writeInputSettings(2)
temperature2, burnup2 = map_saturation_fractional_coverage.run_sciantix(0, final_time, 650, 1800, fission_rate, sigma_hyd, step=temperature_step, variable_name=name, mapped_value=fgr_mapp)

# map - athermal release from open porosity (neural network)
map_saturation_fractional_coverage.writeInputSettings(3)
temperature3, burnup3 = map_saturation_fractional_coverage.run_sciantix(0, final_time, 650, 1800, fission_rate, sigma_hyd, step=temperature_step, variable_name=name, mapped_value=fgr_mapp)

# Map: Haleden threshold
burnup = np.linspace(0.4, 50, 100)

plt.plot(burnup, 9800 / np.log(burnup / 0.005), label = 'Halden threshold')

plt.plot(burnup0, temperature0, label = 'w/o athermal release')
plt.plot(burnup1, temperature1, label = 'w/ vented fraction')
plt.plot(burnup2, temperature2, label = 'w/ athermal release - Claisse')
plt.plot(burnup3, temperature3, label = 'w/ athermal release - ann')

# plt.xlim([1, 16])
# plt.ylim([1200, 1800])

plt.legend()

plt.show()
