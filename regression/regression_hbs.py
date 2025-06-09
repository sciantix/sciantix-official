"""

This is a python script to execute the regression (running the validation database) of sciantix.

@author G. Zullo

"""

""" ------------------- Import requiered depedencies ------------------- """
import os
import subprocess
import numpy as np
import matplotlib.pyplot as plt
import shutil
from regression_functions import *

""" ------------------- Global Variables ------------------- """
# Plot configuration
font_size = 11

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size
""" ------------------- Functions ------------------- """

def do_plot(label_x, label_y, experimental_data_x, experimental_data_y, calculations_x, calculations_y, model_x=None, model_y=None):

    # 1st plot
    fig, ax = plt.subplots(1,1)

    ax.set_xlabel(label_x)
    ax.set_ylabel(label_y)
    ax.plot(experimental_data_x, experimental_data_y, 'o', color = '#B3B3B3', label='Experimental')
    ax.plot(calculations_x, calculations_y, color = 'green', linewidth = 1, label='SCIANTIX 2.0')
    if model_x is not None and model_y is not None:
        ax.plot(model_x, model_y, '-', color='blue', label='Barani (2022)')
    ax.set_frame_on(True)
    ax.legend()
    ax.legend(loc='best')

    plt.show()


# Main function of the Contact regression
def regression_hbs(wpath, mode_HBS, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

    # Exit of the function without doing anything
    if mode_HBS == 0:
        return folderList, number_of_tests, number_of_tests_failed


    files_and_dirs = os.listdir(wpath)
    sorted_files_and_dirs = sorted(files_and_dirs)

    for file in sorted_files_and_dirs:
        if "HBS" in file and os.path.isdir(file):
            folderList.append(file)
            os.chdir(file)
            print(f"Now in folder {file}...")
            number_of_tests += 1

            if mode_gold == 0:
                do_sciantix_only()
                data, data_gold = check_output(file)
                number_of_tests_failed = check_result(number_of_tests_failed)

            elif mode_gold == 1:
                do_sciantix_only()
                data, data_gold = check_output(file)
                print("...golding results.")
                do_gold()

            elif mode_gold == 2:
                data, data_gold = check_output(file)
                number_of_tests_failed = check_result(number_of_tests_failed)

            elif mode_gold == 3:
                data, data_gold = check_output(file)
                print("...golding existing results.")
                do_gold()

            exp_pore_density = import_data("exp_pore_density.txt").astype(float)
            exp_pore_density_2 = import_data("exp_pore_density_2.txt").astype(float)
            exp_pore_density_3 = import_data("exp_pore_density_3.txt").astype(float)
            exp_pore_density_4 = import_data("exp_pore_density_4.txt").astype(float)

            exp_porosity = import_data("exp_porosity.txt").astype(float)
            exp_porosity_2 = import_data("exp_porosity_2.txt").astype(float)
            exp_porosity_3 = import_data("exp_porosity_3.txt").astype(float)
            exp_porosity_4 = import_data("exp_porosity_4.txt").astype(float)
            exp_porosity_5 = import_data("exp_porosity_5.txt").astype(float)
            exp_porosity_6 = import_data("exp_porosity_6.txt").astype(float)
            

            exp_pore_radius = import_data("exp_pore_radius.txt").astype(float)

            model_data = np.genfromtxt('Barani_pore_density.txt')
            model_bu = model_data[:, 0]
            model_density = model_data[:, 1]
            old_model_data = np.genfromtxt('SCIANTIX_2.0_density.txt')
            old_model_bu = old_model_data[:, 0]
            old_model_density = old_model_data[:, 1]
            D_Barani_Np = np.genfromtxt('Np_D_Barani.txt')
            Barani_bu_Np = D_Barani_Np[:,0]
            Barani_density_Np = D_Barani_Np[:,1]
            D_Barani_Porosity = np.genfromtxt('Porosity_D_Barani.txt')
            Barani_bu_P = D_Barani_Porosity[:,0]
            Barani_P_P = D_Barani_Porosity[:,1]
            D_Barani_R= np.genfromtxt('R_D_Barani.txt')
            Barani_bu_R = D_Barani_R[:,0]
            Barani_R_R = D_Barani_R[:,1]
            D_White_Np = np.genfromtxt('Np_D_White.txt')
            White_bu_Np = D_White_Np[:,0]
            White_density_Np = D_White_Np[:,1]
            D_White_Porosity = np.genfromtxt('Porosity_D_White.txt')
            White_bu_P = D_White_Porosity[:,0]
            White_P_P = D_White_Porosity[:,1]
            D_White_R = np.genfromtxt('R_D_White.txt')
            White_bu_R = D_White_R[:,0]
            White_R_R = D_White_R[:,1]


            burnupPos = findSciantixVariablePosition(data, "Burnup (MWd/kgUO2)")
            poreDensityPos = findSciantixVariablePosition(data, "HBS pore density (pores/m3)")
            porosityPos = findSciantixVariablePosition(data, "HBS porosity (/)")
            poreRadiusPos = findSciantixVariablePosition(data, "HBS pore radius (m)")

            burnup = data[1:,burnupPos].astype(float) / 0.8814
            poreDensity = data[1:,poreDensityPos].astype(float)
            porosity = data[1:,porosityPos].astype(float)
            poreRadius = data[1:,poreRadiusPos].astype(float)

            # sciantix dictionary
            sd = sciantix_dictionary('output.txt')

            # Check if the user has chosen to display the various plots
            if mode_plot == 1:

                do_plot(
                    'Effective burnup (MWd kgHM${}^{-1}$)',
                    'Pore number density (pores m${}^{-3}$)',
                    exp_pore_density[:,0],
                    exp_pore_density[:,1],
                    burnup,
                    poreDensity,
                    model_bu,
                    model_density
                )
                
                fig, ax = plt.subplots()
                ax.set_xlabel('Effective burnup (MWd kgHM${}^{-1}$)')
                ax.set_ylabel('Pore number density (pores m${}^{-3}$)')
                ax.plot(exp_pore_density[:,0], exp_pore_density[:,1], 'o', color='#B3B3B3', markersize=6, label='Cappia (2016)')
                ax.plot(exp_pore_density_2[:,0], exp_pore_density_2[:,1], 's', color='red', markersize=5, label='Spino (2006), $Bu_{AV}= 67$ MWd/kgU')
                ax.plot(exp_pore_density_3[:,0], exp_pore_density_3[:,1], 's', color='green', markersize=5, label='Spino (2006), $Bu_{AV}= 80$ MWd/kgU')
                ax.plot(exp_pore_density_4[:,0], exp_pore_density_4[:,1], 's', color='blue', markersize=5, label='Spino (2006), $Bu_{AV}= 98$ MWd/kgU')
                ax.plot(burnup, poreDensity, color='green', linewidth=2.0, label='This work')
                ax.plot(model_bu, model_density, '-', color='purple', linewidth=2.0, label='Barani (2022)')
                ax.plot(old_model_bu, old_model_density, '-', color='blue', linewidth=2.0, label='SCIANTIX 2.0')
                ax.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)
                ax.legend(loc='upper right', frameon=False, fontsize=10)
                #ax.legend(loc='best')
                plt.tight_layout()
                plt.show()

                fig, ax = plt.subplots()
                ax.set_xlabel('Effective burnup (MWd kgHM${}^{-1}$)')
                ax.set_ylabel('Pore number density (pores m${}^{-3}$)')
                ax.plot(exp_pore_density[:,0], exp_pore_density[:,1], 'o', color='#B3B3B3', label='Cappia (2016)')
                ax.plot(burnup, poreDensity, color='green', linewidth=1, label='SCIANTIX 2.0')
                ax.plot(Barani_bu_Np, Barani_density_Np, '-', color='blue', label='$D_{gb}^v$ from Barani (2022)')
                ax.plot(White_bu_Np, White_density_Np, '-', color='red', label='$D_{gb}^v$ from White (2004)')
                ax.legend(loc='best')
                plt.tight_layout()
                plt.show()

                do_plot(
                    'Effective burnup (MWd kgHM${}^{-1}$)',
                    'HBS porosity (/)',
                    exp_porosity[:,0],
                    exp_porosity[:,1],
                    burnup,
                    porosity
                )
            
                fig, ax = plt.subplots()
                ax.set_xlabel('Effective burnup (MWd kgHM${}^{-1}$)')
                ax.set_ylabel('HBS porosity (/)')
                ax.plot(exp_porosity[:, 0], exp_porosity[:, 1], 'o', color='gray', label='Cappia (2016)')
                ax.plot(exp_porosity_2[:, 0], exp_porosity_2[:, 1], 's', color='red', label='Spino (2006)')
                ax.plot(exp_porosity_3[:, 0], exp_porosity_3[:, 1], '^', color='blue', label='Noirot (2008)')
                ax.plot(exp_porosity_4[:, 0], exp_porosity_4[:, 1], 'D', color='orange', label='Lassman (2003)')
                ax.plot(exp_porosity_5[:, 0], exp_porosity_5[:, 1], 'v', color='purple', label='Une (2001), low PCMI')
                ax.plot(exp_porosity_6[:, 0], exp_porosity_6[:, 1], 'v', color='pink', label='Une (2001), strong PCMI')
                ax.plot(burnup, porosity, color='green', linewidth=1.5, label='This work')
                ax.legend(loc='best', fontsize='small')
                plt.tight_layout()
                plt.show()

                fig, ax = plt.subplots()
                ax.set_xlabel('Effective burnup (MWd kgHM${}^{-1}$)')
                ax.set_ylabel('HBS porosity (/)')
                ax.plot(exp_porosity[:, 0], exp_porosity[:, 1], 'o', color='gray', label='Cappia (2016)')
                ax.plot(burnup, porosity, color='green', linewidth=1.5, label='SCIANTIX 2.0')
                ax.plot(Barani_bu_P, Barani_P_P, color='blue', linewidth=1.5, label='$D_{gb}^v$ from Barani (2022)')
                ax.plot(White_bu_P, White_P_P , color='red', linewidth=1.5, label='$D_{gb}^v$ from White (2004)')
                ax.legend(loc='best', fontsize='small')
                plt.tight_layout()
                plt.show()

                do_plot(
                    'Effective burnup (MWd kgHM${}^{-1}$)',
                    'Pore radius (m)',
                    exp_pore_radius[:,0],
                    exp_pore_radius[:,1],
                    burnup,
                    poreRadius
                )
                
                fig, ax = plt.subplots()
                ax.set_xlabel('Effective burnup (MWd kgHM${}^{-1}$)')
                ax.set_ylabel('Pore radius (m)')
                ax.plot(exp_pore_radius[:, 0], exp_pore_radius[:, 1], 'o', color='gray', label='Cappia (2016)')
                ax.plot(burnup, poreRadius, color='green', linewidth=1.5, label='SCIANTIX 2.0')
                ax.plot(Barani_bu_R, Barani_R_R, color='blue', linewidth=1.5, label='$D_{gb}^v$ from Barani (2022)')
                ax.plot(White_bu_R, White_R_R , color='red', linewidth=1.5, label='$D_{gb}^v$ from White (2004)')
                ax.legend(loc='best', fontsize='small')
                plt.tight_layout()
                plt.show()

                # plot: burnup - Xe grain NR + HBS (%)
                def lassmann_fit(x, threshold=60):
                    """
                    K. Lassmann et al. / Journal of Nuclear Materials 226 (1995) 1-8
                    """
                    result = []
                    for value in x:
                        if value < threshold:
                            result.append(1.46e-2 * value)  # linear increase
                        else:
                            result.append(1.46e-2 * (1/0.0584 + (60 - 1/0.0584) * np.exp(-0.0584 * (value - 60)))) # exponential decrease
                    return result

                bu_gwd_tu = np.linspace(0, 200, 1000)

                fig, ax1 = plt.subplots()
                eq = 4.88897e26

                # Data from Walker
                data_walker = np.genfromtxt('walker_data_1999.txt')
                data_bu = data_walker[1:, 0]
                data_xe = data_walker[1:, 1]

                ax1.plot(bu_gwd_tu, lassmann_fit(bu_gwd_tu), color='darkorchid', linestyle='--', label='Lassmann fit (bu$_0$ = 60 GWd/tU)')
                ax1.scatter(data_bu, data_xe, color='navy', edgecolors='black', marker='.', label='Walker data (1999)')
                ax1.plot(sd["bu"] / 0.8814, sd["xe_ig"] / eq, color='limegreen', linestyle='-.', label='Xe in grains - non HBS')
                ax1.plot(sd["bu"] / 0.8814, sd["xe_igs"] / eq, linestyle='-.', label='Xe in dynamic solution - non HBS')
                ax1.plot(sd["bu"] / 0.8814, sd["xe_igb"] / eq, linestyle='-.', label='Xe in intra-granular bubbles - non HBS')
                ax1.plot(sd["bu"] / 0.8814, sd["xe_igHBS"] / eq, color='orangered', linestyle=':', label='Xe in grains - HBS')
                ax1.plot(sd["bu"] / 0.8814, (sd["xe_igHBS"] + sd["xe_ig"]) / eq, color='dimgray', linestyle='-', label='Xe in grains - sum')
                ax1.legend(loc='upper right', fontsize='medium')
                ax1.set_xlabel('Burnup (GWd/tU)', fontsize='large')
                ax1.set_ylabel('Xe in grains (wt%)', color='black', fontsize='large')
                ax1.set_ylim(0, 1.75)
                ax2 = ax1.twinx()
                ax2.plot(sd["bu"] / 0.8814, sd["alpha"], color='gold', linestyle='-', label='Restructured volume fraction (/)')
                # ax2.plot(sd["bu"] / 0.8814, 1 - np.exp(-2.77e-7 * (sd["bu"] / 0.8814)**3.54) , color='magenta', linestyle='--', label='Analytic restructured volume fraction (/)')
                ax2.set_ylabel('Restructured volume fraction (/)', color='black', fontsize='large')
                plt.legend(fontsize='medium')
                plt.tight_layout()
                plt.show()

                # plot: burnup - fuel swelling

                # Data from Walker
                data_spino = np.genfromtxt('spino_swelling_data.txt')
                data_bu = data_spino[1:, 0]
                data_swe = data_spino[1:, 1]
                
                fig, ax1 = plt.subplots()
                ax1.plot(sd["bu"] / 0.8814, 0.00303 * sd["fima"], color='indianred', linestyle='-.', label='Solid fission products (from Olander correlation)')
                ax1.plot(sd["bu"] / 0.8814, sd["swe_igs"], color='orchid', linestyle='-.', label='SCIANTIX gas solution')
                ax1.plot(sd["bu"] / 0.8814, sd["swe_igb"], color='darkgreen', linestyle='-.', label='SCIANTIX gas bubble')
                ax1.plot(sd["bu"] / 0.8814, sd["swe_igs"] + sd["swe_igb"] + 0.0032 * sd["fima"], color='darkblue', linestyle='-', label='Total')
                ax1.scatter(data_bu, data_swe, color='navy', edgecolors='black', marker='.', label='Spino et al. data (2005)')
                ax1.legend(loc='upper right', fontsize='medium')
                ax1.set_xlabel('Burnup (GWd/tU)', fontsize='large')
                ax1.set_ylabel('Fuel matrix swelling (/)', color='black', fontsize='large')
                ax1.set_xlim(0, 145)
                ax2 = ax1.twinx()
                ax2.plot(sd["bu"] / 0.8814, sd["alpha"], color='gold', linestyle='--', label='Restructured volume fraction (/)')
                ax2.set_ylabel('Restructured volume fraction (/)', color='black', fontsize='large')
                plt.legend(fontsize='medium')
                plt.tight_layout()
                plt.show()
            
            os.chdir('..')

    return folderList, number_of_tests, number_of_tests_failed
