import pandas as pd
import numpy as np
from numpy import *
import matplotlib.pyplot as plt
import os
import math
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)
import shutil

from regression_white import regression_white
from regression_kashibe1990 import regression_kashibe1990
from regression_kashibe1991 import regression_kashibe1991

# This is a code to summarize the results of the different regression tests for swelling and FGR.
# It generates summary plots comparing calculated vs experimental values for swelling and FGR.
# It is the result of the work done of the following manuscript:
# "On a physics-based model of grain-boundary bubbles overpressurisation and its effects on fuel fragmentation"
# Authors: Elisa Cappellari, Davide Pizzocri, Giovanni Zullo, Giovanni Nicodemo, Sophie Deanesi, Lelio Luzzi
# Affiliation: Politecnico di Milano, Department of Energy, Via La Masa 34, 20156, Milan, Italy
# and 
# Authors: Paul Van Uffelen, Arndt Schubert. 
# Affiliation: European Commission, Joint Research Centre, Directorate for Nuclear Safety and Security, P.O. Box 2340, 76125, Karlsruhe, Germany

# Please note that to obtain same results as the ones presented in the manuscript, the following input files need to be used:
# 
# 1    #    iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))
# 1    #    iFissionGasDiffusivity (0= constant value, 1= Turnbull et al. (1988))
# 2    #    iDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis)
# 1    #    iIntraGranularBubbleBehavior (1= Pizzocri et al. (2018))
# 1    #    iResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000), 3= thermal resolution, Cognini et al. (2021))
# 1    #    iTrappingRate (0= constant value, 1= Ham (1958))
# 1    #    iNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))
# 1    #    iOutput (1= default output files)
# 2    #    iGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= White (2004))
# 1    #    iGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))
# 2    #    iGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017), 2= Cappellari et al. (2025))
# 0    #    iFuelMatrix (0= UO2, 1= UO2 + HBS)
# 0    #    iGrainBoundaryVenting (0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE, 2 = Claisse and Van Uffelen (2015), 3= Pagani et al.(2025))
# 0    #    iRadioactiveFissionGas (0= not considered)
# 0    #    iHelium (0= not considered)
# 0    #    iHeDiffusivity (0= null value, 1= limited lattice damage, Luzzi et al. (2018), 2= significant lattice damage, Luzzi et al. (2018))
# 0    #    iGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)
# 0    #    iHighBurnupStructureFormation (0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020))
# 0    #    iHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)
# 0    #    iHeliumProductionRate (0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR))
# 0    #    iStoichiometryDeviation (0= not considered, 1= Cox et al. 1986, 2= Bittel et al. 1969, 3= Abrefah et al. 1994, 4= Imamura et al. 1997, 5= Langmuir-based approach)
# 1    #    iBubbleDiffusivity (0= not considered, 1= volume diffusivity)
# 0    #    iChromiumSolubility
# 0    #    iDensification (0 = not considered, 1 = Fit from Van Uffelen, P. (2002), PhD thesis.)
# 1    #    iReleaseMode (0 = coalescence by White (2004), saturation threshold of fractional coverage Pastore et al (2013), 1= coalescence by Pastore et al (2013), Cappellari et al. (2025))
#
# And the following parameters must be changed:
# Sciantix_variables[25] = 2.5e+13;  // Intergranular_bubble_concentration[0]
# matrix_.setSurfaceTension(0.6); // (N/m)
    
labelnew = "This work"
labelold = "SCIANTIX 2.0"

markers = ('o', '^', 's', '<', '>', 'v', 'X', '*', 'D', 'd', 'x', 'p', 'P')
colors = ('C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14')

# Stock the directory path of the current file
shutil.copy("../build/sciantix.x", os.getcwd())
wpath = os.path.dirname(os.path.realpath(__file__))
os.chdir(wpath) # Change the working directory to the path stored in 'wpath'

# Initialize different variables needed for the execution :
# - A list 'folderList' to store the names of every test that will be executed. Different variations of this list are initialized for different test types.
folderList = folderListW = folderListK1990 = folderListS = folderListS1985 = folderListK1991= []
# - Variables to count the number of executed tests. Different counts are maintained for different test types.
number_of_tests = number_of_tests_w = number_of_tests_k1990 = number_of_tests_s = number_of_tests_s1985 = number_of_tests_k1991= 0
# - Variables to count the number of failed tests. Different counts are maintained for different test types.
number_of_tests_failed = number_of_tests_failed_w = number_of_tests_failed_t = number_of_tests_failed_c = number_of_tests_failed_o = number_of_tests_failed_h = number_of_tests_failed_k1990 = number_of_tests_failed_s = number_of_tests_failed_s1985 = number_of_tests_failed_k1991 = 0

mode_reg = 1
mode_plot = 1
mode_gold = 0
folderListW, number_of_tests_w, number_of_tests_failed_w, White_calcsw, White_expsw, white_sciantix = regression_white(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListK1990, number_of_tests_k1990, number_of_tests_failed_k1990, K1990_calcsw, K1990_expsw, K1990sw_sciantix,  K1990_calcfgr, K1990_expfgr, K1990fgr_sciantix = regression_kashibe1990(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListK1991, number_of_tests_k1991, number_of_tests_failed_k1991,  K1991_calcfgr, K1991_expfgr, K1991fgr_sciantix = regression_kashibe1991(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)

plt.rcParams.update({'lines.linewidth': 2}) 

# Plot configuration
font_size = 18

plt.rcParams['axes.labelsize'] = font_size
plt.rcParams['xtick.labelsize'] = font_size
plt.rcParams['ytick.labelsize'] = font_size
plt.rcParams['font.size'] = font_size
plt.rcParams['legend.fontsize'] = font_size 
plt.rcParams['lines.markersize'] = 8

fig, ax = plt.subplots(figsize=(11,8))

ax.scatter(White_expsw, White_calcsw, label='White et al. (2004)', marker = markers[0], color = colors[0])
ax.scatter(K1990_expsw, K1990_calcsw, label='Une et al. (1990)', marker = markers[0], color = colors[2])

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25e-1, 3.5e-1), color='k')
ax.annotate('/2', (3e-1, 1.3e-1), color='k')
ax.legend(prop=dict(),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("Swelling measured (%)")
ax.set_ylabel("Swelling calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major')
ax.set_xlim(1e-1, 100)
ax.set_ylim(1e-1, 100)

plt.savefig('Swellingsummary.png')
plt.show()

fig, ax = plt.subplots(figsize=(11,8))

ax.scatter(K1990_expfgr, K1990_calcfgr, label='Une et al. (1990)', marker = markers[0], color = colors[2])
ax.scatter(K1991_expfgr, K1991_calcfgr, label='Kashibe et al. (1991)', marker = markers[0], color = colors[3])

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25, 3.5), color='k')
ax.annotate('/2', (3, 1.3), color='k')
ax.legend(prop=dict(),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("FGR measured (%)")
ax.set_ylabel("FGR calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major')
ax.set_xlim(1, 100)
ax.set_ylim(1, 100)

plt.savefig('FGRsummary.png')
plt.show()

fig, ax = plt.subplots(figsize=(11,8))

ax.scatter([], [], facecolors='black', marker=markers[0], label=labelnew)
ax.scatter([], [], facecolors='none', edgecolors='black', marker=markers[1], label=labelold)
ax.scatter([], [], marker='', label="\n")
ax.scatter([], [], facecolors=colors[0], marker=markers[2], label="White et al. (2004)")
ax.scatter([], [], facecolors=colors[2], marker=markers[2], label="Une et al. (1990)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(White_expsw, White_calcsw, label='', marker = markers[0], color = colors[0])
ax.scatter(K1990_expsw, K1990_calcsw, label='', marker = markers[0], color = colors[2])
ax.scatter(White_expsw, white_sciantix, label='', marker = markers[1], edgecolors = colors[0], facecolors='none')
ax.scatter(K1990_expsw, K1990sw_sciantix, label='', marker = markers[1], edgecolors = colors[2], facecolors='none')

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25e-1, 3.5e-1), color='k')
ax.annotate('/2', (3e-1, 1.3e-1), color='k')
ax.legend(prop=dict(),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("Swelling measured (%)")
ax.set_ylabel("Swelling calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major')
ax.set_xlim(1e-1, 100)
ax.set_ylim(1e-1, 100)

plt.savefig('Swellingsummarycomparison.png')
plt.show()

fig, ax = plt.subplots(figsize=(11,8))

ax.scatter([], [], facecolors='black', marker=markers[0], label=labelnew)
ax.scatter([], [], facecolors='none', edgecolors='black', marker=markers[1], label=labelold)
ax.scatter([], [], marker='', label="\n")
ax.scatter([], [], facecolors=colors[2], marker=markers[2], label="Une et al. (1990)")
ax.scatter([], [], facecolors=colors[3], marker=markers[2], label="Kashibe et al. (1991)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(K1990_expfgr, K1990_calcfgr, label='', marker = markers[0], color = colors[2])
ax.scatter(K1991_expfgr, K1991_calcfgr, label='', marker = markers[0], color = colors[3])
ax.scatter(K1990_expfgr, K1990fgr_sciantix, label='', marker = markers[1], edgecolors = colors[2], facecolors='none')
ax.scatter(K1991_expfgr, K1991fgr_sciantix, label='', marker = markers[1], edgecolors = colors[3], facecolors='none')

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25, 3.5), color='k')
ax.annotate('/2', (3, 1.3), color='k')
ax.legend(prop=dict(),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("FGR measured (%)")
ax.set_ylabel("FGR calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')

ax.tick_params(axis='both', which='major')
ax.set_xlim(1, 100)
ax.set_ylim(1, 100)

plt.savefig('FGRsummarycomparison.png')
plt.show()
