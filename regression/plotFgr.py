import pandas as pd
import numpy as np
from numpy import *
import matplotlib.pyplot as plt
import os
import math
from matplotlib.ticker import (MultipleLocator, FormatStrFormatter, AutoMinorLocator)

from regression_white import regression_white
from regression_kashibe1990 import regression_kashibe1990
from regression_kashibe1991 import regression_kashibe1991
from regression_small1988 import regression_small1988
from regression_small1985 import regression_small1985

markers = ('o', '^', 's', '<', '>', 'v', 'X', '*', 'D', 'd', 'x', 'p', 'P')
colors = ('C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12', 'C13', 'C14')

# Stock the directory path of the current file
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
mode_gold = 2
folderListW, number_of_tests_w, number_of_tests_failed_w, White_calcsw, White_expsw = regression_white(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListK1990, number_of_tests_k1990, number_of_tests_failed_k1990, K1990_calcsw, K1990_expsw,  K1990_calcfgr, K1990_expfgr = regression_kashibe1990(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
#folderListS, number_of_tests_s, number_of_tests_failed_s, S1988_calcfgr, S1988_expfgr = regression_small1988(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
#folderListS1985, number_of_tests_s1985, number_of_tests_failed_s1985,  S1985_calcfgr, S1985_expfgr = regression_small1985(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListK1991, number_of_tests_k1991, number_of_tests_failed_k1991,  K1991_calcfgr, K1991_expfgr = regression_kashibe1991(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)

#sciantix2.0
# white_sciantix = [0.5046069, 0.5330534, 0.5657517, 0.5948491, 0.6604851, 0.473657, 0.5345111, 0.5295682, 0.4695437, 0.4533384, 0.4430789, 0.5828436, 0.5625288, 0.5022538, 0.4855439, 0.4289218, 0.8173517, 0.7790159, 0.8251233, 0.6453449, 0.6993537, 0.5427738, 0.6153484, 0.6192498000000001, 0.6653234, 0.5973389, 0.7465315, 0.7461945, 0.7176193, 0.5594996, 0.5450752, 0.5222441, 0.5076155, 0.6612224, 0.6548643, 0.6850275, 0.6653209999999999, 0.6069834, 0.8544117, 0.44885430000000004, 0.5114833, 0.43413280000000004, 0.5425215]
# K1990fgr_sciantix = [3.9063403000000005, 18.800232, 29.603153, 28.336619999999996, 8.669065300000002, 23.633841999999998, 34.024443, 32.67869]
# K1990sw_sciantix = [2.884116, 3.305821, 3.079794, 3.156879, 2.379956, 2.760294, 3.0336410000000003, 3.2374140000000002]
# K1991fgr_sciantix = [30.280442999999998, 18.449363, 28.83652, 16.956160000000004, 30.351973, 30.281223000000004, 30.286963, 30.562013, 31.829783, 28.78143, 31.27055, 29.19088, 30.507150000000003]
white_sciantix = [0.4807149, 0.5061325, 0.5355774, 0.5655364, 0.6373828, 0.45893160000000005, 0.5172988, 0.5112292, 0.45213830000000005, 0.44072599999999995, 0.4585477, 0.5652815, 0.5447206, 0.4848695, 0.4684657, 0.4887323, 0.8099508, 0.7716721, 0.8145247, 0.6347471, 0.6874132, 0.5321062, 0.602669, 0.6056888, 0.6494001, 0.5790637000000001, 0.7354021, 0.7350637, 0.706366, 0.5394888, 0.5239209, 0.49943150000000003, 0.48803470000000004, 0.6404367999999999, 0.6335798, 0.6573399, 0.6374654000000001, 0.5814253, 0.8276218999999999, 0.4271179, 0.48652870000000004, 0.4137432, 0.5273566000000001]
K1990fgr_sciantix = [4.7349513000000005, 19.668761999999997, 27.74164, 24.88153, 8.6988853, 23.891071999999998, 31.68827, 28.887140000000006]
K1990sw_sciantix = [2.6845190000000003, 3.1564740000000002, 2.973836, 3.082197, 2.137718, 2.5892180000000002, 2.913462, 3.15285]
K1991fgr_sciantix = [28.410859999999996, 17.83658, 25.389330000000005, 14.450880000000002, 28.393400000000003, 28.34806, 28.376190000000005, 28.63582, 29.7709, 25.276210000000003, 27.588310000000003, 25.69226, 26.91145]

# Plot fgr
plt.rcParams.update({'font.size': 18})
plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
plt.rcParams.update({'lines.linewidth': 2}) 
fig, ax = plt.subplots(figsize=(11,8))

ax.scatter(White_expsw, White_calcsw, s=30, label='White et al. (2004)', marker = markers[0], color = colors[0])
ax.scatter(K1990_expsw, K1990_calcsw, s=30, label='Une et al. (1990)', marker = markers[0], color = colors[2])

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

plt.rcParams.update({'font.size': 18})
plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
plt.rcParams.update({'lines.linewidth': 2}) 
fig, ax = plt.subplots(figsize=(11,8))



ax.scatter(K1990_expfgr, K1990_calcfgr, s=30, label='Une et al. (1990)', marker = markers[0], color = colors[2])
ax.scatter(K1991_expfgr, K1991_calcfgr, s=30, label='Kashibe et al. (1991)', marker = markers[0], color = colors[3])

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

plt.rcParams.update({'font.size': 18})
plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
plt.rcParams.update({'lines.linewidth': 2}) 
fig, ax = plt.subplots(figsize=(11,8))

ax.scatter([], [], facecolors='black', marker=markers[0], label="This work")
ax.scatter([], [], facecolors='none', edgecolors='black', marker=markers[1], label="SCIANTIX 2.0")
ax.scatter([], [], marker='', label="\n")
ax.scatter([], [], facecolors=colors[0], marker=markers[2], label="White et al. (2004)")
ax.scatter([], [], facecolors=colors[2], marker=markers[2], label="Une et al. (1990)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(White_expsw, White_calcsw, s=30, label='', marker = markers[0], color = colors[0])
ax.scatter(K1990_expsw, K1990_calcsw, s=30, label='', marker = markers[0], color = colors[2])
ax.scatter(White_expsw, white_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[0], facecolors='none')
ax.scatter(K1990_expsw, K1990sw_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[2], facecolors='none')

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

plt.rcParams.update({'font.size': 18})
plt.rcParams.update({'lines.markersize': 6})  # Corrected parameter for marker size
plt.rcParams.update({'lines.linewidth': 2}) 
fig, ax = plt.subplots(figsize=(11,8))

ax.scatter([], [], facecolors='black', marker=markers[0], label="This work")
ax.scatter([], [], facecolors='none', edgecolors='black', marker=markers[1], label="SCIANTIX 2.0")
ax.scatter([], [], marker='', label="\n")
ax.scatter([], [], facecolors=colors[2], marker=markers[2], label="Une et al. (1990)")
ax.scatter([], [], facecolors=colors[3], marker=markers[2], label="Kashibe et al. (1991)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(K1990_expfgr, K1990_calcfgr, s=30, label='', marker = markers[0], color = colors[2])
ax.scatter(K1991_expfgr, K1991_calcfgr, s=30, label='', marker = markers[0], color = colors[3])
ax.scatter(K1990_expfgr, K1990fgr_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[2], facecolors='none')
ax.scatter(K1991_expfgr, K1991fgr_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[3], facecolors='none')

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
