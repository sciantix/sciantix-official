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

# labelnew = r"$50$ bubbles/$\mu\mathrm{m}^2$"
# labelold = r"$25$ bubbles/$\mu\mathrm{m}^2$"

# labelnew = "Reynolds and Burton (1979)"
# labelold = "White (2004)"

# labelnew = r"3 J/$\mathrm{m}^2$"
# labelold = r"2 J/$\mathrm{m}^2$"
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
mode_gold = 2
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
