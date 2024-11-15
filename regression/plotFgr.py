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

markers = ('o', 'v', '^', 'v', '<', '>', 's', 'X', '*', 'D', 'd', 'x', 'p', 'P')
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
folderListS, number_of_tests_s, number_of_tests_failed_s, S1988_calcfgr, S1988_expfgr = regression_small1988(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListS1985, number_of_tests_s1985, number_of_tests_failed_s1985,  S1985_calcfgr, S1985_expfgr = regression_small1985(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)
folderListK1991, number_of_tests_k1991, number_of_tests_failed_k1991,  K1991_calcfgr, K1991_expfgr = regression_kashibe1991(wpath, mode_reg, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed)

#sciantix2.0
white_sciantix = [0.5046069, 0.5330534, 0.5657517, 0.5948491, 0.6604851, 0.473657, 0.5345111, 0.5295682, 0.4695437, 0.4533384, 0.4430789, 0.5828436, 0.5625288, 0.5022538, 0.4855439, 0.4289218, 0.8173517, 0.7790159, 0.8251233, 0.6453449, 0.6993537, 0.5427738, 0.6153484, 0.6192498000000001, 0.6653234, 0.5973389, 0.7465315, 0.7461945, 0.7176193, 0.5594996, 0.5450752, 0.5222441, 0.5076155, 0.6612224, 0.6548643, 0.6850275, 0.6653209999999999, 0.6069834, 0.8544117, 0.44885430000000004, 0.5114833, 0.43413280000000004, 0.5425215]

K1990fgr_sciantix = [3.3759655, 18.121944, 25.802892999999997, 27.482639, 7.9969505, 22.887864, 29.635942999999994, 31.023039000000004]
K1990sw_sciantix = [2.8560209999999997, 3.322382, 3.027343, 3.039617, 2.33974, 2.753497, 2.909748, 3.0331580000000002]
K1991fgr_sciantix = [26.360673, 15.394722999999999, 27.777869000000003, 17.329698999999998, 26.708422999999996, 26.511993, 26.398673, 26.536173, 27.567633000000004, 28.178779, 29.951309000000002, 28.050109, 29.045229]
S1985fgr_sciantix = [16.298329999712923, 16.298329999712923, 16.298329999712923, 22.305299999712922, 25.553219999712923, 27.41285999971292, 32.46126999971292, 34.95405999971292, 36.325089999712915, 37.71049999971292, 39.75312999971292, 40.83353999971292]
S1988fgr_sciantix = [20.297863, 28.758153, 33.452343, 20.602413000000006, 39.094503, 40.786583, 30.7595198344953, 39.03843999970745, 43.100719999707444, 45.553449999707446, 47.125149999707446, 48.16338999970745, 20.222240000000003, 24.611760000000007, 30.96274, 27.454723, 32.388903000000006, 35.963973, 30.311973000000002]

# Plot fgr
plt.rcParams.update({'font.size': 14})
fig, ax = plt.subplots(figsize=(9,6))

ax.scatter(White_expsw, White_calcsw, s=30, label='White et al. (2004)', marker = markers[0], color = colors[1])
ax.scatter(K1990_expsw, K1990_calcsw, s=30, label='Une et al. (1990)', marker = markers[1], color = colors[2])

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25e-1, 3.5e-1), size=14, color='k')
ax.annotate('/2', (3e-1, 1.3e-1), size=14, color='k')
ax.legend(prop=dict(size=14),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("Swelling measured (%)")
ax.set_ylabel("Swelling calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.set_xlim(1e-1, 100)
ax.set_ylim(1e-1, 100)

plt.savefig('Swellingsummary.png')
plt.show()

# Plot fgr
fig, ax = plt.subplots(figsize=(9, 6))


ax.scatter(K1990_expfgr, K1990_calcfgr, s=30, label='Une et al. (1990)', marker = markers[1], color = colors[1])
ax.scatter(K1991_expfgr, K1991_calcfgr, s=30, label='Kashibe et al. (1991)', marker = markers[2], color = colors[2])
ax.scatter(S1985_expfgr, S1985_calcfgr, s=30, label='Small et al. (1985)', marker = markers[3], color = colors[3])
ax.scatter(S1988_expfgr, S1988_calcfgr, s=30, label='Small et al. (1988)', marker = markers[4], color = colors[4])

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25, 3.5), size=14, color='k')
ax.annotate('/2', (3, 1.3), size=14, color='k')
ax.legend(prop=dict(size=14),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("FGR measured (%)")
ax.set_ylabel("FGR calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.set_xlim(1, 100)
ax.set_ylim(1, 100)

plt.savefig('FGRsummary.png')
plt.show()

# Plot fgr
fig, ax = plt.subplots(figsize=(9,6))
ax.scatter([], [], facecolors=colors[0], marker=markers[-1], label="This work")
ax.scatter([], [], facecolors='none', edgecolors=colors[1], marker=markers[-1], label="Zullo et al. (2023)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(White_expsw, White_calcsw, s=30, label='White et al. (2004)', marker = markers[0], color = colors[0])
ax.scatter(K1990_expsw, K1990_calcsw, s=30, label='Une et al. (1990)', marker = markers[1], color = colors[0])
ax.scatter(White_expsw, white_sciantix, s=30, label='', marker = markers[0], edgecolors = colors[1], facecolors='none')
ax.scatter(K1990_expsw, K1990sw_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[1], facecolors='none')

#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25e-1, 3.5e-1), size=14, color='k')
ax.annotate('/2', (3e-1, 1.3e-1), size=14, color='k')
ax.legend(prop=dict(size=14),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("Swelling measured (%)")
ax.set_ylabel("Swelling calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')
ax.tick_params(axis='both', which='major', labelsize=14)
ax.set_xlim(1e-1, 100)
ax.set_ylim(1e-1, 100)

plt.savefig('Swellingsummarycomparison.png')
plt.show()

# Plot fgr
fig, ax = plt.subplots(figsize=(9,6))
ax.scatter([], [], facecolors=colors[0], marker=markers[-1], label="This work")
ax.scatter([], [], facecolors='none', edgecolors=colors[1], marker=markers[-1], label="Zullo et al. (2023)")
ax.scatter([], [], marker='', label="\n")
ax.scatter(K1990_expfgr, K1990_calcfgr, s=30, label='Une et al. (1990)', marker = markers[1], color = colors[0])
ax.scatter(K1991_expfgr, K1991_calcfgr, s=30, label='Kashibe et al. (1991)', marker = markers[2], color = colors[0])
ax.scatter(S1985_expfgr, S1985_calcfgr, s=30, label='Small et al. (1985)', marker = markers[3], color = colors[0])
ax.scatter(S1988_expfgr, S1988_calcfgr, s=30, label='Small et al. (1988)', marker = markers[4], color = colors[0])

ax.scatter(K1990_expfgr, K1990fgr_sciantix, s=30, label='', marker = markers[1], edgecolors = colors[1], facecolors='none')
ax.scatter(K1991_expfgr, K1991fgr_sciantix, s=30, label='', marker = markers[2], edgecolors = colors[1], facecolors='none')
ax.scatter(S1985_expfgr, S1985fgr_sciantix, s=30, label='', marker = markers[3], edgecolors = colors[1], facecolors='none')
ax.scatter(S1988_expfgr, S1988fgr_sciantix, s=30, label='', marker = markers[4], edgecolors = colors[1], facecolors='none')

#ax.scatter([], [], color=colors[1], marker=markers[-1], label="SCIANTIX 2.0")
#ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.plot([1e-3, 1e2],[1e-3, 1e2], '-',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[2e-3, 2e2],'--',color='gray',linewidth=0.5)
ax.plot([1e-3, 1e2],[5e-4, 5e1],'--',color='gray',linewidth=0.5)
ax.annotate('x2', (1.25, 3.5), size=14, color='k')
ax.annotate('/2', (3, 1.3), size=14, color='k')
ax.legend(prop=dict(size=14),loc="center left", bbox_to_anchor=(1,0.5), frameon=False)
plt.tight_layout(pad=3)
ax.set_aspect('equal')
ax.set_xlabel("FGR measured (%)")
ax.set_ylabel("FGR calculated (%)")
ax.set_yscale('log')
ax.set_xscale('log')

ax.tick_params(axis='both', which='major', labelsize=14)
ax.set_xlim(1, 100)
ax.set_ylim(1, 100)

plt.savefig('FGRsummarycomparison.png')
plt.show()
