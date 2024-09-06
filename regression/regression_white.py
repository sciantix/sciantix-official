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
import scipy.stats as stats
from sklearn.linear_model import LinearRegression

""" ------------------- Global Variables ------------------- """

# Data from SCIANTIX 1.0
gbSwelling1 = np.array([1.19, 1.16, 1.13, 1.12, 1.29,                   # 4000
               0.91, 0.89, 0.85, 0.79, 0.77, 0.81,             # 4004
               0.89, 0.87, 0.82, 0.76, 0.79,                   # 4005
               1.89, 1.83, 1.68, 1.46, 1.33,                   # 4064
               1.52, 1.48, 1.41, 1.35, 1.31,                   # 4065
               0.79, 0.79, 0.77,                               # 4135
               0.83, 0.83, 0.81, 0.79,                         # 4136
               0.87, 0.86,                                     # 4140
               1.72, 1.69, 1.62, 1.49,                         # 4162
               0.85, 0.84, 0.83, 0.82                          # 4163
               ])

# Data from SCIANTIX 2.0
gbSwellingVersion2 = np.array([[1.008759, 0.9016631, 0.8823770999999999, 0.989169, 1.594425,
                                 1.222006, 1.2902069999999999, 1.0737, 0.8442542000000001, 1.229279, 1.483353, 
                                 1.534709, 1.3644720000000001, 1.044023, 0.9861993, 1.6633269999999998, 
                                 3.1737420000000003, 2.9696219999999998, 1.959618, 1.266134, 1.52619, 
                                 0.8161866, 0.8268307, 0.7252712, 0.7110961, 0.7336258, 
                                 1.7718859999999999, 1.768011, 1.4518069999999998, 
                                 0.7905798, 0.713208, 0.6000579, 0.561775, 
                                 1.030289, 0.9827071999999999,
                                 0.8703542999999999, 0.8268506, 0.7287908, 0.8490229, 
                                 0.3913363, 0.4385216, 0.3674586, 0.9282204000000001]])
# Intergranular gaseous swelling database from White et al. (2006) experiments
#########bubble density##############
gbConcWhite = np.array([0.68, 0.80, 1.32, 1.99, 9.00,                   # 4000
               1.39, 2.12, 3.55, 1.87, 8.51, 27.90,            # 4004
               1.21, 2.93, 2.57, 2.62, 10.62,                  # 4005
               0.95, 0.87, 1.35, 0.74, 3.51,                   # 4064
               0.25, 0.37, 0.49, 0.88, 4.07,                   # 4065
               1.03, 1.52, 1.78,                               # 4135
               0.83, 0.59, 1.19, 1.69,                         # 4136
               1.59, 2.18,                                     # 4140
               3.15, 3.98, 4.92, 5.38,                         # 4162
               2.63, 3.56, 5.19, 5.7                           # 4163
               ])
gbConcWhitesigma = np.array([0.46, 0.17, 0.24, 0.55, 1.13,                   # 4000
               0.32,0.90,1.18,0.84,1.55,0,            # 4004
               0.5,0.97,0.98,1.25,2.16,                  # 4005
               0.49,0.44,0.73,0.31,1.35,                   # 4064
               0.08,0.08,0.24,0.35,0,                   # 4065
               0.51,0.25,0,                               # 4135
               0.24,0.41,0.56,0.33,                         # 4136
               0.57,0.47,                                     # 4140
               0.78,1.27,1.50,1.39,                         # 4162
               0.73,0.82,1.5,2.06                           # 4163
               ])
#########intergranular gas swelling##########
gbSwellingWhite = np.array([0.97, 0.68, 0.53, 0.46, 0.17,                   # 4000
                   0.62, 0.7, 0.44, 0.56, 0.27, 0.16,              # 4004
                   0.94, 0.57, 0.42, 0.54, 0.27,                   # 4005
                   1.07, 0.86, 0.63, 0.74, 0.59,                   # 4064
                   1.25, 1.35, 0.97, 0.79, 0.21,                   # 4065
                   0.42, 0.16, 0.09,                               # 4135
                   0.6, 0.62, 0.26, 0.11,                          # 4136
                   0.26, 0.18,                                     # 4140
                   0.7, 0.46, 0.43, 0.43,                          # 4162
                   0.6, 0.59, 0.35, 0.4                            # 4163
                   ])
gbSwellingWhitesigma = np.array([0.35, 0.12, 0.10, 0.10, 0.04,                   # 4000
                   0.13, 0.26, 0.11, 0.15, 0.07, 0,              # 4004
                   0.16,0.20,0.12,0.15,0.02,                   # 4005
                   0.58,0.32,0.22,0.19,0.26,                   # 4064
                   0.43,0.30,0.26,0.15,0,                   # 4065
                   0.19,0.08,0,                               # 4135
                   0.14,0.19,0.17,0.06,                          # 4136
                   0.16, 0.10,                                     # 4140
                   0.26,0.17,0.18,0.22,                          # 4162
                   0.2,0.18,0.1,0.06                           # 4163
                   ])
#gbSwellingWhite *= 2
#########fractional coverage##########
FcSwellingWhite = np.array([42.96, 30.20, 27.82, 30.84, 24.09,                   # 4000
                   33.39,39.28,30.02,39.88,36.71,36.77,              # 4004
                   39.22,34.71,32.90,43.83,41.72,                   # 4005
                   40.67,36.98,31.42,45.31,45.92,                   # 4064
                   49.68,49.51,44.27,44.78,15.80,                   # 4065
                   22.8,11.5,8.96,                               # 4135
                   32.0,32.7,18.0,10.0,                          # 4136
                   16.3,13.4,                                     # 4140
                   37.11,28.12,30.36,27.74,                          # 4162
                   34.44,31.95,30.22,36.11                            # 4163
                   ])
FcSwellingWhitesigma = np.array([5.95, 3.76, 3.98, 5.24, 0.8,                   # 4000
                   6.19,6.9, 5.82,7.53,5.12,0,              # 4004
                   5.90,8.32,8.49,6.30,2.15,                   # 4005
                   11.51,6.69,6.74,7.66,5.30,                   # 4064
                   7.42,5.51,6.99,6.02,0,                   # 4065
                   8.6,2.6,0,                               # 4135
                   6.0,8.2,8.2,3.5,                          # 4136
                   7.6,5.2,                                     # 4140
                   8.95,7.72,7.87,5.57,                          # 4162
                   8.71,5.12,6.39,6.41                           # 4163
                   ])
#########vented fraction##########
FvSwellingWhite = np.array([55.1,18.8, 7.3, 7.8, 0,                   # 4000
                   17.0,23.0,3.2,33.7, 0, 0,              # 4004
                   21.7,25.9,16.3,45.0,0,                   # 4005
                   32.4,9.6,9.3, 0, 0,                   # 4064
                   84.0,51.4,63.4,0,0,                   # 4065
                   15.0,2.9,1.05,                               # 4135
                   25.5,44.2,24.0,6.5,                          # 4136
                   9.7,0,                                     # 4140
                   0,5,1.5,6.3,                          # 4162
                   10.4,3.0,0,5.0                            # 4163
                   ])
FvSwellingWhitesigma = np.array([13.0, 11.4, 2.8, 5.3, 0,                   # 4000
                   9.1,19.1,1.5,34.3,0,0,              # 4004
                   14.1,16.6,9.0,14.6,0,                   # 4005
                   35.7,13.8,11.5, 0, 0,                   # 4064
                   7.0,16.4,16.8,0,0,                   # 4065
                   22.4,1.4,1.05,                               # 4135
                   16.7,27.1,19.7,0.2,                          # 4136
                   18.3,0,                                     # 4140
                   0,4.7,2.0,9.0,                          # 4162
                   10.9,0.8,0,5.5                            # 4163
                   ])
# Data generated from SCIANTIX 2.0
gbSwelling2 = []
FGR2 = []
bbarea2 = []
bbconc = []
gold = []
Fc = []
Fv = []
f2 = []

bbarea2_gold = []
bbconc_gold = []
FGR_gold = []
Fc_gold = []
Fv_gold = []
fgold = []

sample_number = len(gbSwelling1)


""" ------------------- Functions ------------------- """

# Verify the test results
def check_result(number_of_tests_failed):
  if are_files_equal('output.txt', 'output_gold.txt') == True:
    print(f"Test passed!\n")
  else:
    print(f"Test failed!\n")
    number_of_tests_failed += 1

  return number_of_tests_failed

# Verify the existence of the files: output.txt and output_gold.txt
def check_output(file):
  try :
    data = import_data("output.txt")
  except :
    print(f"output.txt not found in {file}")
    data = np.zeros(shape=(1, 1))

  try :
    data_gold = import_data("output_gold.txt")
  except :
    print(f"output_gold.txt not found in {file}")
    data_gold = np.ones(shape=(1, 1))

  return data, data_gold

# Execute sciantix in the current test folder
def do_sciantix():
  # copying input files from the regression folder into the current folder
  shutil.copy("../input_settings.txt", os.getcwd())
  shutil.copy("../input_scaling_factors.txt", os.getcwd())

  # copying and executing sciantix.exe into cwd
  shutil.copy("../sciantix.x", os.getcwd())
  os.system("./sciantix.x")

  # removing useless file
  os.remove("sciantix.x")
  os.remove("execution.txt")
  os.remove("input_check.txt")
  # os.remove("overview.txt")

# Replace the existing output_gold.txt with the new output.txt
def do_gold():
  try :
    os.path.exists('output.txt')

    os.remove('output_gold.txt')
    os.rename('output.txt', 'output_gold.txt')

  except :
    print(f"output.txt not found in {file}")

# Plot the regression test results
def do_plot():
  
  # GOLD vs. SCIANTIX 2.0, no error bars
  fig, ax = plt.subplots()

  #ax.scatter(gbSwellingWhite, gbSwelling1, edgecolors='#757575', facecolors='red',   marker = '^', s=30, label='SCIANTIX 1.0', zorder = 1)
  ax.scatter(gbSwellingWhite, gbSwelling2,c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=30,label='Barani (2017)', zorder = 2, alpha=0.7)
  
  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  plt.savefig('Swelling-White2004')
  plt.show()

  # GOLD vs. SCIANTIX 2.0, no error bars
  fig, ax = plt.subplots()

  #ax.scatter(gbSwellingWhite, gbSwelling1, edgecolors='#757575', facecolors='red',   marker = '^', s=30, label='SCIANTIX 1.0', zorder = 1)
  ax.scatter(gbSwellingWhite, gbSwelling2,c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder = 2, alpha=0.7)

  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[0.5, 100.5],'--', color = '#757575')
  ax.plot([0, 100],[-0.5, 99.5],'--', color = '#757575')

  ax.set_xlim(0, 5)
  ax.set_ylim(0, 5)

  ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  plt.savefig('Swellinglineare-White2004')
  plt.show()
  
  # GOLD vs. SCIANTIX 2.0, no error bars
  fig, ax = plt.subplots()

  NewSwelling2 = []
  NewSwellinggold= []
  for i in range(len(f2)):
    NewSwelling2.append(gbSwelling2[i]/f2[i])
    NewSwellinggold.append(gold[i]/fgold[i])
    
  ax.scatter(gbSwellingWhite, NewSwelling2,c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  #ax.scatter(gbSwellingWhite, NewSwellinggold, c = '#ff7f0e', marker = 'o', s=30,label='Barani (2017)', zorder = 2, alpha=0.7)
  #ax.scatter(gbSwellingWhite, gbSwelling1, c='red',   marker = 'p', s=30, label='SCIANTIX 1.0', zorder = 3, alpha=0.7)
  ax.scatter(gbSwellingWhite, gbSwellingVersion2, c='green', marker = 'd', s=30, label='SCIANTIX 2.0', zorder = 4, alpha =0.7)

  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  plt.savefig('SwellingCorretto-White2004')
  plt.show()

  # # GOLD vs. SCIANTIX 2.0 + error bars

  # fig, ax = plt.subplots()

  # gbSwellingError = gbSwellingWhite * 0.5
  # igSwellingErrorVertL = np.abs(gbSwelling2 - 100 * np.array([0.00309842, 0.00285444, 0.00545195, 0.00488227, 0.00852239, 0.00365886,
  # 0.00546195, 0.00539069, 0.00431081, 0.00592082, 0.00884021, 0.00620528,
  # 0.00503128, 0.00311326, 0.00348059, 0.00936053, 0.00933353, 0.00886642,
  # 0.00550712, 0.00588977, 0.00455688, 0.00290807, 0.00367743, 0.00308482,
  # 0.00364588, 0.00277625, 0.00323535, 0.00452387, 0.00565546, 0.00425832,
  # 0.00256492, 0.00268151, 0.00306131, 0.00443654, 0.00410574, 0.00469404,
  # 0.00499108, 0.00409438, 0.00604164, 0.00241627, 0.00287818, 0.00199632,
  # 0.00384386]))

  # igSwellingErrorVertU = np.abs(gbSwelling2 - 100 * np.array([0.03099689, 0.02708617, 0.02566999, 0.02358891, 0.02849684, 0.03614354,
  # 0.039007  , 0.03369294, 0.02532625, 0.02945451, 0.02959328, 0.04542349,
  # 0.04282425, 0.03530704, 0.03000697, 0.02936561, 0.07399537, 0.08019407,
  # 0.0502442 , 0.03672561, 0.05521956, 0.03128393, 0.03287336, 0.03071947,
  # 0.02291161, 0.0137135 , 0.03276693, 0.03236039, 0.03020551, 0.01728113,
  # 0.01670874, 0.0128458 , 0.01233106, 0.02754019, 0.02443593, 0.01109099,
  # 0.01102217, 0.0109665 , 0.01412458, 0.0097558 , 0.00991563, 0.00852714,
  # 0.01867004]))

  # #ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', marker = '^', s=20, label='SCIANTIX 1.0', zorder = 1)
  # ax.errorbar(gbSwellingWhite, gbSwelling2, xerr = gbSwellingWhitesigma, yerr = (igSwellingErrorVertL, igSwellingErrorVertU), c = 'green', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  # ax.scatter(gbSwellingWhite, gold, marker = 'v', s=20, label='SCIANTIX 2.0 - GOLD')

  # ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  # ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  # ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  # ax.set_xlim(1e-2, 1e1)
  # ax.set_ylim(1e-2, 1e1)

  # ax.set_xscale('log')
  # ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()

  # plt.show()

  # # GOLD vs. SCIANTIX 2.0 - area
  # fig, ax = plt.subplots()

  # ax.scatter(gbSwellingWhite, bbarea2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='This work')
  # ax.scatter(gbSwellingWhite, bbarea2_gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=20, label='SCIANTIX 2.0 - GOLD')
  
  # ax.set_xscale('log')
  # ax.set_yscale('log')

  # ax.set_xlabel('Experimental swelling(%)')
  # ax.set_ylabel('Calculated bubble area (m2)')
  # ax.legend()

  # plt.show()

  # GOLD vs. SCIANTIX 2.0 - conc
  fig, ax = plt.subplots()

  #ax.errorbar(gbConcWhite, bbconc, xerr = gbConcWhitesigma,c = 'green', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work', zorder=1)
  ax.scatter(gbConcWhite, bbconc,c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(gbConcWhite, bbconc_gold, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder=2, alpha=0.7)

  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.plot([1e-3, 1e3],[1e-3, 1e3], '-', color = '#757575')
  ax.plot([1e-3, 1e3],[2e-3, 2e3],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[5e-4, 5e2],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(1e-2, 1e2)
  ax.set_ylim(1e-2, 1e2)

  ax.set_xlabel('Experimental (bub/m2)')
  ax.set_ylabel('Calculated (bub/m2)')
  ax.set_title('Bubble concentration')
  ax.legend()
  plt.savefig('BubConc-White2004')
  plt.show()

  # GOLD vs. SCIANTIX 2.0 - fractional coverage
  fig, ax = plt.subplots()

  #ax.errorbar(FcSwellingWhite, Fc, xerr = FcSwellingWhitesigma,c = 'green', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  ax.scatter(FcSwellingWhite, Fc, c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(FcSwellingWhite, Fc_gold,c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)', zorder=2, alpha=0.7)


  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[20, 120],'--', color = '#757575')
  ax.plot([0, 100],[-20, 80],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(0, 100)
  ax.set_ylim(0, 100)

  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.set_title('Fractional coverage of grain faces')
  ax.legend()
  plt.savefig('Fc-White2004')
  plt.show()

  # GOLD vs. SCIANTIX 2.0 - vented fraction
  fig, ax = plt.subplots()

  #ax.errorbar(FvSwellingWhite, Fv, xerr = FvSwellingWhitesigma,c = 'green', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  ax.scatter(FvSwellingWhite, Fv,c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(FvSwellingWhite, Fv_gold, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)',zorder=2, alpha=0.7)

  ax.plot([0, 100],[0, 100], '-', color = '#757575')
  ax.plot([0, 100],[20, 120],'--', color = '#757575')
  ax.plot([0, 100],[-20, 80],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(0, 100)
  ax.set_ylim(0, 100)

  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.set_title('Vented fraction of grain faces')
  ax.legend()
  
  plt.savefig('Fv-White2004')
  plt.show()

  # FGR plot
  fig, ax = plt.subplots()
  ax.scatter(gbSwelling2, FGR2, c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(gbSwelling2, FGR_gold, c = '#ff7f0e', marker = 'o', s=30, label='FGR SCIANTIX 2.0 - Gold')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Swelling (%)')
  ax.set_ylabel('FGR (%)')

  ax.legend()

  plt.show()

  #######
  # DATA
  #######

  fig, ax = plt.subplots()

  #ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', edgecolors= '#999AA2', marker = '^', s=20, label='SCIANTIX 1.0')
  ax.scatter(gbSwellingWhite, gbSwelling2, c='#9370DB', marker = '^', s=30, label='This work', zorder = 1)
  ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=30, label='Barani (2017)',zorder=2, alpha=0.7)
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
  ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
  ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')

  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xscale('log')
  ax.set_yscale('log')

  # ax.set_title('Intergranular gaseous swelling')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.set_title('Intergranular gas swelling')
  ax.legend()

  #####################
  # Boxplot + data
  #####################

  fig, ax = plt.subplots()

  data = [gbSwellingWhite, gbSwelling1, gbSwelling2]

  bp = ax.boxplot(data, patch_artist = True)
  colors = ['#80C7E6', '#FA82B4', '#98E18D']

  for patch, color in zip(bp['boxes'], colors):
    patch.set_facecolor(color)

  for whisker in bp['whiskers']:
    whisker.set(color ='#8B008B', linewidth = 1.5, linestyle =":")

  for cap in bp['caps']:
    cap.set(color ='#8B008B', linewidth = 2)

  for median in bp['medians']:
    median.set(color ='black', linewidth = 2)

  for flier in bp['fliers']:
    flier.set(marker ='D', color ='#e7298a', alpha = 0.5)

  ax.tick_params(axis='y', which='minor', bottom=False)
  ax.minorticks_on()
  ax.grid(which='major', color='k', linestyle='--', alpha=0.2)
  ax.grid(which='minor', color='k', linestyle=':', alpha=0.2)

  ax.set_xticklabels(['Experimental', 'SCIANTIX 1.0', 'SCIANTIX 2.0'])
  ax.set_ylabel("Swelling (%)")

  # plt.title("Box plot")

  ax.get_xaxis().tick_bottom()
  ax.get_yaxis().tick_left()

  ################
  # Boxplot: Bias
  ################

  fig, ax = plt.subplots()

  error1 = [gbSwelling1[i] - gbSwellingWhite[i] for i in range(len(gbSwellingWhite))]
  error2 = [gbSwelling2[i] - gbSwellingWhite[i] for i in range(len(gbSwellingWhite))]
  data   = [error1, error2]

  bp = ax.boxplot(data, patch_artist = True)
  colors = ['#FA82B4', '#98E18D']

  for patch, color in zip(bp['boxes'], colors):
    patch.set_facecolor(color)

  for whisker in bp['whiskers']:
    whisker.set(color ='#8B008B', linewidth = 1.5, linestyle =":")

  for cap in bp['caps']:
    cap.set(color ='#8B008B', linewidth = 2)

  for median in bp['medians']:
    median.set(color ='black', linewidth = 2)

  for flier in bp['fliers']:
    flier.set(marker ='D', color ='#e7298a', alpha = 0.5)

  ax.tick_params(axis='y', which='minor', bottom=False)
  ax.minorticks_on()
  ax.grid(which='major', color='k', linestyle='--', alpha=0.2)
  ax.grid(which='minor', color='k', linestyle=':', alpha=0.2)

  ax.set_xticklabels(['SCIANTIX 1.0', 'SCIANTIX 2.0'])
  ax.set_ylabel("Error (%)")

  # plt.title("Box plot")

  ax.get_xaxis().tick_bottom()
  ax.get_yaxis().tick_left()

  plt.show()

  # plt.savefig("boxplot_gb_swelling.png")


# Main function of the white regression
def regression_white(wpath, mode_White, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

  # Exit of the function without doing anything
  if mode_White == 0:
    return folderList, number_of_tests, number_of_tests_failed

  # Get list of all files and directories in wpath
  files_and_dirs = os.listdir(wpath)

  # Sort them by filename
  sorted_files_and_dirs = sorted(files_and_dirs)

  # Iterate over sorted list
  for file in sorted_files_and_dirs:
    # Verify on a given folder, if Baker is in it's name
    if "White" in file and os.path.isdir(file):
      folderList.append(file)
      os.chdir(file)

      print(f"Now in folder {file}...")
      number_of_tests += 1

      # mode_gold = 0 : Use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 0:

        do_sciantix()
        data, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 1 : Use SCIANTIX / Use GOLD
      if mode_gold == 1:

        do_sciantix()
        data, data_gold = check_output(file)
        print("...golding results.")
        do_gold()

      # mode_gold = 2 : Don't use SCIANTIX / Don't use GOLD and check result
      if mode_gold == 2:

        data, data_gold = check_output(file)
        number_of_tests_failed = check_result(number_of_tests_failed)

      # mode_gold = 3 : Don't use SCIANTIX / Use GOLD
      if mode_gold == 3:

        data, data_gold = check_output(file)
        print("...golding existing results.")
        do_gold()


      row_number = -1

      # Retrieve the generated data of Intergranular gas swelling
      interGranularSwellingPos = findSciantixVariablePosition(data, "Intergranular gas swelling (/)")
      gbSwelling2.append(100*data[row_number,interGranularSwellingPos].astype(float))

      # Retrieve the gold data of Intergranular gas swelling
      interGranularSwellingGoldPos = findSciantixVariablePosition(data_gold, "Intergranular gas swelling (/)")
      gold.append(100*data_gold[row_number,interGranularSwellingGoldPos].astype(float))

      # Retrieve the generated data of Intergranular gas swelling
      interGranularIntactPos = findSciantixVariablePosition(data, "Intergranular fractional intactness (/)")
      f2.append(data[row_number,interGranularIntactPos].astype(float))

      # Retrieve the gold data of Intergranular gas swelling
      interGranularIntactGoldPos = findSciantixVariablePosition(data_gold, "Intergranular fractional intactness (/)")
      fgold.append(data_gold[row_number,interGranularIntactGoldPos].astype(float))

      # Retrieve the generated data of Intergranular bubble area (m2)
      bubbleAreaPos = findSciantixVariablePosition(data, "Intergranular bubble area (m2)")
      bbarea2.append(100*data[row_number,bubbleAreaPos].astype(float))

      # Retrieve the gold data of Intergranular bubble area (m2)
      bubbleAreaGoldPos = findSciantixVariablePosition(data_gold, "Intergranular bubble area (m2)")
      bbarea2_gold.append(100*data_gold[row_number,bubbleAreaGoldPos].astype(float))

      # Retrieve the generated data of Fission gas release
      FGRPos = findSciantixVariablePosition(data, "Fission gas release (/)")
      FGR2.append(100*data[row_number,FGRPos].astype(float))

      # Retrieve the gold data of Fission gas release
      FGRPosGold = findSciantixVariablePosition(data_gold, "Fission gas release (/)")
      FGR_gold.append(100*data_gold[row_number,FGRPosGold].astype(float))

      # Retrieve the generated data of Intergranular bubble concentration (bub/m2)
      BubConcPos = findSciantixVariablePosition(data, "Intergranular bubble concentration (bub/m2)")
      bbconc.append(1e-12*data[row_number,BubConcPos].astype(float))

      # Retrieve the gold data of Intergranular bubble concentration
      BubConcPosGold = findSciantixVariablePosition(data_gold, "Intergranular bubble concentration (bub/m2)")
      bbconc_gold.append(1e-12*data_gold[row_number,BubConcPosGold].astype(float))

      # Retrieve the generated data of Intergranular fractional coverage (-)
      FcPos = findSciantixVariablePosition(data, "Intergranular fractional coverage (/)")
      Fc.append(100*data[row_number,FcPos].astype(float))

      # Retrieve the gold data of Intergranular fractional coverage
      FcPosGold = findSciantixVariablePosition(data_gold, "Intergranular fractional coverage (/)")
      Fc_gold.append(100*data_gold[row_number,FcPosGold].astype(float))

      # Retrieve the generated data of Intergranular fractional coverage (-)
      FvPos = findSciantixVariablePosition(data, "Intergranular vented fraction (/)")
      Fv.append(100*data[row_number,FvPos].astype(float))

      # Retrieve the gold data of Intergranular fractional coverage
      FvPosGold = findSciantixVariablePosition(data_gold, "Intergranular vented fraction (/)")
      Fv_gold.append(100*data_gold[row_number,FvPosGold].astype(float))

      os.chdir('..')

  # Check if the user has chosen to display the various plots
  if mode_plot == 1:
    do_plot()

  """ Statistical analysis """

  error1 = [gbSwelling1[i] - gbSwellingWhite[i] for i in range(len(gbSwellingWhite))]
  error2 = [gbSwelling2[i] - gbSwellingWhite[i] for i in range(len(gbSwellingWhite))]

  # Experimental data: mean, median, ...
  print(f"Experimental data - mean: ", np.mean(gbSwellingWhite))
  print(f"Experimental data - median: ", np.median(gbSwellingWhite))
  print(f"Experimental data - Q1: ", np.percentile(gbSwellingWhite, 25, interpolation = 'midpoint'))
  print(f"Experimental data - Q3: ", np.percentile(gbSwellingWhite, 75, interpolation = 'midpoint'))

  # SCIANTIX 1.0: mean, median, ...
  print(f"SCIANTIX 1.0 - mean: ", np.mean(gbSwelling1))
  print(f"SCIANTIX 1.0 - median: ", np.median(gbSwelling1))
  print(f"SCIANTIX 1.0 - Q1: ", np.percentile(gbSwelling1, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 1.0 - Q3: ", np.percentile(gbSwelling1, 75, interpolation = 'midpoint'))
  print(f"SCIANTIX 1.0 - Standard error: {np.std(gbSwelling1, ddof=0) / len(gbSwelling1)}")
  print(f"SCIANTIX 1.0 - 95% CI: {np.mean(gbSwelling1) - 1.96 * np.std(gbSwelling1, ddof=0) / len(gbSwelling1)}, {np.mean(gbSwelling1) + 1.96 * np.std(gbSwelling1, ddof=0) / len(gbSwelling1)}")
  print(f"SCIANTIX 1.0 - BIAS median: ", np.median(error1))

  # SCIANTIX 2.0: mean and median
  print(f"SCIANTIX 2.0 - mean: ", np.mean(gbSwelling2))
  print(f"SCIANTIX 2.0 - median: ", np.median(gbSwelling2))
  print(f"SCIANTIX 2.0 - Q1: ", np.percentile(gbSwelling2, 25, interpolation = 'midpoint'))
  print(f"SCIANTIX 2.0 - Q3: ", np.percentile(gbSwelling2, 75, interpolation = 'midpoint'))
  print(f"SCIANTIX 2.0 - Standard error: {np.std(gbSwelling2, ddof=0) / len(gbSwelling2)}")
  print(f"SCIANTIX 2.0 - 95% CI: {np.mean(gbSwelling2) - 1.96 * np.std(gbSwelling2, ddof=0) / len(gbSwelling2)}, {np.mean(gbSwelling2) + 1.96 * np.std(gbSwelling2, ddof=0) / len(gbSwelling2)}")
  print(f"SCIANTIX 2.0 - BIAS median: ", np.median(error2))

  # Median absolute deviations
  deviations_1 = abs(np.array(gbSwellingWhite) - gbSwelling1)
  deviations_2 = abs(np.array(gbSwellingWhite) - gbSwelling2)
  print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_2))

  # RMSE
  print(f"SCIANTIX 1.0 - RMSE: ", np.mean(np.array(gbSwellingWhite) - gbSwelling1)**2)
  print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(gbSwellingWhite) - gbSwelling2)**2)
  print("\n")

  return folderList, number_of_tests, number_of_tests_failed
