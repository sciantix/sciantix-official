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
# gbSwellingVersion2 = np.array([1.008759, 0.9016631, 0.8823770999999999, 0.989169, 1.594425,
#                                 1.222006, 1.2902069999999999, 1.0737, 0.8442542000000001, 1.229279, 1.483353, 
#                                 1.534709, 1.3644720000000001, 1.044023, 0.9861993, 1.6633269999999998, 
#                                 3.1737420000000003, 2.9696219999999998, 1.959618, 1.266134, 1.52619, 
#                                 0.8161866, 0.8268307, 0.7252712, 0.7110961, 0.7336258, 
#                                 1.7718859999999999, 1.768011, 1.4518069999999998, 
#                                 0.7905798, 0.713208, 0.6000579, 0.561775, 
#                                 1.030289, 0.9827071999999999,
#                                 0.8703542999999999, 0.8268506, 0.7287908, 0.8490229, 
#                                 0.3913363, 0.4385216, 0.3674586, 0.9282204000000001])

# bbareaVersion2 = 0.01*np.array([[4.651219e-11, 3.352436e-11, 2.5521149999999998e-11, 2.0897340000000003e-11, 4.529223e-11, 5.647797e-11, 5.0007779999999995e-11, 3.545341e-11, 2.4288589999999998e-11, 2.827147e-11, 3.565348e-11, 5.678682e-11, 4.87273e-11, 3.463042e-11, 2.391385e-11, 3.603079e-11, 4.5441349999999996e-10, 4.1601290000000004e-10, 1.442324e-10, 7.260004e-11, 8.764244000000001e-11, 7.638748999999999e-11, 6.222416e-11, 4.879217e-11, 3.442372e-11, 1.832204e-11, 1.259591e-10, 1.256746e-10, 9.768258e-11, 3.085038e-11, 2.698882e-11, 2.110955e-11, 1.4684210000000002e-11, 3.3476079999999996e-11, 3.129802e-11, 1.5970599999999998e-11, 1.454514e-11, 1.305754e-11, 1.4339e-11, 9.044315e-12, 8.446771e-12, 7.263476e-12, 1.2915879999999999e-11]])

# bbconcVersion2 = np.array([0.462098, 0.6009097, 0.8004431, 1.155998, 0.6346446, 
#                            0.3646203, 0.400846, 0.5317783, 0.7748976, 0.8917049, 0.7655369, 
#                            0.3622533, 0.408512, 0.5495421, 0.8587945, 0.7904561, 
#                            0.04760926, 0.05147566, 0.1392633, 0.2773507, 0.2124981, 
#                            0.2177955, 0.2553267, 0.2999212, 0.4367395, 1.193519,
#                            0.1231988, 0.1233469, 0.147808, 
#                            0.5641399, 0.6219742, 0.756496, 1.220727, 
#                            0.5655761, 0.5967367,
#                            1.474099, 1.625337, 1.833869, 1.453809, 
#                            1.643632, 1.772661, 2.144419, 1.9135579999999999])

BBconc = [1.831054, 2.098954, 2.384684, 2.791946, 3.278674, 2.851906, 2.9957089999999997, 3.469219, 4.363574, 5.309446, 5.935659, 2.979113, 3.1799749999999998, 3.73034, 4.861085, 6.388502, 0.7720596, 0.8361828, 1.080685, 1.537682, 1.988538, 1.183124, 1.285463, 1.521422, 1.7491109999999999, 2.089751, 1.052338, 1.0534109999999999, 1.1424159999999999, 2.1784179999999997, 2.315804, 2.639525, 3.201736, 2.4464, 2.529912, 1.944985, 2.03914, 2.145055, 2.106609, 4.022781, 4.095258, 4.389879, 4.669807]
BBarea = [1.260601e-13, 1.111544e-13, 9.949157e-14, 8.889074e-14, 8.767544e-14, 8.020851e-14, 7.661456e-14, 6.6968e-14, 5.458579e-14, 3.847651e-14, 3.317658e-14, 7.687601e-14, 7.238745e-14, 6.259403e-14, 4.498522e-14, 2.995378e-14, 2.91783e-13, 2.70319e-13, 2.114882e-13, 1.521934e-13, 1.20818e-13, 1.943568e-13, 1.797661e-13, 1.539431e-13, 1.354853e-13, 1.151746e-13, 1.737338e-13, 1.735651e-13, 1.603962e-13, 1.04977e-13, 9.916066e-14, 8.816211e-14, 7.484056e-14, 9.779194e-14, 9.49736e-14, 1.210406e-13, 1.159347e-13, 1.116324e-13, 1.180462e-13, 5.703922e-14, 5.504777e-14, 4.822694e-14, 4.329072e-14]
Fc = [23.08228, 23.3308, 23.7256, 24.81781, 28.74592, 22.87471, 22.95149, 23.23266, 23.81891, 20.4289, 19.692490000000003, 22.902230000000003, 23.019029999999997, 23.349700000000002, 21.8677, 19.13598, 22.52738, 22.60361, 22.855220000000003, 23.40249, 24.025109999999998, 22.99482, 23.10827, 23.421239999999997, 23.697879999999998, 24.06863, 18.28266, 18.28355, 18.32393, 22.868389999999998, 22.96367, 23.27061, 23.96197, 23.923820000000003, 24.02748, 23.54221, 23.6407, 23.94576, 24.867729999999998, 22.945619999999998, 22.54348, 21.171039999999998, 20.21593]
FGR = [15.69582, 16.358819999999998, 16.43051, 14.64701, 10.11809, 16.609489999999997, 18.384, 17.47048, 13.903960000000001, 12.26294, 10.79592, 20.237930000000002, 19.310109999999998, 16.49166, 14.302029999999998, 10.82828, 24.36419, 23.56817, 25.950879999999998, 21.25069, 22.134999999999998, 14.924029999999998, 16.83427, 16.8355, 17.84724, 16.08962, 29.72205, 29.7159, 29.046650000000003, 15.76755, 15.28034, 14.10367, 12.085550000000001, 16.809569999999997, 16.48567, 24.77701, 24.04692, 21.10942, 23.48249, 12.18138, 13.772490000000001, 11.59518, 12.61883]
SW = [0.5639892, 0.6012945, 0.6398579, 0.6627784, 0.7015412000000001, 0.5115398999999999, 0.5782186, 0.5750397, 0.5065090999999999, 0.36749339999999997, 0.326469, 0.6286595, 0.6081627000000001, 0.5445909, 0.45544840000000003, 0.3222298, 0.8374214, 0.7990155000000001, 0.8538258999999999, 0.6737596, 0.7309926999999999, 0.5690324, 0.6464027, 0.6520156, 0.7031934999999999, 0.6401981, 0.7752941999999999, 0.7749557, 0.7466221000000001, 0.6059695, 0.5913969, 0.5650891, 0.5361165, 0.7036358, 0.6964271, 0.7577047, 0.7381995, 0.6738542999999999, 0.9189585, 0.47969880000000004, 0.5329912, 0.40697629999999996, 0.4395564]

bbconcVersion2=np.array(BBconc)
bbareaVersion2=np.array(BBarea)
FcVersion2 = np.array(Fc)
gbSwellingVersion2 = np.array(SW)
FGRVersion2 = np.array(FGR)

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

gbradiusWhite = np.array([0.22, 0.22, 0.18, 0.15, 0.08,                   # 4000
               0.20, 0.15, 0.13, 0.14, 0.07, 0.04,            # 4004
               0.21, 0.13, 0.12, 0.11, 0.06,                  # 4005
               0.27, 0.25, 0.19, 0.20, 0.13,                   # 4064
               0.28, 0.30, 0.24, 0.16, 0.12,                   # 4065
               0.16, 0.12, 0.09,                               # 4135
               0.16, 0.17, 0.12, 0.09,                         # 4136
               0.11, 0.09,                                     # 4140
               0.15, 0.12, 0.13, 0.11,                         # 4162
               0.15, 0.14, 0.12, 0.1                           # 4163
               ])*1e-6

gblengthWhite = np.array([1.18, 0.47, 0.29, 0.36, 0.04,                   # 4000
               0.31, 0.44,0.18,0.90,0.22,0.11,            # 4004
               0.5,0.28,0.49,0.71,0.25,                  # 4005
               0.76,0.66,0.53,1.66,0.51,                   # 4064
               2.38,1.5,1.6,1.38,0,                   # 4065
               0.47,0.15,0.13,                               # 4135
               0.98,1.34,0.41,0.2,                         # 4136
               0.28,0.22,                                     # 4140
               0.15,0.13,0.12,0.06,                         # 4162
               0.22,0.09,0.09,0.27                          # 4163
               ])*1e-6

gblobesWhite = np.array([4.2, 2.6, 2.3, 2.3, 2.0,                   # 4000
               2.3,3.0,2.1,3.2,2.0,2.0,            # 4004
               2.9,3.1,3.2,3.8,2.2,                  # 4005
               2.9,2.6,3.0,6.3,3.2,                   # 4064
               6.3,3.5,4.3,4.2,2.0,                   # 4065
               3.3,2.0,2.0,                               # 4135
               4.0,4.2,3.4,2.1,                         # 4136
               3.1,2.1,                                     # 4140
               2.1,2.3,2.0,2.0,                         # 4162
               2.7,2.2,2.1,2.2                         # 4163
               ])

gbareaWhite = 2*gblengthWhite*gbradiusWhite + gblobesWhite*np.pi*gbradiusWhite**2/2

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

FGRWhite = np.array([1.84, 1.84,1.84, 1.84, 1.84,                   # 4000
                   0.97,0.97,0.97,0.97,0.97,0.97,              # 4004
                   0.65,0.65,0.65,0.65,0.65,                   # 4005
                   1.26,1.26,1.26,1.26,1.26,                   # 4064
                   6.29,6.29,6.29,6.29,6.29,                   # 4065
                   0.78,0.78,0.78,                               # 4135
                   1.52,1.52,1.52,1.52,                          # 4136
                   2.38,2.38,                                     # 4140
                   0.41,0.41,0.41,0.41,                          # 4162
                   0.32,0.32,0.32,0.32                            # 4163
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
  #k=16
  print(f2[16:39])
  print(f2[0:16])
  print(f2[39:43])
  # # GOLD vs. SCIANTIX 2.0, no error bars
  # fig, ax = plt.subplots()

  # #ax.scatter(gbSwellingWhite, gbSwelling1, edgecolors='#757575', facecolors='red',   marker = '^', s=40, label='SCIANTIX 1.0', zorder = 1)
  # ax.scatter(gbSwellingWhite, gbSwelling2,c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  # ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017)', zorder = 2, alpha=0.7)
  
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
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)

  
  # plt.savefig('Images/Swelling-White2004')
  # plt.show()

  # # GOLD vs. SCIANTIX 2.0, no error bars
  # fig, ax = plt.subplots()

  # #ax.scatter(gbSwellingWhite, gbSwelling1, edgecolors='#757575', facecolors='red',   marker = '^', s=40, label='SCIANTIX 1.0', zorder = 1)
  # ax.scatter(gbSwellingWhite, gbSwelling2,c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  # ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder = 2, alpha=0.7)

  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[0.5, 100.5],'--', color = '#757575')
  # ax.plot([0, 100],[-0.5, 99.5],'--', color = '#757575')

  # ax.set_xlim(0, 5)
  # ax.set_ylim(0, 5)

  # ax.set_title('Intergranular gaseous swelling')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)
  # plt.savefig('Images/Swellinglineare-White2004')
  # plt.show()
  
  NewSwelling2 = []
  NewSwellinggold= []
  for i in range(len(f2)):
    NewSwelling2.append(gbSwelling2[i]/f2[i])
    NewSwellinggold.append(gold[i]/fgold[i])

  # corrected swelling log
  fig, ax = plt.subplots()
  ax.scatter(np.concatenate([gbSwellingWhite[0:16],gbSwellingWhite[39:43]]), np.concatenate([NewSwelling2[0:16],NewSwelling2[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(gbSwellingWhite[16:39], NewSwelling2[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  ax.scatter(np.concatenate([gbSwellingWhite[0:16],gbSwellingWhite[39:43]]), np.concatenate([gbSwellingVersion2[0:16],gbSwellingVersion2[39:43]]), c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Fast ramp', alpha =0.7)
  ax.scatter(gbSwellingWhite[16:39], gbSwellingVersion2[16:39], facecolors = 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Slow ramp', alpha =0.7)
  #ax.scatter(gbSwellingWhite, NewSwellinggold, c = '#ff7f0e', marker = 'o', s=40,label='Barani (2017)', zorder = 2, alpha=0.7)
  #ax.scatter(gbSwellingWhite, gbSwelling1, c='red',   marker = 'p', s=40, label='SCIANTIX 1.0', zorder = 3, alpha=0.7)
  #ax.scatter(gbSwellingWhite[16:21], NewSwelling2[16:21], c='red', marker = '^', s=40)
  #ax.scatter(gbSwellingWhite[16:21], gbSwellingVersion2[16:21], c='red', marker = 'd', s=40)

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

  
  plt.savefig('Images/SwellingCorretto-White2004')
  plt.show()

  # fgr
  fig, ax = plt.subplots()
  ax.scatter(np.concatenate([FGRWhite[0:16],FGRWhite[39:43]]), np.concatenate([FGR2[0:16],FGR2[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(FGRWhite[16:39], FGR2[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  ax.scatter(np.concatenate([FGRWhite[0:16],FGRWhite[39:43]]), np.concatenate([FGRVersion2[0:16],FGRVersion2[39:43]]), c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Fast ramp', alpha =0.7)
  ax.scatter(FGRWhite[16:39], FGRVersion2[16:39], facecolors = 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Slow ramp', alpha =0.7)
  
  ax.plot([0, 1e2],[2.5, 102.5], '-', color = '#757575')
  ax.plot([0, 1e2],[0, 100],'--', color = '#757575')
  ax.plot([0, 1e2],[-2.5, 97.5],'--', color = '#757575')

  ax.set_xlim(0,20)
  ax.set_ylim(0,50)

  ax.set_title('Fission gas release')
  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.legend()
  ax.grid(color='gray', linestyle='--', linewidth=0.5)

  plt.show()

  #   # f
  # fig, ax = plt.subplots()
  # ax.scatter(np.concatenate([fgold[0:16],fgold[39:43]]), np.concatenate([f2[0:16],f2[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  # ax.scatter(fgold[16:39], f2[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  # ax.scatter(fgold[26:33], f2[26:33], c='red', marker = '^', s=40)
  
  # ax.set_xlim(0,1)
  # ax.set_ylim(0,1)

  # ax.set_title('f')
  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.legend()
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)

  # plt.show()

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

  # #ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', marker = '^', s=40, label='SCIANTIX 1.0', zorder = 1)
  # ax.errorbar(gbSwellingWhite, gbSwelling2, xerr = gbSwellingWhitesigma, yerr = (igSwellingErrorVertL, igSwellingErrorVertU), c = '#66CDAA', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  # ax.scatter(gbSwellingWhite, gold, marker = 'v', s=40, label='SCIANTIX 2.0 - GOLD')

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

  # ax.scatter(gbSwellingWhite, bbarea2, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=40, label='This work')
  # ax.scatter(gbSwellingWhite, bbarea2_gold, c = '#C9C954', edgecolors= '#999AA2', marker = 'o', s=40, label='SCIANTIX 2.0 - GOLD')
  
  # ax.set_xscale('log')
  # ax.set_yscale('log')

  # ax.set_xlabel('Experimental swelling(%)')
  # ax.set_ylabel('Calculated bubble area (m2)')
  # ax.legend()

  # plt.show()

  # Bubble concentration
  fig, ax = plt.subplots()

  ax.scatter(np.concatenate([gbConcWhite[0:16],gbConcWhite[39:43]]), np.concatenate([bbconc[0:16],bbconc[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(gbConcWhite[16:39], bbconc[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  ax.scatter(np.concatenate([gbConcWhite[0:16], gbConcWhite[39:43]]), np.concatenate([bbconcVersion2[0:16],bbconcVersion2[39:43]]), c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Fast ramp', alpha =0.7)
  ax.scatter(gbConcWhite[16:39], bbconcVersion2[16:39], facecolors = 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Slow ramp', alpha =0.7)
  
  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.plot([1e-3, 1e3],[1e-3, 1e3], '-', color = '#757575')
  ax.plot([1e-3, 1e3],[2e-3, 2e3],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[5e-4, 5e2],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(1e-2, 1e2)
  ax.set_ylim(1e-2, 1e2)

  ax.set_xlabel('Experimental (bub/um2)')
  ax.set_ylabel('Calculated (bub/um2)')
  ax.set_title('Bubble concentration')
  ax.legend()
  plt.savefig('Images/BubConc-White2004')
  plt.show()

    # Bubble area
  fig, ax = plt.subplots()

  ax.scatter(1e12*np.concatenate([gbareaWhite[0:16],gbareaWhite[39:43]]), 1e12*np.concatenate([bbarea2[0:16],bbarea2[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(1e12*np.array(gbareaWhite[16:39]), 1e12*np.array(bbarea2[16:39]), facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  ax.scatter(1e12*np.concatenate([gbareaWhite[0:16], gbareaWhite[39:43]]), 1e12*np.concatenate([bbareaVersion2[0:16],bbareaVersion2[39:43]]), c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Fast ramp', alpha =0.7)
  ax.scatter(1e12*np.array(gbareaWhite[16:39]), 1e12*np.array(bbareaVersion2[16:39]), facecolors = 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Slow ramp', alpha =0.7)
  
  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.plot([1e-3, 1e3],[1e-3, 1e3], '-', color = '#757575')
  ax.plot([1e-3, 1e3],[2e-3, 2e3],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[5e-4, 5e2],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(1e-2, 1e1)
  ax.set_ylim(1e-2, 1e1)

  ax.set_xlabel('Experimental (um2)')
  ax.set_ylabel('Calculated (um2)')
  ax.set_title('Bubble area')
  ax.legend()
  plt.savefig('Images/BubArea-White2004')
  plt.show()

  # GOLD vs. SCIANTIX 2.0 - fractional coverage
  fig, ax = plt.subplots()

  #ax.errorbar(FcSwellingWhite, Fc, xerr = FcSwellingWhitesigma,c = '#66CDAA', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  #ax.scatter(FcSwellingWhite, Fc_gold,c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  #ax.scatter(FcSwellingWhite[26:33], Fc[26:33], c='red', marker = '^', s=40)
  #ax.scatter(FcSwellingWhite[26:33], FcVersion2[26:33], c='red', marker = 'd', s=40)
  ax.scatter(np.concatenate([FcSwellingWhite[0:16],FcSwellingWhite[39:43]]), np.concatenate([Fc[0:16],Fc[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(FcSwellingWhite[16:39], Fc[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  ax.scatter(np.concatenate([FcSwellingWhite[0:16],FcSwellingWhite[39:43]]), np.concatenate([FcVersion2[0:16],FcVersion2[39:43]]), c='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Fast ramp', alpha =0.7)
  ax.scatter(FcSwellingWhite[16:39], FcVersion2[16:39], facecolors = 'none', edgecolors='#66CDAA', marker = 'd', s=40, label='SCIANTIX 2.0 - Slow ramp', alpha =0.7)
  

  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[1e-3, 1e3], '-', color = '#757575')
  ax.plot([1e-3, 1e3],[2e-3, 2e3],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[5e-4, 5e2],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(1, 100)
  ax.set_ylim(1, 100)
  
  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.set_title('Fractional coverage of grain faces')
  ax.legend()
  plt.savefig('Images/Fc-White2004')
  plt.show()

    # GOLD vs. SCIANTIX 2.0 - fractional coverage
  fig, ax = plt.subplots()

  #ax.errorbar(FcSwellingWhite, Fc, xerr = FcSwellingWhitesigma,c = '#66CDAA', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  #ax.scatter(FcSwellingWhite, Fc_gold,c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)', zorder=2, alpha=0.7)
  #ax.scatter(FcSwellingWhite[26:33], Fc[26:33], c='red', marker = '^', s=40)
  #ax.scatter(FcSwellingWhite[26:33], FcVersion2[26:33], c='red', marker = 'd', s=40)
  ax.scatter(np.concatenate([FvSwellingWhite[0:16],FvSwellingWhite[39:43]]), np.concatenate([Fv[0:16],Fv[39:43]]), c='#9370DB', marker = '^', s=40, label='This work - Fast ramp')
  ax.scatter(FvSwellingWhite[16:39], Fv[16:39], facecolors='none', edgecolors='#9370DB', marker = '^', s=40, label='This work - Slow ramp')
  

  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[2.5, 102.5],'--', color = '#757575')
  # ax.plot([0, 100],[-2.5, 97.5],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[1e-3, 1e3], '-', color = '#757575')
  ax.plot([1e-3, 1e3],[2e-3, 2e3],'--', color = '#757575')
  ax.plot([1e-3, 1e3],[5e-4, 5e2],'--', color = '#757575')
  ax.grid(color='gray', linestyle='--', linewidth=0.5)
  ax.set_xlim(1, 100)
  ax.set_ylim(1, 100)
  
  ax.set_xscale('log')
  ax.set_yscale('log')

  ax.set_xlabel('Experimental (%)')
  ax.set_ylabel('Calculated (%)')
  ax.set_title('Vented fraction of grain faces')
  ax.legend()
  plt.savefig('Images/Fv-White2004')
  plt.show()

  # # GOLD vs. SCIANTIX 2.0 - vented fraction
  # fig, ax = plt.subplots()

  # #ax.errorbar(FvSwellingWhite, Fv, xerr = FvSwellingWhitesigma,c = '#66CDAA', fmt='o', capsize=1, ecolor='#999AA2', elinewidth = 0.6, label='This work')
  # ax.scatter(FvSwellingWhite, Fv,c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  # ax.scatter(FvSwellingWhite, Fv_gold, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)',zorder=2, alpha=0.7)

  # ax.plot([0, 100],[0, 100], '-', color = '#757575')
  # ax.plot([0, 100],[20, 120],'--', color = '#757575')
  # ax.plot([0, 100],[-20, 80],'--', color = '#757575')
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)
  # ax.set_xlim(0, 100)
  # ax.set_ylim(0, 100)

  # ax.set_xlabel('Experimental (%)')
  # ax.set_ylabel('Calculated (%)')
  # ax.set_title('Vented fraction of grain faces')
  # ax.legend()
  
  # plt.savefig('Images/Fv-White2004')
  # plt.show()

  # # FGR plot
  # fig, ax = plt.subplots()
  # ax.scatter(gbSwelling2, FGR2, c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  # ax.scatter(gbSwelling2, FGR_gold, c = '#ff7f0e', marker = 'o', s=40, label='FGR SCIANTIX 2.0 - Gold')
  # ax.grid(color='gray', linestyle='--', linewidth=0.5)
  # ax.set_xscale('log')
  # ax.set_yscale('log')

  # ax.set_xlabel('Swelling (%)')
  # ax.set_ylabel('FGR (%)')

  # ax.legend()

  # plt.show()

  #######
  # DATA
  #######

  fig, ax = plt.subplots()

  #ax.scatter(gbSwellingWhite, gbSwelling1, c = '#FA82B4', edgecolors= '#999AA2', marker = '^', s=40, label='SCIANTIX 1.0')
  ax.scatter(gbSwellingWhite, gbSwelling2, c='#9370DB', marker = '^', s=40, label='This work', zorder = 1)
  ax.scatter(gbSwellingWhite, gold, c = '#ff7f0e', marker = 'o', s=40, label='Barani (2017)',zorder=2, alpha=0.7)
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
      bbarea2.append(data[row_number,bubbleAreaPos].astype(float))

      # Retrieve the gold data of Intergranular bubble area (m2)
      bubbleAreaGoldPos = findSciantixVariablePosition(data_gold, "Intergranular bubble area (m2)")
      bbarea2_gold.append(data_gold[row_number,bubbleAreaGoldPos].astype(float))

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
  deviations_Version2 = abs(np.array(gbSwellingWhite) - gbSwellingVersion2)
  deviations_gold = abs(np.array(gbSwellingWhite)-gold)
  
  print(f"SCIANTIX 1.0 - MAD: ", np.median(deviations_1))
  print(f"This work - MAD: ", np.median(deviations_2))
  print(f"SCIANTIX 2.0 - MAD: ", np.median(deviations_Version2))
  print(f"Barani (2017) - MAD: ", np.median(deviations_gold))

  # RMSE
  print(f"SCIANTIX 1.0 - RMSE: ", np.mean(np.array(gbSwellingWhite) - gbSwelling1)**2)
  print(f"SCIANTIX 2.0 - RMSE: ", np.mean(np.array(gbSwellingWhite) - gbSwelling2)**2)
  print("\n")

  return folderList, number_of_tests, number_of_tests_failed
