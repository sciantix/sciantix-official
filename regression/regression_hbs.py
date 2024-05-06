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
	#shutil.copy("../input_settings.txt", os.getcwd())
	#shutil.copy("../input_scaling_factors.txt", os.getcwd())

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
		try : 
			os.remove('output_gold.txt')
		except :
			pass
		os.rename('output.txt', 'output_gold.txt')

	except :
		print(f"output.txt not found")

# Plot the regression test results
def do_plot(label_x, label_y, experimental_data_x, experimental_data_y, calculations_x, calculations_y):

	# 1st plot
	fig, ax = plt.subplots(1,1)

	ax.set_xlabel(label_x)
	ax.set_ylabel(label_y)
	ax.plot(experimental_data_x, experimental_data_y, 'o', color = '#B3B3B3', label='Experimental')
	ax.plot(calculations_x, calculations_y, color = 'green', linewidth = 1, label='SCIANTIX 2.0')
	ax.set_frame_on(True)
	ax.legend()
	ax.legend(loc='best')

	plt.show()


# Main function of the Contact regression
def regression_hbs(wpath, mode_HBS, mode_gold, mode_plot, folderList, number_of_tests, number_of_tests_failed):

	# Exit of the function without doing anything
	if mode_HBS == 0:
		return folderList, number_of_tests, number_of_tests_failed

	# Get list of all files and directories in wpath
	files_and_dirs = os.listdir(wpath)

	# Sort them by filename
	sorted_files_and_dirs = sorted(files_and_dirs)

	# Iterate over sorted list
	for file in sorted_files_and_dirs:
	# Verify on a given folder, if Baker is in it's name
		if "HBS" in file and os.path.isdir(file):
			folderList.append(file)
			os.chdir(file)

			shutil.copy("../input_scaling_factors.txt", os.getcwd())

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

			exp_pore_density = import_data("exp_pore_density.txt").astype(float)
			exp_porosity = import_data("exp_porosity.txt").astype(float)
			exp_pore_radius = import_data("exp_pore_radius.txt").astype(float)

			# output.txt
			# find indexes
			burnupPos = findSciantixVariablePosition(data, "Burnup (MWd/kgUO2)")
			poreDensityPos = findSciantixVariablePosition(data, "HBS pore density (pores/m3)")
			porosityPos = findSciantixVariablePosition(data, "HBS porosity (/)")
			poreRadiusPos = findSciantixVariablePosition(data, "HBS pore radius (m)")

			# arrays
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
					poreDensity
				)

				do_plot(
					'Effective burnup (MWd kgHM${}^{-1}$)',
					'HBS porosity (/)',
					exp_porosity[:,0],
					exp_porosity[:,1],
					burnup,
					porosity
				)

				do_plot(
					'Effective burnup (MWd kgHM${}^{-1}$)',
					'Pore radius (m)',
					exp_pore_radius[:,0],
					exp_pore_radius[:,1],
					burnup,
					poreRadius
				)

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
				ax1.plot(sd["bu"] / 0.8814, sd["xe_igHBS"] / eq, color='orangered', linestyle=':', label='Xe in grains - HBS')
				ax1.plot(sd["bu"] / 0.8814, (sd["xe_igHBS"] + sd["xe_ig"]) / eq, color='dimgray', linestyle='-', label='Xe in grains - sum')
				ax1.legend(loc='upper right', fontsize='medium')
				ax1.set_xlabel('Burnup (GWd/tU)', fontsize='large')
				ax1.set_ylabel('Xe in grains (wt%)', color='black', fontsize='large')
				ax1.set_ylim(0, 1.75)
				ax2 = ax1.twinx()
				ax2.plot(sd["bu"], sd["alpha"], color='gold', linestyle='--', label='Restructured volume fraction (/)')
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
				ax2.plot(sd["bu"], sd["alpha"], color='gold', linestyle='--', label='Restructured volume fraction (/)')
				ax2.set_ylabel('Restructured volume fraction (/)', color='black', fontsize='large')
				plt.legend(fontsize='medium')
				plt.tight_layout()
				plt.show()
			
			os.chdir('..')

	return folderList, number_of_tests, number_of_tests_failed
