#!/usr/bin/env python
# -*-coding:utf-8 -*-
'''
@Time    :   16/03/2023 09:59:15
@Author  :   Marty 
'''
'''
@Last edit:   16/03/2023 09:59:18
'''


# %% Per eliminare le cartelle
import re
import shutil
for directory in os.listdir():
    if re.match('Gold', directory) and os.path.isdir(directory): shutil.rmtree(directory)
    if re.match('ROM 5',directory) and os.path.isdir(directory): shutil.rmtree(directory)
    if re.match('ROM 10',directory) and os.path.isdir(directory): shutil.rmtree(directory)


#%%
from IPython import get_ipython
get_ipython().magic('clear')
get_ipython().magic('reset -f')

import os
import numpy as np
from matplotlib import pyplot as plt
import math 
import random
import shutil
import pandas as pd
#Bisogna mettere l'eseguibile nella cartella
#%%
root = os.getcwd()
exe = 'sciantix.x'

output_string = 'output.txt'
input_settings_string = 'input_settings.txt'
input_history_string = 'input_history.txt'
input_initialcond_string = 'input_initial_conditions.txt'

exe_path = os.path.join(root,exe)
input_set_path = root+input_settings_string 
input_icond_path = root+input_initialcond_string

gold_txt=open('gold.txt',"w+")
rom5_txt=open('ROM 5.txt',"w+")
rom10_txt=open('ROM 10.txt',"w+")
solver = [1,7,8]


for kk in range (0,50):  #Numero di storie/cartelle/puntini che comporrano il plot
    Number_of_step=random.randint(2,7) 
    Temperature=np.zeros(Number_of_step)
    Fission_rate=np.zeros(Number_of_step)
    Number_of_history=str(kk)
    Time=np.zeros(Number_of_step)
    
    for jj in range (0,Number_of_step):
        if jj != 0: 
            if Time[jj-1] == 10000: 
                Number_of_step = jj
                break
            else: Time[jj]= random.randint(Time[jj-1]+1,10000)
        Temperature[jj]=random.randint(500,3000)       #Sceglie randomicamente un valore di temperatura.
        Fission_rate[jj]=random.randint(0,30)*1e+18     #Sceglie randomicamente un valore di fission rate. random.randint(0,30)*1e+18
     

    for i in solver: 
        if i==1: nome_solver="Gold"
        if i==7: nome_solver="ROM 5"
        if i==8: nome_solver="ROM 10"
        settings_txt = open('input_settings.txt','w+')
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iGrainGrowth (0= no grain growth, 1= Ainscough et al. (1973), 2= Van Uffelen et al. (2013))');
        settings_txt.write('%d\t' %i + '%s\t' %'#' + '%s\r\n' %'iDiffusionSolver (1= SDA with quasi-stationary hypothesis, 2= SDA without quasi-stationary hypothesis, 7 = ROM with 5 basis and quasi-stationary hypothesis, 8 = ROM with 10 basis and quasi-stationary hypothesis)');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iIntraGranularBubbleEvolution (1= Pizzocri et al. (2018))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iResolutionRate (0= constant value, 1= Turnbull (1971), 2= Losonen (2000))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iTrappingRate (0= constant value, 1= Ham (1958))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iNucleationRate (0= constant value, 1= Olander, Wongsawaeng (2006))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iOutput (1= default output files)');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iGrainBoundaryVacancyDiffusivity (0= constant value, 1= Reynolds and Burton (1979), 2= Pastore et al. (2015))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iGrainBoundaryBehaviour (0= no grain boundary bubbles, 1= Pastore et al (2013))');
        settings_txt.write('%d\t' %1 + '%s\t' %'#' + '%s\r\n' %'iGrainBoundaryMicroCracking (0= no model considered, 1= Barani et al. (2017))');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iFuelMatrix (0= UO2, 1= UO2 + HBS)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iGrainBoundaryVenting (0= no model considered, 1= Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iRadioactiveFissionGas (0= not considered)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHelium (0= not considered)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHeDiffusivity (0= null value, 1= limited lattice damage, Luzzi et al. (2018), 2= significant lattice damage, Luzzi et al. (2018))');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iGrainBoundarySweeping (0= no model considered, 1= TRANSURANUS swept volume model)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHighBurnupStructureFormation (0= no model considered, 1= fraction of HBS-restructured volume from Barani et al. (2020))');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHBS_FGDiffusionCoefficient (0= constant value)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHighBurnupStructurePorosity (0= no evolution of HBS porosity, 1= HBS porosity evolution based on Spino et al. (2006) data)');
        settings_txt.write('%d\t' %0 + '%s\t' %'#' + '%s\r\n' %'iHeliumProductionRate (0= zero production rate, 1= helium from ternary fissions, 2= linear with burnup (FR))');
        settings_txt.close()

        initialconditions_txt = open('input_initial_conditions.txt','w+')
        initialconditions_txt.write('%.1e\t' %5e-06 + '%s\t' %'#' + '%s\r\n' %'Initial grain radius (m)')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial Xe (at/m3) produced, ig, ig-solution, ig-bubbles, gb, released')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial Kr (at/m3) produced, ig, ig-solution, ig-bubbles, gb, released')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial He (at/m3) produced, ig, ig-solution, ig-bubbles, gb, released')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial intragranular bubble concentration (at/m3), radius (m)')
        initialconditions_txt.write('%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'Initial fuel burnup (MWd/kgUO2)')
        initialconditions_txt.write('%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'Initial effective burnup (MWd/kgUO2)')
        initialconditions_txt.write('%.1f\t' %10970.0 + '%s\t' %'#' + '%s\r\n' %'Initial fuel density (kg/m3))')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %3.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %97.0 + '%s\t' %'#' + '%s\r\n' %'initial U234 U235 U236 U237 U238 (% of heavy atoms) content')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial Xe133 (at/m3) produced, ig, ig-solution, ig-bubbles, decayed, gb, released')
        initialconditions_txt.write('%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%.1f\t' %0.0 + '%s\t' %'#' + '%s\r\n' %'initial Kr85m (at/m3) produced, ig, ig-solution, ig-bubbles, decayed, gb, released')
        initialconditions_txt.close();

        if os.path.isdir(nome_solver+'_'+Number_of_history): shutil.rmtree(os.path.join(root, nome_solver+'_'+Number_of_history))
        os.mkdir(os.path.join(root, nome_solver+'_'+Number_of_history)) #Creo la cartella
        os.chdir(os.path.join(root, nome_solver+'_'+Number_of_history)) #Mi ci posiziono dentro
    
        history_txt = open('input_history.txt','w+')
        for jj in range(0,Number_of_step):
            if (jj == Number_of_step-1): history_txt.write('%d\t' %Time[jj] +'%d\t' %Temperature[jj] + '%.1e\t' %Fission_rate[jj] + '%.1f' %0.0)
            else: history_txt.write('%d\t' %Time[jj] +'%d\t' %Temperature[jj] + '%.1e\t' %Fission_rate[jj] + '%.1f\n' %0.0)

        history_txt.close()
    
        shutil.copy("../input_settings.txt", os.getcwd())
        shutil.copy("../input_initial_conditions.txt", os.getcwd())
        shutil.copy("../sciantix.x", os.getcwd())
        os.system("./sciantix.x")

        output_file = pd.read_table("output.txt")
        output=output_file.loc[output_file.index[-1],'Xe in grain (at/m3)']

        if i==1: gold_txt.write('%.1f\n' % output)
        if i==7: rom5_txt.write('%.1f\n' % output)
        if i==8: rom10_txt.write('%.1f\n' % output)

        #shutil.rmtree(os.path.join(root, nome_solver+'_'+Number_of_history)) #Elimina la cartella
        os.chdir('..')
    


gold_txt.close();
rom5_txt.close();
rom10_txt.close();


#%% PLOT

gold=np.loadtxt('gold.txt')
rom5=np.loadtxt('ROM 5.txt')
rom10=np.loadtxt('ROM 10.txt')

plt.figure(1, figsize=[5,5])
plt.scatter(gold,rom5, color='#0072bd', label='ROM 5 basis')
plt.scatter(gold,rom10, facecolors='none', edgecolors='#FF7F50', label='ROM 10 basis')
plt.plot(np.linspace(0,max(gold)),np.linspace(0,max(gold)), '--',color='#050402', linewidth=1)
plt.xlabel('Gold', fontsize=18)
plt.ylabel('ROM',fontsize=18)
plt.title('Numerical experiment',fontsize=13)
plt.rcParams['figure.edgecolor'] = '#050402'
plt.rcParams['axes.edgecolor'] = '#050402'
plt.grid(which='both', alpha=0.3)
plt.legend(prop={'size': 13}, loc='best')
plt.savefig('Numerical experiment.pdf',bbox_inches="tight")

# plt.figure(2, figsize=[5,5])
# plt.scatter(gold,rom10, facecolors='none', edgecolors='#FF7F50')
# plt.plot(np.linspace(0,max(gold)),np.linspace(0,max(gold)), '--',color='#050402', linewidth=1)
# plt.xlabel('Gold', fontsize=18)
# plt.ylabel('ROM 10 basis',fontsize=18)
# plt.title('Numerical experiment',fontsize=13)
# plt.rcParams['figure.edgecolor'] = '#050402'
# plt.rcParams['axes.edgecolor'] = '#050402'
# plt.grid(which='both', alpha=0.3)
# plt.savefig('Numerical experiment_10basis.pdf',bbox_inches="tight")

error5=np.zeros(np.size(gold))
error10=np.zeros(np.size(gold))
for i in range(0,np.size(gold)):
    error5[i]=(abs(gold[i]-rom5[i])/gold[i])*100
    error10[i]=(abs(gold[i]-rom10[i])/gold[i])*100

plt.figure(3, figsize=[5,5])
plt.scatter(np.arange(0,np.size(gold),1),error5,color='#0072bd', label = 'ROM 5 basis', linewidth=1.5)
plt.scatter(np.arange(0,np.size(gold),1),error10, facecolors='none', edgecolors='#FF7F50', label = 'ROM 10 basis',  linewidth=1.5)
plt.ylabel('Xenon in grain relative error (%)', fontsize=15)
plt.xlabel('History', fontsize=15)
plt.title('Numerical experiment', fontsize=15)
plt.grid(which='both', alpha=0.3)
plt.legend(prop={'size': 10}, loc='lower center')
plt.savefig("Xe error.pdf", bbox_inches="tight")


# %%
