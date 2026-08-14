#Radial plot

import numpy as np
import matplotlib.pyplot as plt

# === Caricamento dati sperimentali ===
data_exp_40 = np.genfromtxt("exp_porosity_40.txt")  
r_radius_40 = data_exp_40[:, 0]
exp_porosity_40 = data_exp_40[:, 1]
data_exp_67 = np.genfromtxt("exp_porosity_67.txt")  
r_radius_67 = data_exp_67[:, 0]
exp_porosity_67 = data_exp_67[:, 1]
data_exp_97 = np.genfromtxt("exp_porosity_97.txt") 
r_radius_97 = data_exp_97[:, 0]
exp_porosity_97 = data_exp_97[:, 1] 
data_exp_102 = np.genfromtxt("exp_porosity_102.txt") 
r_radius_102 = data_exp_102[:, 0]
exp_porosity_102 = data_exp_102[:, 1] 
data_exp_Manzel = np.genfromtxt("exp_Manzel.txt")
r_radius_Manzel = data_exp_Manzel[:,0]
exp_porosity_Manzel = data_exp_Manzel[:,1]
porosity_Cappia = np.genfromtxt("exp_porosity_Cappia.txt")
r_porosity_Cappia = porosity_Cappia[:, 0]
exp_porosity_Cappia = porosity_Cappia[:, 1]

# === Caricamento modelli da letteratura ===
model_40 = np.genfromtxt("model_porosity_40.txt")
x_40 = model_40[:, 0]
y_40 = model_40[:, 1]
model_67 = np.genfromtxt("model_porosity_67.txt")
x_67 = model_67[:, 0]
y_67 = model_67[:, 1]
model_97 = np.genfromtxt("model_porosity_97.txt")
x_97 = model_97[:, 0]
y_97 = model_97[:, 1]
model_102 = np.genfromtxt("model_porosity_102.txt")
x_102 = model_102[:, 0]
y_102 = model_102[:, 1]
model_Jernkvist = np.genfromtxt("model_Jernkvist.txt")
x_Jernkvist = model_Jernkvist[:, 0]
y_Jernkvist = model_Jernkvist[:, 1]
model_Barani = np.genfromtxt("model_Barani.txt")
x_Barani = model_Barani[:, 0]
y_Barani = model_Barani[:, 1]


# === Caricamento modello del tuo lavoro ===
model_this_work = np.genfromtxt("model_this_work.txt")
x_this = model_this_work[:, 0]
y_this = model_this_work[:, 1]

# === Plot Porosità ===
plt.figure(figsize=(8, 5))
plt.plot(x_this, y_this, '*', color='green', linewidth= 2, label='This work, $bu_{AV}= 79$ MWd/kgU', markersize=13)
plt.plot(x_this, y_this, linestyle='--', color='green')
plt.plot(x_Barani, y_Barani, '*', color='sienna', linewidth= 2, label='Barani (2022)', markersize=13)
plt.plot(x_Barani, y_Barani, linestyle='--', color='sienna')
#plt.plot(x_40, y_40, '-', color='red', linewidth= 1.5, label='Lemes (2023), $bu_{AV}= 40$ MWd/kgU')
#plt.plot(x_67, y_67, '-', color='deeppink', linewidth= 1.5, label='Lemes (2023), $bu_{AV}= 67$ MWd/kgU ')
#plt.plot(x_97, y_97, '-', color='blue', linewidth= 1.5, label='Lemes (2023), $bu_{AV}= 97$ MWd/kgU ')
#plt.plot(x_102, y_102, '-', color='orangered', label='Lemes (2023), $bu_{AV}= 102$ MWd/kgU ')
#plt.plot(x_Jernkvist, y_Jernkvist, '-', color='indigo',linewidth= 1.5, label='Jernkvist (2004)')
#plt.plot(r_radius_40, exp_porosity_40, 'o', color='red', label='Spino (2005), $bu_{AV}= 40$ MWd/kgU', markersize=5)
plt.plot(r_radius_67, exp_porosity_67, 'D', color='deeppink', label='Spino (2005), $bu_{AV}= 67$ MWd/kgU', markersize=4)
plt.plot(r_radius_97, exp_porosity_97, 'D', color='blue', label='Spino (2005), $bu_{AV}= 97$ MWd/kgU', markersize=4)
plt.plot(r_porosity_Cappia, exp_porosity_Cappia, 'o', color='gray', label='Cappia (2016)', markersize=7)
#plt.plot(r_radius_102, exp_porosity_102, 'o', color='orangered', label='Spino (2005), $bu_{AV}= 102$ MWd/kgU', markersize=5)
#plt.plot(r_radius_Manzel, exp_porosity_Manzel, 'D', color='gold', label='Manzel et al. (2000)', markersize=5)
plt.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)

# === Formattazione ===
plt.xlabel('Relative radius (/)', fontsize=15)
plt.ylabel('Porosity (/)', fontsize=15)
plt.ylim(0, 0.22)
plt.xlim(0.8, 1.005)
plt.legend()
#plt.grid(False)
plt.tight_layout()

plt.show()


# === Caricamento dati sperimentali R ===
radius_80 = np.genfromtxt("exp_radius_80.txt")  
r_radius_80 = radius_80[:, 0]
exp_radius_80 = radius_80[:, 1]
radius_98 = np.genfromtxt("exp_radius_98.txt")  
r_radius_98 = radius_98[:, 0]
exp_radius_98 = radius_98[:, 1]


# === Caricamento modelli da letteratura ===
model_80 = np.genfromtxt("model_radius_80.txt")
x_80 = model_80[:, 0]
y_80 = model_80[:, 1]
model_98 = np.genfromtxt("model_radius_98.txt")
x_98 = model_98[:, 0]
y_98 = model_98[:, 1]

# === Caricamento modello del tuo lavoro ===
model_this_work_radius = np.genfromtxt("model_this_work_radius.txt")
x_this_radius = model_this_work_radius[:, 0]
y_this_radius = model_this_work_radius[:, 1]

# === Plot Raggio ===
plt.figure(figsize=(8, 5))
plt.plot(x_this_radius, y_this_radius, '*', color='green', linewidth= 2, label='This work, $bu_{AV}= 90$ MWd/kgU', markersize=13)
plt.plot(x_this_radius, y_this_radius, linestyle='--', color='green')
#plt.plot(x_80, y_80, '-', color='red', linewidth= 1.5, label='Lemes (2023), $bu_{AV}= 80$ MWd/kgU')
#plt.plot(x_98, y_98, '-', color='blue', linewidth= 1.5, label='Lemes (2023), $bu_{AV}= 98$ MWd/kgU ')
plt.plot(r_radius_80, exp_radius_80, 'o', color='red', label='Spino (2006), $bu_{AV}= 80$ MWd/kgU', markersize=5)
plt.plot(r_radius_98, exp_radius_98, 'o', color='blue', label='Spino (2006), $bu_{AV}= 98$ MWd/kgU', markersize=5)
plt.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)

# === Formattazione ===
plt.xlabel('Relative radius (/)', fontsize=15)
plt.ylabel('Average pore radius (m)', fontsize=15)
plt.ylim(4e-7, 1.4e-6)
plt.xlim(0.8, 1.005)
plt.legend()
#plt.grid(False)
plt.tight_layout()

plt.show()

# === Caricamento dati sperimentali Np ===
density_40 = np.genfromtxt("exp_density_40.txt")  
r_density_40 = density_40[:, 0]
exp_density_40 = density_40[:, 1]
density_57 = np.genfromtxt("exp_density_57.txt")  
r_density_57 = density_57[:, 0]
exp_density_57 = density_57[:, 1]
density_67 = np.genfromtxt("exp_density_67.txt")  
r_density_67 = density_67[:, 0]
exp_density_67 = density_67[:, 1]
density_Cappia = np.genfromtxt("exp_density_Cappia.txt")
r_density_Cappia = density_Cappia[:, 0]
exp_density_Cappia = density_Cappia[:, 1]

# === Caricamento modelli da letteratura Np ===
model_r_40 = np.genfromtxt("model_density_40.txt")
x_r_40 = model_r_40[:, 0]
y_r_40 = model_r_40[:, 1]
model_r_57 = np.genfromtxt("model_density_57.txt")
x_r_57 = model_r_57[:, 0]
y_r_57 = model_r_57[:, 1]
model_r_67 = np.genfromtxt("model_density_67.txt")
x_r_67 = model_r_67[:, 0]
y_r_67 = model_r_67[:, 1]
model_Barani_Np = np.genfromtxt("model_Barani_Np.txt")
x_Barani_Np = model_Barani_Np[:, 0]
y_Barani_Np = model_Barani_Np[:, 1]

# === Caricamento modello del tuo lavoro Np ===
model_this_work_Np = np.genfromtxt("model_this_work_Np.txt")
x_this_density = model_this_work_Np[:, 0]
y_this_density = model_this_work_Np[:, 1]
model_this_work_Np_57 = np.genfromtxt("model_this_work_Np_57.txt")
x_this_density_57 = model_this_work_Np_57[:, 0]
y_this_density_57 = model_this_work_Np_57[:, 1]
model_this_work_Np_67 = np.genfromtxt("model_this_work_Np_67.txt")
x_this_density_67 = model_this_work_Np_67[:, 0]
y_this_density_67 = model_this_work_Np_67[:, 1]

# === Plot Densità ===
plt.figure(figsize=(8, 5))
plt.plot(x_this_density, y_this_density, '*', color='green', linewidth= 2, label='This work, $bu_{AV}= 40$ MWd/kgU', markersize=13)
plt.plot(x_this_density, y_this_density, linestyle='--', color='green')
plt.plot(x_this_density_57, y_this_density_57, '*', color='peru', linewidth= 2, label='This work,  $bu_{AV}= 57$ MWd/kgU', markersize=13)
plt.plot(x_this_density_57, y_this_density_57, linestyle='--', color='peru')
plt.plot(x_this_density_67, y_this_density_67, '*', color='aquamarine', linewidth= 2, label='This work, $bu_{AV}= 67$ MWd/kgU', markersize=13)
plt.plot(x_this_density_67, y_this_density_67, linestyle='--', color='aquamarine')
plt.plot(x_Barani_Np, y_Barani_Np, '*', color='sienna', linewidth= 2, label='Barani (2022)', markersize=13)
plt.plot(x_Barani_Np, y_Barani_Np, linestyle='--', color='sienna')
#plt.plot(x_r_40, y_r_40, '-', color='red', linewidth= 1.5, label='Matzke (1992), $bu_{AV}= 40$ MWd/kgU')
#plt.plot(x_r_57, y_r_57, '-', color='blue', linewidth= 1.5, label='Matzke (1992), $bu_{AV}= 57$ MWd/kgU ')
#plt.plot(x_r_67, y_r_67, '-', color='deeppink', linewidth= 1.5, label='Matzke (1992), $bu_{AV}= 67$ MWd/kgU ')
plt.plot(r_density_40, exp_density_40, 'o', color='red', label='Spino (1996), $bu_{AV}= 40$ MWd/kgU', markersize=5)
plt.plot(r_density_57, exp_density_57, 'o', color='blue', label='Spino (1996), $bu_{AV}= 57$ MWd/kgU', markersize=5)
plt.plot(r_density_67, exp_density_67, 'o', color='deeppink', label='Spino (1996), $bu_{AV}= 67$ MWd/kgU', markersize=5)
plt.plot(r_density_Cappia, exp_density_Cappia, 'o', color='gray', label='Cappia (2016)', markersize=5)
plt.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)

# === Formattazione ===
plt.xlabel('Relative radius (/)', fontsize=15)
plt.ylabel('Pore number density (pore $m^{-3}$)', fontsize=15)
plt.ylim(0, 6e17)
plt.xlim(0.9, 1.005)
plt.legend()
#plt.grid(False)
plt.tight_layout()

plt.show()
