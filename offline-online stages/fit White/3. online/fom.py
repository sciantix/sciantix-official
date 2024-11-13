# Import libraries:
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import scipy as sp
import scipy.sparse as sparse
import scipy.sparse.linalg as linalg
import sys
import os
import json

# Import user-defined libraries:
sys.path.append(os.path.abspath('/Users/martina/Library/CloudStorage/OneDrive-PolitecnicodiMilano/PhD/Git/rom-cylinder_DEIM-POD/offline-online stages/fit White/0. full order'))
from FreeFEM import FFmatrix_fread, FFvector_fread

#I valori di default corrispondono al caso implementato in OpenFOAM
def full_order_solution(RADIUS=1E-5, LENGTH=1E-3, FISSION_RATE=3E+19, FISSION_YIELD=0.24, FUEL_DENSITY = 11040, FUEL_SPECIFIC_HEAT = 400, T_BC=2E+03, C_BC=0.0, FUEL_THERMAL_CONDUCTIVITY = 2.208,TIME_FINAL = 1.00E+07, TIME_DELTA = 1.00E+04):
    # Definizione delle costanti 
    N_AVOG = 6.022141E+23  # [-]
    N_BOLT = 1.380649E-23  # [-]
    FISSION_ENERGY = 3.215E-11  # [J/fissions] 

    POWER_DENSITY = FISSION_ENERGY * FISSION_RATE  # q''' [W/m^3]
    SOURCE_T = POWER_DENSITY / (FUEL_DENSITY * FUEL_SPECIFIC_HEAT)  # [K/s]
    SOURCE_C = FISSION_RATE * FISSION_YIELD  # [atm / (m^3 s)]
    ALPHA_T = FUEL_THERMAL_CONDUCTIVITY / (FUEL_DENSITY * FUEL_SPECIFIC_HEAT)  # [m^2 / s]
    GAMMA_T = SOURCE_T * LENGTH**2 / ALPHA_T  # [ - ]

    # Altre costanti
    T_IC = T_BC
    C_IC = C_BC
    N_ITER = int(TIME_FINAL / TIME_DELTA)  # [-]

    # Definizione di una funzione per il coefficiente di diffusione
    def ALPHA_C(ZZ, T_BC=T_BC, GAMMA_T=GAMMA_T):
        return 2.949513e-13 * np.exp(-20487.36244 / (T_BC + GAMMA_T * (1 - ZZ**2) / 2))

    # Caricamento delle coordinate
    data_path = os.path.abspath('/Users/martina/Library/CloudStorage/OneDrive-PolitecnicodiMilano/PhD/Git/rom-cylinder_DEIM-POD/offline-online stages/fit White/0. full order')
    coordinates_Px = FFvector_fread(os.path.join(data_path, 'vv_cc_Px.btxt'))
    coordinates_Pq = FFvector_fread(os.path.join(data_path, 'vv_cc_Pq.btxt'))

    # Identificazione dei gradi di libertà
    sFO_Px = coordinates_Px.shape[0]
    sFO_Pq = coordinates_Pq.shape[0]
    mask_all = np.arange(sFO_Px, dtype=int)

    # Maschere per le condizioni al contorno
    mask_inf_bc = mask_all[np.isclose([coordinates_Px[ii, 2] for ii in range(sFO_Px)], np.zeros((sFO_Px)))]
    mask_sup_bc = mask_all[np.isclose([coordinates_Px[ii, 2] for ii in range(sFO_Px)], np.ones((sFO_Px)))]
    mask_mid_bc = mask_all[np.isclose([np.linalg.norm(coordinates_Px[ii, :2]) for ii in range(sFO_Px)], np.ones((sFO_Px)))]

    mask_bc_T = np.fromiter(set(mask_sup_bc), int)
    mask_bc_C = np.fromiter(set(mask_sup_bc) | set(mask_mid_bc) | set(mask_inf_bc), int)
    mask_in_T = [ii for ii in mask_all if ii not in mask_bc_T]
    mask_in_C = [ii for ii in mask_all if ii not in mask_bc_C]

    # Importazione delle matrici di massa
    mass_Px = FFmatrix_fread(os.path.join(data_path, 'ww_mm_Px.btxt'))
    mass_Pq = FFmatrix_fread(os.path.join(data_path, 'ww_mm_Pq.btxt'))
    volume = mass_Px.dot(np.ones(sFO_Px)).dot(np.ones(sFO_Px))

    weights_Pq = mass_Pq.diagonal()
    project_Pq = sparse.diags(np.reciprocal(weights_Pq))

    # Importazione delle mappe
    PxtoPquu_C = project_Pq.dot(FFmatrix_fread(os.path.join(data_path, 'ww_uu_Px_Pq.btxt'))[:, mask_in_C])
    PxtoPqdx_C = project_Pq.dot(FFmatrix_fread(os.path.join(data_path, 'ww_dx_Px_Pq.btxt'))[:, mask_in_C])
    PxtoPqdy_C = project_Pq.dot(FFmatrix_fread(os.path.join(data_path, 'ww_dy_Px_Pq.btxt'))[:, mask_in_C])
    PxtoPqdz_C = project_Pq.dot(FFmatrix_fread(os.path.join(data_path, 'ww_dz_Px_Pq.btxt'))[:, mask_in_C])

    # Assemblaggio delle matrici
    forc_C = PxtoPquu_C.T.dot(weights_Pq[:, None])
    mass_C = PxtoPquu_C.T.dot(PxtoPquu_C.multiply(weights_Pq[:, None]))
    inte_C = mass_Px.dot(np.ones((sFO_Px)))[mask_in_C] / volume

    stiff_C = PxtoPqdx_C.T.dot(PxtoPqdx_C.multiply(ALPHA_C(coordinates_Pq[:, 2:3]) / RADIUS**2 * weights_Pq[:, None])) + \
            PxtoPqdy_C.T.dot(PxtoPqdy_C.multiply(ALPHA_C(coordinates_Pq[:, 2:3]) / RADIUS**2 * weights_Pq[:, None])) + \
            PxtoPqdz_C.T.dot(PxtoPqdz_C.multiply(ALPHA_C(coordinates_Pq[:, 2:3]) / LENGTH**2 * weights_Pq[:, None]))

    # Soluzione
    sol_new_C = np.zeros((sFO_Px, N_ITER+1)) 
    average_C = np.zeros(N_ITER)

    with open('FOM.csv', 'w') as f:
        f.write('Time (s),Average dC (atm/m^3)\n')

    for ii in range(N_ITER):
        cur_time = ii * TIME_DELTA
        print('Current time:', cur_time, 's')
        average_C[ii] = inte_C @ sol_new_C[mask_in_C, ii]
        print('Average dC:', average_C[ii], 'atm/m^3\n')

        with open('FOM.csv', 'a') as f:
            f.write(f'{cur_time},{average_C[ii]}\n')

        cur_lhs = mass_C + TIME_DELTA * stiff_C
        cur_rhs = mass_C.dot(sol_new_C[mask_in_C, ii:ii+1]) + TIME_DELTA * (SOURCE_C * forc_C)

        sol_new_C[mask_in_C, ii+1], _ = linalg.bicgstab(cur_lhs, cur_rhs)

    final_time = N_ITER * TIME_DELTA
    final_average_dC = mass_Px.dot(sol_new_C[:, -1]).dot(np.ones((sFO_Px, 1)))[0] / volume

    print('Final time:', final_time, 's')
    print('Average dC:', final_average_dC, 'atm/m^3')

    with open('FOM.csv', 'a') as f:
        f.write(f'{final_time},{final_average_dC}\n')

