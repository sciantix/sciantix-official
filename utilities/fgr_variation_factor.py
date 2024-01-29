# Variation factors of ﬁssion gas release: fit from G. Pastore et al. / Journal of Nuclear Materials 456 (2015) 398–408

# FGR                   Variation factor
# 0.05485687542441836	13.52914798206278
# 0.09192672168556029	5.493273542600898
# 0.11446631877106794	4.488789237668161
# 0.14275414629968372	2.8565022421524677
# 0.16854226442104747	2.7309417040358746
# 0.19023486011782775	2.6995515695067276
# 0.20047871331648914	2.291479820627803
# 0.22052777760784048	2.4798206278026917
# 0.25818217412317457	2.448430493273543

import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def fit_function(x, a, b, c):
    return a / x**2 + b / x  + c

points = np.array([
    [0.05485687542441836, 13.52914798206278],
    [0.09192672168556029, 5.493273542600898],
    [0.11446631877106794, 4.488789237668161],
    [0.14275414629968372, 2.8565022421524677],
    [0.16854226442104747, 2.7309417040358746],
    [0.19023486011782775, 2.6995515695067276],
    [0.20047871331648914, 2.291479820627803],
    [0.22052777760784048, 2.4798206278026917],
    [0.25818217412317457, 2.448430493273543]
])

params, covariance = curve_fit(fit_function, points[:,0], points[:,1])
a_fit, b_fit, c_fit = params

print("Fit function = a / x**2 + b / x  + c")
print(f"a = {a_fit}")
print(f"b = {b_fit}")
print(f"c = {c_fit}")

x_fit = np.linspace(0.05, 0.3, 100)
y_fit = fit_function(x_fit, a_fit, b_fit, c_fit)

# plt.scatter(points[:,0], points[:,1], label='Data points')
# plt.plot(x_fit, y_fit, label='Fitted curve', color='red')
# plt.xlabel('FGR mean (/)')
# plt.ylabel('FGR variation factor (/)')
# plt.legend()
# plt.show()

# Exp
calculated = np.array([1.43, 21.18, 2.47, 11.7, 0.83, 24.23, 20.6, 0.63, 17.67, 1.47, 9.9, 0.3, 14.36, 0.46, 21.06, 2.02, 10.12, 0.37, 16.5, 28.9, 15.35, 13.71, 15.1, 19.64, 17.11, 18.45, 18.58, 12.83, 15.91, 6.1, 6.44, 8.63, 5.57, 4.48, 4.37, 4.26, 2.39, 2.41, 4.64, 4.63, 4.52, 4.75])
deviation = np.array([7.15, 0.597, 1.65, 1.15, 2.1, 1.7, 1.11, 3.15, 0.48, 0.98, 0.97, 0.75, 1.02, 2.3, 0.593, 1.35, 0.99, 0.92, 1.17, 1.56, 1.81, 1.01, 0.68, 1.51, 0.61, 0.57, 0.41, 1.35, 1.53, 0.57, 0.4, 0.3, 0.2, 1.28, 0.65, 0.7, 0.56, 0.65, 2.9, 5.79, 0.87, 0.68])

an3_i = 0
regate_i = 2
hatac_c2_i = 4
contact1_i = 6
superramp_i = 19

superramp_tu = np.array([18.20333, 18.3073, 20.65603, 18.81196,
                         24.377, 28.902870000000004, 30.32572, 19.68051, 24.120269999999998,
                         np.nan, np.nan, np.nan, np.nan,
                         5.881843, 6.374357, 5.514548,
                         7.893537, 7.728256,
                         np.nan, np.nan, np.nan, np.nan])

superramp_tu_dev = np.array([2.14156824, 1.346125, 0.93466199, 1.44707385, 0.87060714, 0.90040093, 0.67540579, 2.07163263, 2.31925673, np.nan, np.nan, np.nan, np.nan, 0.20710715, 1.82124486, 0.82306687, 1.29402246, 1.79726884, np.nan, np.nan, np.nan, np.nan])

# plt.scatter(calculated, deviation)

# markers = ( 'o', 's', '^', 'v', '<', '>', '+', 'X', '*')
# colors = ( 'C0', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8')

plt.scatter(np.concatenate([calculated[an3_i:an3_i+2], calculated[an3_i+6:an3_i+8], calculated[an3_i+12:an3_i+14]]),
            np.concatenate([deviation[an3_i:an3_i+2], deviation[an3_i+6:an3_i+8], deviation[an3_i+12:an3_i+14]]),
            label='AN3', marker='o', color = 'C0')

plt.scatter(np.concatenate([calculated[regate_i:regate_i+2], calculated[regate_i+6:regate_i+8], calculated[regate_i+12:regate_i+14]]),
            np.concatenate([deviation[regate_i:regate_i+2], deviation[regate_i+6:regate_i+8], deviation[regate_i+12:regate_i+14]]),
            label='REGATE', marker='s', color = 'C1')

plt.scatter(np.concatenate([calculated[hatac_c2_i:hatac_c2_i+2], calculated[hatac_c2_i+6:hatac_c2_i+8], calculated[hatac_c2_i+12:hatac_c2_i+14]]),
            np.concatenate([deviation[hatac_c2_i:hatac_c2_i+2], deviation[hatac_c2_i+6:hatac_c2_i+8], deviation[hatac_c2_i+12:hatac_c2_i+14]]),
            label='HATATC-C2', marker = '^', color = 'C2')

plt.scatter(np.array([calculated[contact1_i], calculated[contact1_i+12]]),
            np.array([deviation[contact1_i], deviation[contact1_i+12]]),
            label='CONTACT1', marker = 'v', color = 'C3')

plt.scatter(np.concatenate((calculated[19:41], superramp_tu)),
            np.concatenate((deviation[19:41], superramp_tu_dev)),
            label='Super-Ramp', marker='d', color='C4')

# plt.scatter(points[:,0] * 100, points[:,1], label='Data points (Pastore et al. 2015)')
plt.plot(x_fit * 100, y_fit, label='Deviation factor curve (Pastore et al. 2015)', color='red')
plt.plot(np.linspace(0,50), np.ones_like(np.linspace(0,50)), label='Deviation factor = 1', color='black')
plt.plot(np.linspace(0,50), 0.5*np.ones_like(np.linspace(0,50)), label='Deviation factor = 0.5', color='black', linestyle='--')
plt.plot(np.linspace(0,50), 2*np.ones_like(np.linspace(0,50)), label='Deviation factor = 2', color='black', linestyle='--')
plt.xlabel('Calculated fission gas release (%)')
plt.ylabel('Deviation factor w.r.t. measured data (/)')
plt.xlim(0,30)
plt.ylim(0,10)
plt.legend()

# save_path = '/home/giovanni/Documents/LaTeX/Paper - Integral validation SCIANTIX/figures'
# plt.savefig(save_path + "/deviation_factor_plot.png", bbox_inches='tight')

plt.show()