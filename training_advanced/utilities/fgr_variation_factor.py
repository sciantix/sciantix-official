# Variation factors of ﬁssion gas release: fit from G. Pastore et al. / Journal of Nuclear Materials 456 (2015) 398–408
# Author: G. Zullo

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

plt.scatter(points[:,0], points[:,1], label='Data points')
plt.plot(x_fit, y_fit, label='Fitted curve', color='red')
plt.xlabel('FGR mean (/)')
plt.ylabel('FGR variation factor (/)')
plt.legend()
plt.show()
