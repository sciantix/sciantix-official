import sys
sys.path.append('/home/shuo/sciantix-testMCMC/BI/Core')
import tools, subprocess
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(41)
subprocess.run(['./sciantix.x'])
fr_exp_info = tools.get_selected_variables_value_from_output(["Time (h)","He fractional release (/)"], 'output.txt')
np.savetxt('Talip2014_release_data_noiseless.txt', fr_exp_info, delimiter = '\t')
fr_exp = fr_exp_info[:,1].copy()

noise = np.random.normal(0, 0.1 * fr_exp_info[:,1]) # creat noise
fr_exp_info[:,1] = fr_exp_info[:,1] + noise # add noise
np.savetxt('Talip2014_release_data.txt', fr_exp_info, delimiter = '\t')

plt.plot(fr_exp_info[:,0], fr_exp_info[:,1])
plt.plot(fr_exp_info[:,0], fr_exp)
plt.show()