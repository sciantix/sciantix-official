import numpy as np
import tools
import subprocess

subprocess.run(['./sciantix.x'])

time_model = tools.get_selected_variables_value_from_output(["Time (h)"], 'output.txt')
np.savetxt('time_model.txt', time_model)