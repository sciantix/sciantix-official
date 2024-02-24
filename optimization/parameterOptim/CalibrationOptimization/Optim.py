from optimization import Optimization
from user_model import UserModel
from domain_reduction import DomainReduction
import numpy as np
import os, shutil

if os.path.exists('Optimization'):
    shutil.rmtree('Optimization')
os.makedirs('Optimization')
    # Initialize the UserModel with appropriate parameters
model = UserModel(
    case_name='test_Talip2014_1600K',
    # params=np.array(['helium diffusivity pre exponential', 'henry constant pre exponential']),
    # params_initial_values=np.array([0,0]),
    # params_stds=np.array([1.526,1.417])
    # params=np.array(['helium diffusivity pre exponential']),
    # params_initial_values=np.array([0]),
    # params_stds=np.array([1.526])
    params=np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy']),
    params_initial_values=np.array([0,1]),
    params_stds=np.array([1.526, 0.1])
) 
params_info = model.params_info
params_info = {key:params_info[key] for key in sorted(params_info)}
keys = np.array([key for key in params_info.keys()])
initial_values = np.array([info['mu'] for info in params_info.values()])
stds = np.array([info['sigma'] for info in params_info.values()])

op = Optimization(
    method='som',
    keys=keys,
    initial_values=initial_values,
    stds=stds,
    online=True
)
dr = DomainReduction()
dr.initialize(op)

time_points = np.linspace(0, max(model.time_exp), 20)
# time_points = np.array([max(model.time_exp)])
optimized_params = [[info['mu'] for info in params_info.values()]]
optimized_params_nolog = [[1,1]]
bounds_reducted = [op.bounds_dr]
optim_folder = [0]
for i in range(1,len(time_points)):
    optimize_result = op.optimize(model,time_points[:i+1],optimized_params[-1],bounds_reducted[-1])
    optim_folder.append(op.optim_folder)
    optimized_param_nolog = [optimize_result[key] for key in params_info.keys()]
    optimized_params_nolog.append(optimized_param_nolog)
    params_optimized = np.array(optimized_params_nolog)
    for key, value in optimize_result.items():
        if 'pre exponential' in key:
            optimize_result[key] = np.log(value)
    
    optimized_param = [optimize_result[key] for key in params_info.keys()]
    
    optimized_params.append(optimized_param)
    
    
    with open('params_optimized.txt', 'w') as file:
        file.writelines('\t'.join(str(item) for item in row) + '\n' for row in params_optimized[:-1])
        file.write('\t'.join(str(item) for item in params_optimized[-1]))
    if i == len(time_points) - 1:
        with model.change_directory(optim_folder[-1], os.getcwd()):
            final_optim_fr = model.get_selected_variables_value_from_output(["Time (h)","Temperature (K)","He fractional release (/)"],'output.txt')
        np.savetxt('final_optim_fr.txt', final_optim_fr)
    bound = dr.transform(op)
    bounds_reducted.append(bound)

fr_optim = np.zeros_like(time_points)
fr_exp = np.zeros_like(time_points)
variables = np.array(["Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"])

for i in range(1, len(time_points)):
    for folder in os.listdir(op.optim_container_path):
        folder_path = os.path.join(op.optim_container_path, folder)
        if f'to_{np.round(time_points[i],3)}' in folder:
            os.chdir(folder_path)
            fr_optim[i] = model.get_selected_variables_value_from_output_last_line(variables, 'output.txt')[2]
            fr_exp[i] = model._exp(time_points[i])[1]


data = np.vstack((time_points, fr_optim, fr_exp))
data = data.T
os.chdir(model.code_container)
with open('optim_data.txt', 'w') as file:
        file.writelines('\t'.join(str(item) for item in row) + '\n' for row in data[:-1])
        file.write('\t'.join(str(item) for item in data[-1]))