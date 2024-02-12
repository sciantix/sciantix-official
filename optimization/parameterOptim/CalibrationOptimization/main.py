import numpy as np
import matplotlib.pyplot as plt
from user_model import UserModel  # Make sure to import your classes
from optimization import Optimization
from domain_reduction import DomainReduction
from bayesian_calibration_pa import BayesianCalibration
import matplotlib, os, shutil
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
def main():

    
    # Initialize the UserModel with appropriate parameters
    model = UserModel(
        case_name='test_Talip2014_1600K',
        #params=np.array(['helium diffusivity pre exponential', 'henry constant pre exponential']),
        #params_initial_values=np.array([0,0]),
        #params_stds=np.array([1.526,1.417])
        params=np.array(['helium diffusivity pre exponential']),
        params_initial_values=np.array([0]),
        params_stds=np.array([1.526])
    ) 
    params_info = model.params_info
    keys = np.array([key for key in params_info.keys()])
    initial_values = np.array([info['mu'] for info in params_info.values()])
    stds = np.array([info['sigma'] for info in params_info.values()])

    
    time_points = np.linspace(0, max(model.time_exp),201)
    # Perform Bayesian Calibration
    bc = BayesianCalibration(
        keys=keys,
        mean_values=initial_values, 
        stds=stds,
        initial_sampling_number=501, 
        time_point=time_points, 
        online= True,
        data_points_number=501
    )
    
    op = Optimization(
        method='som',
        keys=keys,
        initial_values=initial_values,
        stds=stds,
        online=True
    )
    
    dr = DomainReduction()
    dr.initialize(op)
    
    bc.bayesian_calibration(model, op,dr)

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
        

    shutil.rmtree('Optimization')
    shutil.rmtree('Bayesian_calibration')



if __name__ == "__main__":
    main()
