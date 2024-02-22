import numpy as np
from scipy.stats import norm, multivariate_normal, uniform
import os, shutil
from itertools import product
from data_generation import DataGeneration


class BayesianCalibration:
    def __init__(self, keys, mean_values, stds, initial_sampling_number, time_point, online, data_points_number):
        self.time_point = time_point
        self.sampling_number = initial_sampling_number
        self.online = online
        self.data_points_number = data_points_number
        params_info = {
            keys[i]: {
                'range':np.linspace(mean_values[i]-3*stds[i],mean_values[i]+3* stds[i], initial_sampling_number),
                'mu': mean_values[i],
                'sigma': stds[i]
            } for i in range(len(keys))
        }
        # print(params_info.values())
        self.params_info = {key:params_info[key] for key in sorted(params_info)}
        params_grid = np.meshgrid(*[info['range'] for info in self.params_info.values()], indexing = 'ij')
        self.params_grid = {key : grid for key, grid in zip(self.params_info.keys(), params_grid)}
        priors = [uniform.pdf(grid, loc = info['mu'] - 3*info['sigma'], scale =6* info['sigma']) for grid, info in zip(self.params_grid.values(), self.params_info.values())]
        # priors = [norm.pdf(grid, loc = info['mu'], scale = info['sigma']) for grid, info in zip(self.params_grid.values(), self.params_info.values())]
        joint_prior = np.ones(priors[0].shape)
        for prior in priors:
            joint_prior *= prior
        self.joint_prior = joint_prior/np.sum(joint_prior)
        
        params_combination = product(*[info['range'] for info in self.params_info.values()])

        self.points = np.array([[point for point in combination] for combination in params_combination])


    def bayesian_calibration(self, model, op, dr):
        self.setup_directory('Bayesian_calibration')
        self.setup_directory('Prediction')
        self.calibration_container_path = os.getcwd()
        max_params_over_time, optimized_params = [[info['mu'] for info in self.params_info.values()]], [[info['mu'] for info in self.params_info.values()]]
        priors_over_time = [self.joint_prior.flatten()]
        posteriors_over_time = []
        points_over_time = [self.points]
        bounds_reducted = [op.bounds_dr]
        bounds_global = np.array([[info['mu']-3 * info['sigma'], info['mu'] + 3* info['sigma']] for info in self.params_info.values()])
        optim_folder = [0]
        # optim_folder_for_prediction = [0] #optim folder at t_(i-2)
        # likelihood = 1
        calibration_data = []
        predicted_trace = []
        for i in range(1, len(self.time_point)):
            print(f'current time: {self.time_point[i]}')
            t_0 = self.time_point[i-1] if self.online else 0
            sciantix_folder_path = model._independent_sciantix_folder('Bayesian_calibration',optim_folder[-1], t_0, self.time_point[i])
            
            ##########prediction
            # prediction_sciantix_folder_path = model._independent_sciantix_folder('Prediction', optim_folder, t_0, self.time_point[i])
            if i == 1:
                confidence_boundary = np.array([[info['mu'] - 1.96* info['sigma'] for info in self.params_info.values()],
                                                [info['mu'] + 1.96 * info['sigma'] for info in self.params_info.values()]])
                # sf_to_run: np.array([sf_nominal, sf_at_max_probability, sf_low, sf_up])

                sf_to_run = np.array([[info['mu'] for info in self.params_info.values()],
                                      [info['mu'] for info in self.params_info.values()],
                                      [info['mu'] - 1.96* info['sigma'] for info in self.params_info.values()],
                                      [info['mu'] + 1.96 * info['sigma'] for info in self.params_info.values()]])
                prediction_sciantix_folder_path = model._independent_sciantix_folder('Prediction', optim_folder[-1], t_0, self.time_point[i])
            else:
                confidence_boundary = data_generator.confidence_boundary(confidence_cdf=0.95, number=1000)
                sf_to_run = np.array([[info['mu'] for info in self.params_info.values()],
                                      np.array(max_params_over_time[-1]),
                                      np.array(confidence_boundary[0]),
                                      np.array(confidence_boundary[1])])
                if self.online == False:

                    prediction_sciantix_folder_path = model._independent_sciantix_folder('Prediction', optim_folder[0], t_0, self.time_point[i])
                else:
                    prediction_sciantix_folder_path = model._independent_sciantix_folder('Prediction', optim_folder[i-2], self.time_point[i-2], self.time_point[i])
                

                print(f'confidence sf bounds:\n {confidence_boundary}') 
            # fr_prediction = self.compute_model_values(model, prediction_sciantix_folder_path, confidence_boundary, last_value=False)
            fr_prediction = self.compute_model_values(model, prediction_sciantix_folder_path, sf_to_run, last_value=False)
            predicted_trace.append(fr_prediction)
            np.set_printoptions(threshold=np.inf)
            with open('predicted_trace.txt', 'w') as file:
                for j, array in enumerate(predicted_trace):
                    # Let's add a header for each array for clarity
                    # Format the array into a string and write it to the file
                    # We use np.array2string to convert the 2D array into a readable string format
                    file.write(np.array2string(array, separator=', ') + "\n\n")

            observed = model._exp(time_point=self.time_point[i])
            model_values = self.compute_model_values(model, sciantix_folder_path, points_over_time[-1])
            likelihood = norm.pdf(observed[1], loc = model_values[:,0], scale = observed[2])
            # if self.online == True:
            #     likelihood = norm.pdf(observed[1], loc = model_values[:,0], scale = observed[2])
            # # elif self.online == False and i == 1:
            # #     likelihood = norm.pdf(observed[1], loc = model_values[:,0], scale = observed[2])
            # else:
            #     likelihood = norm.pdf(observed[1], loc = model_values[:,0], scale = observed[2]) * likelihood
            posterior = self.bayesian_update(priors_over_time[-1], likelihood)
            
            posteriors_over_time.append(posterior)

            max_params = self.find_max_params(posterior, points_over_time[-1])
            max_params_over_time.append(max_params)
            self.write_to_file('params_at_max_prob.txt', max_params_over_time)
            print(f'calibrated params(max prob): \n {max_params_over_time[-1]}')
            
            ## calculate output of the max params
            params = {key: value for key, value in zip(self.params_info.keys(), max_params)}
            calib_data = model._sciantix(sciantix_folder_path, params)
            calibration_data.append(calib_data)
            self.write_to_file('calibration_data.txt', calibration_data)

            optimize_result = op.optimize(model,t_0,self.time_point[i],optimized_params[-1],bounds_reducted[-1])
            optim_folder.append(op.optim_folder)

            for key, value in optimize_result.items():
                if 'pre exponential' in key:
                    optimize_result[key] = np.log(value)
            
            optimized_param = [optimize_result[key] for key in self.params_info.keys()]
            optimized_params.append(optimized_param)
            params_optimized = np.array(optimized_params)
            print(f'optimized params:\n {params_optimized[-1]}')
            with open('params_optimized.txt','w') as file:
                file.writelines('\t'.join(str(item) for item in row) + '\n' for row in params_optimized[:-1])
                file.write('\t'.join(str(item) for item in params_optimized[-1]))
            
            # if i == len(self.time_point) - 1:
            #     final_optim_sciantix_path = model._independent_sciantix_folder('Prediction', optim_folder[-1], t_0, self.time_point[i])
            #     fr_final_optim = self.compute_model_values(model, final_optim_sciantix_path, np.array(optimized_params[-1]), last_value = False)
            #     np.savetxt('final_optim_fr.txt', fr_final_optim)
            if i == len(self.time_point) - 1:
                with model.change_directory(optim_folder[-1], os.getcwd()):
                    final_optim_fr = model.get_selected_variables_value_from_output(["Time (h)","Temperature (K)","He fractional release (/)"],'output.txt')
                np.savetxt('final_optim_fr.txt', final_optim_fr)

            bound = dr.transform(op)
            bounds_reducted.append(bound)
            data_generator = DataGeneration(points_over_time[-1], posteriors_over_time[-1], self.data_points_number, [value for value in bound.values()], bounds_global)
            # data_generator = DataGeneration(points_over_time[-1], posteriors_over_time[-1], self.data_points_number, [value for value in bound.values()])
            new_points = data_generator.data_generated()
            new_probabilities = data_generator.probabilities_generated(new_points)
            priors_over_time.append(new_probabilities)
            points_over_time.append(new_points)
            with open('points_over_time.txt','w') as file:
                file.writelines('\t'.join(str(item) for item in row) + '\n' for row in points_over_time[:-1])
                file.write('\t'.join(str(item) for item in points_over_time[-1]))
            with open('priors_over_time.txt','w') as file:
                file.writelines('\t'.join(str(item) for item in row) + '\n' for row in priors_over_time[:-1])
                file.write('\t'.join(str(item) for item in priors_over_time[-1]))


        
        self.max_params_over_time = max_params_over_time
        self.optimized_params = optimized_params
        self.calibration_data = calibration_data
        self.predicted_trace = predicted_trace
        


    def setup_directory(self, dirname):
        if os.path.exists(dirname):
            shutil.rmtree(dirname)
        os.makedirs(dirname)

    def compute_model_values(self, model, folder_path, points, last_value = True):
        model_values = []

        for point in points:
            params = {key: value for key, value in zip(self.params_info.keys(), point)}
            if last_value == True:
                # output the last fr and rr
                model_value = model._sciantix(folder_path, params, last_value)[2:]
                model_values.append(model_value)
            else:
                # output the whole sequence value of fr
                print(params)
                model_value = model._sciantix(folder_path, params, last_value)[:,[0,2]]
                time = model_value[:,0]
                fr = model_value[:,1]
                model_values.append(fr)
        
        if last_value == True:
            model_values = np.array(model_values)
        else:
            model_values.insert(0,time)
            model_values = np.array(model_values).T
        return model_values

    def comput_bivariant_likelihood(self, observed, model_values, scale):
        mean_vectors = np.array([model_values[:,0], model_values[:,1]]).T  # Adjust as per your data structure
        observed_vector = np.array([observed[1], observed[3]])  # Adjust indices as needed
        covariance_matrix = np.array([[scale**2, 0], [0, scale**2]])  # Adjust as per your model

        likelihoods = []
        for mean_vector in mean_vectors:
            likelihood = multivariate_normal.pdf(observed_vector, mean=mean_vector, cov=covariance_matrix)
            likelihoods.append(likelihood)
        return likelihoods

    def find_max_params(self, posterior, points):
        max_index = np.argmax(posterior)
        point = points[max_index]
        return point

    def write_to_file(self, filename, data):
        with open(filename, 'w') as file:
            for row in data[:-1]:
                file.write('\t'.join(map(str, row)) + '\n')
            file.write('\t'.join(map(str, data[-1])))



    @staticmethod
    def bayesian_update(prior, likelihood):
        posterior = prior * likelihood
        return posterior / np.sum(posterior)

