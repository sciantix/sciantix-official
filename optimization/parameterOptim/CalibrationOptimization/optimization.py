import numpy as np
from scipy.optimize import Bounds, minimize
from bayes_opt import BayesianOptimization, UtilityFunction, SequentialDomainReductionTransformer
import os, shutil, copy
from user_model import UserModel

class Optimization:
    """
    Optimization class for adjusting scale factors (sf) in a numerical model.

    Attributes:
        kind (str): Type of optimization ('online' or 'offline').
        method (str): Optimization algorithm ('som' for scipy.optimize.minimize or 'dr' for domain reduction).
        params_info (dict): Stores parameter information, including bounds and initial values.
        bounds_global (Bounds or dict): Global bounds for the optimization parameters.
    """

    def __init__(self, method='som', keys=None, initial_values=None, stds=None, online:bool = False):
        """
        Initializes the Optimization object.

        Args:
            method (str): The optimization method, either 'som' or 'dr'.
            keys (np.ndarray): Names of the parameters to optimize.
            initial_values (np.ndarray): Initial guess values for the parameters.
            stds (np.ndarray): Standard deviations for the parameters, used to set bounds.
        """
        if keys is None or initial_values is None or stds is None:
            raise ValueError("keys, initial_values, and stds must not be None.")

        self.online = online
        self.method = method
        self.params_info = self._create_params_info(keys, initial_values, stds)
        self.bounds_global = self._create_bounds_global(self.method)
        self.bounds_dr = self._create_bounds_global('dr')

    def _create_params_info(self, keys, initial_values, stds):
        bounds = np.vstack((initial_values - 4 * stds, initial_values + 4 * stds)).T
        params_info = {key: {'value': val, 'bounds': bound} for key, val, bound in zip(keys, initial_values, bounds)}
        return {key:params_info[key] for key in sorted(params_info)}

    def _create_bounds_global(self, method):
        if method == 'som':
            return Bounds([info['bounds'][0] for info in self.params_info.values()],
                          [info['bounds'][1] for info in self.params_info.values()])
        elif method == 'dr':
            return {key: info['bounds'] for key, info in self.params_info.items()}
        else:
            raise ValueError("Invalid method specified")

    def optimize(self, model:UserModel,previous_x, current_x, initial_values, initial_bounds_dr):
        """
        Performs optimization using the specified method.

        Args:
            model (UserModel): The model to optimize.
            current_x (float): The current time point for optimization.
            initial_values (np.ndarray): Initial values for the optimization.
            initial_bounds_dr (dict): Initial domain reduction bounds.

        Returns:
            dict: Optimized parameter values.
        """
        destination_name = 'Optimization'
        if not os.path.exists(destination_name):
            os.makedirs(destination_name)
            os.chdir(destination_name)
        else:
            os.chdir(destination_name)

        self.optim_container_path = os.getcwd()

        if previous_x == 0:
            previous_optim_path = 0
        else:
            for folder_name in os.listdir(self.optim_container_path):
                folder_path = os.path.join(self.optim_container_path, folder_name)
                if os.path.isdir(folder_path) and f'to_{np.round(previous_x,3)}' in folder_name:
                    previous_optim_path = folder_path
                    break

        self.sciantix_optim_path = model._independent_sciantix_folder(self.optim_container_path,previous_optim_path, previous_x, current_x)
        cost_function = self._create_cost_function(model,current_x)
        if self.method == 'som':
            optimizer_result, error = self._optimize_som(cost_function, initial_values)
        elif self.method == 'dr':
            optimizer_result, error = self._optimize_dr(cost_function, initial_bounds_dr)
            observed = model._exp(current_x)
            relative_std = observed[2]/observed[1]
            iteration = 0
            while error > relative_std and iteration <3:
                for key, value in initial_bounds_dr.items():
                    initial_bounds_dr[key] = np.array([
                        max(value[0] - 0.3 * np.abs(value[0]), self.bounds_global[key][0]),
                        min(value[1] + 0.3 * np.abs(value[1]), self.bounds_global[key][1])
                        ])
                optimizer_result, error = self._optimize_dr(cost_function, initial_bounds_dr)
                iteration = iteration + 1

        else:
            raise ValueError("Invalid optimization method.")
        
        self.optimizer_result = optimizer_result
        model._sciantix(self.sciantix_optim_path, optimizer_result)
        
        return optimizer_result

    def _create_cost_function(self, model:UserModel, current_x):
        """
        Creates a cost function for optimization.

        Args:
            model (UserModel): The model to optimize.
            current_x (float): The current time point for optimization.

        Returns:
            function: A cost function for optimization.
        """
        if self.method == 'som':
            def cost_function(params):
                optimized_params = {key: param for key, param in zip(self.params_info.keys(), params)}
                # optimized_params = dict(zip(self.params_info.keys(), params))
                model_error = model.calculate_error(self.sciantix_optim_path,optimized_params,current_x)

                return model_error
        if self.method == 'dr':
            def cost_function(**params):
                model_error = model.calculate_error(self.sciantix_optim_path, params, current_x)
                model_error = -model_error
            
                return model_error


        return cost_function

    def _optimize_som(self, cost_function, initial_values):
        """
        Performs optimization using scipy.optimize.minimize.

        Args:
            cost_function (function): The cost function for optimization.
            initial_values (np.ndarray): Initial values for the optimization parameters.

        Returns:
            dict: Optimized parameter values.
            float: error
        """
        solution_nm = minimize(cost_function, initial_values, bounds=self.bounds_global,method = 'Nelder-Mead')
        solution_slsqp = minimize(cost_function, initial_values, bounds=self.bounds_global,method = 'SLSQP')
        solution_powell = minimize(cost_function, initial_values, bounds=self.bounds_global,method = 'Powell')
        error_info = {
            'Nelder-Mead': solution_nm.fun,
            'SLSQP': solution_slsqp.fun,
            'Powell': solution_powell.fun
        }

        # Find the method with the minimum error
        best_method = min(error_info, key=error_info.get)

        # Retrieve the best solution based on the method
        best_solution = {
            'Nelder-Mead': solution_nm,
            'SLSQP': solution_slsqp,
            'Powell': solution_powell
        }[best_method]

        print(f'current_error:{best_solution.fun}')
        return {key: value for key, value in zip(self.params_info.keys(), best_solution.x)}, best_solution.fun

    def _optimize_dr(self, cost_function, initial_bounds_dr):
        """
        Performs optimization using Bayesian optimization with domain reduction.

        Args:
            cost_function (function): The cost function for optimization.
            initial_bounds_dr (dict): Initial domain reduction bounds.

        Returns:
            dict: Optimized parameter values.
            float: error
        """
        bounds_transformer = SequentialDomainReductionTransformer()
        optimizer = BayesianOptimization(f=cost_function, pbounds=initial_bounds_dr, 
                                         verbose=0, bounds_transformer=bounds_transformer,
                                         allow_duplicate_points=True)
        
        acq_function = UtilityFunction(kind='ucb')
        optimizer.maximize(init_points=10, n_iter=50, acquisition_function=acq_function)
        print(f'current_error:{optimizer.max["target"]}')
        return {key: optimizer.max['params'][key] for key in self.params_info.keys()}, np.abs(optimizer.max['target'])

    @property
    def value_optimized(self):
        """
        Returns the optimized values of the parameters.

        Returns:
            np.ndarray: An array of optimized parameter values.
        """
        return np.array([value for value in self.optimizer_result.values()])

    @property
    def optim_folder(self):
        return self.sciantix_optim_path
