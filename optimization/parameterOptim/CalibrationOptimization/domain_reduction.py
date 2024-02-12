import numpy as np
from typing import Optional, Union, List
from optimization import Optimization  

class DomainReduction:
    """
    Domain Reduction for optimization parameters.

    Dynamically adjusts the parameter space for optimization based on previous results,
    aiming to improve efficiency and accuracy of the optimization process.

    Attributes:
        gamma_osc (float): Oscillation factor for domain reduction dynamics.
        gamma_pan (float): Panning factor for domain reduction dynamics.
        eta (float): Contraction rate for domain bounds.
        minimum_window (list or float): Minimum window size for each parameter's bounds.
    """

    def __init__(self, gamma_osc: float = 0.7, gamma_pan: float = 1.0, eta: float = 0.5, 
                 minimum_window: Optional[Union[List[float], float]] = 0.0):
        if not 0 <= gamma_osc <= 1:
            raise ValueError("gamma_osc must be between 0 and 1")
        if not 0 <= gamma_pan <= 1:
            raise ValueError("gamma_pan must be between 0 and 1")
        if not 0 <= eta <= 1:
            raise ValueError("eta must be between 0 and 1")

        self.gamma_osc = gamma_osc
        self.gamma_pan = gamma_pan
        self.eta = eta
        self.minimum_window = minimum_window

    def initialize(self, optim: Optimization) -> None:
        """
        Initializes the domain reduction process with the initial bounds from the optimizer.

        Args:
            optim (Optimization): The optimization object containing initial parameter bounds.
        """
        bounds_global = optim.bounds_dr
        self.keys = np.array([item[0] for item in sorted(bounds_global.items(), key=lambda x: x[0])])
        self.bounds_global = np.array([item[1] for item in sorted(bounds_global.items(), key=lambda x: x[0])])

        self.bounds_original = self.bounds_global.copy()
        self.bounds = [self.bounds_original]
        self.minimum_window = self._prepare_minimum_window(self.minimum_window, len(self.keys))

        self.previous_optimal = np.mean(self.bounds_global, axis=1)
        self.current_optimal = self.previous_optimal.copy()
        self.r = self.bounds_global[:, 1] - self.bounds_global[:, 0]

        self.update_dynamics()

    def _prepare_minimum_window(self, minimum_window, num_params):
        """
        Prepares the minimum window array based on the input value.

        Args:
            minimum_window (list, float): Minimum window size or list of sizes for each parameter.
            num_params (int): Number of parameters.

        Returns:
            list: A list containing the minimum window size for each parameter.
        """
        if isinstance(minimum_window, (list, np.ndarray)):
            if len(minimum_window) != num_params:
                raise ValueError("Length of minimum_window must match the number of parameters.")
            return minimum_window
        else:
            return [minimum_window] * num_params

    def update_dynamics(self):
        """
        Updates the domain reduction dynamics based on the current and previous optimal values.
        """
        self.previous_d = 2.0 * (self.current_optimal - self.previous_optimal) / self.r
        self.current_d = self.previous_d.copy()

        self.c = self.current_d * self.previous_d
        self.c_hat = np.sqrt(np.abs(self.c)) * np.sign(self.c)

        self.gamma = 0.5 * (self.gamma_pan * (1.0 + self.c_hat) + self.gamma_osc * (1.0 - self.c_hat))
        self.contraction_rate = self.eta + np.abs(self.current_d) * (self.gamma - self.eta)
        self.r *= self.contraction_rate

    def _update(self, optimal: Optimization) -> None:
        """
        Updates the bounds based on the latest optimization results.

        Args:
            optimal (Optimization): The latest optimization object with updated parameter values.
        """
        self.previous_optimal = self.current_optimal
        self.current_optimal = optimal.value_optimized

        self.update_dynamics()

    def _trim(self, new_bounds: np.ndarray, global_bounds: np.ndarray) -> np.ndarray:
        """
        Trims the new bounds to ensure they fall within the global bounds and respect the minimum window size.

        Args:
            new_bounds (np.ndarray): Proposed new bounds.
            global_bounds (np.ndarray): Global bounds to adhere to.

        Returns:
            np.ndarray: Trimmed and adjusted bounds.
        """
        trimmed_bounds = np.copy(new_bounds)

        for i, (lower_bound, upper_bound) in enumerate(trimmed_bounds):
            lower_bound = max(lower_bound, global_bounds[i, 0])
            upper_bound = min(upper_bound, global_bounds[i, 1])

            window_width = upper_bound - lower_bound
            if window_width < self.minimum_window[i]:
                mid_point = (upper_bound + lower_bound) / 2
                adjusted_width = self.minimum_window[i] / 2
                lower_bound = max(mid_point - adjusted_width, global_bounds[i, 0])
                upper_bound = min(mid_point + adjusted_width, global_bounds[i, 1])

            trimmed_bounds[i] = [lower_bound, upper_bound]

        return trimmed_bounds

    def transform(self, optimal: Optimization) -> dict:
        """
        Transforms the bounds based on the latest optimization results and dynamics.

        Args:
            optimal (Optimization): The latest optimization object with updated parameter values.

        Returns:
            dict: A dictionary with the updated bounds for each parameter.
        """
        self._update(optimal)
        new_bounds = np.array([self.current_optimal - 0.5 * self.r, self.current_optimal + 0.5 * self.r]).T
        new_bounds = self._trim(new_bounds, self.bounds_global)
        self.bounds.append(new_bounds)

        return {key: new_bounds[i, :] for i, key in enumerate(self.keys)}

    @property
    def _bounds(self):
        return np.array(self.bounds)
