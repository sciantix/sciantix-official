# solver_class.py
# -------------------------
# Francesco A.B. Silva
# 24/09/2024

# Import libraries:
import numpy as np
import scipy.sparse as sparse
import scipy.sparse.linalg as linalg


# ========================================= #
#      Forward Solver Full Order Class      #
# ========================================= #

class FiniteElementCylinderSolver(object):

    # Load the Y and X mass matricess, the list of partial stiffness matricess, the forcing term and the averaging operator:
    YY_FO = np.load('model_files/YY_FO.npy', allow_pickle=True)[0]
    XX_FO = np.load('model_files/XX_FO.npy', allow_pickle=True)[0]
    KK_FO = np.load('model_files/KK_FO.npy', allow_pickle=True)
    FF_FO = np.load('model_files/FF_FO.npy')
    AA_FO = np.load('model_files/AA_FO.npy')

    # Load the interpolation matrix, the interpolation coordinates, and the vecotr of coercivity constants:
    II_FO = np.load('model_files/II_FO.npy')
    ZZ_FO = np.load('model_files/ZZ_FO.npy')
    CC_FO = np.load('model_files/CC_FO.npy')
    
    # Define full order problem size and affine decomposition size:
    sFO = YY_FO.shape[0]
    sHR = len(KK_FO)

    # Define the solver class clonstructor:
    def __init__(self):

        # Define problem size:        
        self.basis = None
        self.sRB = None

    # Define method assembling left-hand-side and right-hand-side:
    def assemble(self, RADIUS=1E-5, LENGTH=1E-3, F_RATE=3E+19, F_YIELD=0.24, F_ENERGY = 3.215e-11, RHO=11040, CP = 400, T_BC=2E+03, C_BC=0.0, KTH = 2.208, dt=1E+4):
        
        Q3 = F_ENERGY * F_RATE
        # Define reference sources intensity:
        self.sgroup_T = ((Q3/(RHO*CP)) * LENGTH**2) / (KTH/(RHO*CP)) 
        self.source_C = F_RATE * F_YIELD 

        # Define heat and mass diffusion coefficiets:
        self.alpha_C = 2.949513e-13 * np.exp(-20487.36244 / (T_BC + self.sgroup_T * (1 - self.ZZ_FO**2) / 2))    #fit White
        
        # Compute interpolation vector and bound to the problem coercivity:
        self.inter = np.hstack((self.II_FO @ self.alpha_C / RADIUS**2, self.II_FO @ self.alpha_C / LENGTH**2))
        self.coerc = self.inter @ self.CC_FO
        self.force = self.source_C * self.FF_FO

        # Assemble the stiffnes term for this parameter configuration:
        self.SS_FO = self.inter @ self.KK_FO
        
        # Assemble left-hand-side and right-hand side (static part):
        self.LHS_static = self.XX_FO + dt * self.SS_FO
        self.RHS_static = dt * self.force

        # Save time delta:
        self.dt = dt

    #  Define solver to solve the problem over a given number of iterations:
    def solve(self, n_steps=1000, verbose=False):

        # Iteratively solve the problem:
        solution = np.zeros((self.sFO, n_steps+1))
        for ii in range(n_steps):
            solution[:, ii+1], _ = linalg.bicgstab(self.LHS_static, self.RHS_static + self.XX_FO.dot(solution[: , ii:ii+1]))

            # Print current iteration and average temperature and concentration:
            if verbose:
                print('Current time:', (ii+1) * self.dt, 's')
                print('Average concentration:', self.AA_FO @ solution[:, ii+1], 'atm/m^3\n')

        # Return solution:
        return solution

    # Define a method to compute the squared norm of a solution:
    def solution_energy(self, solution):
        return self.dt * np.einsum('ik,ik', self.SS_FO @ solution, solution) + self.XX_FO.dot(solution[:, -1]).dot(solution[:, -1])

    # Get the full order representation of the state:
    def get_FO(self, solution):
        return solution

    # Get the full order representation of the state:
    def get_RB(self, solution):
        return solution


# ============================================ #
#      Forward Solver Reduced Basis Class      #
# ============================================ #

class ReducedBasisCylinderSolver(object):
    
    # Load the mass matrix, the list of partial stiffness matricess, the forcing term and the averaging operator:
    YY_FO = np.load('model_files/YY_FO.npy', allow_pickle=True)[0]
    XX_FO = np.load('model_files/XX_FO.npy', allow_pickle=True)[0]
    KK_FO = np.load('model_files/KK_FO.npy', allow_pickle=True)
    FF_FO = np.load('model_files/FF_FO.npy')
    AA_FO = np.load('model_files/AA_FO.npy')

    # Load the interpolation matrix, the interpolation coordinates, and the vecotr of coercivity constants:
    II_FO = np.load('model_files/II_FO.npy')
    ZZ_FO = np.load('model_files/ZZ_FO.npy')
    CC_FO = np.load('model_files/CC_FO.npy')
    
    # Define full order problem size and affine decomposition size:
    sFO = YY_FO.shape[0]
    sHR = len(KK_FO)
    
    # Define the solver class constructor:
    def __init__(self, basis):

        # Save basis functions and reduced basis space size:
        self.basis = basis
        self.sRB = basis.shape[1]

        # Reduce Y mass matrix:
        self.YY_RB = basis.T @ self.YY_FO @ basis
        self.AA_RB = basis.T @ self.AA_FO
        
        # Assemble Rietz representer and reduced basis projection of the forcing term:
        self.FF_rietz = linalg.spsolve(self.YY_FO, self.FF_FO)
        self.FF_RB    = basis.T @ self.FF_FO

        # Assemble Rietz representer and reduced basis projection of the mass matrix:
        self.XX_right = self.XX_FO @ basis
        self.XX_rietz = linalg.spsolve(self.YY_FO, self.XX_right)
        self.XX_RB    = basis.T @ self.XX_right

        # Assemble Rietz representer and reduced basis projection of the partial stiffness matrix:
        self.KK_right = np.array([self.KK_FO[ii] @ basis for ii in range(self.sHR)])
        self.KK_rietz = np.array([linalg.spsolve(self.YY_FO, self.KK_right[ii]) for ii in range(self.sHR)])
        self.KK_RB    = np.array([basis.T @ self.KK_right[ii] for ii in range(self.sHR)])

        # Pre-assemble the scalar and vector terms for the residual energy computation:
        self.TT_pr_bb = self.FF_rietz.T @ self.FF_FO
        self.TT_pr_bm = self.FF_rietz.T @ self.XX_right 
        self.TT_pr_mm = self.XX_rietz.T @ self.XX_right

        # Pre-assemble all the tensor terms for the residual energy computation:
        self.TT_pr_ab = np.array( [self.KK_right[ii, :, :].T @ self.FF_rietz for ii in range(self.sHR)])
        self.TT_pr_am = np.array( [self.KK_right[ii, :, :].T @ self.XX_rietz for ii in range(self.sHR)])
        self.TT_pr_aa = np.array([[self.KK_right[ii, :, :].T @ self.KK_rietz[jj, :, :] for ii in range(self.sHR)] for jj in range(self.sHR)])
    
    # Define method assembling left-hand-side and right-hand-side:
    def assemble(self, RADIUS=1E-5, LENGTH=1E-3, F_RATE=3E+19, F_YIELD=0.24, F_ENERGY = 3.215e-11, RHO=11040, CP = 400, T_BC=2E+03, C_BC=0.0, KTH = 2.208, dt=1E+4):
        
        Q3 = F_ENERGY * F_RATE
        # Define reference sources intensity:
        self.sgroup_T = ((Q3/(RHO*CP)) * LENGTH**2) / (KTH/(RHO*CP))  
        self.source_C = F_RATE * F_YIELD 

        # Define heat and mass diffusion coefficiets:
        self.alpha_C = 2.949513e-13 * np.exp(-20487.36244 / (T_BC + self.sgroup_T * (1 - self.ZZ_FO**2) / 2)) #fit White
        
        # Compute interpolation vector and bound to the problem coercivity:
        self.inter = np.hstack((self.II_FO @ self.alpha_C / RADIUS**2, self.II_FO @ self.alpha_C / LENGTH**2))
        self.coerc = self.inter @ self.CC_FO
        self.force = self.source_C * self.FF_RB

        # Assemble the scalar and vector terms for the residual energy computation:
        self.DD_pr_bb = self.TT_pr_bb * (self.source_C * self.source_C)
        self.DD_pr_bm = self.TT_pr_bm * (- 2 * self.source_C / dt)
        self.DD_pr_mm = self.TT_pr_mm * (  1 / (dt * dt))

        # Assemble matrices for the computation of the residual energy for the current parameter configuration:
        self.DD_pr_ab = np.tensordot(self.inter, self.TT_pr_ab, axes=[0, 0]) * (-2 * self.source_C)
        self.DD_pr_am = np.tensordot(self.inter, self.TT_pr_am, axes=[0, 0]) * ( 2 / dt)
        self.DD_pr_aa = np.tensordot(self.inter, np.tensordot(self.inter, self.TT_pr_aa, axes=[0, 0]), axes=[0, 0])

        # Assemble reduced basis stiffnes matrix for this parameter configuration:
        self.SS_RB = np.tensordot(self.inter, self.KK_RB, axes=[0, 0])
        
        # Assemble left-hand-side and right-hand side (static part):
        self.LHS_static = self.XX_RB + dt * self.SS_RB
        self.RHS_static = dt * self.force

        # Save time delta:
        self.dt = dt

    #  Define reduced basis solver to solve the problem over a given number of iterations:
    def solve(self, n_steps=1000, verbose=False, error_bound=True):

        # Iteratively solve the problem:
        solution = np.zeros((self.sRB, n_steps+1))
        for ii in range(n_steps):
            solution[:, ii+1] = np.linalg.solve(self.LHS_static, self.RHS_static + self.XX_RB.dot(solution[: , ii:ii+1]))[:, 0]

            # Print current iteration and average temperature and concentration:
            if verbose:
                print('Current time:', (ii+1) * self.dt, 's')
                print('Average concentration:', self.AA_RB @ solution[:, ii+1], 'atm/m^3\n')

        # Return solution and in case the error bound:
        if error_bound: return solution, np.sqrt(self.residual_energy(solution) / self.solution_energy(solution) / self.coerc)
        return solution

    # Define a method to compute the squared norm of a solution:
    def solution_energy(self, solution):
        return self.dt * np.einsum('ik,ik', self.SS_RB @ solution, solution) + self.XX_RB.dot(solution[:, -1]).dot(solution[:, -1])

    # Define a method to compute the squared norm of the residual of a solution:
    def residual_energy(self, solution):

        # Compute solution increments:
        increment = solution[:, 1:] - solution[:, :-1]

        # Rreturn squared residual norm:
        return self.dt * (np.einsum('ik,ik', self.DD_pr_am @ increment, solution[:, 1:])       + np.sum(self.DD_pr_bm @ increment) +\
                          np.einsum('ik,ik', self.DD_pr_aa @ solution[:, 1:], solution[:, 1:]) + np.sum(self.DD_pr_ab @ solution[:, 1:]) +\
                          np.einsum('ik,ik', self.DD_pr_mm @ increment, increment)             + self.DD_pr_bb[0] * increment.shape[1] )

    # Get the full order representation of the reduced basis state:
    def get_FO(self, solution):
        return self.basis @ solution

    # Get the reduced basis representation of the full order state:
    def get_RB(self, solution):
        return np.linalg.solve(self.XX_RB, self.XX_right.T @ solution)