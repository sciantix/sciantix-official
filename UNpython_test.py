#!/usr/bin/env python3

# %%
# Celle eseguibili (stile Colab):
# - In VS Code: installa le estensioni "Python" e "Jupyter"
# - Poi puoi cliccare ▶ "Run Cell" su ogni blocco separato da `# %%`

# %% Doc
"""
SCIANTIX UN (Uranium Nitride) - Fission Gas Behavior Solver
============================================================

Risolve il sistema di equazioni differenziali per il comportamento del gas di fissione
in combustibile nucleare UN (Uranium Mononitride), implementando:
- Diffusione spettrale del gas in soluzione
- 3 popolazioni: soluzione (c), bulk bubbles (m_b), dislocation bubbles (m_d)
- Trapping verso entrambe le popolazioni di bolle
- Re-solution da entrambe le popolazioni

Basato sul modello descritto in CONTEXT.md e UNcode.md

python3 UNpython_test.py

"""

# %% Imports
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
from scipy.optimize import fsolve
from dataclasses import dataclass
from typing import Tuple, Optional
import warnings


# %% Config (modifica qui i parametri)
@dataclass(frozen=True)
class UNConfig:
    # --- Materiale (Rizk 2025) ---
    lattice_parameter: float = 4.889e-10  # m
    matrix_density: float = 14300.0  # kg/m^3 (TODO: verificare fonte)
    dislocation_density: float = 3.0e13  # 1/m^2
    dislocation_core_radius: float = 3.46e-10  # m (= a/sqrt(2))

    # --- Condizioni operative ---
    temperature: float = 1500.0  # K
    fission_rate_density: float = 1.0e20  # 1/(m^3 s)
    grain_radius: float = 6.0e-6  # m

    # --- Diffusività Xe in UN (Rizk 2025) ---
    kB_ev_per_K: float = 8.617333262e-5  # eV/K
    D10: float = 1.56e-3  # m^2/s
    Q1: float = 4.94  # eV
    A30: float = 1.85e-39  # m^5
    precursor_factor: float = 1.0  # Xe=1.0, Xe133=1.25, ...

    # --- Produzione gas ---
    xe_yield_per_fission: float = 0.25  # (-) ipotesi
    fragments_per_fission: float = 2.0  # (-)

    # --- Stato bolle (placeholder: impostalo tu) ---
    N_b: float = 1.0e22  # bub/m^3
    N_d: float = 5.0e21  # bub/m^3
    R_b: float = 1.0e-9  # m
    R_d: float = 2.0e-9  # m

    # --- Numerica ---
    n_modes: int = 40
    t_end: float = 100 * 3600  # s
    dt_out: float = 3600  # s
    dt_internal: float = 3600  # s (time step Sciantix-like)
    ode_method: str = "BDF"
    rtol: float = 1e-6
    atol: float = 1e-12
    initialize_modes_from_mean: bool = True


CFG = UNConfig()


@dataclass
class MaterialProperties:
    """Proprietà materiali per UN"""
    # Parametri reticolari
    lattice_parameter: float = CFG.lattice_parameter  # m (Rizk 2025)
    matrix_density: float = CFG.matrix_density  # kg/m^3
    
    # Parametri dislocazioni
    dislocation_density: float = CFG.dislocation_density  # 1/m^2 (Rizk 2025)
    dislocation_core_radius: float = CFG.dislocation_core_radius  # m (Rizk 2025)
    
    # Volumi atomici
    omega_fg: float = None  # m³/atom (calcolato da lattice_parameter)
    omega_v: float = None   # m³/vacancy (≈ omega_fg)
    
    def __post_init__(self):
        """Calcola volumi atomici dal parametro reticolare"""
        if self.omega_fg is None:
            # Volume di un atomo nella cella cubica
            self.omega_fg = self.lattice_parameter**3 / 4.0  # FCC-like
        if self.omega_v is None:
            self.omega_v = self.omega_fg


@dataclass
class OperatingConditions:
    """Condizioni operative del combustibile"""
    temperature: float = CFG.temperature  # K
    fission_rate_density: float = CFG.fission_rate_density  # 1/(m^3 s)
    grain_radius: float = CFG.grain_radius  # m (Rizk 2025)
    burnup: float = 0.0  # at% (per future estensioni)


class DiffusivityModel:
    """Modello di diffusività Xe in UN (Rizk 2025)"""
    
    @classmethod
    def compute(cls, T: float, F_dot: float, precursor_factor: float = 1.0) -> float:
        """
        Calcola diffusività del gas di fissione in UN
        
        Args:
            T: temperatura (K)
            F_dot: fission rate density (1/(m³·s))
            precursor_factor: fattore precursore (1.0 per Xe, 1.25 per Xe133, etc.)
            
        Returns:
            D_g: diffusività (m²/s)
        """
        # Componenti della diffusività
        d1 = CFG.D10 * np.exp(-CFG.Q1 / (CFG.kB_ev_per_K * T))  # Thermal diffusion
        d2 = 0.0  # Irradiation-enhanced (trascurato per Xe)
        d3 = CFG.A30 * F_dot  # Radiation-induced mixing (athermal)
        
        D_g = (d1 + d2 + d3) * precursor_factor
        return D_g


class ResolutionModel:
    """Modello di re-solution per UN (Rizk 2025)"""
    
    @staticmethod
    def b0(R: float) -> float:
        """
        Coefficiente di re-solution per singolo atomo per fissione
        
        Args:
            R: raggio bolla effettivo (m) = R_bubble + r_lattice
            
        Returns:
            b0: probabilità re-solution per atomo per fissione
        """
        return 1.0e-25 * (2.64 - 2.02 * np.exp(-2.61e-9 / R))
    
    @classmethod
    def compute_rates(cls, F_dot: float, R_intra: float, R_disl: float, 
                      r_lattice: float) -> Tuple[float, float]:
        """
        Calcola tassi di re-solution per bulk e dislocation bubbles
        
        Args:
            F_dot: fission rate density (1/(m³·s))
            R_intra: raggio intragranular bubbles (m)
            R_disl: raggio dislocation bubbles (m)
            r_lattice: raggio in lattice units (m)
            
        Returns:
            (b_b, b_d): tassi re-solution bulk e dislocation (1/s)
        """
        R_b_eff = R_intra + r_lattice
        R_d_eff = R_disl + r_lattice
        
        b_b = F_dot * cls.b0(R_b_eff)
        b_d = F_dot * cls.b0(R_d_eff)
        
        return b_b, b_d


class TrappingModel:
    """Modello di trapping per UN"""
    
    @staticmethod
    def compute_bulk_trapping(D_g: float, N_b: float, R_b: float, 
                              r_lattice: float) -> float:
        """
        Trapping verso bulk bubbles (Ham sink)
        
        Args:
            D_g: diffusività gas (m²/s)
            N_b: concentrazione bulk bubbles (bub/m³)
            R_b: raggio bulk bubbles (m)
            r_lattice: raggio in lattice units (m)
            
        Returns:
            g_b: trapping rate verso bulk (1/s)
        """
        if N_b == 0:
            return 0.0
        
        R_eff = R_b + r_lattice
        g_b = 4.0 * np.pi * D_g * R_eff * N_b
        return g_b
    
    @staticmethod
    def compute_dislocation_trapping(D_g: float, N_d: float, R_d: float, 
                                     rho_d: float, r_d: float, r_lattice: float,
                                     Z_d: float = 5.0) -> float:
        """
        Trapping verso dislocazioni (bolle + line sink)
        
        Args:
            D_g: diffusività gas (m²/s)
            N_d: concentrazione dislocation bubbles (bub/m³)
            R_d: raggio dislocation bubbles (m)
            rho_d: dislocation density (1/m²)
            r_d: dislocation core radius (m)
            r_lattice: raggio in lattice units (m)
            Z_d: costante geometrica (default 5.0)
            
        Returns:
            g_d: trapping rate verso dislocazioni (1/s)
        """
        R_d_eff = R_d + r_lattice
        
        # Termine bolle su dislocazioni
        term_bubbles = 4.0 * np.pi * D_g * R_d_eff * N_d
        
        # Termine line-sink per dislocazione nuda
        Gamma_d = 1.0 / np.sqrt(np.pi * rho_d)  # Wigner-Seitz radius
        
        # Protezione numerica per logaritmo
        arg = Gamma_d / (Z_d * r_d)
        if arg <= 1.0:
            warnings.warn("Gamma_d/(Z_d*r_d) ≤ 1, setting ln term to 0")
            den = 1.0  # fallback
        else:
            den = np.log(arg) - 0.6
        
        # Densità dislocazioni libere (non occupate da bolle)
        free_dislocation = max(0.0, rho_d - 2.0 * R_d * N_d)
        
        term_dislocation = (2.0 * np.pi * D_g / den) * free_dislocation
        
        g_d = term_bubbles + term_dislocation
        return g_d


class SpectralSolver:
    """
    Solver spettrale per diffusione in grano sferico
    
    Risolve il sistema:
        dc/dt   = D_g ∇²c - (g_b + g_d) c + b_b m_b + b_d m_d + β
        dm_b/dt = g_b c - b_b m_b
        dm_d/dt = g_d c - b_d m_d
    
    con decomposizione spettrale in autofunzioni sferiche.
    """
    
    def __init__(self, n_modes: int = 40):
        """
        Args:
            n_modes: numero di modi spettrali (default 40)
        """
        self.n_modes = n_modes
        self.eigenvalues = None
        self.grain_radius = None
        
    def setup_modes(self, grain_radius: float):
        """
        Calcola autovalori per grano sferico con BC tipo Dirichlet
        
        Args:
            grain_radius: raggio grano (m)
        """
        self.grain_radius = grain_radius
        
        # Autovalori: λ_n = (n*π/R)² per n = 1, 2, 3, ...
        n = np.arange(1, self.n_modes + 1)
        self.eigenvalues = (n * np.pi / grain_radius)**2

    @staticmethod
    def mode_initialization(n_modes: int, mode_initial_condition: float, diffusion_modes: np.ndarray) -> None:
        """
        Inizializzazione modi come in Sciantix `Solver::modeInitialization`.

        Proietta una condizione iniziale "media sul grano" su una serie di modi, con un ciclo di ricostruzione
        iterativa.
        """
        projection_coeff = -np.sqrt(8.0 / np.pi)
        initial_condition = mode_initial_condition
        projection_remainder = initial_condition

        iteration_max = 20
        for _ in range(iteration_max):
            reconstructed_solution = 0.0
            for n in range(n_modes):
                np1 = n + 1
                n_coeff = ((-1.0) ** np1) / np1
                diffusion_modes[n] += projection_coeff * n_coeff * projection_remainder
                reconstructed_solution += projection_coeff * n_coeff * diffusion_modes[n] * 3.0 / (4.0 * np.pi)
            projection_remainder = initial_condition - reconstructed_solution

    @staticmethod
    def laplace3x3(A: np.ndarray, b: np.ndarray) -> np.ndarray:
        """
        Solve 3x3 lineare come in Sciantix `Solver::Laplace3x3` (formula con determinanti).
        """
        detA = (
            A[0, 0] * (A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1])
            - A[0, 1] * (A[1, 0] * A[2, 2] - A[1, 2] * A[2, 0])
            + A[0, 2] * (A[1, 0] * A[2, 1] - A[1, 1] * A[2, 0])
        )
        if detA == 0.0:
            return b

        detX = (
            b[0] * (A[1, 1] * A[2, 2] - A[1, 2] * A[2, 1])
            - A[0, 1] * (b[1] * A[2, 2] - A[1, 2] * b[2])
            + A[0, 2] * (b[1] * A[2, 1] - A[1, 1] * b[2])
        )
        detY = (
            A[0, 0] * (b[1] * A[2, 2] - A[1, 2] * b[2])
            - b[0] * (A[1, 0] * A[2, 2] - A[1, 2] * A[2, 0])
            + A[0, 2] * (A[1, 0] * b[2] - b[1] * A[2, 0])
        )
        detZ = (
            A[0, 0] * (A[1, 1] * b[2] - b[1] * A[2, 1])
            - A[0, 1] * (A[1, 0] * b[2] - b[1] * A[2, 0])
            + b[0] * (A[1, 0] * A[2, 1] - A[1, 1] * A[2, 0])
        )
        return np.array([detX / detA, detY / detA, detZ / detA], dtype=float)

    def step_3equations_exchange_sciantix(
        self,
        modes_c: np.ndarray,
        modes_m_b: np.ndarray,
        modes_m_d: np.ndarray,
        parameter: np.ndarray,
        dt: float,
    ) -> tuple[float, float, float]:
        """
        Un singolo time-step *identico* a Sciantix: `Solver::SpectralDiffusion3equationsExchange`.

        Aggiorna i modi in-place e ritorna (c, m_b, m_d) ricostruiti come medie sul grano.
        """
        n_modes = int(parameter[0])
        D_g = float(parameter[1])
        radius = float(parameter[2])
        beta = float(parameter[3])
        g_b = float(parameter[4])
        g_d = float(parameter[5])
        b_b = float(parameter[6])
        b_d = float(parameter[7])

        diffusion_rate_coeff = (np.pi**2) * D_g / (radius**2)
        projection_coeff = -2.0 * np.sqrt(2.0 / np.pi)
        source_rate_coeff = projection_coeff * beta

        c_solution = 0.0
        m_b_solution = 0.0
        m_d_solution = 0.0

        vol_coeff = (4.0 / 3.0) * np.pi

        for n in range(n_modes):
            np1 = n + 1
            n_coeff = ((-1.0) ** np1) / np1

            diffusion_rate = diffusion_rate_coeff * (np1**2)
            source_rate = source_rate_coeff * n_coeff

            A = np.array(
                [
                    [1.0 + (diffusion_rate + g_b + g_d) * dt, -b_b * dt, -b_d * dt],
                    [-g_b * dt, 1.0 + b_b * dt, 0.0],
                    [-g_d * dt, 0.0, 1.0 + b_d * dt],
                ],
                dtype=float,
            )

            b = np.array([modes_c[n] + source_rate * dt, modes_m_b[n], modes_m_d[n]], dtype=float)
            x = self.laplace3x3(A, b)

            modes_c[n] = x[0]
            modes_m_b[n] = x[1]
            modes_m_d[n] = x[2]

            c_solution += projection_coeff * n_coeff * x[0] / vol_coeff
            m_b_solution += projection_coeff * n_coeff * x[1] / vol_coeff
            m_d_solution += projection_coeff * n_coeff * x[2] / vol_coeff

        return c_solution, m_b_solution, m_d_solution
        
    # Nota: per replicare Sciantix 1:1 non usiamo `solve_ivp` per il caso UN 3-eq. Il time stepping
    # è gestito nel wrapper `SciantixUNSolver.solve(...)` con `step_3equations_exchange_sciantix(...)`.


class SciantixUNSolver:
    """
    Solver completo per fission gas behavior in UN
    """
    
    def __init__(self, 
                 material: MaterialProperties = None,
                 conditions: OperatingConditions = None,
                 n_modes: int = None):
        """
        Args:
            material: proprietà materiali UN
            conditions: condizioni operative
            n_modes: numero modi spettrali
        """
        self.material = material or MaterialProperties()
        self.conditions = conditions or OperatingConditions()
        self.solver = SpectralSolver(n_modes=(n_modes or CFG.n_modes))
        
        # Setup solver
        self.solver.setup_modes(self.conditions.grain_radius)
        
        # Stato bolle (da aggiornare dinamicamente o fissare)
        self.N_b = CFG.N_b  # bub/m^3
        self.N_d = CFG.N_d  # bub/m^3
        self.R_b = CFG.R_b  # m
        self.R_d = CFG.R_d  # m
        
    def compute_parameters(self, precursor_factor: float = 1.0) -> dict:
        """
        Calcola tutti i parametri del modello
        
        Args:
            precursor_factor: fattore precursore per gas radioattivi
            
        Returns:
            dizionario con tutti i parametri
        """
        T = self.conditions.temperature
        F_dot = self.conditions.fission_rate_density
        r_grain = self.conditions.grain_radius
        
        # Diffusività
        D_g = DiffusivityModel.compute(T, F_dot, precursor_factor)
        
        # Produzione gas
        beta = CFG.fragments_per_fission * F_dot * CFG.xe_yield_per_fission
        
        # Raggio in lattice units
        r_lattice = self.material.lattice_parameter / 2.0
        
        # Re-solution
        b_b, b_d = ResolutionModel.compute_rates(F_dot, self.R_b, self.R_d, r_lattice)
        
        # Trapping
        g_b = TrappingModel.compute_bulk_trapping(D_g, self.N_b, self.R_b, r_lattice)
        g_d = TrappingModel.compute_dislocation_trapping(
            D_g, self.N_d, self.R_d,
            self.material.dislocation_density,
            self.material.dislocation_core_radius,
            r_lattice
        )
        
        return {
            'D_g': D_g,
            'beta': beta,
            'g_b': g_b,
            'g_d': g_d,
            'b_b': b_b,
            'b_d': b_d,
            'r_grain': r_grain,
            'T': T,
            'F_dot': F_dot
        }

    def build_sciantix_parameter_vector(self, params: dict) -> np.ndarray:
        """
        Costruisce il vettore `parameter` nello stesso ordine usato da Sciantix
        (`Solver::SpectralDiffusion3equationsExchange`).
        """
        return np.array(
            [
                float(self.solver.n_modes),
                float(params["D_g"]),
                float(self.conditions.grain_radius),
                float(params["beta"]),
                float(params["g_b"]),
                float(params["g_d"]),
                float(params["b_b"]),
                float(params["b_d"]),
            ],
            dtype=float,
        )
    
    def solve(self, t_end: float, dt_out: float = None, 
              initial_state: dict = None, **kwargs) -> dict:
        """
        Risolve il sistema completo
        
        Args:
            t_end: tempo finale simulazione (s)
            dt_out: passo output (s), se None usa valori automatici
            initial_state: stato iniziale {'c': ..., 'm_b': ..., 'm_d': ...}
            **kwargs: argomenti per solve_ivp
            
        Returns:
            dizionario con risultati
        """
        # Stato iniziale (medie + modi)
        if initial_state is None:
            initial_state = {"c": 0.0, "m_b": 0.0, "m_d": 0.0}

        modes_c = np.array(initial_state.get("modes_c", np.zeros(self.solver.n_modes)), dtype=float)
        modes_m_b = np.array(initial_state.get("modes_m_b", np.zeros(self.solver.n_modes)), dtype=float)
        modes_m_d = np.array(initial_state.get("modes_m_d", np.zeros(self.solver.n_modes)), dtype=float)

        if CFG.initialize_modes_from_mean and (
            np.all(modes_c == 0.0) and np.all(modes_m_b == 0.0) and np.all(modes_m_d == 0.0)
        ):
            self.solver.mode_initialization(self.solver.n_modes, float(initial_state.get("c", 0.0)), modes_c)
            self.solver.mode_initialization(self.solver.n_modes, float(initial_state.get("m_b", 0.0)), modes_m_b)
            self.solver.mode_initialization(self.solver.n_modes, float(initial_state.get("m_d", 0.0)), modes_m_d)

        # Parametri
        params = self.compute_parameters(precursor_factor=CFG.precursor_factor)
        parameter_vec = self.build_sciantix_parameter_vector(params)

        # Time stepping Sciantix-like
        if dt_out is None:
            dt_out = CFG.dt_out
        dt = float(CFG.dt_internal)
        if dt <= 0.0:
            raise ValueError("CFG.dt_internal must be > 0")
        if dt_out <= 0.0:
            raise ValueError("dt_out must be > 0")

        n_out = int(np.floor(t_end / dt_out)) + 1
        t = np.linspace(0.0, dt_out * (n_out - 1), n_out)

        c = np.zeros_like(t)
        m_b = np.zeros_like(t)
        m_d = np.zeros_like(t)

        # Ricostruisci stato iniziale coerente con i modi (come Sciantix farebbe dopo una chiamata)
        c[0], m_b[0], m_d[0] = self.solver.step_3equations_exchange_sciantix(
            modes_c, modes_m_b, modes_m_d, parameter_vec, 0.0
        )

        current_time = 0.0
        success = True
        message = "OK"

        for i_out in range(1, n_out):
            target_time = t[i_out]
            while current_time + 1e-18 < target_time:
                step = min(dt, target_time - current_time)
                try:
                    c_i, m_b_i, m_d_i = self.solver.step_3equations_exchange_sciantix(
                        modes_c, modes_m_b, modes_m_d, parameter_vec, step
                    )
                except Exception as exc:  # pragma: no cover
                    success = False
                    message = str(exc)
                    break
                current_time += step
            if not success:
                break
            c[i_out] = c_i
            m_b[i_out] = m_b_i
            m_d[i_out] = m_d_i
        
        # Gas totale nel grano
        m_total = c + m_b + m_d
        
        # Frazione in bolle
        frac_bubbles = (m_b + m_d) / (m_total + 1e-30)
        
        return {
            't': t,
            'c': c,
            'm_b': m_b,
            'm_d': m_d,
            'm_total': m_total,
            'frac_bubbles': frac_bubbles,
            'params': params,
            'result': None,
            'success': success,
            'message': message,
            'modes_c': modes_c,
            'modes_m_b': modes_m_b,
            'modes_m_d': modes_m_d,
        }
    
    def plot_results(self, solution: dict, figsize=(12, 8)):
        """
        Plot risultati simulazione
        
        Args:
            solution: output di solve()
            figsize: dimensioni figura
        """
        fig, axes = plt.subplots(2, 2, figsize=figsize)
        
        t = solution['t']
        t_hours = t / 3600.0
        
        # 1) Concentrazioni assolute
        ax = axes[0, 0]
        ax.plot(t_hours, solution['c'], label='In soluzione', lw=2)
        ax.plot(t_hours, solution['m_b'], label='Bulk bubbles', lw=2)
        ax.plot(t_hours, solution['m_d'], label='Dislocation bubbles', lw=2)
        ax.plot(t_hours, solution['m_total'], 'k--', label='Totale', lw=2)
        ax.set_xlabel('Tempo (h)')
        ax.set_ylabel('Concentrazione (at/m³)')
        ax.set_title('Evoluzione gas di fissione')
        ax.legend()
        ax.grid(True, alpha=0.3)
        ax.set_yscale('log')
        
        # 2) Frazione in bolle
        ax = axes[0, 1]
        ax.plot(t_hours, solution['frac_bubbles'] * 100, lw=2, color='crimson')
        ax.set_xlabel('Tempo (h)')
        ax.set_ylabel('Frazione in bolle (%)')
        ax.set_title('Frazione gas intrappolato in bolle')
        ax.grid(True, alpha=0.3)
        ax.set_ylim([0, 105])
        
        # 3) Distribuzione popolazioni
        ax = axes[1, 0]
        final_c = solution['c'][-1]
        final_m_b = solution['m_b'][-1]
        final_m_d = solution['m_d'][-1]
        total = final_c + final_m_b + final_m_d
        
        labels = ['Soluzione', 'Bulk\nbubbles', 'Dislocation\nbubbles']
        values = [final_c/total*100, final_m_b/total*100, final_m_d/total*100]
        colors = ['skyblue', 'orange', 'green']
        
        ax.bar(labels, values, color=colors, alpha=0.7, edgecolor='black')
        ax.set_ylabel('Frazione (%)')
        ax.set_title(f'Distribuzione finale (t={t_hours[-1]:.1f} h)')
        ax.grid(True, alpha=0.3, axis='y')
        
        # 4) Parametri
        ax = axes[1, 1]
        ax.axis('off')
        
        params = solution['params']
        info_text = f"""
        PARAMETRI SIMULAZIONE
        ──────────────────────
        Temperatura:     {params['T']:.0f} K
        Fission rate:    {params['F_dot']:.2e} 1/(m³·s)
        Raggio grano:    {params['r_grain']*1e6:.1f} μm
        
        Diffusività D_g: {params['D_g']:.2e} m²/s
        Produzione β:    {params['beta']:.2e} at/(m³·s)
        
        Trapping g_b:    {params['g_b']:.2e} 1/s
        Trapping g_d:    {params['g_d']:.2e} 1/s
        
        Resolution b_b:  {params['b_b']:.2e} 1/s
        Resolution b_d:  {params['b_d']:.2e} 1/s
        
        ──────────────────────
        Gas totale finale:
        {solution['m_total'][-1]:.2e} at/m³
        
        Frazione in bolle:
        {solution['frac_bubbles'][-1]*100:.1f} %
        """
        
        ax.text(0.1, 0.95, info_text, transform=ax.transAxes,
                fontsize=9, verticalalignment='top', family='monospace',
                bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.3))
        
        plt.tight_layout()
        return fig


# ============================================================================
# ESEMPIO DI UTILIZZO
# ============================================================================

def run_example():
    """Esegue una simulazione di esempio"""
    
    print("="*70)
    print("SCIANTIX UN SOLVER - Simulazione Fission Gas Behavior")
    print("="*70)
    print()
    
    # Setup materiale e condizioni (da CFG in testa al file)
    material = MaterialProperties()
    conditions = OperatingConditions()
    
    print(f"Condizioni operative:")
    print(f"  - Temperatura: {conditions.temperature} K")
    print(f"  - Fission rate: {conditions.fission_rate_density:.2e} 1/(m³·s)")
    print(f"  - Raggio grano: {conditions.grain_radius*1e6:.1f} μm")
    print()
    
    # Crea solver
    solver = SciantixUNSolver(
        material=material,
        conditions=conditions,
        n_modes=CFG.n_modes
    )
    
    print("Parametri bolle:")
    print(f"  - N_b (bulk): {solver.N_b:.2e} bub/m³")
    print(f"  - N_d (disl): {solver.N_d:.2e} bub/m³")
    print(f"  - R_b: {solver.R_b*1e9:.1f} nm")
    print(f"  - R_d: {solver.R_d*1e9:.1f} nm")
    print()
    
    # Simula 100 ore
    print("Inizio simulazione (100 ore, ~ 6 giorni)...")
    t_end = CFG.t_end
    
    solution = solver.solve(
        t_end=t_end,
        dt_out=CFG.dt_out,
        method=CFG.ode_method,
        rtol=CFG.rtol,
        atol=CFG.atol,
    )
    
    if solution['success']:
        print(f"✓ Simulazione completata con successo")
        print(f"  Messaggio: {solution['message']}")
    else:
        print(f"✗ Errore nella simulazione")
        print(f"  Messaggio: {solution['message']}")
        return
    
    print()
    print("Risultati finali:")
    print(f"  - Gas in soluzione:       {solution['c'][-1]:.2e} at/m³")
    print(f"  - Gas in bulk bubbles:    {solution['m_b'][-1]:.2e} at/m³")
    print(f"  - Gas in disl bubbles:    {solution['m_d'][-1]:.2e} at/m³")
    print(f"  - Gas totale:             {solution['m_total'][-1]:.2e} at/m³")
    print(f"  - Frazione in bolle:      {solution['frac_bubbles'][-1]*100:.1f} %")
    print()
    
    # Plot risultati
    print("Generazione grafici...")
    fig = solver.plot_results(solution)
    out_png = "sciantix_un_results.png"
    plt.savefig(out_png, dpi=150, bbox_inches="tight")
    print(f"✓ Grafici salvati in: {out_png}")
    print()
    
    return solver, solution


# %% Esecuzione
if __name__ == "__main__":
    solver, solution = run_example()
    print("="*70)
    print("Simulazione completata. Usa solver e solution per analisi ulteriori.")
    print("="*70)
