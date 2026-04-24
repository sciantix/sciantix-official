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


@dataclass
class MaterialProperties:
    """Proprietà materiali per UN"""
    # Parametri reticolari
    lattice_parameter: float = 4.88e-10  # m
    matrix_density: float = 14300.0  # kg/m³
    
    # Parametri dislocazioni
    dislocation_density: float = 1.0e14  # 1/m²
    dislocation_core_radius: float = 3.8e-10  # m (~ Burgers vector)
    
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
    temperature: float = 1500.0  # K
    fission_rate_density: float = 1.0e20  # 1/(m³·s)
    grain_radius: float = 5.0e-6  # m
    burnup: float = 0.0  # at% (per future estensioni)


class DiffusivityModel:
    """Modello di diffusività Xe in UN (Rizk 2025)"""
    
    # Costanti
    kB = 8.617333262e-5  # eV/K
    D10 = 1.56e-3  # m²/s
    Q1 = 4.94  # eV
    A30 = 1.85e-39  # m⁵
    
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
        d1 = cls.D10 * np.exp(-cls.Q1 / (cls.kB * T))  # Thermal diffusion
        d2 = 0.0  # Irradiation-enhanced (trascurato per Xe)
        d3 = cls.A30 * F_dot  # Radiation-induced mixing (athermal)
        
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
        
    def solve_3equations_exchange(self, y0: np.ndarray, t_span: Tuple[float, float],
                                  params: dict, method: str = 'BDF',
                                  **kwargs) -> dict:
        """
        Risolve il sistema 3-equazioni con metodo backward Euler spettrale
        
        Args:
            y0: stato iniziale [c, m_b, m_d, modes_c, modes_m_b, modes_m_d]
            t_span: (t_start, t_end)
            params: dizionario parametri (D_g, beta, g_b, g_d, b_b, b_d)
            method: metodo ODE (default 'BDF' per stiff problems)
            **kwargs: argomenti aggiuntivi per solve_ivp
            
        Returns:
            risultato solve_ivp
        """
        if self.eigenvalues is None:
            raise ValueError("Call setup_modes() first")
        
        def rhs(t, y):
            """Right-hand side del sistema ODE"""
            # Estrai variabili mediate
            c = y[0]
            m_b = y[1]
            m_d = y[2]
            
            # Estrai modi
            modes_c = y[3:3+self.n_modes]
            modes_m_b = y[3+self.n_modes:3+2*self.n_modes]
            modes_m_d = y[3+2*self.n_modes:3+3*self.n_modes]
            
            # Parametri
            D_g = params['D_g']
            beta = params['beta']
            g_b = params['g_b']
            g_d = params['g_d']
            b_b = params['b_b']
            b_d = params['b_d']
            
            # RHS per modi (accoppiati)
            # Per ogni modo: sistema lineare 3x3
            d_modes_c = np.zeros(self.n_modes)
            d_modes_m_b = np.zeros(self.n_modes)
            d_modes_m_d = np.zeros(self.n_modes)
            
            for i in range(self.n_modes):
                lam = self.eigenvalues[i]
                
                # Sistema per modo i:
                # dc_i/dt   = -D_g*λ_i*c_i - (g_b+g_d)*c_i + b_b*m_b_i + b_d*m_d_i
                # dm_b_i/dt = g_b*c_i - b_b*m_b_i
                # dm_d_i/dt = g_d*c_i - b_d*m_d_i
                
                d_modes_c[i] = -D_g * lam * modes_c[i] - (g_b + g_d) * modes_c[i] \
                               + b_b * modes_m_b[i] + b_d * modes_m_d[i]
                d_modes_m_b[i] = g_b * modes_c[i] - b_b * modes_m_b[i]
                d_modes_m_d[i] = g_d * modes_c[i] - b_d * modes_m_d[i]
            
            # Medie spaziali (somma modi + termine sorgente)
            dc = np.sum(d_modes_c) + beta
            dm_b = np.sum(d_modes_m_b)
            dm_d = np.sum(d_modes_m_d)
            
            # Assembla derivata
            dy = np.zeros_like(y)
            dy[0] = dc
            dy[1] = dm_b
            dy[2] = dm_d
            dy[3:3+self.n_modes] = d_modes_c
            dy[3+self.n_modes:3+2*self.n_modes] = d_modes_m_b
            dy[3+2*self.n_modes:3+3*self.n_modes] = d_modes_m_d
            
            return dy
        
        # Risolvi ODE
        result = solve_ivp(rhs, t_span, y0, method=method, **kwargs)
        return result


class SciantixUNSolver:
    """
    Solver completo per fission gas behavior in UN
    """
    
    def __init__(self, 
                 material: MaterialProperties = None,
                 conditions: OperatingConditions = None,
                 n_modes: int = 40):
        """
        Args:
            material: proprietà materiali UN
            conditions: condizioni operative
            n_modes: numero modi spettrali
        """
        self.material = material or MaterialProperties()
        self.conditions = conditions or OperatingConditions()
        self.solver = SpectralSolver(n_modes=n_modes)
        
        # Setup solver
        self.solver.setup_modes(self.conditions.grain_radius)
        
        # Stato bolle (da aggiornare dinamicamente o fissare)
        self.N_b = 1.0e22  # bub/m³ (placeholder)
        self.N_d = 1.0e21  # bub/m³ (placeholder)
        self.R_b = 1.0e-9  # m (placeholder)
        self.R_d = 2.0e-9  # m (placeholder)
        
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
        
        # Produzione gas (assumo yield Xe = 0.25 per fissione)
        yield_xe = 0.25
        beta = 2.0 * F_dot * yield_xe  # 2 frammenti per fissione
        
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
        # Stato iniziale
        if initial_state is None:
            initial_state = {
                'c': 0.0,
                'm_b': 0.0,
                'm_d': 0.0,
                'modes_c': np.zeros(self.solver.n_modes),
                'modes_m_b': np.zeros(self.solver.n_modes),
                'modes_m_d': np.zeros(self.solver.n_modes)
            }
        
        # Assembla vettore iniziale
        y0 = np.concatenate([
            [initial_state['c'], initial_state['m_b'], initial_state['m_d']],
            initial_state['modes_c'],
            initial_state['modes_m_b'],
            initial_state['modes_m_d']
        ])
        
        # Parametri
        params = self.compute_parameters()
        
        # Setup output times
        if dt_out is not None:
            t_eval = np.arange(0, t_end, dt_out)
            kwargs['t_eval'] = t_eval
        
        # Risolvi
        result = self.solver.solve_3equations_exchange(
            y0, (0, t_end), params, **kwargs
        )
        
        # Estrai risultati
        c = result.y[0, :]
        m_b = result.y[1, :]
        m_d = result.y[2, :]
        
        # Gas totale nel grano
        m_total = c + m_b + m_d
        
        # Frazione in bolle
        frac_bubbles = (m_b + m_d) / (m_total + 1e-30)
        
        return {
            't': result.t,
            'c': c,
            'm_b': m_b,
            'm_d': m_d,
            'm_total': m_total,
            'frac_bubbles': frac_bubbles,
            'params': params,
            'result': result,
            'success': result.success,
            'message': result.message
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
    
    # Setup materiale e condizioni
    material = MaterialProperties(
        dislocation_density=1.0e14,  # 1/m²
        dislocation_core_radius=3.8e-10  # m
    )
    
    conditions = OperatingConditions(
        temperature=1500.0,  # K
        fission_rate_density=1.0e20,  # 1/(m³·s)
        grain_radius=5.0e-6  # m (5 μm)
    )
    
    print(f"Condizioni operative:")
    print(f"  - Temperatura: {conditions.temperature} K")
    print(f"  - Fission rate: {conditions.fission_rate_density:.2e} 1/(m³·s)")
    print(f"  - Raggio grano: {conditions.grain_radius*1e6:.1f} μm")
    print()
    
    # Crea solver
    solver = SciantixUNSolver(
        material=material,
        conditions=conditions,
        n_modes=40
    )
    
    # Setup bolle (valori realistici)
    solver.N_b = 1.0e22  # bub/m³
    solver.N_d = 5.0e21  # bub/m³
    solver.R_b = 1.0e-9  # m (1 nm)
    solver.R_d = 2.0e-9  # m (2 nm)
    
    print("Parametri bolle:")
    print(f"  - N_b (bulk): {solver.N_b:.2e} bub/m³")
    print(f"  - N_d (disl): {solver.N_d:.2e} bub/m³")
    print(f"  - R_b: {solver.R_b*1e9:.1f} nm")
    print(f"  - R_d: {solver.R_d*1e9:.1f} nm")
    print()
    
    # Simula 100 ore
    print("Inizio simulazione (100 ore, ~ 6 giorni)...")
    t_end = 100 * 3600  # s
    
    solution = solver.solve(
        t_end=t_end,
        dt_out=3600,  # output ogni ora
        method='BDF',  # Per problemi stiff
        rtol=1e-6,
        atol=1e-12
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
