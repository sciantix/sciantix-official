"""Fixed objectives and diagnostic-only guardrails; no solver calls. Usable offline."""
import numpy as np
import pandas as pd
import Optuna_rho_constant_Rizk_model as model
OBJECTIVES = ['J_swelling', 'J_micro', 'J_FGR']
GAS = {'matrix': ('matrix_gas', 'Matrix'), 'bulk': ('bulk_gas', 'Bulk bubbles'),
       'dislocation': ('dislocation_gas', 'Dislocation bubbles'),
       'grainface': ('grainface_gas', 'Grain boundary bubbles'), 'FGR': ('FGR', 'FGR')}
CORE_OUTPUTS = ['swelling_d','swelling_bulk','swelling_gf','Rd','Nd','Rb','Nb','Rgf','Ngf',
                'matrix_gas','bulk_gas','dislocation_gas','grainface_gas','FGR',
                'p_d','p_b','p_gf','p_d_over_eq','p_b_over_eq','p_gf_over_eq',
                'c','mb','md','q_gf','q_rel','nvb','nvd','ng_gf','nv_gf','generated']

def rms(values):
    return float(np.sqrt(np.mean(np.square(values))))

def predict(frame, case, column, temperatures):
    sub = frame.loc[frame['case'].eq(case)].sort_values('T')
    temperatures = np.asarray(temperatures)
    assert len(sub)
    assert temperatures.min() >= sub['T'].min() and temperatures.max() <= sub['T'].max(), 'No extrapolation'
    return np.interp(temperatures, sub['T'], sub[column])

def evaluate(frame):
    frame = frame.copy()
    for column in CORE_OUTPUTS+["rho_d_eff","rho_d","psi_b","psi_d"]:
        if column not in frame: frame[column] = np.nan
    metrics = {'points': len(frame), 'solver_failures': int(frame.status.ne('ok').sum())}
    ok = frame[frame.status.eq('ok')]
    arr = ok.reindex(columns=CORE_OUTPUTS).to_numpy(dtype=float)
    metrics['nonfinite_values'] = int((~np.isfinite(arr)).sum())
    inventories = ['c','mb','md','q_gf','q_rel','nvb','nvd','ng_gf','nv_gf','Nb','Nd','Ngf']
    metrics['negative_inventory_values'] = int((ok.reindex(columns=inventories).to_numpy() < 0).sum())
    balances = ok.reindex(columns=['matrix_gas','bulk_gas','dislocation_gas','grainface_gas','FGR']).sum(axis=1, min_count=5)
    metrics['gas_balance_max_abs_pp'] = float((balances-100).abs().max())
    metrics['gas_balance_failures'] = int(((balances-100).abs()>0.1).sum())
    metrics['rho_constant_max_relative_error'] = float(((ok.rho_d_eff-ok.rho_d)/ok.rho_d).abs().max()) if len(ok) else np.nan
    # A volume fraction >=1 cannot describe non-overlapping bubble populations.
    metrics['gross_nonphysical_points'] = int(((ok.swelling_d+ok.swelling_bulk+ok.swelling_gf)>=100).sum()) if len(ok) else 0
    metrics['geometry_warning_points'] = int(((ok.psi_b>=0.8)|(ok.psi_d>=0.8)).sum()) if len(ok) else 0
    for c in ['b','d','gf']:
        metrics[f'p_{c}_over_eq_max'] = float(ok[f'p_{c}_over_eq'].max())
        metrics[f'p_{c}_max_Pa'] = float(ok[f'p_{c}'].max())
    warnings = []
    for case, sub in ok.groupby('case'):
        rv = ~((sub.Rgf > sub.Rd) & (sub.Rd > sub.Rb))
        # Compare all number densities in volume units, not areal grain-face density.
        nv = ~((sub.Ngf < sub.Nd) & (sub.Nd < sub.Nb))
        key = case.replace('.', 'p')
        metrics[f'radius_order_violation_fraction_{key}'] = float(rv.mean())
        metrics[f'density_order_violation_fraction_{key}'] = float(nv.mean())
        def longest(mask):
            return max((sum(1 for _ in group) for flag, group in __import__('itertools').groupby(mask) if flag), default=0)
        sr = sub.sort_values('T'); rv = ~((sr.Rgf>sr.Rd)&(sr.Rd>sr.Rb)); nv = ~((sr.Ngf<sr.Nd)&(sr.Nd<sr.Nb))
        metrics[f'radius_order_longest_run_{key}'] = longest(rv)
        metrics[f'density_order_longest_run_{key}'] = longest(nv)
        if longest(rv)>=3: warnings.append(f'persistent_radius_order:{case}')
        if longest(nv)>=3: warnings.append(f'persistent_density_order:{case}')
    metrics['numeric_valid'] = bool(not any(metrics[k] for k in ['solver_failures','nonfinite_values','negative_inventory_values','gas_balance_failures','gross_nonphysical_points']) and metrics['rho_constant_max_relative_error']==0)
    if any(metrics[k] for k in ['solver_failures','nonfinite_values','negative_inventory_values']):
        metrics.update({o: np.nan for o in OBJECTIVES}); metrics['warnings'] = 'invalid_numeric_state;'+';'.join(warnings)
        return metrics
    for case in model.EXPERIMENT_CASE_ORDER:
        points = [p for p in model.experimental_swelling_points_for_case(case) if 900<=p['T']<=1800]
        temperatures = [p['T'] for p in points]; exp = np.array([p['swelling'] for p in points])
        error = predict(frame,case,'swelling_d',temperatures)-exp
        key = case.replace('.', 'p')
        metrics[f'swelling_RMSE_{key}_pp'] = rms(error)
        metrics[f'swelling_error_{key}'] = rms(error)/float(exp.max())
        metrics[f'swelling_n_{key}'] = len(exp)
    metrics['J_swelling'] = float(np.mean([metrics[f'swelling_error_{c.replace(".","p")}'] for c in model.EXPERIMENT_CASE_ORDER]))
    for suffix, predicate in [('lowT',lambda t:900<=t<=1600),('highT',lambda t:1600<t<=1800)]:
        rp = [p for p in model.EXP_RD_T_13 if predicate(p['T'])]
        np_ = [p for p in model.EXP_ND_T_13 if predicate(p['T'])]
        r_exp = np.array([p['R_nm'] for p in rp]); n_exp = np.array([p['N'] for p in np_])
        r_mod = predict(frame,'AP3.4','Rd',[p['T'] for p in rp])*1e9
        n_mod = predict(frame,'AP3.4','Nd',[p['T'] for p in np_])
        metrics[f'Rd_{suffix}_RMSE_nm'] = rms(r_mod-r_exp)
        metrics[f'Rd_{suffix}_relative_RMSE'] = rms((r_mod-r_exp)/r_exp)
        metrics[f'Rd_{suffix}_mean_signed_nm'] = float(np.mean(r_mod-r_exp))
        metrics[f'Nd_{suffix}_log10_RMSE'] = rms(np.log10(n_mod)-np.log10(n_exp))
        metrics[f'Nd_{suffix}_mean_log10_bias'] = float(np.mean(np.log10(n_mod)-np.log10(n_exp)))
        metrics[f'Rd_{suffix}_n'] = len(rp); metrics[f'Nd_{suffix}_n'] = len(np_)
    metrics['E_R'] = metrics['Rd_lowT_relative_RMSE']; metrics['E_N'] = metrics['Nd_lowT_log10_RMSE']
    metrics['J_micro'] = .6*metrics['E_R']+.4*metrics['E_N']
    # Both 1.1% pins share the published reference. Average their MSEs first;
    # then give the 1.1% and 3.2% burnups equal weight. Never average model curves before error calculation.
    for name,(column,refcolumn) in GAS.items():
        by_burnup = {1.1:[],3.2:[]}
        for case in ('AP3.2','AP3.8','ANP6'):
            bu = model.EXPERIMENT_CASES[case]['burnup']; ref = model.RIZK2025_GAS_PARTITION[bu]
            mask = (ref['T_K']>=900)&(ref['T_K']<=1800)
            error = predict(frame,case,column,ref['T_K'][mask])-ref[refcolumn][mask]
            mse = float(np.mean(error**2)); by_burnup[bu].append(mse)
            metrics[f'gas_{name}_RMSE_{case.replace(".","p")}_pp'] = np.sqrt(mse)
        for bu, values in by_burnup.items(): metrics[f'gas_{name}_RMSE_{str(bu).replace(".","p")}FIMA_pp'] = np.sqrt(np.mean(values))
        metrics[f'gas_{name}_RMSE_pp'] = np.sqrt(np.mean([np.mean(v) for v in by_burnup.values()]))
    metrics['J_FGR'] = metrics['gas_FGR_RMSE_pp']
    metrics['gas_partition_core_RMS'] = rms([metrics[f'gas_{name}_RMSE_pp'] for name in ('matrix','bulk','dislocation')])
    metrics['partition_warning'] = metrics['gas_partition_core_RMS']>10
    metrics['strong_partition_warning'] = metrics['gas_partition_core_RMS']>20
    if not metrics['numeric_valid']: warnings.append('excluded_numeric_or_gross_nonphysical_state')
    if metrics['partition_warning']: warnings.append('partition_warning')
    if metrics['strong_partition_warning']: warnings.append('strong_partition_warning')
    if metrics['geometry_warning_points']: warnings.append('single_size_geometry_warning')
    if metrics['p_gf_over_eq_max']>100: warnings.append('grainface_pressure_over_eq_gt100_diagnostic_only')
    metrics['warnings'] = ';'.join(warnings)
    return metrics
