"""Execute explicit, manually chosen batches using the verbatim notebook solver."""
import argparse
import json
import time
import traceback
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import asdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

def worker(spec):
    import model as m
    from evaluate import BASE, save_evaluation, contact_sheet
    import numpy as np
    import pandas as pd
    out = ROOT/'runs'/spec['id']
    out.mkdir(parents=True, exist_ok=True)
    params = {**BASE, **spec['changes']}
    m.NGF_AREAL_0 = params['NGF_AREAL_0']
    cand = m.Candidate(label=spec['id'], **{k:v for k,v in params.items() if k!='NGF_AREAL_0'})
    config = json.loads((ROOT/'runs/baseline/configuration.json').read_text())
    config['MANUAL_PARAMS'] = asdict(cand)
    config['NGF_AREAL_0'] = m.NGF_AREAL_0
    (out/'configuration.json').write_text(json.dumps(config,indent=2))
    (out/'parameters.json').write_text(json.dumps(params,indent=2))
    (out/'specification.json').write_text(json.dumps(spec,indent=2,ensure_ascii=False))
    audits=[]
    exchange = m.sciantix_3x3_exchange_step
    counters = dict(negative_exchange_steps=0, nonfinite_exchange_steps=0)
    def monitored_exchange(*args, **kwargs):
        values = exchange(*args, **kwargs)
        counters['negative_exchange_steps'] += int(any(v<0 for v in values))
        counters['nonfinite_exchange_steps'] += int(any(not np.isfinite(v) for v in values))
        return values
    m.sciantix_3x3_exchange_step = monitored_exchange
    solver = m.solve_UN
    def audited_solver(p, keep_history=False):
        for k in counters: counters[k] = 0
        hist, rates = solver(p, keep_history=True)
        h=pd.DataFrame(hist)
        mask=h.generated > 0
        balance=(h.c+h.mb+h.md+h.q_gf+h.q_rel-h.generated)[mask]/h.generated[mask]
        audit=dict(case=next(k for k,v in m.EXPERIMENT_CASES.items() if v['fission_rate']==p.fission_rate),
                   T=p.temperature, steps=len(h)-1,
                   max_history_relative_mass_residual=float(abs(balance).max()),
                   min_history_gas=float(h[['c','mb','md','q_gf','q_rel']].min().min()),
                   max_history_capture_fraction=float(h.f_cap_step.max()),
                   max_history_raw_capture=float(h.cap_raw_step.max()),
                   max_history_Fc=float(h.Fc_gf.max()),
                   min_history_Nd=float(h.Nd.min()),
                   max_history_psi_d=float((h.Rd*(4*np.pi*h.Nd/3)**(1/3)).max()),
                   max_history_zeta_guard=int(((h.Rd*(4*np.pi*h.Nd/3)**(1/3))>1e40).sum()),
                   negative_release_increments=int((h.q_rel.diff().dropna() < -1e-10).sum()),
                   **counters)
        audits.append(audit)
        # Archive the complete time histories at the three diagnostic temperatures.
        if p.temperature in (1200,1600,1800):
            np.savez_compressed(out/f"history_{audit['case'].replace('.','p')}_{p.temperature:g}K.npz",
                                **{k:np.asarray(v) for k,v in hist.items()})
        return ({k:[v[-1]] for k,v in hist.items()} if not keep_history else hist), rates
    m.solve_UN=audited_solver
    start=time.perf_counter()
    try:
        rows=m.simulate_grid(cand)
        df=pd.DataFrame([{k:v for k,v in row.items() if k not in ('hist','rates')} for row in rows])
        df.to_csv(out/'results.csv',index=False)
        metrics,diagnostics=save_evaluation(df,params,out)
        audit=pd.DataFrame(audits)
        audit.to_csv(out/'history_audit.csv',index=False)
        diagnostics.update(max_history_relative_mass_residual=float(audit.max_history_relative_mass_residual.max()),
                           negative_exchange_steps=int(audit.negative_exchange_steps.sum()),
                           nonfinite_exchange_steps=int(audit.nonfinite_exchange_steps.sum()),
                           max_history_raw_capture=float(audit.max_history_raw_capture.max()),
                           negative_release_increments=int(audit.negative_release_increments.sum()),
                           seconds=time.perf_counter()-start)
        (out/'diagnostics.json').write_text(json.dumps(diagnostics,indent=2))
        ref=pd.read_csv(ROOT/'runs/baseline/RizkUN_L3_unificato/L3_unificato_grid.csv')
        contact_sheet(df,ref,spec['id'],ROOT/'plots'/f"{spec['id']}_contact_sheet.png")
        return dict(id=spec['id'], seconds=time.perf_counter()-start,diagnostics=diagnostics)
    except Exception:
        (out/'FAILED.txt').write_text(traceback.format_exc())
        raise

def main():
    parser=argparse.ArgumentParser()
    parser.add_argument('batch',type=Path)
    parser.add_argument('--workers',type=int,default=4)
    args=parser.parse_args()
    specs=json.loads(args.batch.read_text())
    assert 4<=len(specs)<=8
    assert json.loads((ROOT/'runs/baseline/reproduction.json').read_text())['all_columns_match']
    with ProcessPoolExecutor(max_workers=args.workers) as pool:
        futures={pool.submit(worker,s):s['id'] for s in specs}
        for f in as_completed(futures):
            result=f.result()
            print(json.dumps(result),flush=True)

if __name__=='__main__': main()
