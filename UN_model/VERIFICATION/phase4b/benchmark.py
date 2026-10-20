#!/usr/bin/env python3
"""Required five 4000 h benchmarks, before any 110-run campaign."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import argparse
import time
import subprocess
from common import HERE, ROOT, END, rv, np, make_cases, save_cases, write_csv, solution


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--output',type=Path,required=True)
    args=p.parse_args();out=args.output.resolve()
    assert out.is_relative_to(HERE)
    out.mkdir(parents=True,exist_ok=False)
    cases=make_cases();save_cases(cases)
    start_all=time.perf_counter();compilation=rv.build(out)
    # Three real configurations and two opposite diffusion/trapping extremes.
    selected=[cases[i] for i in [0,1,2,5,8]]
    rows=[]
    for case in selected:
        start=time.perf_counter()
        t,means,modes,timing=rv.run_cpp(out/case['name'],case,40,225.,END)
        assert timing['steps']==64000 and t[-1]==END
        begin=time.perf_counter();exact,em=solution(case,40,t)
        scale=max(abs(exact).max(),1.)
        mean_error=float(abs(means-exact).max()/scale)
        modal_error=float(np.linalg.norm(modes-em,axis=(1,2)).max()/max(np.linalg.norm(em,axis=(1,2)).max(),1.))
        analytic_seconds=time.perf_counter()-begin
        np.savez_compressed(out/case['name']/'comparison.npz',time=t,cpp=means,analytic=exact,
                            absolute_mean_error=abs(means-exact))
        complete=time.perf_counter()-start
        row={'case':case['name'],'N':40,'dt_s':225.,'end_s':END,'steps':timing['steps'],
             'stored_rows':len(t),'solver_seconds':timing['solver_seconds'],
             'cpp_process_including_IO_seconds':timing['process_seconds'],
             'python_analysis_seconds':analytic_seconds,'complete_case_including_IO_seconds':complete,
             'mean_normalized_error':mean_error,'modal_L2_normalized_error':modal_error,
             'output_bytes':(out/case['name']/'output.bin').stat().st_size}
        assert np.isfinite(means).all() and np.isfinite(modes).all()
        rows.append(row);write_csv(out/'timings.csv',rows)
        print(case['name'],row,flush=True)
    result={'status':'completed','main_campaign_started':False,'benchmarks':5,
        'compile':compilation,'rows':rows,'total_wall_seconds':time.perf_counter()-start_all,
        'head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        'note':'Five cost trials only. The 110 main tests require a separate cost/accuracy decision.'}
    rv.save(out/'results.json',result)


if __name__=='__main__':main()
