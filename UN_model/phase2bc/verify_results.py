#!/usr/bin/env python3
"""Recheck saved component results/invariants and the untouched frozen workspace."""
import hashlib,json,subprocess
from pathlib import Path
import numpy as np
from run_components import ROOT,HERE,save,sha

def main():
    results=HERE/'results'
    selected=['B1_transport','B2_seed','B2_vacancy','B3_intragranular_cached','B3_intragranular_chain',
              'B4_solver_state_recheck','C1_limited','C1_3x3','C2_initial','C2_grainface','C2_grainface_chain',
              'C3_lookup_nodes','C3_ig_sanitized','C3_gf_sanitized','C3_final_solver_state']
    summary=[]
    for name in selected:
        path=results/name/'results.json';m=json.loads(path.read_text());assert m['status']=='pass',name
        metrics=m.get('metrics',[])
        summary.append({'test':name,'result_sha256':sha(path),'values':m.get('values',m.get('values_compared')),
                        'max_normalized_error':max((r['max_normalized_error'] for r in metrics),default=0.0),
                        'failures':sum(r['failures'] for r in metrics)})
    invariants={}
    for name in ['C2_grainface','C2_grainface_chain']:
        m=json.loads((results/name/'results.json').read_text())
        for p in (results/name).glob('*/comparison.npz'):
            fields=[r['field'] for r in m['metrics'] if r['case']==p.parent.name];idx={k:i for i,k in enumerate(fields)}
            with np.load(p) as data:a,b=data['actual'],data['expected']
            inputs=np.loadtxt(p.parent/'input.txt',skiprows=1)
            old_q,old_rel=inputs[:,1],inputs[:,2]
            if name.endswith('chain'):
                old_q=np.r_[old_q[0],a[:-1,idx['q_gf']]];old_rel=np.r_[old_rel[0],a[:-1,idx['q_rel']]]
            target=old_q+old_rel+inputs[:,5]
            normalized=np.abs(a[:,idx['q_gf']]+a[:,idx['q_rel']]-target)/np.maximum(abs(target),1.0)
            sat=a[:,idx['gf_saturated']]>0
            assert np.array_equal(sat,b[:,idx['gf_saturated']]>0)
            coverage=float(np.max(np.abs(a[sat,idx['Fc']]-0.5))) if sat.any() else 0.0
            assert normalized.max()<1e-12 and coverage<5e-10
            assert np.isfinite(a).all() and np.all(a[:,[idx[k] for k in ['q_gf','q_rel','qv_gf','Ngf_areal']]]>=0)
            invariants[name+'/'+p.parent.name]={'gas_balance_max_normalized':float(normalized.max()),
                'saturated_coverage_max_absolute_error':coverage,
                'first_saturated_step_zero_based':int(np.flatnonzero(sat)[0]) if sat.any() else None,
                'saturation_flags_identical_to_reference':True}
    save(results/'grainface_invariants.json',{'status':'pass','checks':invariants})
    baseline=results/'C3_nonregression';original=ROOT/'UN_model/baseline_sciantix/runs/pre_port_2026-10-18'
    audit=json.loads((baseline/'audit.json').read_text());identical=[]
    for row in audit['named_column_audit']:
        name=row['case'];a=baseline/'snapshot/regression'/name/'output.txt';b=original/'snapshot/regression'/name/'output.txt'
        assert a.read_bytes()==b.read_bytes(),name;identical.append(name)
    assert (baseline/'legacy_un_notebook8_summary.csv').read_bytes()==(original/'legacy_un_notebook8_summary.csv').read_bytes()
    save(baseline/'comparison_to_phase1.json',{'status':'pass','case_count':len(identical),'all_outputs_identical_bytes':True,'cases':identical})
    before=json.loads((HERE/'protection_before.json').read_text());changed=[]
    allowed={'CMakeLists.txt','include/models/UNParameters.h','include/models/UNState.h',
             'include/models/UNIntraGranular.h','src/models/UNIntraGranular.C'}
    for name,h in before['files'].items():
        p=ROOT/name;assert p.is_file(),name
        if sha(p)!=h:changed.append(name)
    assert set(changed)<=allowed,changed
    assert hashlib.sha256(subprocess.check_output(['git','diff','--cached','--binary'],cwd=ROOT)).hexdigest()==before['staged_diff_sha256']
    assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()==before['head']
    preservation={'status':'pass','files_checked':len(before['files']),'changed_existing_files':changed,
                  'all_reference_gold_legacy_and_other_fuel_files_unchanged':True,'staged_diff_unchanged':True,'head_unchanged':True}
    save(HERE/'preservation_check.json',preservation)
    save(results/'summary.json',{'status':'pass','scope':'2B/2C component tests only; no 2D/3/4',
        'tests':summary,'grainface_invariants':invariants,'nonregression':audit['standard_all_counts'],
        'preexisting_supplemental':audit['supplemental_counts'],'preservation':preservation})
    print('PASS: components, invariants, 110 unchanged historical outputs and',len(before['files']),'protected files')

if __name__=='__main__':main()
