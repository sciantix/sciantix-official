#!/usr/bin/env python3
"""Freeze 128 LHS points and eight coefficient states per frozen UN history."""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import csv
import gzip
import json
import hashlib
sys.path.insert(0,str(Path(__file__).resolve().parent.parent/'phase4b/resumed'))
from reference import rv, np, ROOT
from common import BOUNDS, SOURCE, decay_rates
from scipy.stats import qmc
import scipy

HERE=Path(__file__).resolve().parent
SEED=20261020
KEYS=['D','gb','gd','bb','bd']


def csv_write(path,rows):
    with open(path,'x',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(rows[0]));w.writeheader();w.writerows(rows)


def main():
    assert not (HERE/'cases.json').exists(), 'Already frozen; do not regenerate'
    inputs=HERE/'inputs';inputs.mkdir(exist_ok=False)
    engine=qmc.LatinHypercube(d=5,scramble=True,rng=np.random.default_rng(SEED))
    unit=engine.random(n=128)
    assert all(np.array_equal(np.sort(np.floor(unit[:,j]*128).astype(int)),np.arange(128)) for j in range(5))
    rows=[];cases=[];latent=[];history_all=[];selection=[]
    for i,u in enumerate(unit):
        name=f'LHS_{i+1:03d}'
        params={}
        for j,key in enumerate(KEYS):
            lo,hi=BOUNDS[key]
            params[key]=float(lo+u[j]*(hi-lo)) if key=='gb' else float(np.exp(np.log(lo)+u[j]*np.log(hi/lo)))
        cases.append({'name':name,**params,'R':6e-6,'initial':[0.,0.,0.],'source':SOURCE,
            'group':'LHS','provenance':{'seed':SEED,'unit_row':i,'generator':'scipy.stats.qmc.LatinHypercube'}})
        latent.append({'case':name,**{key:float(u[j]) for j,key in enumerate(KEYS)}})
    for label in ['AP3p2_900K','AP3p4_1600K','AP3p4_1800K','ANP6_1600K']:
        path=ROOT/'UN_model/regression_reference/results/full'/label/'trace.jsonl.gz'
        digest=rv.sha(path);states=[]
        with gzip.open(path,'rt') as f:
            for line_no,line in enumerate(f,1):
                event=json.loads(line)
                if event['stage']!='after_exchange':continue
                s=event['scalars']
                state={'history':label,'step':event['step'],'source_time_s':s['t'],
                    'source_interval_end_s':s['t']+s['dt'],'source_dt_s':s['dt'],
                    **{k:s[v] for k,v in zip(KEYS,['Dg','g_b','g_d','b_b','b_d'])},
                    'source_file':str(path.relative_to(ROOT)),'source_sha256':digest,
                    'source_line':line_no,'stage':'after_exchange','original_beta':s['beta']}
                states.append(state)
        assert states and states[0]['source_time_s']==0
        # Six evenly distributed irradiation times, including both endpoints.
        times=np.array([s['source_time_s'] for s in states])
        chosen={0:['initial_coefficients'],len(states)-1:['last_available_coefficients']}
        for fraction in [.2,.4,.6,.8]:
            index=int(np.argmin(abs(times-fraction*times[-1])))
            chosen.setdefault(index,[]).append(f'time_fraction_{fraction:g}')
        # Add the two largest relative changes (zero-to-positive included),
        # preserving the actual values rather than clipping to Table B.1.
        values=np.array([[s[k] for k in KEYS] for s in states])
        changes=np.linalg.norm(np.divide(abs(np.diff(values,axis=0)),
                   np.maximum(np.maximum(abs(values[1:]),abs(values[:-1])),np.finfo(float).tiny)),axis=1)
        for index in np.argsort(-changes,kind='stable')+1:
            if len(chosen)>=8:break
            if int(index) not in chosen:chosen[int(index)]=['major_relative_coefficient_change']
        assert len(chosen)==8
        for j,index in enumerate(sorted(chosen),1):
            state=states[index]
            name=f'UN_{label}_S{j}'
            selection.append({'case':name,**state,'selection_reason':';'.join(chosen[index]),
                'relative_jump_score':float(changes[index-1]) if index else 0.})
            cases.append({'name':name,**{k:state[k] for k in KEYS},'R':6e-6,'initial':[0.,0.,0.],
                'source':SOURCE,'group':'UN_history','provenance':dict(selection[-1])})
        history_all.extend(states)
    assert len(cases)==160
    outside=[]
    for c in cases:
        assert c['D']>0 and c['gd']>0 and c['gb']>=0 and c['bb']>0 and c['bd']>0
        bad=[k for k,(lo,hi) in BOUNDS.items() if not lo<=c[k]<=hi]
        assert not bad or c['group']=='UN_history'
        if bad:outside.append({'case':c['name'],'coefficients_outside_TableB1':bad,'values':{k:c[k] for k in bad}})
        p=c['provenance']
        rows.append({'case':c['name'],'group':c['group'],**{k:c[k] for k in KEYS},'R':c['R'],
           'source_c':SOURCE[0],'initial_c':0,'initial_mb':0,'initial_md':0,'seed':SEED if c['group']=='LHS' else '',
           'history':p.get('history',''),'step':p.get('step',''),'source_time_s':p.get('source_time_s',''),
           'source_interval_end_s':p.get('source_interval_end_s',''),'stage':p.get('stage',''),
           'source_file':p.get('source_file','inputs/lhs_unit.csv'),'source_sha256':p.get('source_sha256',''),
           'source_line':p.get('source_line',''),'selection_reason':p.get('selection_reason','Latin hypercube'),
           'outside_TableB1':';'.join(bad),'tau_fast_N40_s':float(1/max(decay_rates(c,40)))})
    csv_write(inputs/'lhs_unit.csv',latent)
    lhs_sha=rv.sha(inputs/'lhs_unit.csv')
    for r in rows:
        if r['group']=='LHS':r['source_sha256']=lhs_sha
    csv_write(HERE/'combinations_160.csv',rows)
    csv_write(inputs/'history_coefficients_complete.csv',history_all)
    csv_write(inputs/'history_selection_32.csv',selection)
    rv.save(HERE/'cases.json',cases)
    rv.save(HERE/'configuration.json',{'seed':SEED,'scipy':scipy.__version__,'numpy':np.__version__,
        'bounds':BOUNDS,'coordinate_mapping':{k:'linear' if k=='gb' else 'log' for k in KEYS},
        'R_m':6e-6,'source_common':SOURCE,'initial_common':[0,0,0],'coefficients':'constant throughout each IVP',
        'end_s':14400000,'temporal_N':40,'dt_s':[3600,1800,900,450,225],
        'spatial_N':[10,20,40,80,160,320],
        'fixed_hours':[0,1,2,4,8,24,100,500,1000,2000,4000],
        'temporal_storage_hours':1,'relative_errors':'absolute analytical value > max(1,20*reference_uncertainty)',
        'order_assessment':{'clearly_first_order_interval':[.9,1.1],
            'precision_screen':'max(100 eps*history scale,20*pointwise direct-vs-semigroup gap,20*split gap*scale)',
            'max_extra_halvings':5,'smallest_possible_dt_s':7.03125,
            'note':'screening criteria, not application accuracy targets; all raw p values preserved'},
        'spatial_reference_protocol':'unchanged 4B doubling checks <=1% on mean increment / N320 error and relative tail-energy increment',
        'budget_seconds':5400,'LHS_stratification_verified':True,'cases_sha256':rv.sha(HERE/'cases.json'),
        'combinations_csv_sha256':rv.sha(HERE/'combinations_160.csv'),
        'complete_history_rows':len(history_all),'real_states_outside_bounds':outside,
        'initial_history_semantics':'first after_exchange coefficients belong to state t=0; last coefficients are those actually used in last interval, not recomputed at end'})
    print('Frozen 128 LHS + 32 UN states;',len(history_all),'history rows;',len(outside),'real states outside Table B.1',flush=True)


if __name__=='__main__':main()
