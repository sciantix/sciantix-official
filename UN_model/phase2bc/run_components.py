#!/usr/bin/env python3
"""Component tests using only frozen Phase 1 outputs, never executing Python physics."""
import argparse,csv,gzip,hashlib,json,os,re,subprocess,time
from pathlib import Path
import numpy as np

ROOT=Path(__file__).resolve().parents[2]
HERE=Path(__file__).resolve().parent
REF=ROOT/'UN_model/regression_reference/results/full'
CASES=['AP3p2_900K','AP3p4_1600K','AP3p4_1800K','ANP6_1600K']

def sha(p): return hashlib.sha256(Path(p).read_bytes()).hexdigest()
def save(p,v): Path(p).write_text(json.dumps(v,indent=2,allow_nan=False)+'\n')
def scalar_events(case):
    m=json.loads((REF/case/'manifest.json').read_text())
    for name,h in m['output_sha256'].items(): assert sha(REF/case/name)==h,(case,name)
    events=[]
    with gzip.open(REF/case/'trace.jsonl.gz','rt') as f:
        for line in f: events.append(json.loads(line))
    return m,events

def parameter_loader(out):
    text=(ROOT/'include/models/UNParameters.h').read_text().split('struct Parameters')[1].split('struct PhysicsOptions')[0]
    members=[]
    for kind,block in re.findall(r'\b(double|bool|std::string)\s+([^;]+);',text):
        for item in block.split(','):
            name=item.split('=')[0].strip()
            if re.fullmatch(r'\w+',name): members.append((kind,name))
    lines=['#include "UNParameters.h"','#include <fstream>','#include <stdexcept>',
           'inline void loadParameters(const char* path, un_l3::Parameters& p) {',
           'std::ifstream f(path); std::string name,value; while(f>>name>>value) {']
    for j,(kind,name) in enumerate(members):
        expression='value' if kind=='std::string' else ('std::stod(value)!=0.0' if kind=='bool' else 'std::stod(value)')
        lines.append(('if' if j==0 else 'else if')+f'(name=="{name}") p.{name}={expression};')
    lines += ['else throw std::runtime_error("Unknown fixture parameter: "+name);','} }']
    (out/'parameter_loader.h').write_text('\n'.join(lines)+'\n')
    return [name for _,name in members]

def compare(actual,expected,fields,case):
    assert actual.shape==expected.shape,(actual.shape,expected.shape)
    metrics=[]
    for j,name in enumerate(fields):
        a,b=actual[:,j],expected[:,j];d=np.abs(a-b)
        # Per-value scaling, with an absolute unit floor only for exact zero.
        scale=np.where(b!=0,np.abs(b),1.0)
        bad=~np.isfinite(a)|(d>1e-11*scale)
        idx=int(np.argmax(d/scale))
        metrics.append({'case':case,'field':name,'count':len(a),'failures':int(bad.sum()),
                        'max_absolute_error':float(d.max()),'max_normalized_error':float((d/scale).max()),
                        'max_step':idx,'first_failure':int(np.flatnonzero(bad)[0]) if bad.any() else None})
    return metrics

def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--component',choices=['transport','lookup','vacancy','seed','intragranular','intragranular_chain','grainface_limited','grainface_3x3','grainface','grainface_chain','grainface_initial'],required=True)
    ap.add_argument('--output',type=Path,required=True)
    ap.add_argument('--sanitize',action='store_true',help='ASan/UBSan; leak detection disabled for the known ptrace environment')
    args=ap.parse_args(); out=args.output.resolve();out.mkdir(parents=True,exist_ok=False)
    members=parameter_loader(out)
    sources=['src/classes/Solver.C','src/models/UNIntraGranular.C','src/models/UNTransport.C','src/models/UNBubblePhysics.C','src/models/UNGrainFace.C']
    command=['g++','-std=c++11','-O2','-Wall','-Wextra','-I'+str(ROOT/'include/classes'),'-I'+str(ROOT/'include/models'),'-I'+str(out),str(HERE/'component_driver.cpp')]+[str(ROOT/s) for s in sources]+['-o',str(out/'component_driver.x')]
    if args.sanitize:command[2:3]=['-O1','-g','-fsanitize=address,undefined','-fno-omit-frame-pointer']
    build_command=list(command); start=time.perf_counter()
    with open(out/'build.log','w') as f:subprocess.run(command,stdout=f,stderr=subprocess.STDOUT,check=True)
    build_seconds=time.perf_counter()-start
    metrics=[];provenance={};commands=[]
    fields=['Dg','D1_Xe','D2_Xe','D3_Xe','Dv','Dv1','Dv2','Dv_dislocation','b_b','b_d','g_b','g_d','Gamma_d','den','free_dislocation','term_bubbles','term_dislocation','beta','nu_b','Dv_gb']
    for case in CASES:
        manifest,events=scalar_events(case);folder=out/case;folder.mkdir()
        p=manifest['parameters']
        (folder/'parameters.txt').write_text(''.join(f'{k} {int(p[k]) if isinstance(p[k],bool) else p[k]}\n' for k in members))
        if args.component=='lookup':
            settings=json.loads((REF/'notebook_settings.json').read_text());inputs=[];expected=[];fields=['log10_diffusivity']
            for kind,key in enumerate(['MATTHEWS2025_DXE_LOG_LOOKUP','MATTHEWS2025_DU_LOG_LOOKUP']):
                for x,y in settings[key]:inputs.append([kind,x]);expected.append([y])
        elif args.component=='transport':
            selected=[e for e in events if e['stage']=='after_exchange']
            inputs=[[e['scalars'][k] for k in ['Rb_old','Nb_old','Rd_old','Nd_old','c_old']] for e in selected]
            expected=[];finals={e['step']:e for e in events if e['stage']=='step_end'}
            for e in selected:
                allvalues={**e.get('D_parts',{}),**e.get('Dv_parts',{}),**e.get('trapping_parts',{}),**e['scalars']}
                allvalues['Dv_gb']=finals[e['step']]['last_rates']['Dv_gb']
                expected.append([allvalues[k] for k in fields])
        elif args.component=='vacancy':
            selected=[e for e in events if e['stage']=='function_return:vacancy_concentration_implicit_step']
            inputs=[[e['scalars'][k] for k in ['Dv','R','N','m_gas','n_old','dt']] for e in selected]
            fields=['inventory','rate'];expected=[]
            for e in selected:
                v=e['scalars'];value=v.get('n_new',v['n_old'])
                expected.append([value,(value-v['n_old'])/v['dt']])
        elif args.component=='seed':
            selected=[e for e in events if e['stage']=='function_return:seed_vacancies_for_pressure_factor']
            inputs=[[e['scalars'][k] for k in ['gas_atoms_per_bubble','pressure_factor']] for e in selected]
            fields=['vacancies_per_bubble','radius','pressure_ratio'];expected=[]
            seeds={e['step']:e['scalars'] for e in events if e['stage']=='after_seed'}
            for e in selected:
                v=e['scalars'];s=seeds[e['step']]
                radius=s['_Rb_seed_from_gas'] if s['bulk_first_gas_seed_applied'] else s['_Rd_seed_from_gas']
                expected.append([v['nv'],radius,v['pressure_factor']])
        elif args.component in ['grainface_limited','grainface_3x3']:
            stage='function_return:'+('_limited_growth_grainface' if args.component=='grainface_limited' else '_solve_grainface_3x3')
            selected=[e for e in events if e['stage']==stage]
            fields=['nv1'] if args.component=='grainface_limited' else ['V1','A1','N1']
            inputs=[];expected=[]
            updates={e['step']:e['scalars'] for e in events if e['stage']=='function_return:update_grainface_state'}
            for e in selected:
                v=e['scalars']
                if args.component=='grainface_limited':
                    inputs.append([v[k] for k in ['nv0','growth_rate','equilibrium_term','dt']]);expected.append([updates[e['step']]['nv_candidate']])
                else:
                    inputs.append([float(v[k]) for k in ['V0','A0','N0','Fc0','gasvol0','gasvol1','delta_nv','omega_v','kappa_gf','saturated_branch']]);expected.append([v[k] for k in fields])
        elif args.component in ['grainface','grainface_chain','grainface_initial']:
            selected=[e for e in events if e['stage']==('initial' if args.component=='grainface_initial' else 'function_return:update_grainface_state')]
            old={e['step']:e['gf'] for e in events if e['stage']=='after_capture'}
            fields=['q_gf','q_rel','qv_gf','Ngf_areal','Ngf_vol','ng_gf','nv_gf','V_gf','R_gf','A_gf','Fc','p_gf','p_gf_eq','zeta_gf',
                    'dnv_gf_dt','dA_gf_dt','dNgf_dt','dq_gf_in_step','dq_rel_step','gf_saturated']
            inputs=[];expected=[]
            for e in selected:
                if args.component=='grainface_initial':
                    inputs.append([0.0]*8);expected.append([float(e['gf'].get(k,0.0)) for k in fields]);continue
                v=e['scalars'];previous=old[e['step']]
                inputs.append([previous[k] for k in ['Ngf_areal','q_gf','q_rel','qv_gf']]+[int(previous.get('saturation_reached',False)),v['dq_to_gb'],v['dt'],v['Dv1_bulk']])
                expected.append([float(e['gf'][k]) for k in fields])
        else:
            old={e['step']:e['scalars'] for e in events if e['stage']=='old_state'}
            exchange={e['step']:e['scalars'] for e in events if e['stage']=='after_exchange'}
            seed={e['step']:e['scalars'] for e in events if e['stage']=='after_seed'}
            vacancies={e['step']:e['scalars'] for e in events if e['stage']=='after_vacancies'}
            selected=[e for e in events if e['stage']=='after_capture']
            base=['c_new','mb_new','md_new','N_b','N_d','R_b','R_d','V_b','V_d','nvb','nvd','bulk_first_gas_seed_done','dislocation_first_gas_seed_done']
            diag=['Dg','Dv','Dv_dislocation','b_b','b_d','g_b','g_d','nu_b','phi_b','phi_d','lambda_d','dVd_growth_dt','dnvb_dt','dnvd_dt','f_cap','cap_raw_step','source_c','source_mb']
            fields=base+diag+['nvb_after_seed','nvd_after_seed','nvb_after_vacancy','nvd_after_vacancy','V_b_growth']
            fields += [f'{stage}_{pop}_{i}' for stage in ['exchange','capture'] for pop in ['c','mb','md'] for i in range(160)]
            inputs=[];expected=[]
            with np.load(REF/case/'modes.npz') as modes:
                # NPZ members are decompressed on every indexing operation;
                # cache once to avoid quadratic repeated decompression.
                initial_modes=modes['initial'];end_modes=modes['step_end']
                exchange_modes=modes['after_exchange'];capture_modes=modes['after_capture']
                for e in selected:
                    i=e['step'];v=e['scalars'];previous=initial_modes[0] if i==0 else end_modes[i-1]
                    inputs.append([old[i][k] for k in ['N_b','N_d','R_b','R_d','V_b','V_d','nvb','nvd','bulk_first_gas_seed_done','dislocation_first_gas_seed_done','dt']]+previous.ravel().tolist())
                    allvalues={**e.get('D_parts',{}),**e.get('Dv_parts',{}),**v}
                    row=[float(allvalues[k]) for k in base+diag]
                    row += [seed[i]['nvb'],seed[i]['nvd'],vacancies[i]['nvb'],vacancies[i]['nvd'],v['V_b_growth']]
                    row += exchange_modes[i].ravel().tolist()+capture_modes[i].ravel().tolist()
                    expected.append(row)
        np.savetxt(folder/'input.txt',np.asarray(inputs),header=str(len(inputs)),comments='',fmt='%.17g')
        command=[str(out/'component_driver.x'),args.component,str(folder/'parameters.txt'),str(folder/'input.txt'),str(folder/'output.bin')]
        env=dict(os.environ)
        if args.sanitize:env['ASAN_OPTIONS']='detect_leaks=0'
        start=time.perf_counter();subprocess.run(command,check=True,env=env)
        commands.append({'case':case,'argv':command,'wall_seconds':time.perf_counter()-start})
        actual=np.fromfile(folder/'output.bin',dtype='<f8').reshape(len(inputs),len(fields))
        np.savez_compressed(folder/'comparison.npz',actual=actual,expected=np.asarray(expected))
        metrics+=compare(actual,np.asarray(expected),fields,case)
        provenance[case]={str(REF/case/n):sha(REF/case/n) for n in ['manifest.json','trace.jsonl.gz','modes.npz']}
    result={'component':args.component,'status':'pass' if not sum(r['failures'] for r in metrics) else 'fail',
            'values':sum(r['count'] for r in metrics),'metrics':metrics,'reference_sha256':provenance,
            'build_command':build_command,'build_seconds':build_seconds,'commands':commands,
            'sanitizers':args.sanitize,'ASAN_OPTIONS':'detect_leaks=0' if args.sanitize else None,
            'source_sha256':{s:sha(ROOT/s) for s in sources},'runner_sha256':sha(__file__)}
    save(out/'results.json',result)
    with open(out/'errors.csv','w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(metrics[0]));w.writeheader();w.writerows(metrics)
    print(args.component,result['status'],result['values'],'values; max normalized',max(r['max_normalized_error'] for r in metrics),flush=True)
    return result['status']!='pass'

if __name__=='__main__':raise SystemExit(main())
