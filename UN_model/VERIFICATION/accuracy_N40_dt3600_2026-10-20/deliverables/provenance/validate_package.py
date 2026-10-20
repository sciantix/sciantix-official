#!/usr/bin/env python3
"""Audit saved results and preservation, then package without launching C++."""
import sys
sys.dont_write_bytecode = True
import csv, gzip, hashlib, json, shutil, subprocess, time, zipfile
from core import *

OUT = HERE/'deliverables'


def digest(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(4*1024*1024), b''):
            h.update(chunk)
    return h.hexdigest()


def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT)


def equal(a, b):
    assert np.array_equal(np.asarray(a), np.asarray(b)), 'Lossless data mismatch'


def check_peaks(rows, pop, kind, times, numerical, analytic, scale, uncertainty):
    """Recompute each maximum and its own location directly from saved arrays."""
    error = numerical-analytic
    valid = abs(analytic) > max(1., 20*uncertainty)
    values = {'absolute': abs(error), 'normalized_population_scale': abs(error)/scale,
              'relative_instantaneous': np.divide(abs(error), abs(analytic),
                  out=np.full_like(error, np.nan), where=valid)}
    for metric, v in values.items():
        row, = [r for r in rows if r['population']==pop and r['error_kind']==kind and r['metric']==metric]
        finite = np.flatnonzero(np.isfinite(v))
        if not len(finite):
            assert row['maximum_observed']=='' and row['t_s']==''
        else:
            at = int(finite[np.argmax(v[finite])])
            assert float(row['maximum_observed'])==v[at]
            assert float(row['t_s'])==times[at]
        assert int(row['undefined_relative_rows'])==int((~valid).sum())
        assert row['continuous_maximum_claim']=='False'


def identity_rows(path):
    gap = 0.
    for r in load_csv(path):
        a, b, c = [float(r[k]) for k in ['signed_temporal_component','signed_spatial_component','signed_total']]
        scale = max(float(r['population_scale']), 1.)
        residual = c-a-b
        assert float(r['identity_residual'])==residual
        gap = max(gap, abs(residual)/scale)
        assert gap < 32*np.finfo(float).eps
        expected = {'temporal':a, 'spatial':b, 'total':c}[r['error_kind']]
        assert float(r['signed_error'])==expected
        assert float(r['absolute_error'])==abs(expected)
        assert float(r['normalized_population_scale'])==abs(expected)/scale
        if r['relative_status']=='defined':
            assert float(r['relative_instantaneous'])==abs(expected/float(r['analytic_reference']))
        else:
            assert r['relative_instantaneous']==''
    return gap


def main():
    start = time.perf_counter()
    guard(180.)
    summary = json.loads((HERE/'summary.json').read_text())
    before = json.loads((HERE/'preservation_before.json').read_text())
    selected = summary['critical_cases']; byname = {c['name']:c for c in CASES}
    assert len(CASES)==160 and len(byname)==160
    assert sum(c['group']=='LHS' for c in CASES)==128
    assert digest(SOURCE/'cases.json')==summary['source_cases_sha256']
    assert digest(SOURCE/'combinations_160.csv')==summary['source_combinations_sha256']
    assert (OUT/'combinations_160.csv').read_bytes()==(SOURCE/'combinations_160.csv').read_bytes()
    built = build()
    assert digest(HERE/'build/solver_harness.x')==built['binary_sha256']
    assert digest(HERE.parent/'solver_harness.cpp')==built['harness_sha256']
    hourly_rows = 0; undefined = 0; inactive = 0; identity_gap = 0.; old_outputs = set()
    # Verify every exported hourly datum, not just maxima or a temporal subset.
    with open(OUT/'temporal_hourly_lossless.csv', newline='') as f:
        wide = csv.DictReader(f)
        for case in CASES:
            low, high, modes, manifest, uncertainty = old_data(case)
            cpp, cost = old_cpp(case, 40, 3600.)
            old_outputs.add(cost['source'])
            block = [next(wide) for _ in range(4001)]
            assert all(r['case']==case['name'] for r in block)
            equal([int(r['hour']) for r in block], np.arange(4001))
            arr = np.array([[float(r[k]) for k in ['analytic_c','analytic_mb','analytic_md','error_c','error_mb','error_md']] for r in block])
            equal(arr[:,:3], low); equal(arr[:,3:], cpp-low)
            valid = abs(low) > np.maximum(1., 20*uncertainty)
            equal([int(r['relative_valid_mask']) for r in block], valid@np.array([1,2,4]))
            undefined += int((~valid).sum()); inactive += int((abs(low).max(axis=0)==0).sum())
            peakrows = load_csv(HERE/'baseline'/case['name']/'maxima.csv')
            for j, pop in enumerate(POPS):
                check_peaks(peakrows, pop, 'temporal', HOURS, cpp[:,j], low[:,j], max(abs(low[:,j]).max(),1.), uncertainty[j])
                check_peaks(peakrows, pop, 'spatial', OBS, average(modes[:,:,:40])[:,j], high[:,j], max(abs(high[:,j]).max(),1.), uncertainty[j])
                check_peaks(peakrows, pop, 'total', OBS, cpp[np.array(OBS_HOURS),j], high[:,j], max(abs(high[:,j]).max(),1.), uncertainty[j])
            convergence = manifest['high_convergence']
            assert convergence['max_mean_increment_over_N320_spatial']<=.01
            assert convergence['max_tail_energy_relative_increment']<=.01
            identity_gap = max(identity_gap, identity_rows(HERE/'baseline'/case['name']/'spatial_total_errors.csv'))
            hourly_rows += len(block)
        assert next(wide, None) is None
    assert hourly_rows==640160
    print('Validated all 640160 hourly rows and all baseline maxima', flush=True)
    with gzip.open(OUT/'temporal_errors_hourly.csv.gz', 'rt') as f:
        # Streaming through EOF verifies gzip CRC as well as the row count.
        long_rows = sum(1 for _ in f)-1
    assert long_rows==hourly_rows*3
    new_configs = set(); new_run_records = []; be_checks=[]; rich_checks=[]
    # Check true-solver manifests/inputs, and an independent affine BE power at
    # saved endpoints. This is an algebraic check, not a repeated C++ simulation.
    paths = sorted((HERE/'targeted').glob('*/cpp_N*/manifest.json')) + sorted((HERE/'rapid').glob('*/*/manifest.json'))
    assert len(paths)==30
    for path in paths:
        guard()
        m = json.loads(path.read_text()); case = byname[m['case']]; n=m['N']
        folder = ROOT/m['source']; inp = np.loadtxt(folder/'input.txt')
        expected = [n,m['dt_s'],m['end_s'],inp[3],case['D'],case['R'],case['gb'],case['gd'],case['bb'],case['bd'],*case['source'],*case['initial']]
        equal(inp, expected)
        key = tuple(np.delete(inp, 3)); assert key not in new_configs
        new_configs.add(key)
        assert digest(folder/'output.bin')==m['output_sha256']
        a = np.fromfile(folder/'output.bin', dtype='<f8').reshape(-1,4+3*n)
        assert np.isfinite(a).all() and a[0,0]==0 and a[-1,0]==m['end_s']
        assert np.all(np.diff(a[:,0])>0)
        assert n<=160 and not m['reused']
        if m['end_s']==END:
            equal(a[:,0], HOURS)
            active, scale, b, y0 = ref.system(case,n,duration=END)
            e = np.asarray(np.linalg.solve(np.eye(len(y0))[None]-m['dt_s']*b,
                           np.broadcast_to(np.eye(len(y0)),b.shape)), dtype=np.longdouble)
            q=projections(n)
            for step in [1,24,4000]:
                state = np.linalg.matrix_power(e,step)@np.asarray(y0,dtype=np.longdouble)
                modal = np.zeros((3,n)); modal[active,:]=(state[:,:-1]*(scale*q[:,None])).T
                cpp_modes=a[step,4:].reshape(3,n)
                norm=np.maximum(np.linalg.norm(a[:,4:].reshape(-1,3,n),axis=2).max(axis=0),1.)
                gap=float((np.linalg.norm(modal-cpp_modes,axis=1)/norm).max())
                assert gap<1e-8, (case['name'],n,step,gap)
                be_checks.append({'case':case['name'],'N':n,'hour':step,'max_normalized_modal_gap':gap})
        new_run_records.append(m)
    # Detect identical physical integrations in *all* preserved harness inputs,
    # including previous 4B diagnostics (record stride is immaterial).
    old_input_count=0
    for name in before['files']:
        if name.startswith('UN_model/VERIFICATION/') and name.endswith('/input.txt'):
            try:
                v=np.loadtxt(ROOT/name)
            except (ValueError,OSError):
                continue
            if v.shape==(16,):
                old_input_count+=1
                assert tuple(np.delete(v,3)) not in new_configs, 'Repeated old integration: '+name
    for name in selected:
        case=byname[name]; folder=HERE/'targeted'/name
        _, high0, hm, meta, unc=old_data(case)
        conv=json.loads((folder/'high_reference/convergence.json').read_text())
        last=conv['checks'][-1]
        assert last['max_mean_increment_over_N320_spatial']<=.01
        assert last['max_tail_energy_relative_increment']<=.01
        with np.load(folder/'high_reference/combined_reference.npz') as z:
            times=z['time']; high=z['high']; lows={n:z[f'N{n}'] for n in [40,80,160]}
            ids=[int(np.flatnonzero(times==t)[0]) for t in OBS]
            equal(high[ids],high0)
            for n in [40,80,160]:equal(lows[n][ids],average(hm[:,:,:n]))
            assert np.all(times%3600==0) and np.array_equal(times/3600,conv['hours'])
        costs=load_csv(folder/'costs.csv'); assert len(costs)==5
        peaks=load_csv(folder/'refinement_maxima.csv')
        assert len(peaks)==5*27
        for dt in [1800.,900.]:
            _,cost=old_cpp(case,40,dt); old_outputs.add(cost['source'])
        for row in peaks:
            if row['improvement_factor']:
                assert float(row['improvement_factor'])==float(row['candidate_peak_on_same_grid'])/float(row['maximum_observed'])
            if row['error_kind']!='temporal':
                assert int(row['observed_times'])==len(times)
            else:assert int(row['observed_times'])==4001
        identity_gap=max(identity_gap,identity_rows(folder/'refinement_errors.csv'))
        identity_gap=max(identity_gap,identity_rows(folder/'candidate_enriched_errors.csv'))
        rich_checks.append({'case':name,**last,'observed_times':len(times)})
    assert len(old_outputs)==178
    print('Validated 30 new runs, 178 reused runs, shared grids and BE powers',flush=True)
    sampling=[]
    for name in summary['rapid_cases']:
        case=byname[name]; folder=HERE/'rapid'/name
        tau=json.loads((folder/'completed.json').read_text())['tau_fast_N40_s']
        times=np.arange(41)*tau/2
        exact=average(evaluated_modes(case,40,times,folder/'reference'))
        scale=np.maximum(abs(exact).max(axis=0),1.)
        for resolution in [10,20,40]:
            m=json.loads((folder/f'tau{resolution}/manifest.json').read_text())
            a=np.fromfile(ROOT/m['source']/'output.bin',dtype='<f8').reshape(-1,124)
            ids=np.array([int(np.argmin(abs(a[:,0]-t))) for t in times])
            actual=a[ids,0]; drift=float(abs(actual-times).max())
            assert drift<20*tau*2e-12
            # Quantify the effect of float clock differences on the reference.
            at_actual=average(evaluated_modes(case,40,actual,folder/'validation_reference'))
            gap=float((abs(at_actual-exact)/scale).max())
            assert gap<1e-10
            for i in range(len(times)):
                sampling.append({'case':name,'resolution_per_tau':resolution,'nominal_t_s':times[i],
                    'actual_output_t_s':actual[i],'clock_difference_s':actual[i]-times[i],
                    'max_reference_time_shift_normalized':float((abs(at_actual[i]-exact[i])/scale).max())})
        for r in load_csv(folder/'short_errors.csv'):
            assert r['candidate_value_inside_step']==''
        bridge=load_csv(folder/'first_hour_comparison.csv')
        assert all(float(r['time_s'])==3600 for r in bridge)
    table(OUT/'rapid_sampling_validation.csv',sampling)
    table(OUT/'independent_BE_validation.csv',be_checks)
    table(OUT/'highN_added_observations_validation.csv',rich_checks)
    # The complete preservation list includes ignored results, not just git diff.
    checked_bytes=0; changes=[]
    for name, oldsha in before['files'].items():
        path=ROOT/name
        if not path.is_file() or digest(path)!=oldsha:changes.append(name)
        else:checked_bytes+=path.stat().st_size
    assert not changes, changes
    assert git('branch','--show-current').decode().strip()==before['branch']
    assert git('rev-parse','HEAD').decode().strip()==before['head']
    assert git('rev-parse','origin/development/nitride').decode().strip()==before['origin']
    assert hashlib.sha256(git('diff','--cached','--binary')).hexdigest()==before['staged_diff_sha256']
    assert git('diff','--name-only').decode()==before['unstaged']
    assert len(git('diff','--cached','--name-only').decode().splitlines())==before['staged_files']
    print(f'Preserved {len(before["files"])} files, {checked_bytes} bytes, Git state and index',flush=True)
    elapsed=time.time()-json.loads((HERE/'budget.json').read_text())['start_epoch']
    assert elapsed<5400
    new_solver=sum(m['solver_seconds'] for m in new_run_records)
    high_seconds=sum(json.loads((HERE/'targeted'/n/'completed.json').read_text())['high_reference_original_seconds'] for n in selected)
    low_seconds=sum(json.loads((HERE/'targeted'/n/'completed.json').read_text())['low_reference_original_seconds'] for n in selected)
    record={'status':'PASS','cases_checked':160,'hourly_rows_checked_losslessly':hourly_rows,
        'population_hourly_observations':long_rows,'baseline_maxima_checked':160*27,
        'undefined_temporal_relative_observations':undefined,'structurally_inactive_case_populations':inactive,
        'unique_existing_cpp_runs_reused':len(old_outputs),'new_cpp_runs':len(paths),
        'old_harness_inputs_checked_for_duplicate_runs':old_input_count,'duplicate_cpp_runs_found':0,
        'new_4000h_cpp_runs':sum(m['end_s']==END for m in new_run_records),
        'new_short_or_first_hour_cpp_runs':sum(m['end_s']!=END for m in new_run_records),
        'new_cpp_solver_seconds':new_solver,'new_high_reference_original_seconds':high_seconds,
        'new_lowN_reference_original_seconds':low_seconds,'build_seconds':built['seconds'],
        'baseline_analysis_seconds':summary['baseline_analysis_seconds'],
        'max_signed_identity_residual_normalized':identity_gap,
        'independent_BE_checks':len(be_checks),'max_independent_BE_modal_normalized_gap':max(r['max_normalized_modal_gap'] for r in be_checks),
        'rapid_sampling_points_checked':len(sampling),'max_rapid_clock_difference_s':max(abs(r['clock_difference_s']) for r in sampling),
        'max_rapid_reference_time_shift_normalized':max(r['max_reference_time_shift_normalized'] for r in sampling),
        'original_highN_convergence_checks_passed':160,'added_highN_convergence_checks_passed':len(rich_checks),
        'preserved_files_checked':len(before['files']),'preserved_bytes_checked':checked_bytes,
        'preserved_file_differences':changes,'branch':before['branch'],'HEAD':before['head'],
        'index_preserved':True,'staged_files':before['staged_files'],'solver_source_preserved':True,
        'elapsed_budget_seconds_before_packaging':elapsed,'operational_limit_seconds':5400,
        'validation_seconds_before_packaging':time.perf_counter()-start,
        'no_continuous_maximum_or_candidate_subhour_claim':True,
        'acceptance_threshold_applied_to_candidate':False}
    rv.save(OUT/'validation.json',record)
    report=OUT/'REPORT_ACCURATEZZA.md'
    report.write_text(report.read_text()+f'\n## Validazione e tempi misurati\n\n'
        f'Validazione PASS: {hourly_rows:,} righe orarie controllate senza perdita, '
        f'{long_rows:,} osservazioni popolazione/ora, 160 reference high-N originali e nove griglie arricchite. '
        f'L\'identità con segno ha residuo massimo normalizzato {identity_gap:.3g}; '
        f'54 controlli indipendenti della potenza affine Backward Euler sui nuovi output N80/N160 '
        f'hanno scarto modale massimo normalizzato {record["max_independent_BE_modal_normalized_gap"]:.3g}. '
        f'Lo scarto di clock massimo delle prove rapide è {record["max_rapid_clock_difference_s"]:.3g} s, '
        f'con effetto sul riferimento ≤{record["max_rapid_reference_time_shift_normalized"]:.3g} sulla scala di popolazione.\n\n'
        f'Compilazione {built["seconds"]:.3f} s; analisi candidato {summary["baseline_analysis_seconds"]:.3f} s; '
        f'kernel dei 30 nuovi run C++ {new_solver:.3f} s; valutazioni high-N aggiunte {high_seconds:.3f} s; '
        f'cache orarie N80/N160 {low_seconds:.3f} s. Queste misure sono componenti, non somme '
        f'dei tempi originali riutilizzati. Tempo trascorso prima del confezionamento {elapsed/60:.2f} min '
        f'(include preparazione, audit e report; limite 90 min).\n\n'
        f'Conservati byte per byte {len(before["files"]):,} file preesistenti, inclusi risultati ignorati da Git; '
        f'branch, HEAD e index iniziali invariati. Dettagli in validation.json.\n')
    # Small provenance/scripts are useful in the compact package; raw binary
    # arrays and the redundant long table remain local and fully indexed.
    provenance=OUT/'provenance'; provenance.mkdir(exist_ok=True)
    for name in ['README.md','REQUEST.md','selection.json','selection_nominations.csv','rapid_selection.json',
                 'budget.json','benchmark_baseline.json','benchmark_targeted.json','build_manifest.json',
                 'core.py','baseline.py','targeted.py','collect.py','validate_package.py']:
        shutil.copy2(HERE/name,provenance/name)
    rv.save(provenance/'source_locations.json',{
        'raw_existing_root':str(SOURCE.relative_to(ROOT)), 'raw_new_root':str(HERE.relative_to(ROOT)),
        'cases_sha256':summary['source_cases_sha256'],'combinations_sha256':summary['source_combinations_sha256'],
        'source_solver_sha256':built['solver_sha256'],'existing_file_snapshot_sha256':digest(HERE/'preservation_before.json'),
        'old_results_not_embedded':True,'new_binary_files_not_embedded':True})
    raw_index=[]
    for path in sorted(HERE.rglob('*')):
        if not path.is_file() or OUT in path.parents or HERE/'checkpoints' in path.parents:continue
        if path.name in ['accuracy_N40_dt3600_compact.zip','package_info.json']:continue
        raw_index.append({'file':str(path.relative_to(HERE)),'bytes':path.stat().st_size,'sha256':digest(path)})
    table(OUT/'local_reproducibility_files.csv',raw_index)
    packaged=[p for p in sorted(OUT.rglob('*')) if p.is_file() and p.name not in ['temporal_errors_hourly.csv.gz','SHA256SUMS.json']]
    sums={str(p.relative_to(OUT)):digest(p) for p in packaged}
    rv.save(OUT/'SHA256SUMS.json',sums)
    archive=HERE/'accuracy_N40_dt3600_compact.zip'
    with zipfile.ZipFile(archive,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=6) as z:
        for p in packaged+[OUT/'SHA256SUMS.json']:z.write(p,str(p.relative_to(OUT)))
    with zipfile.ZipFile(archive) as z:
        assert z.testzip() is None
        for name, expected_sha in sums.items():
            h=hashlib.sha256()
            with z.open(name) as f:
                for chunk in iter(lambda:f.read(4*1024*1024),b''):h.update(chunk)
            assert h.hexdigest()==expected_sha
        assert not any(n.endswith(('.bin','.npz','.x')) for n in z.namelist())
        entries=len(z.namelist())
    finish_elapsed=time.time()-json.loads((HERE/'budget.json').read_text())['start_epoch']
    assert finish_elapsed<5400
    package={'archive':archive.name,'bytes':archive.stat().st_size,'sha256':digest(archive),
        'entries':entries,'all_archive_crc_and_sha256_checked':True,'all_4001_hours_included':True,
        'lossless_hourly_format':'temporal_hourly_lossless.csv + normalization_scales.csv + README formulas',
        'duplicated_long_gzip_omitted':True,'large_binary_arrays_excluded':True,
        'elapsed_budget_seconds':finish_elapsed,'validation_and_packaging_seconds':time.perf_counter()-start}
    rv.save(HERE/'package_info.json',package)
    checkpoint=HERE/'checkpoints/final'; checkpoint.mkdir(parents=True,exist_ok=True)
    rv.save(checkpoint/'manifest.json',{'status':'completed_and_validated','validation':record,'package':package,
        'deliverable_sha256':sums,'SHA256SUMS_sha256':digest(OUT/'SHA256SUMS.json'),
        'preservation_snapshot_sha256':digest(HERE/'preservation_before.json'),
        'stop_after_this_campaign':True})
    print(json.dumps({'validation':record,'package':package},indent=2),flush=True)


if __name__=='__main__':
    main()
