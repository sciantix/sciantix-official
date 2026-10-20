"""Cached SciPy expm reference; imports the unchanged 4A basis and matrices."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import json
import time
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from common import HERE as OLD, ROOT, rv, np, matrices, projections, VOLUME, expm

HERE = Path(__file__).resolve().parent
OBS_HOURS = [0, 1, 2, 4, 8, 24, 100, 500, 1000, 2000, 4000]
OBS = np.array(OBS_HOURS, dtype=float)*3600
HOURS = np.arange(4001, dtype=float)*3600


def system(case, n, start=0, duration=14400000.):
    active = [0, 2] if case['gb'] == 0 and case['initial'][1] == case['source'][1] == 0 else [0, 1, 2]
    scale = max(max(abs(np.asarray(case['initial']))), max(abs(np.asarray(case['source'])))*duration, 1.)
    j = matrices(case, n)[start:][:, active][:, :, active]
    b = np.zeros((len(j), len(active)+1, len(active)+1))
    b[:, :-1, :-1] = j
    b[:, :-1, -1] = np.asarray(case['source'])[active]/scale
    y0 = np.r_[np.asarray(case['initial'])[active]/scale, 1.]
    return active, scale, b, y0


def direct_modes(case, n, times, start=0):
    active, scale, b, y0 = system(case, n, start, max(times))
    q = projections(n)[start:]
    result = np.zeros((len(times), 3, n-start))
    for i, t in enumerate(times):
        y = np.broadcast_to(y0, (n-start, len(y0))) if t == 0 else expm(b*t) @ y0
        result[i, active, :] = (y[:, :-1]*(scale*q[:, None])).T
    return result


def average(modes):
    return np.einsum('tpn,n->tp', modes, projections(modes.shape[-1])/VOLUME)


def hourly(case, n, folder):
    """Cache exact semigroup propagation, checked against direct expm and h/2.

    This advances the analytical exponential, never Backward Euler. Extended
    precision accumulation reduces roundoff in the 4000 products. No coefficient
    or exponential entry is clipped or renormalized.
    """
    path = folder/f'{case["name"]}_N{n}_hourly.npz'
    if path.exists():
        with np.load(path) as z: return z['means'], z['modes'], json.loads(str(z['metadata']))
    begin = time.perf_counter()
    active, scale, b, y0 = system(case, n)
    q = projections(n)
    def propagate(h):
        e = np.asarray(expm(b*h), dtype=np.longdouble)
        state = np.broadcast_to(np.asarray(y0, dtype=np.longdouble), (n, len(y0))).copy()
        result = np.zeros((4001, 3, n))
        for i in range(4001):
            result[i, active, :] = (state[:, :-1]*(scale*q[:, None])).T
            if i < 4000:
                for _ in range(round(3600/h)): state = np.einsum('nij,nj->ni', e, state)
        return result
    full = propagate(3600.)
    fine = propagate(1800.)
    direct = direct_modes(case, n, OBS)
    at = fine[np.array(OBS_HOURS)]
    means = average(fine)
    scale_mean = np.maximum(abs(means).max(axis=0), 1.)
    scale_modes = np.maximum(np.linalg.norm(fine, axis=2).max(axis=0), 1.)
    split_gap = abs(average(full)-means).max(axis=0)/scale_mean
    direct_gap = abs(average(at)-average(direct)).max(axis=0)/scale_mean
    modal_gap = np.linalg.norm(at-direct, axis=2).max(axis=0)/scale_modes
    meta = {'method': 'SciPy expm augmented system; exact 1800 s semigroup, longdouble accumulation',
            'direct_check_hours': OBS_HOURS, 'split_gap_normalized': split_gap.tolist(),
            'direct_gap_normalized': direct_gap.tolist(), 'modal_direct_gap_normalized': modal_gap.tolist(),
            'seconds': time.perf_counter()-begin, 'case_sha256': rv.sha(OLD/'cases.json'),
            'normalization': 'maximum over history of each analytical population, floor 1 at/m3',
            'roundoff_warning_threshold': 1e-8}
    assert max(split_gap.max(), direct_gap.max(), modal_gap.max()) < 1e-8, ('Reference roundoff', case['name'], n, meta)
    np.savez_compressed(path, time=HOURS, means=means, modes=fine, metadata=json.dumps(meta))
    return means, fine, meta


def high_reference(case, folder):
    """Direct expm at physical observations; expand only previously missing modes.

    Convergence is checked on each population's mean and squared spatial L2
    error at N=320. Retained modal coefficients do not depend on truncation N.
    All intermediate references are saved, so expansion never changes data.
    """
    summary = folder/f'{case["name"]}_high_reference.json'
    if summary.exists():
        meta = json.loads(summary.read_text())
        with np.load(folder/meta['file']) as z: return z['means'], z['modes'], meta
    old_modes = np.zeros((len(OBS), 3, 0)); records = []
    start_all = time.perf_counter()
    for n in [320, 640, 1280, 2560, 5120, 10240, 20480, 40960, 81920, 163840, 327680]:
        path = folder/f'{case["name"]}_direct_N{n}.npz'
        begin = time.perf_counter()
        if path.exists():
            with np.load(path) as z: modes = z['modes']
        else:
            tail = direct_modes(case, n, OBS, old_modes.shape[-1])
            modes = np.concatenate([old_modes, tail], axis=2)
            np.savez_compressed(path, time=OBS, modes=modes)
        means = average(modes)
        energy = np.sum(modes[:, :, 320:]**2, axis=2)
        if old_modes.shape[-1] >= 640:
            previous = average(old_modes)
            spatial = abs(means-average(modes[:, :, :320]))
            gap = abs(means-previous)
            active = abs(means) > 0
            mean_ratio = np.divide(gap, spatial, out=np.zeros_like(gap), where=spatial>0)
            old_energy = np.sum(old_modes[:, :, 320:]**2, axis=2)
            energy_ratio = np.divide(abs(energy-old_energy), energy, out=np.zeros_like(energy), where=energy>0)
            mr = float(mean_ratio[active].max()); er = float(energy_ratio[active].max())
        else: mr = er = None
        row = {'N': n, 'seconds': time.perf_counter()-begin,
               'max_mean_increment_over_N320_spatial': mr,
               'max_tail_energy_relative_increment': er}
        records.append(row)
        rv.save(folder/f'{case["name"]}_reference_progress.json', records)
        print('HIGH_REF', case['name'], row, flush=True)
        if mr is not None and mr <= .01 and er <= .01:
            # Do not claim the whole field vector changed by <1%; the checked
            # scalar L2 norm changes by <=0.5% for a <=1% energy increment.
            result = {'case': case['name'], 'N': n, 'file': path.name,
                      'criteria': {'mean_increment_over_N320_spatial': .01,
                                   'relative_increment_of_squared_tail_L2': .01},
                      'records': records, 'seconds': time.perf_counter()-start_all,
                      'observations_hours': OBS_HOURS,
                      'retained_modes': 'identical prefix by independent modal equations; no truncation error on retained coefficient',
                      'limitation': 'successive doubling evidence; not a rigorous infinite-tail bound'}
            # Means can be reconstructed without writing over the modal cache.
            final = folder/f'{case["name"]}_high_converged.npz'
            np.savez_compressed(final, time=OBS, means=means, modes=modes)
            result['file'] = final.name
            rv.save(summary, result)
            return means, modes, result
        old_modes = modes
    raise RuntimeError('High-N reference has not converged: '+case['name'])
