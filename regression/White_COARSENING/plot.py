"""Plot legacy, frozen Hart and analytic rho results for 38 White locations."""
from pathlib import Path
import csv
import json
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
from matplotlib.ticker import MultipleLocator, ScalarFormatter

ROOT = Path(__file__).resolve().parent
FIGURES = ROOT / 'figures'
FIGURES.mkdir(exist_ok=True)
plt.rcParams.update({'font.size': 10, 'savefig.dpi': 180})
COLORS = {'legacy': '#777777', 'barani': '#d08a32', 'analytic_rho': '#287db5'}
LABELS = {'legacy': 'SCIANTIX legacy', 'barani': 'Frozen Hart (constant rho)',
          'analytic_rho': 'Analytic rho + nucleation (selected)'}


def load(folder):
    with (folder / 'output.txt').open() as stream:
        header = stream.readline().strip().split('\t')
        values = np.loadtxt(stream, ndmin=2)
    return {key.split(' (')[0]: column for key, column in zip(header, values.T)}


def finish(figure, filename):
    figure.savefig(FIGURES / filename, bbox_inches='tight')
    plt.close(figure)


def error_table(observations, predictions, properties, log_metrics):
    """Export experimental error metrics alongside the three regression figures."""
    names = {**LABELS, 'published': 'Barani 2020 published'}
    quantities = {
        'radius_m': ('Bubble radius', 'm'),
        'density_m3': ('Bubble number density', 'm^-3'),
        'swelling_ig_pct': ('Intragranular swelling', 'percentage points'),
        'swelling_gb_pct': ('Intergranular swelling', 'percentage points')}
    lookup = {(row['model'], row['case']): row for row in predictions}
    records, cells = [], []
    for model in names:
        for _, exp, published, factor, key, _ in properties:
            quantity, unit = quantities[key]
            record = dict(model=names[model], quantity=quantity, unit=unit, cases=38,
                          RMSE_log10='', RMSE='', MAE='', MAPE_percent='')
            if model == 'published' and published is None:
                record['cases'] = 0
                cells.append([names[model], quantity + ' (pp)', 'N/A', 'N/A', 'N/A', 'N/A'])
            else:
                x = np.array([float(row[exp]) * factor for row in observations])
                y = np.array([float(row[published]) * factor if model == 'published'
                              else lookup[model, row['case']][key] for row in observations])
                assert len(x) == 38 and np.all(x > 0) and np.all(y > 0)
                record.update(RMSE_log10=float(np.sqrt(np.mean(np.log10(y / x) ** 2))),
                              RMSE=float(np.sqrt(np.mean((y - x) ** 2))),
                              MAE=float(np.mean(abs(y - x))),
                              MAPE_percent=float(100 * np.mean(abs(y / x - 1))))
                previous = next(r for r in log_metrics if r['model'] == model and r['quantity'] == key)
                assert abs(record['RMSE_log10'] - previous['RMSE_log10']) < 1e-12
                physical = lambda value: f'{value:.3e}' if key in ['radius_m', 'density_m3'] else f'{value:.4f}'
                quantity_label = quantity + (' (m)' if key == 'radius_m' else ' (m⁻³)' if key == 'density_m3' else ' (pp)')
                cells.append([names[model], quantity_label, f"{record['RMSE_log10']:.4f}",
                              physical(record['RMSE']), physical(record['MAE']), f"{record['MAPE_percent']:.2f}"])
            records.append(record)
    with (FIGURES / 'error_metrics.csv').open('w', newline='') as stream:
        writer = csv.DictWriter(stream, records[0]); writer.writeheader(); writer.writerows(records)
    fig, ax = plt.subplots(figsize=(17, 8.5)); ax.axis('off')
    table = ax.table(cellText=cells,
                     colLabels=['Model', 'Quantity / error units', 'RMSE log10', 'RMSE', 'MAE', 'MAPE (%)'],
                     cellLoc='center', loc='center', colWidths=[.23, .30, .13, .12, .12, .12])
    table.auto_set_font_size(False); table.set_fontsize(10); table.scale(1, 1.9)
    for (r, c), cell in table.get_celld().items():
        cell.set_edgecolor('#cccccc'); cell.set_linewidth(.5)
        if r == 0:
            cell.set_facecolor('#263b50'); cell.set_text_props(color='white', weight='bold')
        else:
            cell.set_facecolor('#f0f4f7' if (r - 1) // 4 % 2 == 0 else 'white')
            if c in [0, 1]: cell.set_text_props(ha='left')
    ax.set_title('Error metrics against experiments — 38 White cases', fontsize=14, pad=24)
    fig.text(.5, .055, 'RMSE log10 = sqrt(mean(log10(calculated/experimental)²)); RMSE and MAE use the units shown.\n'
             'MAPE = 100 × mean(|calculated/experimental − 1|). pp = percentage points. Lower errors are better.\n'
             'N/A: intergranular swelling predictions are not tabulated in the publication.',
             ha='center', va='top', fontsize=9)
    fig.subplots_adjust(left=.04, right=.96, top=.87, bottom=.19)
    finish(fig, '04_error_metrics.png')


rows = list(csv.DictReader((ROOT / 'cases.csv').open()))
cases = [row['case'] for row in rows]
assert len(cases) == len(set(cases)) == 38
data = {(mode, case): load(ROOT / mode / case)
        for mode in LABELS for case in cases}
checks = []
endpoints = []
for (mode, case), d in data.items():
    produced = sum(d[g + ' produced'] for g in ['Xe', 'Kr'])
    counted = sum(d[g + ' ' + part] for g in ['Xe', 'Kr']
                  for part in ['in intragranular solution', 'in intragranular bubbles', 'at grain boundary', 'released'])
    balance = float(np.max(abs(produced - counted) / np.maximum(produced, 1)))
    finite = all(np.isfinite(value).all() for value in d.values())
    entry = dict(case=case, model=mode, gas_balance=balance, finite=bool(finite))
    if mode != 'legacy':
        bubble_total = sum(d[g + ' in intragranular bubbles'] for g in ['Xe', 'Kr'])
        split = sum(d[g + ' ' + part] for g in ['Xe', 'Kr']
                    for part in ['in bulk bubbles', 'in dislocation bubbles'])
        entry['partition_balance'] = float(np.max(abs(split - bubble_total) / np.maximum(bubble_total, 1)))
        entry['initializations'] = int(np.count_nonzero(np.diff(d['Coarsening initialized'], prepend=0) > 0))
        entry['number_nonincreasing'] = bool(np.all(np.diff(d['Coarsening dislocation concentration']) <= 4e7))
        entry['radius_within_grain'] = bool(np.all(d['Coarsening dislocation radius'] < d['Grain radius']))
        assert entry['initializations'] == 1
        if mode == 'barani':
            assert entry['number_nonincreasing']
        assert entry['partition_balance'] < 1e-5 and entry['radius_within_grain']
    assert finite and balance < 1e-5 and np.max(d['Intragranular gas bubble swelling']) < 1
    checks.append(entry)
    endpoints.append(dict(case=case, model=mode,
                          radius_m=float(d['Intragranular bubble radius' if mode == 'legacy' else 'Coarsening dislocation radius'][-1]),
                          density_m3=float(d['Intragranular bubble concentration' if mode == 'legacy' else 'Coarsening dislocation concentration'][-1]),
                          swelling_ig_pct=float(d['Intragranular gas bubble swelling'][-1] * 100),
                          swelling_gb_pct=float(d['Intergranular gas swelling'][-1] * 100)))
with (ROOT / 'results.csv').open('w', newline='') as stream:
    writer = csv.DictWriter(stream, endpoints[0]); writer.writeheader(); writer.writerows(endpoints)
(ROOT / 'validation.json').write_text(json.dumps(dict(locations=38, simulations=len(checks),
    selected_model='analytic_rho', checks=checks), indent=2))

fig, axes = plt.subplots(2, 2, figsize=(11.7, 10.8))
properties = [
    ('Bubble radius (m)', 'radius_exp_nm', 'radius_published_nm', 1e-9, 'radius_m', (1e-10, 1e-6)),
    ('Bubble number density (m$^{-3}$)', 'density_exp_m3', 'density_published_m3', 1, 'density_m3', (1e17, 1e25)),
    ('Intragranular swelling (%)', 'swelling_exp_pct', 'swelling_published_pct', 1, 'swelling_ig_pct', (.01, 100)),
    ('Intergranular swelling (%)', 'gb_swelling_exp_pct', None, 1, 'swelling_gb_pct', (.01, 10))]
lookup = {(row['model'], row['case']): row for row in endpoints}
metrics = []
for ax, (title, exp, published, factor, key, limits) in zip(axes.flat, properties):
    observed = np.array([float(row[exp]) * factor for row in rows])
    x = np.geomspace(*limits)
    ax.plot(x, x, '--', color='.4', label='Parity')
    ax.plot(x, x * 2, ':', color='.65'); ax.plot(x, x / 2, ':', color='.65')
    if published:
        prediction = np.array([float(row[published]) * factor for row in rows])
        rmse = float(np.sqrt(np.mean(np.log10(prediction / observed) ** 2)))
        metrics.append(dict(model='published', quantity=key, RMSE_log10=rmse, locations=38))
        ax.scatter([float(row[exp]) * factor for row in rows],
                   [float(row[published]) * factor for row in rows],
                   c='#d84141', marker='x', s=29, label='Barani 2020 published', zorder=4)
    for mode in LABELS:
        prediction = np.array([lookup[mode, row['case']][key] for row in rows])
        rmse = float(np.sqrt(np.mean(np.log10(prediction / observed) ** 2)))
        metrics.append(dict(model=mode, quantity=key, RMSE_log10=rmse, locations=38))
        ax.scatter([float(row[exp]) * factor for row in rows],
                   [lookup[mode, row['case']][key] for row in rows],
                   color=COLORS[mode], s=23, alpha=.8, label=LABELS[mode])
    ax.set(xscale='log', yscale='log', xlim=limits, ylim=limits,
           title=title, xlabel='Experimental', ylabel='Calculated')
    ax.grid(which='both', alpha=.2)
    ax.legend(loc='upper left', fontsize=8, framealpha=.95)
fig.subplots_adjust(top=.95, bottom=.09, hspace=.28, wspace=.25)
finish(fig, '01_parity_comparison.png')
with (ROOT / 'metrics.csv').open('w', newline='') as stream:
    writer = csv.DictWriter(stream, metrics[0]); writer.writeheader(); writer.writerows(metrics)
error_table(rows, endpoints, properties, metrics)

palette = ['#4f7fac', '#f1cf55', '#e95a59', '#6ab4b2', '#805293']
parts = ['In solution', 'Bulk bubbles', 'Dislocation bubbles', 'Grain boundary', 'Released']
inventories = []
fig, ax = plt.subplots(figsize=(23, 6.4))
for mode, offset, hatch in [('legacy', -.27, '//'), ('barani', 0., '..'), ('analytic_rho', .27, '')]:
    for i, case in enumerate(cases):
        d = data[mode, case]
        gas = lambda part: sum(d[g + ' ' + part][-1] for g in ['Xe', 'Kr'])
        dis = gas('in dislocation bubbles') if mode != 'legacy' else 0
        values = np.array([gas('in intragranular solution'), gas('in intragranular bubbles') - dis,
                           dis, gas('at grain boundary'), gas('released')]) / gas('produced') * 100
        bottom = 0
        for value, color in zip(values, palette):
            ax.bar(i + offset, value, .25, bottom=bottom, color=color, hatch=hatch, edgecolor='.35', linewidth=.25)
            bottom += value
        inventories.append(dict(case=case, model=mode, **dict(zip(parts, map(float, values)))))
handles = [Patch(facecolor=color, label=part) for color, part in zip(palette, parts)]
handles += [Patch(facecolor='white', edgecolor='.3', hatch=hatch, label=LABELS[mode])
            for mode, hatch in [('legacy', '//'), ('barani', '..'), ('analytic_rho', '')]]
ax.legend(handles=handles, ncol=4, loc='upper center', bbox_to_anchor=(.5, 1.20), frameon=False)
ax.set(xticks=range(38), xticklabels=[row['paper_location'] for row in rows],
       ylabel='End-of-history gas inventory (%)', ylim=(0, 101))
ax.tick_params(axis='x', labelrotation=65, labelsize=8)
ax.grid(axis='y', alpha=.2); fig.tight_layout()
finish(fig, '02_gas_inventory.png')
with (ROOT / 'inventory.csv').open('w', newline='') as stream:
    writer = csv.DictWriter(stream, inventories[0]); writer.writeheader(); writer.writerows(inventories)

case = 'test_White2004_4005-1'
history = np.loadtxt(ROOT / 'barani' / case / 'input_history.txt')
start = history[np.flatnonzero(np.diff(history[:, 1]) > 0)[0], 0] - .2
fig, axes = plt.subplots(2, 1, figsize=(10.5, 8.8), sharex=True)
d = data['analytic_rho', case]
# Include the preceding computed point so the pre-ramp curve reaches the left edge.
first = max(0, np.searchsorted(d['Time'], start) - 1)
mask = np.arange(len(d['Time'])) >= first
# Display origin requested for the paper-style plot; elapsed durations are unchanged.
display_origin_h = 2.908e2
t = display_origin_h + d['Time'][mask] - start
top = axes[0]
top.plot(t, d['Coarsening pressure'][mask], '--', color='blue', linewidth=1.8,
         label='Bubble at dislocation pressure')
top.set(ylabel='Pressure (Pa)', ylim=(0, max(1e9, np.ceil(np.max(d['Coarsening pressure'][mask]) / 2e8) * 2e8)))
top.yaxis.set_major_locator(MultipleLocator(2e8))
top.ticklabel_format(axis='y', style='sci', scilimits=(9, 9), useMathText=True)
vac = top.twinx()
vac.plot(t, d['Coarsening vacancies per bubble'][mask], color='red', linewidth=1.8,
         label='Vacancies per bubble at dislocations')
vac.plot(t, d['Temperature'][mask] * 1e4, '--', color='red', linewidth=1.8, label='Temperature')
vac_limit = max(2.5e7, np.ceil(np.max(d['Coarsening vacancies per bubble'][mask]) / .5e7) * .5e7)
vac.set(ylabel=r'Vacancies per bubble (vac bbl$^{-1}$) / Temperature $\times10^4$', ylim=(0, vac_limit))
vac.yaxis.set_major_locator(MultipleLocator(.5e7 if vac_limit <= 3e7 else 2e7))
vac.ticklabel_format(axis='y', style='sci', scilimits=(7, 7), useMathText=True)
lines = top.get_lines() + vac.get_lines()
top.legend(lines, [line.get_label() for line in lines], loc='lower left',
           bbox_to_anchor=(0, 1.06), frameon=False, fontsize=9.5)
bottom = axes[1]; swell = bottom.twinx()
bottom.plot(t, d['Coarsening dislocation radius'][mask], color='red', linewidth=1.8,
            label='Calculated bubble radius')
swell.plot(t, d['Intragranular gas bubble swelling'][mask] * 100, '--', color='blue', linewidth=1.8,
           label='Calculated gaseous swelling')
experimental = next(row for row in rows if row['case'] == case)
bottom.plot(t[-1], float(experimental['radius_exp_nm']) * 1e-9, marker='s', color='red',
            linestyle='none', markersize=7, label='Experimental bubble radius')
swell.plot(t[-1], float(experimental['swelling_exp_pct']), marker='o', color='blue',
           linestyle='none', markersize=7, label='Experimental gaseous swelling')
bottom.set(ylabel='Bubble radius (m)', ylim=(0, max(8e-8, np.ceil(np.max(d['Coarsening dislocation radius'][mask]) / 2e-8) * 2e-8)))
bottom.yaxis.set_major_locator(MultipleLocator(2e-8))
bottom.ticklabel_format(axis='y', style='sci', scilimits=(-8, -8), useMathText=True)
swell.set(ylabel='Gaseous swelling (%)', ylim=(0, max(10, np.ceil(np.max(d['Intragranular gas bubble swelling'][mask]) * 100 / 2) * 2)))
swell.yaxis.set_major_locator(MultipleLocator(2))
lines = bottom.get_lines() + swell.get_lines()
bottom.legend(lines, [line.get_label() for line in lines], ncol=2, loc='lower left',
              bbox_to_anchor=(0, 1.06), frameon=False, fontsize=9)
for ax in axes:
    ax.set_xlim(display_origin_h, display_origin_h + .35)
    ax.set_xlabel('Time (h)')
    ax.xaxis.set_major_locator(MultipleLocator(.05))
    formatter = ScalarFormatter(useOffset=display_origin_h, useMathText=True)
    formatter.set_scientific(False)
    ax.xaxis.set_major_formatter(formatter)
    ax.tick_params(axis='x', labelbottom=True)
    ax.grid(alpha=.2)
    ax.xaxis.get_offset_text().set_visible(False)
    ax.text(1, -.19, r'$+2.908\times10^2$', transform=ax.transAxes, ha='right', va='top')
fig.subplots_adjust(left=.12, right=.86, top=.87, bottom=.10, hspace=.65)
finish(fig, '03_transient_4005A.png')
print('Validated 114 simulations; regenerated parity, inventory, selected-model transient and error table.', flush=True)
