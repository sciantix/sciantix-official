"""Verify sources and inputs (hashes) and outputs (numerical tolerance) against the saved reference.

Outputs are compared on the final row and on the column-wise maximum over time of each output.txt, with a
relative tolerance RTOL (floor: SCALE times the maximum magnitude of the column over time, for values near zero): byte-exact output hashes differ between compilers (14 of 114 differed on the CI runner,
12 November 2026). reference_outputs.json holds the reference values; --write-outputs rewrites it from the current
outputs and must only be used after inspecting a deliberate model change (record it in README.md).
"""
import argparse
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent
REPOSITORY = ROOT.parents[1]
RTOL = 1e-4      # relative tolerance on outputs
SCALE = 1e-6     # differences below SCALE x (column maximum magnitude over time) count as equal


def digest(path, text=False):
    content = path.read_bytes()
    if text:
        content = content.replace(b'\r\n', b'\n')
    return hashlib.sha256(content).hexdigest()


def summary(path):
    """Column names, final row, column-wise maximum and maximum magnitude of a SCIANTIX output.txt."""
    rows = [line.rstrip('\n').split('\t') for line in path.read_text().splitlines() if line.strip()]
    columns = [c for c in rows[0] if c]
    values = [[float(x) for x in row[:len(columns)]] for row in rows[1:]]
    return dict(columns=columns, final=values[-1], max=[max(col) for col in zip(*values)],
                scale=[max(abs(x) for x in col) for col in zip(*values)])


def close(a, b, scale):
    return abs(a - b) <= RTOL * max(abs(a), abs(b), SCALE * scale)


def verify(inputs_only=False, write_outputs=False):
    reference = json.loads((ROOT / 'reference_manifest.json').read_text())
    mismatches = []
    actual_sources = {str(path.relative_to(REPOSITORY))
                      for directory in ['src', 'include']
                      for path in (REPOSITORY / directory).rglob('*') if path.is_file()}
    if actual_sources != set(reference['sources_lf_sha256']):
        mismatches.append('Source-file set differs from the recorded reference')
    for group, base in [('sources_lf_sha256', REPOSITORY), ('inputs_lf_sha256', ROOT)]:
        for relative, expected in reference[group].items():
            path = base / relative
            if not path.is_file() or digest(path, text=True) != expected:
                mismatches.append(f'{group}: {relative}')
    output_counts, worst = {}, (0.0, None)
    if not inputs_only:
        reference_file = ROOT / 'reference_outputs.json'
        if write_outputs:
            outputs = {relative: summary(ROOT / relative) for relative in json.loads(reference_file.read_text())}
            reference_file.write_text(json.dumps(outputs, indent=1) + '\n')
        for relative, expected in json.loads(reference_file.read_text()).items():
            path = ROOT / relative
            if not path.is_file():
                mismatches.append(f'output missing: {relative}')
                continue
            actual = summary(path)
            if actual['columns'] != expected['columns']:
                mismatches.append(f'output columns: {relative}')
                continue
            scale = expected['scale']
            bad = [c for i, c in enumerate(expected['columns'])
                   if not (close(actual['final'][i], expected['final'][i], scale[i]) and
                           close(actual['max'][i], expected['max'][i], scale[i]))]
            for key in ('final', 'max'):
                for a, b, c, s in zip(actual[key], expected[key], expected['columns'], scale):
                    if max(abs(a), abs(b), SCALE * s) > 0:
                        deviation = abs(a - b) / max(abs(a), abs(b), SCALE * s)
                        if deviation > worst[0]:
                            worst = (deviation, f'{relative} | {c} | {key}')
            if bad:
                mismatches.append(f'output: {relative} ({", ".join(bad[:3])}{", ..." if len(bad) > 3 else ""})')
            else:
                mode = Path(relative).parts[0]
                output_counts[mode] = output_counts.get(mode, 0) + 1
    report = dict(reference='White coarsening reference (see README.md, regenerated 12 November 2026)',
                  source_files=len(reference['sources_lf_sha256']),
                  input_files=len(reference['inputs_lf_sha256']),
                  outputs_within_tolerance=output_counts,
                  max_relative_deviation=dict(value=worst[0], where=worst[1]),
                  mismatches=mismatches,
                  comparison=f'sources/inputs: SHA256 after CRLF-to-LF normalization; outputs: final row and '
                             f'column maximum, relative tolerance {RTOL} (floor {SCALE} x column magnitude)')
    print(json.dumps(report, indent=2), flush=True)
    if not inputs_only:
        (ROOT / 'reproduction_validation.json').write_text(json.dumps(report, indent=2) + '\n')
    if mismatches:
        raise SystemExit('Reference mismatch: do not publish before inspecting the differences.')
    return report


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--inputs-only', action='store_true')
    parser.add_argument('--write-outputs', action='store_true',
                        help='rewrite reference_outputs.json from the current outputs (deliberate model change only)')
    arguments = parser.parse_args()
    verify(arguments.inputs_only, arguments.write_outputs)
