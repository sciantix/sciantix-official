"""Verify sources, inputs and byte-exact outputs against the saved local campaign."""
import argparse
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent
REPOSITORY = ROOT.parents[1]


def digest(path, text=False):
    content = path.read_bytes()
    if text:
        content = content.replace(b'\r\n', b'\n')
    return hashlib.sha256(content).hexdigest()


def verify(inputs_only=False):
    reference = json.loads((ROOT / 'reference_manifest.json').read_text())
    mismatches = []
    actual_sources = {str(path.relative_to(REPOSITORY))
                      for directory in ['src', 'include']
                      for path in (REPOSITORY / directory).rglob('*') if path.is_file()}
    if actual_sources != set(reference['sources_lf_sha256']):
        mismatches.append('Source-file set differs from the recorded local campaign')
    for group, base in [('sources_lf_sha256', REPOSITORY), ('inputs_lf_sha256', ROOT)]:
        for relative, expected in reference[group].items():
            path = base / relative
            if not path.is_file() or digest(path, text=True) != expected:
                mismatches.append(f'{group}: {relative}')
    output_counts = {}
    if not inputs_only:
        for relative, expected in reference['outputs_sha256'].items():
            path = ROOT / relative
            if not path.is_file() or digest(path) != expected:
                mismatches.append(f'output: {relative}')
            else:
                mode = Path(relative).parts[0]
                output_counts[mode] = output_counts.get(mode, 0) + 1
    report = dict(reference='Saved local analytic_rho campaign and frozen Hart controls',
                  source_files=len(reference['sources_lf_sha256']),
                  input_files=len(reference['inputs_lf_sha256']),
                  byte_identical_outputs=output_counts, mismatches=mismatches,
                  text_comparison='SHA256 after CRLF-to-LF normalization; outputs use raw bytes')
    print(json.dumps(report, indent=2), flush=True)
    if not inputs_only:
        (ROOT / 'reproduction_validation.json').write_text(json.dumps(report, indent=2) + '\n')
    if mismatches:
        raise SystemExit('Reference mismatch: do not publish before inspecting the differences.')
    return report


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--inputs-only', action='store_true')
    verify(parser.parse_args().inputs_only)
