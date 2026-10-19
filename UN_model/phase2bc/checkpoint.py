#!/usr/bin/env python3
"""Archive the implemented code and only tests actually reported as passed."""
import argparse,hashlib,json,subprocess,zipfile
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
def main():
    p=argparse.ArgumentParser();p.add_argument('name');p.add_argument('results',nargs='+',type=Path);a=p.parse_args()
    folder=ROOT/'UN_model/phase2bc/checkpoints'/a.name;folder.mkdir(parents=True,exist_ok=False)
    tests=[]
    for path in a.results:
        result=json.loads(path.read_text());assert result['status']=='pass',path
        tests.append({'file':str(path),'sha256':hashlib.sha256(path.read_bytes()).hexdigest(),'values':result.get('values',result.get('values_compared'))})
    files=[ROOT/'CMakeLists.txt']
    files+=list((ROOT/'include/models').glob('UN*.h'))+list((ROOT/'src/models').glob('UN*.C'))
    files+=list((ROOT/'UN_model/phase2bc').glob('*.py'))+list((ROOT/'UN_model/phase2bc').glob('*.cpp'))
    files+=list((ROOT/'UN_model/phase2bc').glob('*.md'))
    report=ROOT/'context/UN_L3_PHASE2BC_RESULTS_2026-10-18.md'
    if report.is_file():files.append(report)
    with zipfile.ZipFile(folder/'source.zip','x',zipfile.ZIP_DEFLATED) as z:
        for f in sorted(files):z.write(f,f.relative_to(ROOT))
    manifest={'name':a.name,'status':'pass','tests_passed':tests,'source_sha256':{str(f.relative_to(ROOT)):hashlib.sha256(f.read_bytes()).hexdigest() for f in files},'head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),'note':'Local documented checkpoint; no Git commit, no staged user files included.'}
    (folder/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n');print(folder)
if __name__=='__main__':main()
