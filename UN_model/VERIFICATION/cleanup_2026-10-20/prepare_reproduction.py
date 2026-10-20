#!/usr/bin/env python3
"""Prepare a fresh sibling run from retained scripts and frozen inputs.

Never starts a simulation, refits parameters or overwrites a concluded campaign.
"""
import sys
sys.dont_write_bytecode=True
from pathlib import Path
import argparse, ast, hashlib, json, platform, shutil, subprocess, time

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[2]
VER=HERE.parent
sys.path.insert(0,str(VER/'dependencies'))
SOURCES={'extended':VER/'extended_convergence_2026-10-20',
         'accuracy':VER/'accuracy_N40_dt3600_2026-10-20',
         'coefficients':VER/'coefficient_histories_2026-10-20'}


def sha(path):
    h=hashlib.sha256()
    with open(path,'rb') as f:
        for b in iter(lambda:f.read(4*1024*1024),b''):h.update(b)
    return h.hexdigest()


def git(*args):return subprocess.check_output(['git',*args],cwd=ROOT)
def save(path,v):path.write_text(json.dumps(v,indent=2)+'\n')


def snapshot():
    assert not git('diff','--name-only') and not git('diff','--cached','--name-only'), 'Use a clean tracked tree for a fresh reproduction'
    files={}
    for name in git('ls-files','-z').decode().split('\0'):
        if name and (ROOT/name).is_file():files[name]=sha(ROOT/name)
    return {'branch':git('branch','--show-current').decode().strip(),
        'head':git('rev-parse','HEAD').decode().strip(),'origin':git('rev-parse','origin/development/nitride').decode().strip(),
        'staged_diff_sha256':hashlib.sha256(git('diff','--cached','--binary')).hexdigest(),
        'index_sha256':hashlib.sha256(git('diff','--cached','--binary')).hexdigest(),
        'staged_files':0,'unstaged':'','files':files}


def check():
    count=0
    for folder in SOURCES.values():
        for path in folder.glob('*.py'):ast.parse(path.read_text());count+=1
    for path in [VER/'phase4b/resumed/reference.py',VER/'phase4b/common.py',VER/'analytic.py',
                 VER/'solver_harness.cpp',VER/'run_verification.py',ROOT/'src/classes/Solver.C']:
        assert path.is_file(),path
    for name in ['cases.json','configuration.json','combinations_160.csv','execution_protocol.json']:
        assert (SOURCES['extended']/name).is_file()
    cases=json.loads((SOURCES['extended']/'cases.json').read_text());assert len(cases)==160
    for path in (SOURCES['extended']/'inputs').iterdir():assert path.is_file()
    print(json.dumps({'status':'PASS','original_top_level_scripts_checked':count,
        'frozen_cases':len(cases),'preparation_only_no_numerical_campaign_started':True},indent=2))


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--check-only',action='store_true')
    p.add_argument('--kind',choices=list(SOURCES))
    p.add_argument('--destination',help='New folder basename directly under UN_model/VERIFICATION')
    p.add_argument('--source-extended',help='Completed regenerated extended campaign basename, required for accuracy')
    args=p.parse_args();check()
    if args.check_only:return
    assert args.kind and args.destination and Path(args.destination).name==args.destination
    assert args.destination.startswith('reproduce_'+args.kind+'_'), 'Use the ignored scratch prefix'
    destination=VER/args.destination;assert not destination.exists()
    source=SOURCES[args.kind];old_snapshot=snapshot();destination.mkdir()
    for path in source.glob('*.py'):shutil.copy2(path,destination/path.name)
    if args.kind=='extended':
        for name in ['cases.json','configuration.json','combinations_160.csv','execution_protocol.json']:
            shutil.copy2(source/name,destination/name)
        shutil.copytree(source/'inputs',destination/'inputs')
        import numpy, scipy
        save(destination/'environment.json',{'python':sys.version,'platform':platform.platform(),
            'numpy':numpy.__version__,'scipy':scipy.__version__,
            'compiler':subprocess.check_output(['g++','--version'],text=True).splitlines()[0],
            'solver_sha256':sha(ROOT/'src/classes/Solver.C'),
            'source_environment_preserved_at':str((source/'environment.json').relative_to(ROOT)),
            'new_reproduction_uses_identical_frozen_cases':True})
    elif args.kind=='accuracy':
        assert args.source_extended and Path(args.source_extended).name==args.source_extended
        extended=VER/args.source_extended
        assert extended!=SOURCES['extended'], 'Original raw arrays were deliberately removed; first regenerate the extended campaign'
        assert sha(extended/'cases.json')==sha(SOURCES['extended']/'cases.json')
        assert sha(extended/'combinations_160.csv')==sha(SOURCES['extended']/'combinations_160.csv')
        for case in json.loads((extended/'cases.json').read_text()):
            manifest=json.loads((extended/'raw'/case['name']/'dt_3600/cpp_manifest.json').read_text())
            assert (ROOT/manifest['source']/'output.bin').is_file()
        # Redirect I/O in memory. Original equation/solver/helper files stay exact.
        launcher='''import sys,runpy,json\nfrom pathlib import Path\nimport core\ncore.SOURCE=Path(SOURCE_LITERAL)\ncore.CASES=json.loads((core.SOURCE/'cases.json').read_text())\ntask=sys.argv[1];sys.argv=[str(Path(__file__).parent/(task+'.py')),*sys.argv[2:]]\nrunpy.run_path(sys.argv[0],run_name='__main__')\n'''.replace('SOURCE_LITERAL',repr(str(extended)))
        (destination/'run_task.py').write_text(launcher)
    save(destination/'preservation_before.json',old_snapshot)
    now=time.time();save(destination/'budget.json',{'start_epoch':now,'limit_seconds':5400,'deadline_epoch':now+5400})
    save(destination/'reproduction_origin.json',{'kind':args.kind,'source':str(source.relative_to(ROOT)),
        'source_script_sha256':{p.name:sha(p) for p in source.glob('*.py')},
        'new_output_folder':str(destination.relative_to(ROOT)),
        'parameters_and_seed_refitted':False,'simulation_started':False,
        'session_bound_old_validators':'Historical validators include original-session no-duplicate/preservation requirements; do not treat those provenance checks as numerical failures in an intentional new reproduction.'})
    print('Prepared only:',destination)


if __name__=='__main__':main()
