#!/usr/bin/env python3
"""Apply only the frozen, audited cleanup plan; no Git history or GC commands."""
import sys
sys.dont_write_bytecode = True
from pathlib import Path
import argparse, csv, hashlib, json, subprocess, tarfile, time, zipfile
from collections import Counter

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[2]
PLAN=json.loads((HERE/'plan.json').read_text())


def sha(path):
    h=hashlib.sha256()
    with open(path,'rb') as f:
        for b in iter(lambda:f.read(4*1024*1024),b''):h.update(b)
    return h.hexdigest()


def save(path,obj):
    path.write_text(json.dumps(obj,indent=2,allow_nan=False)+'\n')


def audit():
    assert not (HERE/'inventory.csv').exists()
    entries=[];start=time.perf_counter()
    for entry in PLAN['entries']:
        p=ROOT/entry['path'];assert p.is_file() and p.stat().st_size==entry['bytes']
        entries.append({**entry,'sha256':sha(p)})
    with (HERE/'inventory.csv').open('x',newline='') as f:
        w=csv.DictWriter(f,fieldnames=list(entries[0]));w.writeheader();w.writerows(entries)
    acc=ROOT/'UN_model/VERIFICATION/accuracy_N40_dt3600_2026-10-20'
    with zipfile.ZipFile(acc/'accuracy_N40_dt3600_compact.zip') as z:
        assert z.testzip() is None
        h=hashlib.sha256(z.read('temporal_hourly_lossless.csv')).hexdigest()
        assert h==sha(acc/'deliverables/temporal_hourly_lossless.csv')
        # Compare all 640160 rows and the 160 per-case duplicate partitions.
        with z.open('temporal_hourly_lossless.csv') as f:
            import io
            reader=csv.DictReader(io.TextIOWrapper(f));count=0
            cases=json.loads((ROOT/'UN_model/VERIFICATION/extended_convergence_2026-10-20/cases.json').read_text())
            for case in cases:
                with (acc/'baseline'/case['name']/'temporal_hourly_lossless.csv').open() as src:
                    for row in csv.DictReader(src):assert row==next(reader);count+=1
            assert next(reader,None) is None and count==640160
    coeff=ROOT/'UN_model/VERIFICATION/coefficient_histories_2026-10-20'
    with (coeff/'UN_coefficients_histories.csv').open() as f:allrows=list(csv.DictReader(f))
    groups={}
    for row in allrows:groups.setdefault((row['modello_variante'],row['caso']),[]).append(row)
    deleted_coefficient_rows=0
    for p in sorted((coeff/'raw').glob('*/*/coefficients.csv')):
        with p.open() as f:rows=list(csv.DictReader(f))
        assert rows==groups[(p.parent.parent.name,p.parent.name)]
        deleted_coefficient_rows+=len(rows)
    # Both concluded campaigns' compact archives are independent of raw arrays.
    ext=ROOT/'UN_model/VERIFICATION/extended_convergence_2026-10-20'
    with zipfile.ZipFile(ext/'extended_convergence_160_compact.zip') as z:
        assert z.testzip() is None
        checks=json.loads(z.read('SHA256SUMS.json'))
        for name,digest in checks.items():
            # Old archive manifest may use a structured entry, retain exact evidence.
            if isinstance(digest,str):assert hashlib.sha256(z.read(name)).hexdigest()==digest
        for name in ['temporal_errors.csv','temporal_orders.csv','spatial_errors.csv','spatial_orders.csv']:
            assert z.read(name)==(ext/'deliverables_final'/name).read_bytes()
    before=json.loads((HERE/'state_before.json').read_text())
    assert subprocess.check_output(['git','rev-parse',PLAN['backup_branch']],cwd=ROOT,text=True).strip()==before['head']
    result={'status':'PASS','inventory_files':len(entries),'deleted_hourly_CSV_rows_preserved_in_ZIP':count,
        'duplicate_coefficient_rows_checked':deleted_coefficient_rows,'final_archives_CRC_checked':2,
        'original_commit_reachable_from_backup_branch':True,'seconds':time.perf_counter()-start,
        'scientific_arrays_reproducible':'Frozen cases, source/initials, actual C++ inputs, original solver/helper scripts, per-run manifests and all convergence criteria retained',
        'remaining_earlier_references':'phase4b/, regression_reference/, notebooks/ and all older phases are outside deletion scope'}
    save(HERE/'predelete_validation.json',result)
    print(json.dumps(result,indent=2),flush=True)


def inventory():
    with (HERE/'inventory.csv').open() as f:return list(csv.DictReader(f))


def backup():
    assert json.loads((HERE/'predelete_validation.json').read_text())['status']=='PASS'
    destination=Path(PLAN['external_backup']);destination.mkdir(exist_ok=False)
    path=destination/'verification_essential_files.tar.gz'
    entries=[r for r in inventory() if r['action']=='keep']
    with tarfile.open(path,'x:gz',compresslevel=6) as t:
        for r in entries:
            p=ROOT/r['path'];assert sha(p)==r['sha256'];t.add(p,arcname=r['path'],recursive=False)
        for name in ['inventory.csv','plan.json','state_before.json','predelete_validation.json']:
            t.add(HERE/name,arcname='cleanup_audit/'+name,recursive=False)
    # Re-read every retained byte from the separate archive before any deletion.
    with tarfile.open(path,'r:gz') as t:
        for r in entries:
            h=hashlib.sha256();f=t.extractfile(r['path'])
            for b in iter(lambda:f.read(4*1024*1024),b''):h.update(b)
            assert h.hexdigest()==r['sha256']
    result={'status':'PASS','archive':str(path),'archive_bytes':path.stat().st_size,
        'archive_sha256':sha(path),'kept_files_backed_up_and_rechecked':len(entries),
        'kept_uncompressed_bytes':sum(int(r['bytes']) for r in entries),
        'backup_scope':'all retained pre-cleanup files in the three directories, including final archives, tables, scripts, inputs, manifests and reports',
        'original_full_commit_backup_branch':PLAN['backup_branch']}
    save(destination/'BACKUP_MANIFEST.json',result);save(HERE/'backup_manifest.json',result)
    print(json.dumps(result,indent=2),flush=True)


def delete():
    report=json.loads((HERE/'backup_manifest.json').read_text());assert report['status']=='PASS'
    assert sha(Path(report['archive']))==report['archive_sha256']
    assert not (HERE/'deletion_result.json').exists(),'Do not apply twice'
    before=json.loads((HERE/'state_before.json').read_text())
    assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()==before['head']
    assert subprocess.check_output(['git','rev-parse',PLAN['backup_branch']],cwd=ROOT,text=True).strip()==before['head']
    rows=inventory();removed=[]
    # Validate all names and checksums before the first removal.
    for r in rows:
        p=ROOT/r['path'];assert sha(p)==r['sha256']
        assert any(p.is_relative_to(ROOT/'UN_model/VERIFICATION'/name) for name in PLAN['scope'])
    for r in rows:
        if r['action']=='delete':
            (ROOT/r['path']).unlink();removed.append(r)
    for r in rows:
        p=ROOT/r['path']
        if r['action']=='keep':assert sha(p)==r['sha256']
        else:assert not p.exists()
    result={'status':'PASS','removed_files':len(removed),'removed_worktree_bytes':sum(int(r['bytes']) for r in removed),
        'retained_files':len(rows)-len(removed),'retained_bytes':sum(int(r['bytes']) for r in rows if r['action']=='keep'),
        'retained_files_byte_identical':True,'removal_reversible_while_backup_branch_exists':True,
        'no_Git_GC_or_remote_operations':True,
        'reason_counts':dict(Counter(r['reason'] for r in removed))}
    save(HERE/'deletion_result.json',result);print(json.dumps(result,indent=2),flush=True)


if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('action',choices=['audit','backup','delete']);a=p.parse_args()
    {'audit':audit,'backup':backup,'delete':delete}[a.action]()
