"""Idempotent official GEO download + exact GSM/column mapping. Python stdlib only."""
from pathlib import Path
import csv, gzip, hashlib, json, re, sys, urllib.request
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[1]
BASE = ROOT / 'data/public/GSE208041'
RAW = BASE / 'raw'
RAW.mkdir(parents=True, exist_ok=True)
URLS = {
    'counts.txt.gz': 'https://ftp.ncbi.nlm.nih.gov/geo/series/GSE208nnn/GSE208041/suppl/GSE208041_Thp1_raw_rna_counts.txt.gz',
    'family.soft.gz': 'https://ftp.ncbi.nlm.nih.gov/geo/series/GSE208nnn/GSE208041/soft/GSE208041_family.soft.gz',
}
def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def main():
    events = []
    old = json.loads((BASE/'manifest.json').read_text()) if (BASE/'manifest.json').exists() else {}
    for name, url in URLS.items():
        dest = RAW/name
        action = 'verified_existing'
        if not dest.exists():
            part = dest.with_suffix(dest.suffix + '.part')
            with urllib.request.urlopen(url, timeout=60) as response, part.open('wb') as out:
                while block := response.read(1024*1024): out.write(block)
            part.replace(dest)
            action = 'downloaded'
        digest = sha(dest)
        expected = {'counts.txt.gz':'d662465fefd0f9657edc24c6e3d6b79d63a4354e37579b0bc4db33fbe7cecf01','family.soft.gz':'84102e7fd824ead3620a3ed3a3485287d875d83d18f35c1c30cdb2e291724fd0'}
        assert digest == expected[name], 'Official source changed; stop instead of accepting a new dataset'
        prior = {x['path']: x for x in old.get('files', [])}.get(str(dest.relative_to(ROOT)).replace('\\','/'))
        if prior and prior['sha256'] != digest:
            raise ValueError(f'Existing raw file changed: {dest}; restore the recorded original; do not overwrite.')
        events.append({'path':str(dest.relative_to(ROOT)).replace('\\','/'), 'url':url,
                       'bytes':dest.stat().st_size, 'sha256':digest, 'action':action})
    soft = gzip.open(RAW/'family.soft.gz', 'rt', encoding='utf-8').read()
    mapping = {}
    for block in soft.split('^SAMPLE = ')[1:]:
        gsm = block.splitlines()[0].strip()
        def field(key):
            return re.findall(r'^!Sample_'+key+r' = (.*)$', block, re.M)
        desc = field('description')
        if desc and desc[0].startswith('RNAseq/'):
            treatment = next(x.split(': ',1)[1] for x in field('characteristics_ch1') if x.startswith('treatment:'))
            assert 'raw count matrix' in '\n'.join(field('data_processing'))
            assert desc[0] not in mapping
            mapping[desc[0]] = {'gsm':gsm,'title':field('title')[0], 'treatment':treatment,
                               'group': {'Vehicle':'C','LPS':'D','LPS+Dex':'T'}.get(treatment)}
    with gzip.open(RAW/'counts.txt.gz','rt',encoding='utf-8') as handle:
        rows = list(csv.reader(handle,delimiter='\t'))
    names = rows[0][1:]
    assert len(names)==12 and len(set(names))==12 and set(names)==set(mapping), 'GSM/columns do not match'
    ids = [r[0] for r in rows[1:]]
    assert len(set(ids))==len(ids) and all(ids), 'Duplicate/empty gene IDs'
    counts = []
    for r in rows[1:]:
        assert len(r)==13, 'Ragged count table'
        vals = [int(x) for x in r[1:]]
        assert all(str(v)==s and v>=0 for v,s in zip(vals,r[1:])), 'Not nonnegative integer raw counts'
        counts.append(vals)
    libs = [sum(r[j] for r in counts) for j in range(12)]
    assert min(libs)>0
    # Columns are renamed only through the explicit one-to-one mapping; every raw row is retained.
    with (BASE/'counts.tsv').open('w',newline='',encoding='utf-8') as handle:
        w=csv.writer(handle,delimiter='\t',lineterminator='\n')
        w.writerow(['gene_id']+[mapping[n]['gsm'] for n in names]); w.writerows(rows[1:])
    with (BASE/'counts.tsv').open(encoding='utf-8') as handle:
        assert list(csv.reader(handle,delimiter='\t'))[1:]==rows[1:], 'Adapted counts differ from raw rows'
    with (BASE/'samples.tsv').open('w',newline='',encoding='utf-8') as handle:
        w=csv.writer(handle,delimiter='\t',lineterminator='\n')
        w.writerow(['sample_id','group','role','biological_replicate','data_scale','independent_biological_replicate'])
        for n in names:
            m=mapping[n]; assert m['group'] is not None, m
            w.writerow([m['gsm'],m['group'],m['group'],n.rsplit('rep',1)[1],'raw_counts','true'])
    with (BASE/'sample_mapping.tsv').open('w',newline='',encoding='utf-8') as handle:
        w=csv.writer(handle,delimiter='\t',lineterminator='\n')
        w.writerow(['original_column','sample_id','GEO_title','treatment','group','source_url'])
        for n in names:
            m=mapping[n]; w.writerow([n,m['gsm'],m['title'],m['treatment'],m['group'],
                                     'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc='+m['gsm']])
    for name in ['counts.tsv','samples.tsv','sample_mapping.tsv']:
        f=BASE/name; events.append({'path':str(f.relative_to(ROOT)).replace('\\','/'),'bytes':f.stat().st_size,'sha256':sha(f)})
    manifest={'status':'DOWNLOADED_AND_ADAPTED', 'retrieved_utc':old.get('retrieved_utc',datetime.now(timezone.utc).isoformat()),
              'last_verified_utc':datetime.now(timezone.utc).isoformat(), 'command':'python scripts/fetch_release_public.py',
              'exit_code':0,'python_version':sys.version,'seed':None,'files':events,
              'validation':{'genes':len(ids),'samples':12,'groups':{'C':4,'D':4,'T':4},'nonnegative_integer_counts':True,
                            'missing_values':0,'duplicate_genes':0,'duplicate_samples':0,'library_sizes':dict(zip(names,libs)),
                            'GSM_one_to_one':True, 'raw_rows_preserved':True,
                            'design':'independent groups; batch not supplied by repository'},
              'scope':'Metadata-supported prototype case; not a ground-truth benchmark or clinical efficacy test'}
    (BASE/'manifest.json').write_text(json.dumps(manifest,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
    print(json.dumps({'status':manifest['status'],'genes':len(ids),'samples':12,'raw_counts_sha256':events[0]['sha256']}))

if __name__=='__main__':
    try: main()
    except Exception as exc:
        (BASE/'download_failure.json').write_text(json.dumps({'status':'FAILED','exit_code':1,'error':str(exc)},indent=2),encoding='utf-8')
        raise
