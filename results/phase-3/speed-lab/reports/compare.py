import argparse, json, hashlib
from pathlib import Path

def load(path):
    rows=json.loads(Path(path).read_text())
    result={}
    for row in rows:
        p=row['params']; key=(row['benchmark'].rsplit('.',1)[-1],p['operation'],p['sourceKind'],p['size'],p['workload'],p['takeCount'])
        impl=p['implementation']
        assert (key,impl) not in result
        result[key,impl]=row
    return result

def bounds(row):
    m=row['primaryMetric']; return m['score'],m['scoreConfidence'][0],m['scoreConfidence'][1]
def compare(a,b):
    av,al,ah=bounds(a);bv,bl,bh=bounds(b)
    return av/bv,al/bh,ah/bl if bl>0 else float('inf')
def metric(row):
    s,l,h=bounds(row); return f'{s:.3f} [{l:.3f}, {h:.3f}]'

p=argparse.ArgumentParser();p.add_argument('result');p.add_argument('--prior');p.add_argument('--impl',default='candidate-direct');args=p.parse_args()
rows=load(args.result);prior=load(args.prior) if args.prior else None
print('Source:',args.result)
print('SHA256:',hashlib.sha256(Path(args.result).read_bytes()).hexdigest())
print('Intervals are conservative ratios of JMH reported confidence endpoints; they are not a separately estimated ratio confidence interval.')
print('| Cell | Core ops/s [CI] | Candidate ops/s [CI] | Candidate/core % [bounds] | Prior-candidate % [bounds] | Core drift % |')
print('|---|---:|---:|---:|---:|---:|')
for key,impl in rows:
    if impl!=args.impl: continue
    a=rows[key,impl]; b=rows[key,'core-direct']; ratios=compare(a,b)
    delta='';drift=''
    if prior and (key,'candidate-direct') in prior:
        delta='/'.join(f'{(v-1)*100:+.2f}' for v in compare(a,prior[key,'candidate-direct']))
        drift=f'{(b["primaryMetric"]["score"]/prior[key,"core-direct"]["primaryMetric"]["score"]-1)*100:+.2f}'
    print('| '+' / '.join(key[:-1])+' | '+metric(b)+' | '+metric(a)+' | '+' / '.join(f'{(v-1)*100:+.2f}' for v in ratios)+' | '+delta+' | '+drift+' |')
