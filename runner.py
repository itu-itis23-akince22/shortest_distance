import random
import os
random.seed(32) 

def load_mid2name_no_duplicates(filename):
    mid2name = {}
    with open(filename, encoding='utf-8') as f:
        for line in f:
            mid, name = line.strip().split('\t')
            if mid not in mid2name:
                mid2name[mid] = name
    return mid2name

mid2name = load_mid2name_no_duplicates("mid2name.tsv")
mids = list(mid2name.keys())
sample_size = min(5, len(mids)//2)
mids = random.sample(mids, sample_size * 2)

os.system("ghc mainV2.hs -package text -o pathfinder")
print("Compiled Haskell code to pathfinder executable.")
for i in range(sample_size):
    source = mids[i]
    target = mids[i + sample_size]
    print(f"Finding path from {mid2name[source]} to {mid2name[target]}...",flush=True)
    os.system(f"pathfinder {source} {target}")
