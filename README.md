# Shortest Distance in Freebase Graph

## Overview
This project implements a shortest path finder for entities in the Freebase knowledge base.  
Freebase, developed by Metaweb and acquired by Google in 2010, was a large collaborative database of structured information. It stored data as triplets — `(entity1, relationship, entity2)` — enabling semantic connections between entities.  
Although Freebase was discontinued in 2015, its data remains a rich source for graph-based exploration.

In this homework, you will:
- Parse Freebase triplets.
- Build an entity relationship graph.
- Implement an algorithm to find the shortest distance between two entities (MIDs).
- Output both the distance and the full path between them.

---

## Provided Files
- **`mid2name.tsv`**  
  Maps each MID (Machine ID) to its human-readable name.  
- **`freebase.tsv`**  
Contains the relationship triplets in the form:
That you can find in the needed.zip
## To run
python runner.py
