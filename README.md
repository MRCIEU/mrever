# mrever: R package to generate and query MR-EvE Neo4j database

Neo4j database is available here: http://shark.epi.bris.ac.uk:8474/browser/

Two roles:

## 1. A wrapper for querying the database

```
query.r
mr.r
zzz.r
```

## 2. Functions for generating the MR-EvE surface

Ported over from the `mr-eve` repository, which included a package called `makemrever`.

```
determine_analyses.r
read.r
setup.r
neo4j.r
```


## To do

- Migrate from `RNeo4j` to `neo4r`: https://github.com/neo4j-rstats/neo4r
- Tests are failing
- Moved from Depends to Imports for all packages - check bindings
- Add tests for functions migrated from `makemrever`
