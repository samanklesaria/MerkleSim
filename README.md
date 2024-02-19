The code in this repository allowed for the experiments described in [this report](https://samanklesaria.github.io/assets/pdfs/patricia.pdf), originally for the
graduate Distributed Computing class at UT Austin. The `report` subdirectory contains the Pandoc-flavored markdown source for this report. Each CRDT reconsiliation algorithm described
in the report (Causal Dags, Patricia Chains, and Merkle Trees) has a separate module within `src`, along with an associated set of tests. You can build each of these libraries, as well as
the main executable that compares their performance, using the standard Cabal build system; the makefile is just for profiling. 
