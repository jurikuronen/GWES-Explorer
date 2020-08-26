# GWESalyzer
Genome-wide epistasis analyzer.

## Dependencies
GWESalyzer's circular plot depends on [Node.js](https://nodejs.org/en/).

## Installation guide
Begin by installing the `BiocManager` and `devtools` packages. Then, install `ggtree` with
```
BiocManager::install("ggtree")
```
and finally install `GWESalyzer` with
```
devtools::install_github("jurikuronen/GWESalyzer")
```

## How to use
```
library(GWESalyzer)

launch_GWESalyzer()
```
