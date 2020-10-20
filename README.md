# GWESalyzer
Genome-wide epistasis analyzer.

## Dependencies
GWESalyzer's circular plot depends on [Node.js](https://nodejs.org/en/) and a browser that support ES6 (this includes all modern browsers and the 1.2 version of the RStudio IDE).

## Installation guide
Begin by installing the `BiocManager` and `devtools` packages. Then, install `ggtree` and `treeio` with
```
BiocManager::install("ggtree")
BiocManager::install("treeio")
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
