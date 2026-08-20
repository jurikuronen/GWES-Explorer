# GWES-Explorer

An interactive browser-based visualization tool for exploring genome-wide epistasis study results.

[Open GWES-Explorer](https://jurikuronen.shinyapps.io/gwes-explorer/)

## Dependencies

GWES-Explorer requires R 4.0.0 or later and a modern web browser.

## Installation guide

Install the `BiocManager` and `devtools` packages:

```r
install.packages(c("BiocManager", "devtools"))
```

Then, install `ggtree` and `treeio`:

```r
BiocManager::install(c("ggtree", "treeio"))
```

Finally, install `GWESExplorer`:

```r
devtools::install_github("jurikuronen/GWES-Explorer")
```

## How to use

```r
library(GWESExplorer)
launch_GWESExplorer()
```

By default, the maximum upload size is 32 MiB. For larger files, change the limit with the `max_request_size`
parameter:

```r
launch_GWESExplorer(max_request_size = 256 * 1024 * 1024) # Allow uploads up to 256 MiB.
```
