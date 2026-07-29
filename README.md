# GWES-Explorer
Genome-wide epistasis analyzer.

https://jurikuronen.shinyapps.io/gwes-explorer/

## Dependencies
GWES-Explorer requires a modern browser with ES6 support.

## Installation guide
Begin by installing the `BiocManager` and `devtools` packages. Then, install `ggtree` and `treeio` with
```
BiocManager::install("ggtree")
BiocManager::install("treeio")
```
and finally install `GWESExplorer` with
```
devtools::install_github("jurikuronen/GWES-Explorer")
```

## How to use
```
library(GWESExplorer)
launch_GWESExplorer()
```

By default, maximum input file size is limited to 32MB. If you are working with larger files, you can modify this limit with the `max_request_size` parameter:
```
launch_GWESExplorer(max_request_size = 256 * 1024 * 1024) # Set maximum file size limit to 256MB.
```
