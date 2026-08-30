# GWES-Explorer

GWES-Explorer is an interactive browser-based tool for exploring genome-wide epistasis study results.
It provides three visualizations:

- GWES Manhattan plot
- Tree-MSA plot
- Circular plot

The GWES Manhattan plot allows the user to examine the signal peak regions. The Tree-MSA plot provides information
about the population allele distribution at the related SNP loci together with optional phenotype data. The circular
plot uses genomic annotations from a GFF3 file and allows the user to explore interactions at the gene level. All three
plots update in response to the selected interaction.

[Open GWES-Explorer](https://jurikuronen.shinyapps.io/gwes-explorer/)

## Requirements

GWES-Explorer requires R 4.0.0 or later and a modern web browser.

## Installation

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

Optionally, install `svglite` to enable SVG downloads:

```r
install.packages("svglite")
```

## Run GWES-Explorer

```r
library(GWESExplorer)
launch_GWESExplorer()
```

The maximum web request size is 32 MiB by default, which limits file uploads. Increase it with `max_request_size`:

```r
launch_GWESExplorer(max_request_size = 256 * 1024 * 1024) # Allow uploads up to 256 MiB.
```

The "Upload data" tab describes the required input formats. An example dataset is also included.
