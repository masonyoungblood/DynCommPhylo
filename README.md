# DynCommPhylo
*Phylogenetic Reconstruction via Dynamic Community Detection*

This package includes functions for converting and simplifying the output of the [TILES](https://github.com/GiulioRossetti/TILES) dynamic community detection algorithm so that it can be plotted as a phylogeny. Network simplification is conducted using the fast greedy modularity optimization algorithm.

To install the package run the following in R:

```
install.packages("devtools")
devtools::install_github("masonyoungblood/DynCommPhylo")
```

In order to use the functions included in this package, you need to have already analyzed your data with the Python implementation of [TILES](https://github.com/GiulioRossetti/TILES). The functions are currently only compatible with yearly TILES snapshots (e.g. 1970, 1971, 1972), but support for other timeframes will be added in future versions. The R workspace needs to include both the input file and a subdirectory called "output" that has the unzipped graph and strong-communities output files. A basic example of the core functions is below:

```
original_network <- network_construction(file = "edges.tsv", years = c(1970:1999))
simplified_network <- network_simplification(links = original_network$links, nodes = original_network$nodes)
plot <- phylo_plotting(links = simplified_network$links, nodes = simplified_network$nodes)
```

The plotted phylogeny ([D3.js](https://d3js.org/) sankey plot) can be saved as an interactive HTML file using [*htmlwidgets*](https://cran.r-project.org/web/packages/htmlwidgets/index.html) or as a static PNG file using [*r2d3*](https://cran.r-project.org/web/packages/r2d3/index.html).

```
htmlwidgets::saveWidget(plot, file = "phylogeny.html", title = "Phylogeny")
r2d3::save_d3_png(plot, file = "phylogeny.png", background = "white", width = 1000, height = 600, delay = 1, zoom = 10)
```

For more details about the methods in this package, check out the corresponding manuscript:

Youngblood, M., Baraghith, K., & Savage, P. E. (2020). Phylogenetic reconstruction of the cultural evolution of electronic music via dynamic community detection (1975–1999). *arXiv*: [XXX](LINK).
