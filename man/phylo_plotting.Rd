% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phylo_plotting.R
\name{phylo_plotting}
\alias{phylo_plotting}
\title{Phylogeny Plotting}
\usage{
phylo_plotting(links, nodes, captions = NULL, iterations = 25, nodePadding = 3)
}
\arguments{
\item{links}{The links from the original or simplified network.}

\item{nodes}{The nodes from the original or simplified network.}

\item{captions}{The captions that will appear when you hover over each node in the phylogeny.}

\item{iterations}{The number of iterations used to optimize the sankey plot (default is 25).}

\item{nodePadding}{The vertical padding between nodes in the sankey plot (default is 3).}
}
\value{
A D3.js sankey plot that can be saved as an interactive HTML file using \emph{htmlwidgets} or a static PNG file using \emph{r2d3}.
}
\description{
Plot a phylogeny based on either the original or simplified network.
}
