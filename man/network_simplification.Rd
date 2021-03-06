% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_simplification.R
\name{network_simplification}
\alias{network_simplification}
\title{Network Simplification}
\usage{
network_simplification(links, nodes, method = "cluster_fast_greedy")
}
\arguments{
\item{links}{The links from the original network.}

\item{nodes}{The nodes from the original network.}

\item{method}{The clustering method used for network simplification (defaults to \code{cluster_fast_greedy}, but also accepts \code{cluster_louvain}, \code{cluster_edge_betweenness}, \code{cluster_walktrap}, \code{cluster_label_prop}, \code{cluster_spinglass}, \code{cluster_leading_eigen}, \code{cluster_infomap}). See igraph documentation for details.}
}
\value{
A list with links and nodes, as well as the proportion of communities included in the largest connected component and the modularity of the simplified network. The links are directed and weighted. The nodes are in the format populationID_year, where a single population can persist across multiple years (e.g. 24_1975 -> 24_1976 -> 24_1977).
}
\description{
Extract the largest connected component of the original network and simplify it using the fast greedy modularity optimization algorithm into a network where each node is a population and each link is the number of individuals moving between populations.
}
