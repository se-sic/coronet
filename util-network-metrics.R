## (c) Thomas Bock, February 2015
## bockthom@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("igraph")

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Metric functions --------------------------------------------------------


#' Determine the maximum degree for the given network.
#'
#' @param network The network to be examined
#' @param mode The mode to be used for determining the degrees.
#'
#' @return A dataframe containing the name of the vertex with with maximum degree its degree.
metrics.hub.degree = function(network, mode = c("total", "in", "out")){
    match.arg(mode)
    degrees = igraph::degree(network, mode = c(mode))
    vertex = which.max(degrees)
    df = data.frame("name" = names(vertex), "degree" = unname(degrees[vertex]))
    return(df)
}

#' Calculate the average degree of a network.
#'
#' @param network The network to be examined
#' @param mode The mode to be used for determining the degrees.
#'
#' @return The average degree of the nodes in the network.
metrics.avg.degree = function(network, mode = c("total", "in", "out")) {
    match.arg(mode)
    degrees = igraph::degree(network, mode = c(mode))
    avg = mean(degrees)
    return(avg)
}

#' Calculate all node degrees for the given network
#'
#' @param network The network to be examined
#' @param sort Whether the resulting dataframe is to be sorted by the node degree
#' @param sort.decreasing If sorting is active, this says whether the dataframe is to be sorted
#' in descending or ascending order.
#'
#' @return A dataframe containing the nodes and their respective degrees.
metrics.node.degrees = function(network, sort = TRUE, sort.decreasing = TRUE) {
    if(sort) {
        degrees = sort(igraph::degree(network, mode="total"), decreasing = sort.decreasing)
    } else {
        igraph::degree(network, mode="total")
    }
    return(data.frame("name" = names(degrees), "degree" = unname(degrees)))
}

#' Calculate the density of the given network.
#'
#' @param network The network to be examined.
#'
#' @return The density of the network.
metrics.density = function(network) {
    density = igraph::graph.density(network)
    return(density)
}

#' Calculate the average path length for the given network.
#'
#' @param network The network to be examined.
#' @param directed Wehther the given network is directed or undirected.
#' @param unconnected Whether all nodes of the network are connected.
#'
#' @return The average pathlength of the given network.
metrics.avg.pathlength = function(network, directed, unconnected) {
    avg.pathlength = igraph::average.path.length(network, directed = directed, unconnected = unconnected)
    return(avg.pathlength)
}

#' Calculate the average local clustering coefficient for the given network.
#'
#' @param network The network to be examined.
#' @param cc.type The type of cluserting coefficient to be calculated.
#'
#' @return The clustering coefficient of the network.
metrics.clustering.coeff = function(network, cc.type = c("global", "local", "barrat", "localaverage")) {
    match.arg(cc.type)
    cc = igraph::transitivity(network, type = cc.type, vids = NULL)
    return(cc)
}

#' Calculate the modularity metric for the given network.
#'
#' @param network The network to be examined
#' @param community.detection.algorithm The algorithm to be used for the detection of communities which
#' is required for the calculation of the clustering coefficient.
#'
#' @return The modularity value for the given network.
metrics.modularity = function(network, community.detection.algorithm = igraph::cluster_walktrap) {
    comm = community.detection.algorithm(network)
    mod = igraph::modularity(network, igraph::membership(comm))
    return(data.frame("name" = name, "modularity" = mod))
}

#' This function determines whether a network can be considered a
#' small-world network based on a quantitative categorical decision.
#'
#' The procedure used in this function is based on the work "Network
#' 'Small-World-Ness': A Quantitative Method for Determining Canonical
#' Network Equivalence" by Mark D. Humphries and Kevin Gurney [1].
#' [1] http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0002051
#'
#' The algorithm relies on the Erdös-Renyi random network with the same number
#' of nodes and edges as the given network.
#'
#' @param network The network to be examined. This network needs to be simplified for the calculation to work.
#'
#' @return The smallworldness value of the network.
metrics.smallworldness = function(network) {

    # construct Erdös-Renyi network with same number of nodes and edges as g
    h = igraph::erdos.renyi.game(n=igraph::vcount(network), p.or.m=igraph::gsize(network), type="gnm", directed=FALSE)

    # compute clustering coefficients
    g.cc = igraph::transitivity(network, type = 'global')
    h.cc = igraph::transitivity(h, type = 'global')
    # compute average shortest-path length
    g.l = igraph::average.path.length(network, unconnected = TRUE)
    h.l = igraph::average.path.length(h, unconnected = TRUE)

    # binary decision
    # intermediate variables
    gamma = g.cc / h.cc
    lambda = g.l / h.l

    # indicator s.delta
    s.delta = gamma / lambda

    # if s.delta > 1, then the network is a small-world network
    #is.smallworld = ifelse(s.delta > 1, TRUE, FALSE)

    return ("smallworldness" = s.delta)
}

#' Determine scale freeness of a network using the power law fitting method.
#'
#' @param network The network to be examined
#'
#' @return A dataframe containing the different values, connected to scale-freeness.
metrics.scale.freeness = function(network) {
    v.degree <- sort(igraph::degree(network, mode="total"), decreasing=TRUE)

    ## Power-law fiting
    ## (from  Mitchell Joblin <mitchell.joblin.ext@siemens.com>, Siemens AG,  2012, 2013)
    p.fit = igraph::power.law.fit(v.degree, implementation="plfit")
    param.names = c("alpha", "xmin", "KS.p")
    res = list()
    res[param.names] = p.fit[param.names]

    ## Check percent of vertices under power-law
    res$num.power.law = length(which(v.degree >= res$xmin))
    res$percent.power.law = 100 * (res$num.power.law / length(v.degree))
    df = data.frame(res$alpha,res$xmin,res$KS.p,res$num.power.law,res$percent.power.law)
    return(df)
}

#' Calculate the hierarchy for a network.
#'
#' @param network The network to be examined
#'
#' @return A dataframe containing the logarithm of the node degree and the logarithm of the local clustering coefficient for each node.
metrics.hierarchy = function(network) {
    degrees = igraph::degree(network, mode="total")
    cluster.coeff = igraph::transitivity(network, type = "local", vids = NULL)
    degrees.without.cluster.coeff = subset(degrees, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    cluster.coeff = subset(cluster.coeff, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    return(data.frame(log.deg = log(degrees.without.cluster.coeff), log.cc = log(cluster.coeff)))
}

