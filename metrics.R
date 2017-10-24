requireNamespace("igraph")


#' Determine the maximum degree for the given network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the name of the vertex with the maximum degree, the degree and
#' the name of the network that this value belongs to.
metrics.hub.degree = function(network, name){
    degrees = igraph::degree(network, mode = c("total"))
    vertex = which.max(degrees)
    df = data.frame("name" = names(vertex), "degree" = unname(degrees[vertex]), "name" = name)
    return(df)
}

#' Calculate the average degree of a network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the average degree of the network and the name of the network.
metrics.avg.degree = function(network, name) {
    degrees = igraph::degree(network, mode = c("total"))
    avg = mean(degrees)
    df = data.frame("name" = name, "avg.degree" = avg)
    return(df)
}

#' Calculate all node degrees for the given network
#'
#' @param network The network to be examined
#'
#' @return A dataframe containing the nodes and their respective degrees.
metrics.node.degrees = function(network) {
    degrees = sort(igraph::degree(network, mode="total"), decreasing = TRUE)
    return(data.frame("name" = names(degrees), "degree" = unname(degrees)))
}

#' Calculate the density of the given network
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the network density and the name of the network.
metrics.density = function(network, name) {
    density = igraph::graph.density(network)
    return(data.frame("name" = name, "density" = unname(density)))
}

#' Calculate the average path length for the given network.
#'
#' @param network The network to e examined
#' @param name The name of the network
#'
#' @return A dataframe containing the average path length and the name of the network.
metrics.avg.pathlength = function(network, name) {
    return(data.frame("name" = name, "avg.pathlength" =
                          igraph::average.path.length(network, directed = FALSE, unconnected = TRUE)))
}

#' Calculate the average local clustering coefficient for the given network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the average local clustering coefficient and the name of the network.
metrics.clustering.coeff = function(network, name) {
    cc = igraph::transitivity(network, type = "localaverage", vids = NULL)
    return(data.frame("name" = name, "clustering.coeff" = cc))
}

#' Calculate the global clustering coefficient for the given network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the global clustering coefficient of the network and the name of the network.
metrics.clustering.coeff.global = function(network, name) {
    cc = igraph::transitivity(network, type = "global", vids = NULL)
    return(data.frame("name" = name, "clustering.coeff" = cc))
}

#' Calculate the modularity metric for the given network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the modularity value for the given network and the name of the network.
metrics.modularity = function(network, name) {
    comm = igraph::cluster_walktrap(network)
    mod = igraph::modularity(network, igraph::membership(comm))
    return(data.frame("name" = name, "modularity" = mod))
}

#' Count the number of nodes for the given network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the number of nodes in the network and the name of the network.
metrics.amount.nodes = function(network, name) {
    return(data.frame("name" = name, "amount.nodes" = igraph::vcount(network)))
}

#' Calculate the smallworldness value for the given network.
#' This metric requires a simplified network.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the smallworldness value of the network and the name of the network.
metrics.smallworldness = function(network, name) {

    # construct Erdös-Renyi network with same number of nodes and edges as g
    h = igraph::erdos.renyi.game(n=igraph::vcount(network), p.or.m=igraph::gsize(network), type="gnm", directed=FALSE)

    ## compute clustering coefficients
    g.cc = igraph::transitivity(network)
    h.cc = igraph::transitivity(h)
    ## compute average shortest-path length
    g.l = igraph::average.path.length(network, unconnected = TRUE)
    h.l = igraph::average.path.length(h, unconnected = TRUE)

    ## binary decision
    # intermediate variables
    gamma = g.cc / h.cc
    lambda = g.l / h.l

    # indicator s.delta
    s.delta = gamma / lambda

    # if s.delta > 1, then the network is a small-world network
    #is.smallworld = ifelse(s.delta > 1, TRUE, FALSE)

    return (data.frame("name" = name, "smallworldness" = s.delta))
}

#' Determine scale freeness of a network using the power law fitting method.
#'
#' @param network The network to be examined
#' @param name The name of the network
#'
#' @return A dataframe containing the scale freeness value of the network and the name of the network.
metrics.scale.freeness = function(network, name) {
    v.degree <- sort(igraph::degree(network, mode="all"), decreasing=TRUE)

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
    return(data.frame("name" = name, "KS.p" = res$KS.p))
}

#' Calculate the hierarchy for a network
#'
#' @param network The network to be examined
#'
#' @return A dataframe containing the logarithm of the node degree and the logarithm of the local clustering coefficient for each node.
metrics.hierarchy = function(network) {
    degrees = igraph::degree(network, mode="total")
    cluster.coeff = igraph::transitivity(network, type = "local", vids = NULL)
    degrees.without.cc = subset(degrees, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    cluster.coeff = subset(cluster.coeff, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    return(data.frame(log.deg = log(degrees.without.cc), log.cc = log(cluster.coeff)))
}

