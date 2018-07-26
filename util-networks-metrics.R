## This file is part of codeface-extraction-r, which is free software: you
## can redistribute it and/or modify it under the terms of the GNU General
## Public License as published by  the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
## Copyright 2015 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Barbara Eckl <ecklbarb@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("igraph")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Metric functions --------------------------------------------------------

#' Determine the maximum degree for the given network.
#'
#' @param network the network to be examined
#' @param mode the mode to be used for determining the degrees
#'
#' @return A dataframe containing the name of the vertex with with maximum degree its degree.
metrics.hub.degree = function(network, mode = c("total", "in", "out")) {
    mode = match.arg(mode)
    degrees = igraph::degree(network, mode = c(mode))
    vertex = which.max(degrees)
    df = data.frame("name" = names(vertex), "degree" = unname(degrees[vertex]))
    return(df)
}

#' Calculate the average degree of a network.
#'
#' @param network the network to be examined
#' @param mode the mode to be used for determining the degrees
#'
#' @return The average degree of the nodes in the network.
metrics.avg.degree = function(network, mode = c("total", "in", "out")) {
    mode = match.arg(mode)
    degrees = igraph::degree(network, mode = c(mode))
    avg = mean(degrees)
    return(c(avg.degree = avg))
}

#' Calculate all node degrees for the given network
#'
#' @param network the network to be examined
#' @param sort whether the resulting dataframe is to be sorted by the node degree
#' @param sort.decreasing if sorting is active, this says whether the dataframe is to be
#'            sorted in descending or ascending order
#'
#' @return A dataframe containing the nodes and their respective degrees.
metrics.node.degrees = function(network, sort = TRUE, sort.decreasing = TRUE) {
    if (sort) {
        degrees = sort(igraph::degree(network, mode = "total"), decreasing = sort.decreasing)
    } else {
        degrees = igraph::degree(network, mode = "total")
    }
    return(data.frame("name" = names(degrees), "degree" = unname(degrees)))
}

#' Calculate the density of the given network.
#'
#' @param network the network to be examined
#'
#' @return The density of the network.
metrics.density = function(network) {
    density = igraph::graph.density(network)
    return(c(density = density))
}

#' Calculate the average path length for the given network.
#'
#' @param network the network to be examined
#' @param directed whether to consider directed paths in directed networks
#' @param unconnected whether all nodes of the network are connected
#'
#' @return The average pathlength of the given network.
metrics.avg.pathlength = function(network, directed, unconnected) {
    avg.pathlength = igraph::average.path.length(network, directed = directed, unconnected = unconnected)
    return(c(avg.pathlength = avg.pathlength))
}

#' Calculate the average local clustering coefficient for the given network.
#'
#' @param network the network to be examined
#' @param cc.type the type of cluserting coefficient to be calculated
#'
#' @return The clustering coefficient of the network.
metrics.clustering.coeff = function(network, cc.type = c("global", "local", "barrat", "localaverage")) {
    cc.type = match.arg(cc.type)
    cc = igraph::transitivity(network, type = cc.type, vids = NULL)
    return(c(clustering = cc))
}

#' Calculate the modularity metric for the given network.
#'
#' @param network the network to be examined
#' @param community.detection.algorithm the algorithm to be used for the detection of communities
#'            which is required for the calculation of the clustering coefficient
#'
#' @return The modularity value for the given network.
metrics.modularity = function(network, community.detection.algorithm = igraph::cluster_walktrap) {
    comm = community.detection.algorithm(network)
    mod = igraph::modularity(network, igraph::membership(comm))
    return(c(modularity = mod))
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
#' To check the result value \code{s.delta} for a binary (true/false) decision on smallworldness,
#' do this: \code{is.smallworld = s.delta > 1}.
#'
#' Important: The given network needs to be simplified for the calculation to work!
#'
#' @param network the simplified network to be examined
#'
#' @return The smallworldness value of the network.
metrics.smallworldness = function(network) {
    # construct Erdös-Renyi network with same number of nodes and edges as g
    h = igraph::erdos.renyi.game(n = igraph::vcount(network),
                                 p.or.m = igraph::ecount(network),
                                 type = "gnm",
                                 directed = FALSE)

    # compute clustering coefficients
    g.cc = igraph::transitivity(network, type = "global")
    h.cc = igraph::transitivity(h, type = "global")
    # compute average shortest-path length
    g.l = igraph::average.path.length(network, unconnected = TRUE)
    h.l = igraph::average.path.length(h, unconnected = TRUE)

    # binary decision
    # intermediate variables
    gamma = g.cc / h.cc
    lambda = g.l / h.l

    # indicator s.delta
    s.delta = gamma / lambda

    ## if s.delta > 1, then the network is a small-world network
    # is.smallworld = s.delta > 1
    return (c(smallworldness = s.delta))
}

#' Determine scale freeness of a network using the power law fitting method.
#'
#' @param network the network to be examined
#' @param minimum.number.vertices the minimum number of vertices with which
#'  a network can be scale free [default: 30]
#'
#' @return A dataframe containing the different values, connected to scale-freeness.
metrics.scale.freeness = function(network, minimum.number.vertices = 30) {
    v.degree = sort(igraph::degree(network, mode = "total"), decreasing = TRUE)

    ## Power-law fiting
    ## (by  Mitchell Joblin <mitchell.joblin.ext@siemens.com>, Siemens AG,  2012, 2013)
    p.fit = igraph::power.law.fit(v.degree, implementation = "plfit")
    param.names = c("alpha", "xmin", "KS.p")
    res = list()
    res[param.names] = p.fit[param.names]

    ## Check percent of vertices under power-law
    res["num.power.law"] = length(which(v.degree >= res[["xmin"]]))
    res["percent.power.law"] = 100 * (res[["num.power.law"]] / length(v.degree))

    ## If less than minimum.number.vertices vertices are in the power law, set x_min manually
    ## to include a minimum of number of vertices and recompute the powerlaw fit
    non.zero.degree.v.count = length(v.degree[v.degree > 0])
    if(res[["num.power.law"]] < minimum.number.vertices
       & non.zero.degree.v.count >= minimum.number.vertices) {
        ## vertex degree is sorted above
        x.min = v.degree[[minimum.number.vertices]]
        p.fit = power.law.fit(v.degree, implementation = "plfit", xmin = x.min)
        res[param.names] = p.fit[param.names]

        ## Check percent of vertices under power-law
        res[["num.power.law"]] = length(which(v.degree >= res[["xmin"]]))
        res[["percent.power.law"]] = 100 * (res[["num.power.law"]] / length(v.degree))
    }

    ## Remove non conclusive sample sizes
    if(res[["num.power.law"]] < minimum.number.vertices) {
        res[["KS.p"]] = 0 # 0 instead of NA
    }

    df = as.data.frame(res, row.names = "scale.freeness")
    return(df)
}

#' Decide, whether a network is scale free or not.
#'
#' @param network the network to be examined
#' @param minimum.number.vertices the minimum number of vertices with which
#'  a network can be scale free [default: 30]
#'
#' @return TRUE, if the network is scale free,
#'         FALSE, otherwise.
metrics.is.scale.free = function(network, minimum.number.vertices = 30) {
    df = metrics.scale.freeness(network, minimum.number.vertices)
    return(df[["KS.p"]] >= 0.05)
}

#' Calculate the hierarchy for a network.
#'
#' @param network the network to be examined
#'
#' @return A dataframe containing the logarithm of the node degree and the logarithm
#' of the local clustering coefficient for each node as well as the non-logarithmic values
#' for these.
metrics.hierarchy = function(network) {
    degrees = igraph::degree(network, mode = "total")
    cluster.coeff = igraph::transitivity(network, type = "local", vids = NULL)
    degrees.without.cluster.coeff = subset(degrees, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    cluster.coeff = subset(cluster.coeff, !(is.nan(cluster.coeff) | cluster.coeff == 0))
    return(data.frame(deg = degrees.without.cluster.coeff, cc = cluster.coeff,
                      log.deg = log(degrees.without.cluster.coeff), log.cc = log(cluster.coeff)))
}


