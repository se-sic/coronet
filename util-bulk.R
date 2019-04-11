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
## Copyright 2016-2017 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Multi networks ----------------------------------------------------------

#' Collect the multi networks of the different Codeface ranges.
#'
#' @param project.conf the project configuration
#' @param network.conf the network configuration
#' @param step the step size of which ranges get processed (i.e. 2 means every second range)
#'
#' @return the multi networks
collect.multi.networks = function(project.conf, network.conf, step = 1) {
    ## we need to iterate over all ranges
    ranges = project.conf$get.value("ranges")
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = RangeData$new(project.conf, range)

        ## construct network builder
        network.builder <- NetworkBuilder$new(range.data, network.conf)

        ## get the multi network
        multi.network = network.builder$get.multi.network()

        ## set range attribute
        multi.network = igraph::set.graph.attribute(multi.network, "range", range)
        attr(multi.network, "range") = range

        # add to global list
        return(multi.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bipartite networks ------------------------------------------------------

#' Collect the bipartite networks of the different Codeface ranges.
#'
#' @param project.conf the project configuration
#' @param network.conf the network configuration
#' @param step the step size of which ranges get processed (i.e. 2 means every second range)
#'
#' @return the bipartite networks
collect.bipartite.networks = function(project.conf, network.conf, step = 1) {
    ## we need to iterate over all ranges
    ranges = project.conf$get.value("ranges")
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = RangeData$new(project.conf, range)

        ## construct network builder
        network.builder <- NetworkBuilder$new(range.data, network.conf)

        ## get the bipartite network
        bp.network = network.builder$get.bipartite.network()

        ## set range attribute
        bp.network = igraph::set.graph.attribute(bp.network, "range", range)
        attr(bp.network, "range") = range

        # add to global list
        return(bp.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author networks ---------------------------------------------------------

#' Collect the author networks of the different Codeface ranges.
#'
#' @param project.conf the project configuration
#' @param network.conf the network configuration
#' @param step the step size of which ranges get processed (i.e. 2 means every second range)
#'
#' @return the author networks
collect.author.networks = function(project.conf, network.conf, step = 1) {
    ## we need to iterate over all ranges
    ranges = project.conf$get.value("ranges")
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = RangeData$new(project.conf, range)

        ## construct network builder
        network.builder <- NetworkBuilder$new(range.data, network.conf)

        ## get the author network
        author.network = network.builder$get.author.network()

        ## set range attribute
        author.network = igraph::set.graph.attribute(author.network, "range", range)
        attr(author.network, "range") = range

        # add to global list
        return(author.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Artifact networks -------------------------------------------------------

#' Collect the artifact networks of the different Codeface ranges.
#'
#' @param project.conf the project configuration
#' @param network.conf the network configuration
#' @param step the step size of which ranges get processed (i.e. 2 means every second range)
#'
#' @return the artifact networks
collect.artifact.networks = function(project.conf, network.conf, step = 1) {
    ## we need to iterate over all ranges
    ranges = project.conf$get.value("ranges")
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    networks = lapply(ranges, function(range) {
        ## construct range data
        range.data = RangeData$new(project.conf, range)

        ## construct network builder
        network.builder <- NetworkBuilder$new(range.data, network.conf)

        ## get the artifact network
        artifact.network = network.builder$get.artifact.network()

        ## set range attribute
        artifact.network = igraph::set.graph.attribute(artifact.network, "range", range)
        attr(artifact.network, "range") = range

        # add to global list
        return(artifact.network)
    })

    ## set names of list to range values
    names(networks) = ranges

    return(networks)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Construct data ----------------------------------------------------------

#' Construct the range data for the Codeface ranges.
#'
#' @param project.conf the project configuration
#' @param callgraphs whether or not callgraph data is existing
#' @param step the step size of which ranges get processed (i.e. 2 means every second range)
#'
#' @return the constructed data
construct.data = function(project.conf, callgraphs = FALSE, step = 1) {
    ## we need to iterate over all ranges
    ranges = project.conf$get.value("ranges")
    ## subset according to given step size
    ranges = ranges[seq(1, length(ranges), step)]

    ## collect the network objects for all the ranges
    data = lapply(ranges, function(range) {
        revision.callgraph = ifelse(callgraphs,
                                    project.conf$get.callgraph.revision.from.range(range),
                                    "")

        ## construct range data
        range.data = RangeData$new(project.conf, range, revision.callgraph)
        attr(range.data, "range") = range

        # add to global list
        return(range.data)
    })

    ## set names of list to range values
    names(data) = ranges

    return(data)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Run function on list of RangeData ---------------------------------------

#' Run a given function on the list of RangeData.
#'
#' @param data the given list of RangeData
#' @param fun the function to be run
#'
#' @return the result of the function
run.lapply = function(data, fun) {
    res = parallel::mclapply(data, function(dat) dat[[fun]]())
    return(res)
}
