## This file is part of coronet, which is free software: you
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
## Copyright 2017-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2024-2025 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.

context("Splitting functionality, time-based splitting of networks.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split network -----------------------------------------------------------

## * helper functions ------------------------------------------------------

#' Construct a (sub-)network with the same vertices and new edges
#'
#' Note: This method is an adoption of \code{split.network.by.bins} with less functionality.
#'
#' @param network The network to extract the vertices from
#' @param edges The edges to add to the network [default: NULL]
#' @param remove.isolates Whether to remove isolated vertices [default: TRUE]
#'
#' @return The partial edge
#'
#' @seealso \code{split.network.by.bins}
subnet.with.new.edges = function(network, edges = NULL, remove.isolates = TRUE) {

    ## return empty graph if no edges
    if (is.null(edges)) {
        return(igraph::subgraph_from_edges(network, c(), delete.vertices = remove.isolates))
    }

    ## ensure that edges are in the correct format
    if (!is.data.frame(edges)) {
        edges = as.data.frame(edges)
    }

    ## extract vertices from network and bring edges in correct format
    vertices = igraph::subgraph_from_edges(network, c(), delete.vertices = FALSE)
    edge.vertices = as.vector(rbind(edges[["from"]], edges[["to"]]))
    edge.attributes = edges[igraph::edge_attr_names(network)]

    ## construct subnet
    subnet = igraph::add_edges(vertices, edge.vertices, attr = edge.attributes)

    ## remove isolates if requested
    if (remove.isolates) {
        subnet = igraph::delete_vertices(subnet, which(igraph::degree(subnet) == 0))
    }

    return(subnet)
}

#' Extract a partial edge from a simplified edge with multiple components
#'
#' Note: This method is an adoption of \code{split.network.by.bins} with less functionality.
#'
#' @param edge The edge to extract the partial edge from
#' @param partials The indicess of the partial edge to extract
#'
#' @return The partial edge
#'
#' @seealso \code{split.network.by.bins}
extract.partial.edge = function(edge, partials) {

    ## start with a copy of the edge
    partial.edge = edge

    ## extract all edge attributes and build new edge
    for (attr in names(edge)) {
        attr.values = partial.edge[[attr]]

        ## if attribute is a list, extract only the values that belong to the current bin
        ## else, the attribute is a single value and does not need to be adjusted
        if (is.list(attr.values)) {
            partial.edge[[attr]][[1]] = attr.values[[1]][partials]
        }

        ## assume equal distribution of weights across partials
        else if (attr == "weight") {
            partial.edge[[attr]] = length(partials)
        }
    }

    return(partial.edge)
}

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.network.time.based(..., time.period = ...)
##

patrick::with_parameters_test_that("Split a network time-based (time.period = ...).", {

    ## time period
    time.period = "2 mins"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ##
    ## simplify = FALSE
    ##

    ## retrieve author network
    author.net = net.builder$get.author.network()

    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = igraph::subgraph_from_edges(author.net, c(1:2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = igraph::subgraph_from_edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, time.period = "2 mins")

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

    ## check bins
    expected.bins = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:59",
                                           "2016-07-12 16:02:59", "2016-07-12 16:04:59",
                                           "2016-07-12 16:06:33"))
    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

    ##
    ## simplify = TRUE
    ##

    ## update network configuration
    net.builder$update.network.conf(list(author.relation = "cochange", simplify = TRUE))
    net.builder$reset.environment()

    ## retrieve author network
    author.net = net.builder$get.author.network()

    edges = igraph::as_data_frame(author.net, "edges")
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = subnet.with.new.edges(author.net, edges[1, ]),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = subnet.with.new.edges(author.net, rbind(edges[2, ],
                                                                                            edges[3, ],
                                                                                            edges[4, ]))
    )
    results = split.network.time.based(author.net, time.period = "2 mins")

    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

##
## Tests for split.networks.time.based(..., time.period = ...)
##

patrick::with_parameters_test_that("Split a list of networks time-based, ", {

    ## time period
    time.period = "2 years"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(simplify = FALSE, author.directed = TRUE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## obtain networks:
    ## 1) co-change network
    net.builder$update.network.conf(list(author.relation = "cochange"))
    net.cochange = net.builder$get.author.network()
    ## 2) mail network
    net.builder$update.network.conf(list(author.relation = "mail"))
    net.mail = net.builder$get.author.network()

    ## split networks
    net.split = split.networks.time.based(
        networks = list(net.cochange, net.mail),
        time.period = time.period,
        sliding.window = test.sliding.window
    )

    ## check whether the splitting information of the two split networks are identical
    expect_identical(attributes(net.split[[1]]), attributes(net.split[[2]]), info = "Splitting information.")

    ## check whether this works also with one network in the list (if not, an error will occur)
    net.split = split.networks.time.based(
        networks = list(net.mail),
        time.period = time.period,
        sliding.window = test.sliding.window
    )

}, cases.cross.product(
    patrick::cases(
        "sliding window: FALSE" = list(test.sliding.window = FALSE),
        "sliding window: TRUE" = list(test.sliding.window = TRUE)
    ),
    patrick::cases(
        "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
        "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
    )
))

## * * time period, sliding windows ----------------------------------------

##
## Tests for split.network.time.based(..., time.period = ...) using sliding windows
##

patrick::with_parameters_test_that("Split a network time-based (time.period = ... , sliding.window = TRUE).", {

    ## time period
    time.period = "2 mins"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ##
    ## simplify = FALSE
    ##

    ## retrieve author network
    author.net = net.builder$get.author.network()

    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = igraph::subgraph_from_edges(author.net, c(1:2)),
        "2016-07-12 15:59:59-2016-07-12 16:01:59" = igraph::subgraph_from_edges(author.net, c(2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:01:59-2016-07-12 16:03:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:03:59-2016-07-12 16:05:59" = igraph::subgraph_from_edges(author.net, c(3,5)),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = igraph::subgraph_from_edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, time.period = "2 mins", sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

    ## check bins
    expected.bins = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 15:59:59",
                                           "2016-07-12 16:00:59", "2016-07-12 16:01:59",
                                           "2016-07-12 16:02:59", "2016-07-12 16:03:59",
                                           "2016-07-12 16:04:59", "2016-07-12 16:05:59",
                                           "2016-07-12 16:06:33"))
    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

    ##
    ## simplify = TRUE
    ##

    ## update network configuration
    net.builder$update.network.conf(list(author.relation = "cochange", simplify = TRUE))
    net.builder$reset.environment()

    ## retrieve author network
    author.net = net.builder$get.author.network()

    edges = igraph::as_data_frame(author.net, "edges")
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = subnet.with.new.edges(author.net, edges[1, ]),
        "2016-07-12 15:59:59-2016-07-12 16:01:59" = subnet.with.new.edges(author.net, extract.partial.edge(edges[1, ], 2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:01:59-2016-07-12 16:03:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:03:59-2016-07-12 16:05:59" = subnet.with.new.edges(author.net, rbind(extract.partial.edge(edges[2, ], 1),
                                                                                            extract.partial.edge(edges[3, ], 1))),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = subnet.with.new.edges(author.net, rbind(edges[2, ],
                                                                                            edges[3, ],
                                                                                            edges[4, ]))
    )
    results = split.network.time.based(author.net, time.period = "2 mins", sliding.window = TRUE)

    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * bins ----------------------------------------------------------------

##
## Tests for split.network.time.based(..., bins = ...)
##

patrick::with_parameters_test_that("Split a network time-based (bins = ...), ", {

    ## bins
    bins = c("2016-07-12 15:58:00", "2016-07-12 16:00:59", "2016-07-12 16:02:59",
             "2016-07-12 16:04:59", "2016-07-12 17:21:43")

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ##
    ## simplify = FALSE
    ##

    ## retrieve author network
    author.net = net.builder$get.author.network()

    ## results
    expected = list(
        "2016-07-12 15:58:00-2016-07-12 16:00:59" = igraph::subgraph_from_edges(author.net, c(1:2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:04:59-2016-07-12 17:21:43" = igraph::subgraph_from_edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, bins = bins, sliding.window = test.sliding.window)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

    ## check bins
    expected.bins = get.date.from.string(c("2016-07-12 15:58:00", "2016-07-12 16:00:59",
                                           "2016-07-12 16:02:59", "2016-07-12 16:04:59",
                                           "2016-07-12 17:21:43"))
    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

    ##
    ## simplify = TRUE
    ##

    ## update network configuration
    net.conf$update.values(list(author.relation = "cochange", simplify = TRUE))
    net.builder$reset.environment()

    ## retrieve author network
    author.net = net.builder$get.author.network()

    edges = igraph::as_data_frame(author.net, "edges")
    expected = list(
        "2016-07-12 15:58:00-2016-07-12 16:00:59" = subnet.with.new.edges(author.net, edges[1, ]),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = subnet.with.new.edges(author.net),
        "2016-07-12 16:04:59-2016-07-12 17:21:43" = subnet.with.new.edges(author.net, rbind(edges[2, ],
                                                                                            edges[3, ],
                                                                                            edges[4, ]))
    )
    results = split.network.time.based(author.net, bins = bins, sliding.window = test.sliding.window)

    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

}, cases.cross.product(
    patrick::cases(
        "sliding window (ignored): FALSE" = list(test.sliding.window = FALSE),
        "sliding window (ignored): TRUE" = list(test.sliding.window = TRUE)
    ),
    patrick::cases(
        "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
        "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
    )
))

## * * ranges --------------------------------------------------------------------

##
## Test splitting network by ranges.
##

patrick::with_parameters_test_that("Test splitting network by ranges", {


    ## bins
    bins = c("2016-07-12 15:58:00", "2016-07-12 16:00:59", "2016-07-12 16:02:59",
             "2016-07-12 16:04:59", "2016-07-12 17:21:43")
    ranges = construct.ranges(bins, sliding.window = FALSE, raw = TRUE)

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## retrieve author network
    author.net = net.builder$get.author.network()
    expected.results = split.network.time.based(author.net, bins = bins)
    results = split.network.time.based.by.ranges(author.net, ranges)

    ## check time ranges
    expect_equal(names(results), names(ranges), info = "Time ranges.")

    ## check data for all ranges
    check.identical = mapply(results, expected.results, FUN = function(r, e) {
        return(igraph::identical_graphs(r, e))
    })
    expect_true(all(check.identical), info = "Network equality (split by ranges).")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * window numbers ---------------------------------------------------------

##
## Tests for split.network.time.based(..., number.windows = ...)
##

patrick::with_parameters_test_that("Split a network time-based with equal-sized windows (number.windows = ...).", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ##
    ## simplify = FALSE
    ##

    ## retrieve author network
    author.net = net.builder$get.author.network()

    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:53" = igraph::subgraph_from_edges(author.net, c(1:2)),
        "2016-07-12 16:00:53-2016-07-12 16:02:47" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:02:47-2016-07-12 16:04:41" = igraph::subgraph_from_edges(author.net, c()),
        "2016-07-12 16:04:41-2016-07-12 16:06:33" = igraph::subgraph_from_edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, number.windows = 4)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

    ## check bins
    expected.bins = get.date.from.string(c("2016-07-12 15:58:59", "2016-07-12 16:00:53",
                                           "2016-07-12 16:02:47", "2016-07-12 16:04:41",
                                           "2016-07-12 16:06:33"))
    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

    ##
    ## simplify = TRUE
    ##

    ## update network configuration
    net.builder$update.network.conf(list(author.relation = "cochange", simplify = TRUE))
    net.builder$reset.environment()

    ## retrieve author network
    author.net = net.builder$get.author.network()

    edges = igraph::as_data_frame(author.net, "edges")
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:00:53" = subnet.with.new.edges(author.net, edges[1, ]),
        "2016-07-12 16:00:53-2016-07-12 16:02:47" = subnet.with.new.edges(author.net),
        "2016-07-12 16:02:47-2016-07-12 16:04:41" = subnet.with.new.edges(author.net),
        "2016-07-12 16:04:41-2016-07-12 16:06:33" = subnet.with.new.edges(author.net, rbind(edges[2, ],
                                                                                            edges[3, ],
                                                                                            edges[4, ]))
    )
    results = split.network.time.based(author.net, number.windows = 4)

    expect_equal(expected.bins, attr(results, "bins"))

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

##
## Tests for split.networks.time.based(..., number.windows = ...)
##

patrick::with_parameters_test_that("Split a list of networks time-based with equal-sized windows", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
    net.conf = NetworkConf$new()
    net.conf$update.values(list(simplify = FALSE, author.directed = TRUE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## obtain networks:
    ## 1) co-change network
    net.builder$update.network.conf(list(author.relation = "cochange"))
    net.cochange = net.builder$get.author.network()
    ## 2) mail network
    net.builder$update.network.conf(list(author.relation = "mail"))
    net.mail = net.builder$get.author.network()

    ## split networks
    net.split = split.networks.time.based(
        networks = list(net.cochange, net.mail),
        number.windows = 3,
        sliding.window = test.sliding.window # this parameter should be ignored if number.windows is given
    )

    ## check whether the splitting information of the two split networks are identical
    expect_identical(attributes(net.split[[1]]), attributes(net.split[[2]]), info = "Splitting information.")

    ## check whether the splitting information is as expected
    expected = list (
        "bins" = c(get.date.from.string("2010-07-12 12:05:41"),
                   get.date.from.string("2012-07-12 05:25:58"),
                   get.date.from.string("2014-07-12 22:46:15"),
                   get.date.from.string("2016-07-12 16:06:33")),
        "names" = c("2010-07-12 12:05:41-2012-07-12 05:25:58",
                    "2012-07-12 05:25:58-2014-07-12 22:46:15",
                    "2014-07-12 22:46:15-2016-07-12 16:06:33")
    )

    ## R 3.4 fails if this is expect_identical
    expect_equal(expected, attributes(net.split[[1]]), info = "Splitting information.")

    ## check whether this works also with one network in the list (if not, an error will occur)
    net.split = split.networks.time.based(
        networks = list(net.mail),
        number.windows = 3,
        sliding.window = test.sliding.window
    )

}, cases.cross.product(
    patrick::cases(
        "sliding window: FALSE" = list(test.sliding.window = FALSE),
        "sliding window: TRUE" = list(test.sliding.window = TRUE)
    ),
    patrick::cases(
        "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
        "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
    )
))


## * * simplified networks ----------------------------------------------------

test_that("Split network with singular and simplified edges", {

    ## construct network
    edges = data.frame(comb.1. = c("A", "A", "A", "A", "A", "C"),
                       comb.2. = c("B", "B", "D", "D", "D", "D"),
                       date = get.date.from.string(c("2025-01-01 12:00:00", "2025-01-01 12:00:00",
                                                     "2025-01-01 12:00:00", "2025-01-01 12:03:00",
                                                     "2025-01-01 12:03:00", "2025-01-01 12:03:00")),
                       thread = c("<thread-1#1>", "<thread-2#1>", "<thread-3#1>", "<thread-4#1>", "<thread-4#1>", "<thread-5#1>"))
    network = igraph::graph_from_data_frame(edges, vertices = c("A", "B", "C", "D"))
    network = simplify.network(convert.edge.attributes.to.list(network))
    splits = split.network.time.based(network, time.period = "1 mins")

    ## construct expected networks
    edges = igraph::as_data_frame(network, "edges")
    expected = list(
        "2025-01-01 12:00:00-2025-01-01 12:01:00" = subnet.with.new.edges(network, rbind(edges[1, ],
                                                                                         extract.partial.edge(edges[2, ], 1))),
        "2025-01-01 12:01:00-2025-01-01 12:02:00" = subnet.with.new.edges(network),
        "2025-12-01 12:02:00-2025-01-01 12:03:01" = subnet.with.new.edges(network, rbind(extract.partial.edge(edges[2, ], c(2, 3)),
                                                                                         edges[3, ]))
    )

    ## check networks
    check.identical = mapply(splits, expected, FUN = function(s, e) {
        igraph::identical_graphs(s, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

})

test_that("Split network with numeric edge attributes", {

    ##
    ## splits contain partial edges
    ##

    ## construct network
    edges = data.frame(comb.1. = c("A", "A", "A", "A", "C", "E"),
                       comb.2. = c("B", "B", "D", "D", "D", "D"),
                       date = get.date.from.string(c("2025-01-01 12:00:00", "2025-01-01 12:00:00",
                                                     "2025-01-01 12:01:00", "2025-01-01 12:03:00",
                                                     "2025-01-01 12:00:00", "2025-01-01 12:03:00")),
                       thread = c("<thread-1#1>", "<thread-2#1>", "<thread-3#1>", "<thread-4#1>", "<thread-5#1>", "<thread-6#1>"),
                       diff.size = c(0, 12, 777, 1337, 42, 91))
    network = igraph::graph_from_data_frame(edges, vertices = c("A", "B", "C", "D", "E"))
    network = simplify.network(convert.edge.attributes.to.list(network))
    splits = split.network.time.based(network, time.period = "1 mins")

    ## construct expected networks
    edges = igraph::as_data_frame(network, "edges")
    expected = list(
        "2025-01-01 12:00:00-2025-01-01 12:01:00" = subnet.with.new.edges(network, rbind(edges[1, ],
                                                                                         edges[3, ])),
        "2025-01-01 12:01:00-2025-01-01 12:02:00" = subnet.with.new.edges(network, extract.partial.edge(edges[2, ], 1)),
        "2025-12-01 12:02:00-2025-01-01 12:03:01" = subnet.with.new.edges(network, rbind(extract.partial.edge(edges[2, ], 2),
                                                                                         edges[4, ]))
    )
    ## remove the 'diff.size' attribute as subnets 2 and 3
    ## contain splits of previously simplifed edges
    expected = lapply(expected, function(net) {
        net = igraph::delete_edge_attr(net, "diff.size")
    })

    ## check networks
    check.identical = mapply(splits, expected, FUN = function(s, e) {
        igraph::identical_graphs(s, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

    ##
    ## splits only contain complete edges
    ##

    ## construct network
    edges = data.frame(comb.1. = c("A", "A", "A", "A", "C", "E"),
                       comb.2. = c("B", "B", "D", "D", "D", "D"),
                       date = get.date.from.string(c("2025-01-01 12:00:00", "2025-01-01 12:00:00",
                                                     "2025-01-01 12:01:00", "2025-01-01 12:01:00",
                                                     "2025-01-01 12:00:00", "2025-01-01 12:03:00")),
                       thread = c("<thread-1#1>", "<thread-2#1>", "<thread-3#1>", "<thread-4#1>", "<thread-5#1>", "<thread-6#1>"),
                       diff.size = c(0, 12, 777, 1337, 42, 91))
    network = igraph::graph_from_data_frame(edges, vertices = c("A", "B", "C", "D", "E"))
    network = simplify.network(convert.edge.attributes.to.list(network))
    splits = split.network.time.based(network, time.period = "1 mins")

    ## construct expected networks
    edges = igraph::as_data_frame(network, "edges")
    expected = list(
        "2025-01-01 12:00:00-2025-01-01 12:01:00" = subnet.with.new.edges(network, rbind(edges[1, ],
                                                                                         edges[3, ])),
        "2025-01-01 12:01:00-2025-01-01 12:02:00" = subnet.with.new.edges(network, edges[2, ]),
        "2025-12-01 12:02:00-2025-01-01 12:03:01" = subnet.with.new.edges(network, edges[4, ])
    )
    ## do not the remove the 'diff.size' attribute as all subnets
    ## contain only complete edges

    ## check networks
    check.identical = mapply(splits, expected, FUN = function(s, e) {
        igraph::identical_graphs(s, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

})

test_that("Split network with arbitrarily set weight attribute", {

    ## construct network
    edges = data.frame(comb.1. = c("A", "A", "A", "A", "A", "C"),
                       comb.2. = c("B", "B", "D", "D", "D", "D"),
                       date = get.date.from.string(c("2025-01-01 12:00:00", "2025-01-01 12:01:00",
                                                     "2025-01-01 12:01:00", "2025-01-01 12:03:00",
                                                     "2025-01-01 12:03:00", "2025-01-01 12:03:00")),
                       thread = c("<thread-1#1>", "<thread-2#1>", "<thread-3#1>", "<thread-4#1>", "<thread-4#1>", "<thread-5#1>"),
                       weight = c(3, 4, 0, 1, 2, 3))
    network = igraph::graph_from_data_frame(edges, vertices = c("A", "B", "C", "D"))
    network = simplify.network(convert.edge.attributes.to.list(network))
    splits = split.network.time.based(network, time.period = "1 mins")

    ## construct expected networks
    edges = igraph::as_data_frame(network, "edges")
    expected = list(
        "2025-01-01 12:00:00-2025-01-01 12:01:00" = subnet.with.new.edges(network, extract.partial.edge(edges[1, ], 1)),
        "2025-01-01 12:01:00-2025-01-01 12:02:00" = subnet.with.new.edges(network, rbind(extract.partial.edge(edges[1, ], 2),
                                                                                         extract.partial.edge(edges[2, ], 1))),
        "2025-12-01 12:02:00-2025-01-01 12:03:01" = subnet.with.new.edges(network, rbind(extract.partial.edge(edges[2, ], c(2, 3)),
                                                                                         edges[3, ]))
    )
    ## adjust the weight attribute
    ## contains 1 edge comprised of 1 partial
    expected[[1]] = igraph::set_edge_attr(expected[[1]], "weight", value = 1)
    ## contains 2 edges comprised of 1 partial each
    expected[[2]] = igraph::set_edge_attr(expected[[2]], "weight", value = c(1, 1))
    ## contains 1 edge comprised of 2 partials and 1 complete edge with weight 3
    expected[[3]] = igraph::set_edge_attr(expected[[3]], "weight", value = c(2, 3))

    ## check networks
    check.identical = mapply(splits, expected, FUN = function(s, e) {
        igraph::identical_graphs(s, e)
    })
    expect_true(all(check.identical), info = "Network equality.")

})
