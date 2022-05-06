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
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## All Rights Reserved.

context("Splitting functionality, activity-based splitting of networks.")

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

## * activity-based ------------------------------------------------------------

##
## Tests for split.network.activity.based(...)
##

test_that("Split a network activity-based (number.edges, number.windows).", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## retrieve author network
    author.net = net.builder$get.author.network()

    ##
    ## number.edges (1)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(1, 2)),
        "2016-07-12 16:05:41-2016-07-12 16:06:10" = igraph::subgraph.edges(author.net, c(3, 5)),
        "2016-07-12 16:06:10-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(4, 7)),
        "2016-07-12 16:06:32-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(6, 8))
    )
    results = split.network.activity.based(author.net, number.edges = 2)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.edges (1)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.edges (1)).")

    ##
    ## number.edges (2)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(1:igraph::ecount(author.net)))
    )
    results = split.network.activity.based(author.net, number.edges = igraph::ecount(author.net) + 10)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.edges (2)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.edges (2)).")

    ##
    ## number.windows (1)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(1, 2, 3)),
        "2016-07-12 16:05:41-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(4, 5, 7)),
        "2016-07-12 16:06:32-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(6, 8))
    )
    results = split.network.activity.based(author.net, number.windows = 3)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.windows (1)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.windows (1)).")

    ##
    ## number.windows (2)
    ##

    expect_error(
        split.network.activity.based(author.net, number.windows = igraph::ecount(author.net) + 10),
        info = "Error expected (number.windows (2))."
    )

})

## * * sliding windows

##
## Tests for split.network.activity.based(...) using sliding windows
##

test_that("Split a network activity-based (number.edges, number.windows, sliding.window = TRUE).", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## retrieve author network
    author.net = net.builder$get.author.network()

    ##
    ## number.edges (1)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(1, 2)),
        "2016-07-12 16:00:45-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(2, 3)),
        "2016-07-12 16:05:41-2016-07-12 16:06:10" = igraph::subgraph.edges(author.net, c(3, 5)),
        "2016-07-12 16:05:41-2016-07-12 16:06:10" = igraph::subgraph.edges(author.net, c(5, 4)),
        "2016-07-12 16:06:10-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(4, 7)),
        "2016-07-12 16:06:10-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(7, 6)),
        "2016-07-12 16:06:32-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(6, 8))
    )
    results = split.network.activity.based(author.net, number.edges = 2, sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.edges (1)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.edges (1)).")

    ##
    ## number.edges (2)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(1:igraph::ecount(author.net)))
    )
    results = split.network.activity.based(author.net, number.edges = igraph::ecount(author.net) + 10,
                                           sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.edges (2)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.edges (2)).")

    ##
    ## number.windows (1) (i.e., ignoring sliding windows)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(1, 2, 3)),
        "2016-07-12 16:05:41-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(4, 5, 7)),
        "2016-07-12 16:06:32-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(6, 8))
    )
    results = split.network.activity.based(author.net, number.windows = 3, sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.windows (1)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.windows (1)).")

    ##
    ## number.windows (2) (i.e., ignoring sliding windows)
    ##

    expect_error(
        split.network.activity.based(author.net, number.windows = igraph::ecount(author.net) + 10,
                                     sliding.window = TRUE),
        info = "Error expected (number.windows (2))."
    )

})

test_that("Split a network activity-based (number.edges, number.windows, sliding.window = TRUE), continued.", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)

    ## retrieve author network and add an additional edge in the end
    author.net = net.builder$get.author.network()
    author.net = igraph::add_edges(author.net, c("Olaf", "Thomas"),
                                   attr = list(date = get.date.from.string("2020-02-20 20:20:20")))

    ##
    ## number.edges (1)
    ##

    ## results
    expected = list(
        "2016-07-12 15:58:59-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(1, 2)),
        "2016-07-12 16:00:45-2016-07-12 16:05:41" = igraph::subgraph.edges(author.net, c(2, 3)),
        "2016-07-12 16:05:41-2016-07-12 16:06:10" = igraph::subgraph.edges(author.net, c(3, 5)),
        "2016-07-12 16:05:41-2016-07-12 16:06:10" = igraph::subgraph.edges(author.net, c(5, 4)),
        "2016-07-12 16:06:10-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(4, 7)),
        "2016-07-12 16:06:10-2016-07-12 16:06:32" = igraph::subgraph.edges(author.net, c(7, 6)),
        "2016-07-12 16:06:32-2020-02-20 20:20:20" = igraph::subgraph.edges(author.net, c(6, 8)),
        "2016-07-12 16:06:32-2020-02-20 20:20:21" = igraph::subgraph.edges(author.net, c(8, 9))
    )
    results = split.network.activity.based(author.net, number.edges = 2, sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges (number.edges (1)).")

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Network equality (number.edges (1)).")

})
