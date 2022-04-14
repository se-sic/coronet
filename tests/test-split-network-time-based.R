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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## All Rights Reserved.

context("Splitting functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature"

## use only when debugging this file independently
if (!dir.exists(CF.DATA)) CF.DATA = file.path(".", "tests", "codeface-data")


##
## NOTE
##

## In this test file, we rather test the raw data contents of the data objects
## instead of the networks that can be constructed from these data items!


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split network -----------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.network.time.based(..., time.period = ...)
##

test_that("Split a network time-based (time.period = ...).", {

    ## time period
    time.period = "2 mins"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = igraph::subgraph.edges(author.net, c(1:2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, time.period = "2 mins")

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

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

    expect_error(split.network.time.based(author.net, bins = bins), info = "Illegal split.")

})

##
## Tests for split.networks.time.based(..., time.period = ...)
##

patrick::with_parameters_test_that("Split a list of networks time-based, ", {

    ## time period
    time.period = "2 years"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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

}, patrick::cases(
    "sliding window: FALSE" = list(test.sliding.window = FALSE),
    "sliding window: TRUE" = list(test.sliding.window = TRUE)
))

## * * time period, sliding windows ----------------------------------------

##
## Tests for split.network.time.based(..., time.period = ...) using sliding windows
##

test_that("Split a network time-based (time.period = ... , sliding.window = TRUE).", {

    ## time period
    time.period = "2 mins"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
        "2016-07-12 15:58:59-2016-07-12 16:00:59" = igraph::subgraph.edges(author.net, c(1:2)),
        "2016-07-12 15:59:59-2016-07-12 16:01:59" = igraph::subgraph.edges(author.net, c(2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:01:59-2016-07-12 16:03:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:03:59-2016-07-12 16:05:59" = igraph::subgraph.edges(author.net, c(3,5)),
        "2016-07-12 16:04:59-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, time.period = "2 mins", sliding.window = TRUE)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

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

    expect_error(split.network.time.based(author.net, bins = bins, sliding.window = TRUE), info = "Illegal split.")

})

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
        "2016-07-12 15:58:00-2016-07-12 16:00:59" = igraph::subgraph.edges(author.net, c(1:2)),
        "2016-07-12 16:00:59-2016-07-12 16:02:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:02:59-2016-07-12 16:04:59" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:04:59-2016-07-12 17:21:43" = igraph::subgraph.edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, bins = bins, sliding.window = test.sliding.window)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

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

    expect_error(split.network.time.based(author.net, bins = bins, sliding.window = test.sliding.window),
                 info = "Illegal split.")

}, patrick::cases(
    "sliding window (ignored): FALSE" = list(test.sliding.window = FALSE),
    "sliding window (ignored): TRUE" = list(test.sliding.window = TRUE)
))

## * * ranges --------------------------------------------------------------------

##
## Test splitting network by ranges.
##

test_that("Test splitting network by ranges", {


    ## bins
    bins = c("2016-07-12 15:58:00", "2016-07-12 16:00:59", "2016-07-12 16:02:59",
             "2016-07-12 16:04:59", "2016-07-12 17:21:43")
    ranges = construct.ranges(bins, sliding.window = FALSE, raw = TRUE)

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
})

## * * window numbers ---------------------------------------------------------

##
## Tests for split.network.time.based(..., number.windows = ...)
##

test_that("Split a network time-based with equal-sized windows (number.windows = ...).", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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
        "2016-07-12 15:58:59-2016-07-12 16:00:53" = igraph::subgraph.edges(author.net, c(1:2)),
        "2016-07-12 16:00:53-2016-07-12 16:02:47" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:02:47-2016-07-12 16:04:41" = igraph::subgraph.edges(author.net, c()),
        "2016-07-12 16:04:41-2016-07-12 16:06:33" = igraph::subgraph.edges(author.net, c(3:8))
    )
    results = split.network.time.based(author.net, number.windows = 4)

    ## check ranges (labels)
    expect_equal(names(results), names(expected), info = "Time ranges.")

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

    expect_error(split.network.time.based(author.net, bins = bins), info = "Illegal split.")

})

##
## Tests for split.networks.time.based(..., number.windows = ...)
##

patrick::with_parameters_test_that("Split a list of networks time-based with equal-sized windows", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
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

    expect_identical(expected, attributes(net.split[[1]]), info = "Splitting information.")

    ## check whether this works also with one network in the list (if not, an error will occur)
    net.split = split.networks.time.based(
        networks = list(net.mail),
        number.windows = 3,
        sliding.window = test.sliding.window
    )

}, patrick::cases(
    "sliding window: FALSE" = list(test.sliding.window = FALSE),
    "sliding window: TRUE" = list(test.sliding.window = TRUE)
))
