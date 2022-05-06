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
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
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
## TODO
##

## - net.conf$update.values(list(pasta = TRUE, synchronicity = TRUE))

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split raw data (data and networks by bins) ------------------------------

##
## Tests for split.data.by.bins and split.network.by.bins
##

test_that("Split network and data on low level (split.data.by.bins, split.network.by.bins).", {

    length.dates = 15
    length.bins = 5

    ## generate dates
    dates = c("2000-01-25", "2000-01-23", "2000-01-15", "2000-01-27", "2000-01-13",
              "2000-01-03", "2000-01-05", "2000-01-29", "2000-01-19", "2000-01-01",
              "2000-01-11", "2000-01-07", "2000-01-21", "2000-01-09", "2000-01-17")
    # ## ## generated with:
    # sprintf("c(\"%s\")", paste(
    #     get.date.string(sample(
    #         seq.POSIXt(get.date.from.string("2000-01-01"), get.date.from.string("2000-02-01"), by = "1 days"),
    #         length.dates,
    #         replace = FALSE
    #     )), collapse = "\", \""))

    ## generate bins
    bins = seq_len(length.bins)
    bins.vector = c("1", "3", "5", "4", "1", "3", "1", "3", "2", "5", "4", "2", "4", "3", "5")
    ## ## generated with:
    ## sprintf("c(\"%s\")", paste( sample(bins, size = length.dates, replace = TRUE), collapse = "', '") )

    ##
    ## split.data.by.bins
    ##

    ## generate data frame with dates and IDs
    df = data.frame(
        id = 1:length.dates,
        date = dates
    )

    ## results
    expected = list(
        "1" = df[ c(1,  5,  7), ],
        "2" = df[ c(9, 12), ],
        "3" = df[ c(2,  6,  8, 14), ],
        "4" = df[ c(4, 11, 13), ],
        "5" = df[ c(3, 10, 15), ]
    )
    results = split.data.by.bins(df, bins.vector)

    ## check result
    expect_equal(results, expected, info = "Split data by bins.")

    ##
    ## split.network.by.bins
    ##

    ## generate data frame with dates and IDs
    vcount = 4
    net = igraph::make_empty_graph(n = vcount, directed = FALSE)
    for (e.id in seq_len(length.dates)) {
        net = net + igraph::edge(
            sample(seq_len(vcount), 1), # from vertex
            sample(seq_len(vcount), 1), # to vertex
            date = get.date.from.string(dates[e.id])
            )
    }

    ## results
    expected = list(
        igraph::subgraph.edges(net, c(1,  5,  7)),
        igraph::subgraph.edges(net, c(9, 12)),
        igraph::subgraph.edges(net, c(2,  6,  8, 14)),
        igraph::subgraph.edges(net, c(4, 11, 13)),
        igraph::subgraph.edges(net, c(3, 10, 15))
    )
    results = split.network.by.bins(net, bins, bins.vector)

    ## check networks
    check.identical = mapply(results, expected, FUN = function(r, e) {
        igraph::identical_graphs(r, e)
    })
    expect_true(all(check.identical), info = "Split network by bins (network equality).")

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bin identification ------------------------------------------------------

##
## Tests for split.get.bins.time.based and split.get.bins.activity.based
##

test_that("Get bins for network and data on low level (split.get.bins.time.based, split.get.bins.activity.based).", {

    length.dates = 15
    length.bins = 5

    ## generate dates
    dates = c("2000-01-25", "2000-01-23", "2000-01-15", "2000-01-27", "2000-01-13",
              "2000-01-03", "2000-01-05", "2000-01-29", "2000-01-19", "2000-01-01",
              "2000-01-11", "2000-01-07", "2000-01-21", "2000-01-09", "2000-01-17")
    dates.posixct = get.date.from.string(dates)
    ## ## generated with:
    ## sprintf("c(\"%s\")", paste(
    ##     get.date.string(sample(
    ##         seq.POSIXt(get.date.from.string("2000-01-01"), get.date.from.string("2000-02-01"), by = "1 days"),
    ##         length.dates,
    ##         replace = FALSE
    ##     )), collapse = "\", \""))

    ##
    ## split.get.bins.time.based (1)
    ##

    ## results
    expected.bins  = c("2000-01-01 00:00:00", "2000-01-11 00:00:00", "2000-01-21 00:00:00", "2000-01-29 00:00:01")
    expected = list(
        vector = factor(head(expected.bins, -1))[c(3, 3, 2, 3, 2,
                                                   1, 1, 3, 2, 1,
                                                   2, 1, 3, 1, 2)],
        bins = expected.bins
    )
    results = split.get.bins.time.based(dates.posixct, "10 days")

    ## check result
    expect_equal(results, expected, info = "split.get.bins.time.based (1)")

    ##
    ## split.get.bins.time.based (2)
    ##

    ## results
    expected.bins  = c("2000-01-01 00:00:00", "2000-01-29 00:00:01")
    expected = list(
        vector = factor(head(expected.bins, -1))[ rep(1, length.dates) ],
        bins = expected.bins
    )
    results = split.get.bins.time.based(dates.posixct, "1 year")

    ## check result
    expect_equal(results, expected, info = "split.get.bins.time.based (2)")

    ##
    ## split.get.bins.time.based (3)
    ##

    ## results
    dates.unround = get.date.from.string(c("2004-01-01 00:00:00", "2004-01-01 00:00:14", "2004-01-01 00:00:22"))
    expected.bins = c("2004-01-01 00:00:00", "2004-01-01 00:00:05", "2004-01-01 00:00:10",
                      "2004-01-01 00:00:15", "2004-01-01 00:00:20", "2004-01-01 00:00:23") # adding 4.2 seconds each
    expected = list(
        vector = factor(head(expected.bins, -1))[ c(1, 3, 5) ],
        bins = expected.bins
    )
    results = split.get.bins.time.based(dates.unround, number.windows = length.bins)

    ## check result
    expect_equal(results, expected, info = "split.get.bins.time.based (3)")

    ##
    ## split.get.bins.activity.based (1)
    ##

    ## construct data.frame
    df = data.frame(date = dates.posixct, id = seq_len(length.dates))
    df = df[ order(df$date), ]

    ## results
    expected = list(
        vector = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4),
        bins = c("2000-01-01 00:00:00", "2000-01-09 00:00:00", "2000-01-17 00:00:00", "2000-01-25 00:00:00", "2000-01-29 00:00:01")
    )
    results = split.get.bins.activity.based(df, "id", 4)

    ## check result
    expect_equal(results, expected, info = "split.get.bins.activity.based (1)")

    ##
    ## split.get.bins.activity.based (2)
    ##

    ## construct data.frame
    df = data.frame(date = dates.posixct, id = seq_len(length.dates))
    df = df[ order(df$date), ]

    ## results
    expected = list(
        vector = rep(1, length.out = length.dates),
        bins = c("2000-01-01 00:00:00", "2000-01-29 00:00:01")
    )
    results = split.get.bins.activity.based(df, "id", nrow(df) + 10)

    ## check result
    expect_equal(results, expected, info = "split.get.bins.activity.based (2)")

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Consistency tests -------------------------------------------------------

##
## Tests for consistency of data and network time-based splitting
##

test_that("Check consistency of data and network time-based splitting.", {

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))

    ## retrieve project data and network builder
    project.data = ProjectData$new(proj.conf)
    net.builder = NetworkBuilder$new(project.data, net.conf)
    ## retrieve author network
    project.net = net.builder$get.author.network()

    ## set time period for splitting
    time.period = "7 mins"

    ## split data
    results.data = split.data.time.based(project.data, time.period = time.period, split.basis = "commits")
    results.data.network = lapply(results.data, function(d) NetworkBuilder$new(d, net.conf)$get.author.network())

    ## split network
    results.network = split.network.time.based(project.net, time.period = time.period)

    ## check ranges
    expect_equal(names(results.network), names(results.data.network), info = "Range equality.")

    ## the chosen time-window size results in the following condition:
    ## 1) Thomas and Karl only appear in the second time window, both working on the base feature.
    ## 2) Olaf only appears in the first time window, working on the base feature as the only author.
    ## Thus, when splitting the project-level network, there are edges from Olaf to Karl and Thomas,
    ## crossing the time-window border. Hence, when deleting the respective vertices from the networks,
    ## the data-based networks should match the network-based networks.
    results.network[[1]] = igraph::delete.vertices(results.network[[1]], c("Thomas", "Karl"))
    results.network[[2]] = igraph::delete.vertices(results.network[[2]], c("Olaf"))
    check.identical = mapply(results.data.network, results.network, FUN = function(d, n) {
        igraph::identical_graphs(d, n)
    })
    expect_true(all(check.identical), info = "Network equality.")

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unification of range names ----------------------------------------------

##
## Tests for duplicate range names
##

test_that("Check and correct duplicate range names during network activity-based splitting.", {

    ## define dates for edges and the resulting changes
    dates = get.date.from.string(c(
        "2000-01-01 01:00:00", "2001-01-01 12:00:00",

        "2001-01-01 12:00:00", "2001-01-01 12:00:00",
        "2001-01-01 12:00:00", "2001-01-01 12:00:00",
        "2001-01-01 12:00:00", "2001-01-01 12:00:00",

        "2002-01-01 12:00:00", "2002-01-01 12:00:00",
        "2002-01-01 12:00:00", "2002-01-01 12:00:00",
        "2002-01-01 12:00:00", "2002-01-01 12:00:00",
        "2002-01-01 12:00:00", "2002-01-01 12:00:00",

        "2002-01-01 12:00:00", "2003-01-01 12:00:00"
    ))
    expected.ranges = c(
        "2000-01-01 01:00:00-2001-01-01 12:00:00",

        "2001-01-01 12:00:00-2001-01-01 12:00:00",
        "2001-01-01 12:00:00-2001-01-01 12:00:00",

        "2001-01-01 12:00:00-2002-01-01 12:00:00",

        "2002-01-01 12:00:00-2002-01-01 12:00:00",
        "2002-01-01 12:00:00-2002-01-01 12:00:00",
        "2002-01-01 12:00:00-2002-01-01 12:00:00",
        "2002-01-01 12:00:00-2002-01-01 12:00:00",

        "2002-01-01 12:00:00-2003-01-01 12:00:01"
    )
    expected.ranges.corrected = c(
        "2000-01-01 01:00:00-2001-01-01 12:00:00",

        "2001-01-01 12:00:00-2001-01-01 12:00:00 (1)",
        "2001-01-01 12:00:00-2001-01-01 12:00:00 (2)",

        "2001-01-01 12:00:00-2002-01-01 12:00:00",

        "2002-01-01 12:00:00-2002-01-01 12:00:00 (1)",
        "2002-01-01 12:00:00-2002-01-01 12:00:00 (2)",
        "2002-01-01 12:00:00-2002-01-01 12:00:00 (3)",
        "2002-01-01 12:00:00-2002-01-01 12:00:00 (4)",

        "2002-01-01 12:00:00-2003-01-01 12:00:01"
    )

    ## construct a small network
    net = igraph::make_empty_graph(directed = FALSE) +
        igraph::vertices(c("A", "B")) +
        igraph::edges(rep(c("A", "B"), times = length(dates)))
    ## set some date attributes that are appropriate for the test case
    net = igraph::set.edge.attribute(net, "date", value = dates)

    ## define split arguments
    split.function = split.network.activity.based
    split.activity.amount = 2
    split.arguments = list(network = net, number.edges = split.activity.amount, sliding.window = FALSE)

    ## check for issued warning
    expect_output(
        do.call(split.function, split.arguments),
        "WARNING::Due to the splitting, there are duplicated range names.",
        fixed = TRUE,
        info = "Generate warning."
    )

    ## check range names
    net.split = do.call(split.function, split.arguments)
    ranges = names(net.split)
    expect_equal(ranges, expected.ranges, info = "Ranges (original).")

    ## correct ranges
    ranges.corrected = split.unify.range.names(ranges)
    expect_equal(ranges.corrected, expected.ranges.corrected, info = "Ranges (unified).")


    ## Arbitrary range names (1)
    ranges = c("A-B", "B-C", "C-D")
    expected = c("A-B", "B-C", "C-D")
    result = split.unify.range.names(ranges)
    expect_identical(result, expected, info = "Arbitrary ranges (1).")

    ## Arbitrary range names (2)
    ranges = c("A-B", "A-B", "B-C", "B-C", "C-D")
    expected = c("A-B (1)", "A-B (2)", "B-C (1)", "B-C (2)", "C-D")
    result = split.unify.range.names(ranges)
    expect_identical(result, expected, info = "Arbitrary ranges (2).")

    ## Arbitrary range names (3)
    ranges = c("A-B", "A-B", "B-C", "A-B", "B-C")
    expected = c("A-B (1)", "A-B (2)", "B-C (1)", "A-B (1)", "B-C (1)")
    result = split.unify.range.names(ranges)
    expect_identical(result, expected, info = "Arbitrary ranges (3).")

    ## Arbitrary range names (4)
    ranges = c("A-B", "A-B", "B-C", "C-D", "C-D")
    expected = c("A-B (1)", "A-B (2)", "B-C", "C-D (1)", "C-D (2)")
    result = split.unify.range.names(ranges)
    expect_identical(result, expected, info = "Arbitrary ranges (4).")

    ##
    ## the removal duplicate ranges
    ##

    df = data.frame(date = dates, id = 1:length(dates))
    expected = expected.ranges[c(1, 4, 9)]
    result = construct.ranges(
        split.get.bins.activity.based(df, "id", activity.amount = split.activity.amount, remove.duplicate.bins = TRUE)[["bins"]],
        sliding.window = FALSE
    )
    expect_identical(result, expected, info = "Removal of duplicate ranges.")

})
