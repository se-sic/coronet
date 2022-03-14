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


context("Splitting functionality, using equally sized windows.")

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


##
## TODO
##

## - net.conf$update.values(list(pasta = TRUE, synchronicity = TRUE))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split data --------------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.data.time.based(..., number.windows = ..., split.basis = 'commits')
##

test_that(
    "Split a data object time-based with equally sized windows (number.windows = ..., split.basis = 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, number.windows = 3,
                                    split.basis = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:01:30",
        "2016-07-12 16:01:30-2016-07-12 16:04:01",
        "2016-07-12 16:04:01-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must mot modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 months",
        split.basis = "commits",
        split.sliding.window = FALSE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:01:30", "2016-07-12 16:04:01", "2016-07-12 16:06:33"),
        split.revision.dates = NULL
    )
    lapply(results, function(res) {
        actual = lapply(names(expected.config), res$get.project.conf()$get.value)
        names(actual) = names(expected.config)
        expect_equal(expected.config, actual)
    })

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$commits[1:2, ],
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$commits[0, ],
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$commits[3:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$commit.messages,
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$commit.messages,
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$issues[rownames(data$issues) %in% c(20:22, 37:40), ],
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$issues[rownames(data$issues) %in% c(14, 15, 29, 47:49), ],
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% c(23, 41, 45:46), ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$mails[0, ],
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$mails[0, ],
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$mails[rownames(data$mails) == 16 | rownames(data$mails) == 17, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$pasta,
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$pasta,
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:30" = data$synchronicity,
            "2016-07-12 16:01:30-2016-07-12 16:04:01" = data$synchronicity,
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")

})


##
## Tests for split.data.time.based(..., split.basis = 'mails')
##

test_that("Split a data object time-based with equal-sized windows (number.windows = ..., split.basis = 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, number.windows = 4,
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2007-09-18 06:00:04",
        "2007-09-18 06:00:04-2010-08-26 17:21:55",
        "2010-08-26 17:21:55-2013-08-04 04:43:46",
        "2013-08-04 04:43:46-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must mot modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 months",
        split.basis = "mails",
        split.sliding.window = FALSE,
        split.revisions = c("2004-10-09 18:38:13", "2007-09-18 06:00:04", "2010-08-26 17:21:55",
                            "2013-08-04 04:43:46", "2016-07-12 16:05:38"),
        split.revision.dates = NULL
    )
    lapply(results, function(res) {
        actual = lapply(names(expected.config), res$get.project.conf()$get.value)
        names(actual) = names(expected.config)
        expect_equal(expected.config, actual)
    })

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$commits[0, ],
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$commits[0, ],
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$commits[0, ],
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$commit.messages,
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$commit.messages,
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$commit.messages,
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$issues[0, ],
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$issues[0, ],
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 27:29, 37:40, 43:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$mails[rownames(data$mails) %in% 1:2, ],
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$mails[0, ],
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$pasta,
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$pasta,
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$pasta,
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$synchronicity,
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$synchronicity,
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$synchronicity,
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")
})


##
## Tests for split.data.time.based(..., split.basis = 'issues')
##

test_that("Split a data object time-based with equal-sized windows (number.windows = ..., split.basis = 'issues').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, number.windows = 3,
                                    split.basis = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2014-09-01 12:05:39",
        "2014-09-01 12:05:39-2016-01-12 00:19:09",
        "2016-01-12 00:19:09-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must mot modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 months",
        split.basis = "issues",
        split.sliding.window = FALSE,
        split.revisions = c("2013-04-21 23:52:09", "2014-09-01 12:05:39", "2016-01-12 00:19:09", "2017-05-23 12:32:40"),
        split.revision.dates = NULL
    )
    lapply(results, function(res) {
        actual = lapply(names(expected.config), res$get.project.conf()$get.value)
        names(actual) = names(expected.config)
        expect_equal(expected.config, actual)
    })

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$commits[0, ],
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$commits[0, ],
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$commits
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$commit.messages,
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$commit.messages,
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$issues[0, ],
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% 14:49, ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$mails[0, ],
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$mails[0, ],
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 14:17, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$pasta,
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$pasta,
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2014-09-01 12:05:39" = data$synchronicity,
            "2014-09-01 12:05:39-2016-01-12 00:19:09" = data$synchronicity,
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")

})

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split network -----------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.network.time.based(..., time.period = ...)
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
## Tests for split.networks.time.based(..., time.period = ...)
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
