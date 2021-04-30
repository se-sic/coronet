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


##
## TODO
##

## - net.conf$update.values(list(pasta = TRUE, synchronicity = TRUE))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split data --------------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.data.time.based(..., split.basis = 'commits')
##

test_that("Split a data object time-based (split.basis = 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, time.period = "3 min",
                                    split.basis = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:01:59",
        "2016-07-12 16:01:59-2016-07-12 16:04:59",
        "2016-07-12 16:04:59-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 min",
        split.basis = "commits",
        split.sliding.window = FALSE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:01:59", "2016-07-12 16:04:59", "2016-07-12 16:06:33"),
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
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commits[1:2, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$commits[0, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commits[3:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commit.messages,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$commit.messages,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$issues[rownames(data$issues) %in% c(14, 20:22, 37:40), ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$issues[rownames(data$issues) %in% c(15,29, 47:49), ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% c(23,41,45:46), ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$mails[0, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[rownames(data$mails) == 16, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) == 17, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$pasta,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$pasta,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$synchronicity,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$synchronicity,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")

})


##
## Tests for split.data.time.based(..., split.basis = 'mails')
##

test_that("Split a data object time-based (split.basis = 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, time.period = "3 years",
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2007-10-10 12:38:13",
        "2007-10-10 12:38:13-2010-10-10 06:38:13",
        "2010-10-10 06:38:13-2013-10-10 00:38:13",
        "2013-10-10 00:38:13-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 years",
        split.basis = "mails",
        split.sliding.window = FALSE,
        split.revisions = c("2004-10-09 18:38:13", "2007-10-10 12:38:13", "2010-10-10 06:38:13",
                            "2013-10-10 00:38:13", "2016-07-12 16:05:38"),
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
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$commits[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$commits[0, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$commits[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$commit.messages,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$commit.messages,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$commit.messages,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$issues[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$issues[0, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 27:29, 37:40, 43:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$mails[rownames(data$mails) %in% 1:2, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$mails[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$pasta,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$pasta,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$pasta,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$synchronicity,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$synchronicity,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$synchronicity,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")
})


##
## Tests for split.data.time.based(..., split.basis = 'issues')
##

test_that("Split a data object time-based (split.basis = 'issues').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, time.period = "2 years",
                                    split.basis = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2015-04-22 11:52:09",
        "2015-04-22 11:52:09-2017-04-21 23:52:09",
        "2017-04-21 23:52:09-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "2 years",
        split.basis = "issues",
        split.sliding.window = FALSE,
        split.revisions = c("2013-04-21 23:52:09", "2015-04-22 11:52:09", "2017-04-21 23:52:09", "2017-05-23 12:32:40"),
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
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$commits[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commits,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$commits[0, ]
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$commit.messages,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commit.messages,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$issues[rownames(data$issues) %in% c(14:34, 37:49), ],
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% 35:36, ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$mails[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$mails[rownames(data$mails) %in% 14:17, ],
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$mails[0, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$pasta,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$pasta,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$synchronicity,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$synchronicity,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")

})

## * * bins ----------------------------------------------------------------

##
## Tests for split.data.time.based(..., bins = ...)
##

test_that("Split a data object time-based (bins = ... ).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, bins = c("2016-01-01 00:00:00", "2016-12-31 23:59:59"),
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-01-01 00:00:00-2016-12-31 23:59:59"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = c("2016-01-01 00:00:00", "2016-12-31 23:59:59"),
        split.basis = NULL,
        split.sliding.window = FALSE,
        split.revisions = c("2016-01-01 00:00:00", "2016-12-31 23:59:59"),
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
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commits
        ),
        commit.messages = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commit.messages
        ),
        issues = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$issues[rownames(data$issues) %in% c(14:34, 37:49), ]
        ),
        mails = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        pasta = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$pasta
        ),
        synchronicity = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expect_equal(results.data, expected.data, info = "Data for ranges.")
})

## * * ranges --------------------------------------------------------------

##
## Test splitting data by network names.
##

test_that("Test splitting data by networks", {
    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))

    ## construct project data
    project.data = ProjectData$new(proj.conf)

    ## split data
    mybins = get.date.from.string(c("2016-07-12 15:00:00", "2016-07-12 16:00:00",
                                    "2016-07-12 16:05:00", "2016-10-05 09:00:00"))
    input.data = split.data.time.based(project.data, bins = mybins)
    input.data.network = lapply(input.data, function(d) NetworkBuilder$new(d, net.conf)$get.author.network())

    ## split data by networks
    aggregation.level = c("range", "cumulative", "all.ranges",
                          "project.cumulative", "project.all.ranges",
                          "complete")
    results = lapply(aggregation.level, function(level)
        split.data.by.networks(input.data.network, project.data, level)
    )
    names(results) = aggregation.level

    ## construct expected ranges
    expected.ranges = list(
        range = c("2016-07-12 15:00:00-2016-07-12 16:00:00",
                  "2016-07-12 16:00:00-2016-07-12 16:05:00",
                  "2016-07-12 16:05:00-2016-10-05 09:00:00"),
        cumulative = c("2016-07-12 15:00:00-2016-07-12 16:00:00",
                       "2016-07-12 15:00:00-2016-07-12 16:05:00",
                       "2016-07-12 15:00:00-2016-10-05 09:00:00"),
        all.ranges = c("2016-07-12 15:00:00-2016-10-05 09:00:00",
                       "2016-07-12 15:00:00-2016-10-05 09:00:00",
                       "2016-07-12 15:00:00-2016-10-05 09:00:00"),
        project.cumulative = c("2004-10-09 18:38:13-2016-07-12 16:00:00",
                               "2004-10-09 18:38:13-2016-07-12 16:05:00",
                               "2004-10-09 18:38:13-2016-10-05 09:00:00"),
        project.all.ranges = c("2004-10-09 18:38:13-2016-10-05 09:00:00",
                               "2004-10-09 18:38:13-2016-10-05 09:00:00",
                               "2004-10-09 18:38:13-2016-10-05 09:00:00"),
        complete = c("2004-10-09 18:38:13-2017-05-23 12:32:40",
                     "2004-10-09 18:38:13-2017-05-23 12:32:40",
                     "2004-10-09 18:38:13-2017-05-23 12:32:40")
    )

    ## test the ranges
    test.each.network = function(aggregation.level) {
        result.data = results[[aggregation.level]]
        expected.range.names = expected.ranges[[aggregation.level]]

        lapply(seq_along(result.data), function(i) {
            result.entry = result.data[[i]]

            expect_true(igraph::identical_graphs(result.entry[["network"]], input.data.network[[i]]))
            expect_equal(result.entry[["data"]]$get.range(), expected.range.names[[i]])
        })
    }
    lapply(aggregation.level, test.each.network)
})

##
## Test splitting data by ranges.
##

test_that("Test splitting data by ranges", {
    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    net.conf = NetworkConf$new()
    net.conf$update.values(list(author.relation = "cochange", simplify = FALSE))

    ## construct project data
    project.data = ProjectData$new(proj.conf)

    ## split data
    my.bins = get.date.from.string(c("2016-07-12 15:00:00", "2016-07-12 16:00:00",
                                     "2016-07-12 16:05:00", "2016-10-05 09:00:00"))
    my.ranges = construct.ranges(my.bins, sliding.window = FALSE)
    expected.results = split.data.time.based(project.data, bins = my.bins)
    results = split.data.time.based.by.ranges(project.data, my.ranges)

    ## check time ranges
    expect_equal(names(results), my.ranges, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = lapply(expected.results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(expected.results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(expected.results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(expected.results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(expected.results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(expected.results, function(cf.data) cf.data$get.synchronicity())
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")
})

## * activity-based --------------------------------------------------------

##
## Tests for split.data.activity.based(..., activity.type = 'commits')
##

test_that("Split a data object activity-based (activity.type = 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 3,
                                    activity.type = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:10",
        "2016-07-12 16:06:10-2016-07-12 16:06:32",
        "2016-07-12 16:06:32-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (activity.amount).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 3,
        split.basis = "commits",
        split.sliding.window = FALSE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:06:10", "2016-07-12 16:06:32", "2016-07-12 16:06:33"),
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
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commits[1:3, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commits[4:6, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commits[7:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commit.messages,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commit.messages,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:41, 45:49), ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$issues[0, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$mails[0, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$pasta,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (activity.amount).")

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$commits) + 10,
                                        activity.type = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (too-large activity amount).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 18,
        split.basis = "commits",
        split.sliding.window = FALSE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:06:33"),
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
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$commits
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% c(14:15, 20:23, 29, 37:41, 45:49), ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) %in% 16:17, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges for too-large activity amount (activity.amount).")

    ##
    ## split by number of windows
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:20",
        "2016-07-12 16:06:20-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (number.windows).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 4,
        split.basis = "commits",
        split.sliding.window = FALSE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:06:20", "2016-07-12 16:06:33"),
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
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$commits[1:4, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$commit.messages,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:41, 45:49), ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits", number.windows = nrow(project.data$get.commits()) + 10),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits", number.windows = 0),
        info = "Error expected (number.windows) (2)."
    )

})


##
## Tests for split.data.activity.based(..., activity.type = 'mails')
##

test_that("Split a data object activity-based (activity.type = 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 3,
                                    activity.type = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2010-07-12 11:05:35",
        "2010-07-12 11:05:35-2010-07-12 12:05:41",
        "2010-07-12 12:05:41-2010-07-12 12:05:44",
        "2010-07-12 12:05:44-2016-07-12 15:58:40",
        "2016-07-12 15:58:40-2016-07-12 16:05:37",
        "2016-07-12 16:05:37-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 3,
        split.basis = "mails",
        split.sliding.window = FALSE,
        split.revisions = c("2004-10-09 18:38:13", "2010-07-12 11:05:35", "2010-07-12 12:05:41",
                            "2010-07-12 12:05:44" ,"2016-07-12 15:58:40", "2016-07-12 16:05:37",
                            "2016-07-12 16:05:38"),
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
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$commits[0, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$commits[0, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$commits[0, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$commits[0, ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$commits[1:2, ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$commits[0, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$commit.messages,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$commit.messages,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$commit.messages,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$commit.messages,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$commit.messages,
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$issues[0, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$issues[0, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$issues[0, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$issues[rownames(data$issues) %in% c(1:13, 27:28, 43:44), ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:40, 45:49), ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$issues[0, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$mails[rownames(data$mails) %in% 1:3, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$mails[rownames(data$mails) %in% 4:6, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$mails[rownames(data$mails) %in% 7:9, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$mails[rownames(data$mails) %in% 10:12, ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$mails[rownames(data$mails) %in% 14:16, ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 17, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$pasta,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$pasta,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$pasta,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$pasta,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$pasta,
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$synchronicity,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$synchronicity,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$synchronicity,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$synchronicity,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$synchronicity,
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$mails) + 10,
                                        activity.type = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (too-large activity amount).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 26,
        split.basis = "mails",
        split.sliding.window = FALSE,
        split.revisions = c("2004-10-09 18:38:13", "2016-07-12 16:05:38"),
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
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29, 37:40, 43:45, 46:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$mails
        ),
        pasta = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (too-large activity amount).")

    ##
    ## split by number of windows
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "mail", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2010-07-12 12:05:43",
        "2010-07-12 12:05:43-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (number.windows).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 8,
        split.basis = "mails",
        split.sliding.window = FALSE,
        split.revisions = c("2004-10-09 18:38:13", "2010-07-12 12:05:43", "2016-07-12 16:05:38"),
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
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$commits[0, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$commit.messages,
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$issues[0, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29, 37:40, 43:45, 46:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$mails[rownames(data$mails) %in% 1:8, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 9:17, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$pasta,
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$synchronicity,
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows

    expect_error(
        split.data.activity.based(project.data, activity.type = "mails", number.windows = nrow(project.data$get.mails()) + 10),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "mails", number.windows = 0),
        info = "Error expected (number.windows) (2)."
    )
})


##
## Tests for split.data.activity.based(..., activity.type = 'issues')
##

test_that("Split a data object activity-based (activity.type = 'issues').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues.filtered(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 9,
                                        activity.type = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2013-05-25 06:22:23",
        "2013-05-25 06:22:23-2016-07-12 15:59:59",
        "2016-07-12 15:59:59-2016-07-12 16:06:30",
        "2016-07-12 16:06:30-2016-10-05 15:30:02",
        "2016-10-05 15:30:02-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 9,
        split.basis = "issues",
        split.sliding.window = FALSE,
        split.revisions = c("2013-04-21 23:52:09", "2013-05-25 06:22:23", "2016-07-12 15:59:59",
                            "2016-07-12 16:06:30", "2016-10-05 15:30:02", "2017-05-23 12:32:40"),
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
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$commits[0, ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$commits[1, ],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$commits[2:5, ],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$commits[6:8, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$commits[0, ]
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$commit.messages,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$commit.messages,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$commit.messages,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$commit.messages,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$issues[rownames(data$issues) %in% 1:10, ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$issues[rownames(data$issues) %in% c(11:13, 20:21, 27:28, 43:44, 37:38), ],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$issues[rownames(data$issues) %in% c(14:15, 22, 29, 39:41, 45:49), ],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$issues[rownames(data$issues) %in% c(16:19, 23:25, 30, 42), ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(26, 31:36), ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$mails[0, ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$mails[0, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$mails[0, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$pasta,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$pasta,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$pasta,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$pasta,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$synchronicity,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$synchronicity,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$synchronicity,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$synchronicity,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$issues) + 10,
                                        activity.type = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (too-large activity amount).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 59,
        split.basis = "issues",
        split.sliding.window = FALSE,
        split.revisions = c("2013-04-21 23:52:09", "2017-05-23 12:32:40"),
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
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$commits
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$issues
        ),
        mails = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 14:17, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (too-large activity amount).")

    ##
    ## split by number of windows
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2016-07-12 16:02:02",
        "2016-07-12 16:02:02-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (number.windows).")
    })
    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "splitting must mot modify the original ProjectConf.")
    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 21,
        split.basis = "issues",
        split.sliding.window = FALSE,
        split.revisions = c("2013-04-21 23:52:09", "2016-07-12 16:02:02", "2017-05-23 12:32:40"),
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
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$commits[1:2, ],
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$commits[3:8, ]
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$commit.messages,
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$issues[rownames(data$issues) %in% c(1:14, 20:22, 27:28, 37:40, 43:44), ],
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(15:19, 23:26, 29:36, 41:42, 45:49), ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 16:17, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$pasta,
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:02" = data$synchronicity,
            "2016-07-12 16:02:02-2017-05-23 12:32:40" = data$synchronicity
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues.filtered()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues", number.windows = nrow(project.data$get.issues.filtered()) + 10),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues", number.windows = 0),
        info = "Error expected (number.windows) (2)."
    )
})


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
