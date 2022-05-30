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
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2019 by Anselm Fehnker <fehnker@fim.uni-passau.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## All Rights Reserved.

context("Splitting functionality, time-based splitting of data.")

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
## Split data --------------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.data.time.based(..., split.basis = 'commits')
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

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
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[15, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[16, ]
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., split.basis = 'mails')
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

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
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$mails[1:2, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$mails[3:12, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$mails[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$mails[13:16, ]
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., split.basis = 'issues')
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'issues').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

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
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$mails[13:16, ],
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * time period, sliding windows ----------------------------------------

##
## Tests for split.data.time.based(..., split.basis = 'commits'), using sliding windows
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'commits', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
    results = split.data.time.based(project.data, time.period = "3 min",
                                    split.basis = "commits", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:01:59",
        "2016-07-12 16:00:29-2016-07-12 16:03:29",
        "2016-07-12 16:01:59-2016-07-12 16:04:59",
        "2016-07-12 16:03:29-2016-07-12 16:06:29",
        "2016-07-12 16:04:59-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 min",
        split.basis = "commits",
        split.sliding.window = TRUE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:00:29", "2016-07-12 16:01:59",
                            "2016-07-12 16:03:29", "2016-07-12 16:04:59", "2016-07-12 16:06:29",
                            "2016-07-12 16:06:33"),
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
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$commits[2, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$commits[0, ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$commits[3:5, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commits[3:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commit.messages,
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$commit.messages,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$commit.messages,
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$commit.messages,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$issues[rownames(data$issues) %in% c(14, 20:22, 37:40), ],
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$issues[rownames(data$issues) %in% c(14:15, 40, 47:49), ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$issues[rownames(data$issues) %in% c(15, 29, 47:49), ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$issues[rownames(data$issues) %in% c(29,41,45,46), ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% c(23,41,45,46), ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$mails[0, ],
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$mails[0, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[15, ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$mails[15:16, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[16, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$pasta,
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$pasta,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$pasta,
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$pasta,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$synchronicity,
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$synchronicity,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$synchronicity,
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$synchronicity,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$synchronicity
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
    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., split.basis = 'mails'), using sliding windows
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'mails', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
    results = split.data.time.based(project.data, time.period = "3 years",
                                    split.basis = "mails", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2007-10-10 12:38:13",
        "2006-04-10 15:38:13-2009-04-10 09:38:13",
        "2007-10-10 12:38:13-2010-10-10 06:38:13",
        "2009-04-10 09:38:13-2012-04-10 03:38:13",
        "2010-10-10 06:38:13-2013-10-10 00:38:13",
        "2012-04-10 03:38:13-2015-04-10 21:38:13",
        "2013-10-10 00:38:13-2016-07-12 16:05:38"
    )

    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "3 years",
        split.basis = "mails",
        split.sliding.window = TRUE,
        split.revisions = c("2004-10-09 18:38:13", "2006-04-10 15:38:13", "2007-10-10 12:38:13",
                            "2009-04-10 09:38:13", "2010-10-10 06:38:13", "2012-04-10 03:38:13",
                            "2013-10-10 00:38:13", "2015-04-10 21:38:13", "2016-07-12 16:05:38"),
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
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$commits[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$commits[0, ],
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$commits[0, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$commits[0, ],
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$commits[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$commit.messages,
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$commit.messages,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$commit.messages,
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$commit.messages,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$commit.messages,
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$commit.messages,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$issues[0, ],
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$issues[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$issues[0, ],
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$issues[0, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 27:29, 37:40, 43:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$mails[1:2, ],
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$mails[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$mails[3:12, ],
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$mails[3:12, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$mails[0, ],
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$mails[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$mails[13:16, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$pasta,
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$pasta,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$pasta,
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$pasta,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$pasta,
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$pasta,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$synchronicity,
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$synchronicity,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$synchronicity,
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$synchronicity,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$synchronicity,
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$synchronicity,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$synchronicity
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
    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., split.basis = 'issues'), using sliding windows
##

patrick::with_parameters_test_that("Split a data object time-based (split.basis = 'issues', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
    results = split.data.time.based(project.data, time.period = "2 years",
                                    split.basis = "issues", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2015-04-22 11:52:09",
        "2014-04-22 05:52:09-2016-04-21 17:52:09",
        "2015-04-22 11:52:09-2017-04-21 23:52:09",
        "2016-04-21 17:52:09-2017-05-23 12:32:40"
    )

    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "2 years",
        split.basis = "issues",
        split.sliding.window = TRUE,
        split.revisions = c("2013-04-21 23:52:09", "2014-04-22 05:52:09", "2015-04-22 11:52:09",
                            "2016-04-21 17:52:09", "2017-04-21 23:52:09", "2017-05-23 12:32:40"),
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
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$commits[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commits,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$commits
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$commit.messages,
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$commit.messages,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commit.messages,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$issues[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$issues[rownames(data$issues) %in% c(14:34, 37:49), ],
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(14:36, 37:49), ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$mails[0, ],
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$mails[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$mails[13:16, ],
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$mails[13:16, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$pasta,
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$pasta,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$pasta,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$synchronicity,
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$synchronicity,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$synchronicity,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$synchronicity
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
    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * bins ----------------------------------------------------------------

##
## Tests for split.data.time.based(..., bins = ...)
##

patrick::with_parameters_test_that("Split a data object time-based (bins = ... ).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

##
## Tests for split.data.time.based(..., bins = ...), sliding windows parameter ignored
##

patrick::with_parameters_test_that("Split a data object time-based (bins = ... , sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
    results = split.data.time.based(project.data, bins = c("2016-01-01 00:00:00", "2016-12-31 23:59:59",
                                                           "2017-06-03 03:03:03"),
                                    split.basis = "mails", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2016-01-01 00:00:00-2016-12-31 23:59:59",
        "2016-12-31 23:59:59-2017-06-03 03:03:03"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = c("2016-01-01 00:00:00", "2016-12-31 23:59:59", "2017-06-03 03:03:03"),
        split.basis = NULL,
        split.sliding.window = FALSE,
        split.revisions = c("2016-01-01 00:00:00", "2016-12-31 23:59:59", "2017-06-03 03:03:03"),
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
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commits,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$commits[0, ]
        ),
        commit.messages = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commit.messages,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$commit.messages
        ),
        issues = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$issues[rownames(data$issues) %in% c(14:34, 37:49), ],
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$issues[rownames(data$issues) %in% 35:36, ]
        ),
        mails = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$mails[rownames(data$mails) %in% 13:17, ],
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$mails[0, ]
        ),
        pasta = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$pasta,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$pasta
        ),
        synchronicity = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$synchronicity,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$synchronicity
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
    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * ranges --------------------------------------------------------------

##
## Test splitting data by network names.
##

patrick::with_parameters_test_that("Test splitting data by networks", {
    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

##
## Test splitting data by ranges.
##

patrick::with_parameters_test_that("Test splitting data by ranges", {
    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("commits.filter.base.artifact", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
        commits = lapply(expected.results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(expected.results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(expected.results, function(cf.data) cf.data$get.issues()),
        mails = lapply(expected.results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(expected.results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(expected.results, function(cf.data) cf.data$get.synchronicity())
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))

## * * window numbers ---------------------------------------------------------

##
## Tests for split.data.time.based(..., number.windows = ..., split.basis = 'commits')
##

patrick::with_parameters_test_that("Split a data object time-based with equal-sized windows (number.windows = ..., split.basis = 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information

    expected.config = list(
        split.type = "time-based",
        split.length = "2M 31S",
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
            "2016-07-12 16:04:01-2016-07-12 16:06:33" = data$mails[15:16, ]
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

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., number.windows = ..., split.basis = 'mails')
##

patrick::with_parameters_test_that("Split a data object time-based with equal-sized windows (number.windows = ..., split.basis = 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "2y 0m 342d 23H 21M 51S",
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
            "2004-10-09 18:38:13-2007-09-18 06:00:04" = data$mails[1:2, ],
            "2007-09-18 06:00:04-2010-08-26 17:21:55" = data$mails[3:12, ],
            "2010-08-26 17:21:55-2013-08-04 04:43:46" = data$mails[0, ],
            "2013-08-04 04:43:46-2016-07-12 16:05:38" = data$mails[13:16, ]
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

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")
}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))


##
## Tests for split.data.time.based(..., number.windows = ..., split.basis = 'issues')
##

patrick::with_parameters_test_that("Split a data object time-based with equal-sized windows (number.windows = ..., split.basis = 'issues').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    proj.conf$update.values(list(pasta = test.pasta, synchronicity = test.synchronicity))
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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "time-based",
        split.length = "1y 0m 132d 6H 13M 30S",
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
            "2016-01-12 00:19:09-2017-05-23 12:32:40" = data$mails[13:16, ]
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

    expected.data = lapply(expected.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))
    results.data = lapply(results.data, function(d) lapply(d, function (df) {
        row.names(df) = NULL
        return(df)
    }))

    expect_equal(results.data, expected.data, info = "Data for ranges.")

}, patrick::cases(
    "pasta, synchronicity: FALSE" = list(test.pasta = FALSE, test.synchronicity = FALSE),
    "pasta, synchronicity: TRUE" = list(test.pasta = TRUE, test.synchronicity = TRUE)
))
