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
## Split data --------------------------------------------------------------

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
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits", number.windows = nrow(project.data$get.commits.unfiltered()) + 10),
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
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
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
                 info = "Splitting must mot modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues", number.windows = nrow(project.data$get.issues()) + 10),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues", number.windows = 0),
        info = "Error expected (number.windows) (2)."
    )
})


## * * sliding windows
##
## Tests for split.data.activity.based(..., activity.type = 'commits') using sliding windows
##

test_that("Split a data object activity-based (activity.type = 'commits', sliding.window = TRUE).", {

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
    results = split.data.activity.based(project.data, activity.amount = 3,
                                        activity.type = "commits", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:10",
        "2016-07-12 16:00:45-2016-07-12 16:06:20",
        "2016-07-12 16:06:10-2016-07-12 16:06:32",
        "2016-07-12 16:06:20-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (activity.amount).")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 3,
        split.basis = "commits",
        split.sliding.window = TRUE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:06:10",
                            "2016-07-12 16:06:20", "2016-07-12 16:06:32", "2016-07-12 16:06:33"),
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
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commits[2:4, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commits[4:6, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commit.messages,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commit.messages,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commit.messages,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:41, 45:49), ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 29, 40:41, 45:49), ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$mails[0, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity
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
    expect_equal(results.data, expected.data, info = "Data for ranges (activity.amount).")

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$commits) + 10,
                                        activity.type = "commits", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 18,
        split.basis = "commits",
        split.sliding.window = TRUE,
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges for too-large activity amount (activity.amount).")

    ##
    ## split by number of windows (i.e., ignoring sliding windows)
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "commits", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows (i.e., ignoring sliding windows)

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits",
                                  number.windows = nrow(project.data$get.commits.unfiltered()) + 10, sliding.window = TRUE),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits", number.windows = 0, sliding.window = TRUE),
        info = "Error expected (number.windows) (2)."
    )

})

test_that("Split a data object activity-based (activity.type = 'commits', sliding.window = TRUE), continued.", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)

    ## add one commit to the commit data having same date as latest commit
    commit.data = project.data$get.commits.unfiltered()
    latest.commit = commit.data[nrow(commit.data), ]
    latest.commit[1, "commit.id"] = "<commit-added>"
    latest.commit[1, "hash"] = "abcdefghijklmnopqrstuvxyz"
    commit.data = rbind(commit.data, latest.commit)
    project.data$set.commits(commit.data)

    data = list(
        commits = project.data$get.commits.unfiltered(),
        commit.messages = project.data$get.commit.messages(),
        issues = project.data$get.issues(),
        mails = project.data$get.mails(),
        pasta = project.data$get.pasta(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 3,
                                        activity.type = "commits", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:10",
        "2016-07-12 16:00:45-2016-07-12 16:06:20",
        "2016-07-12 16:06:10-2016-07-12 16:06:32",
        "2016-07-12 16:06:20-2016-07-12 16:06:33",
        "2016-07-12 16:06:32-2016-07-12 16:06:33"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected,
                     info = "Time ranges (activity.amount).")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 3,
        split.basis = "commits",
        split.sliding.window = TRUE,
        split.revisions = c("2016-07-12 15:58:59", "2016-07-12 16:00:45", "2016-07-12 16:06:10",
                            "2016-07-12 16:06:20", "2016-07-12 16:06:32", "2016-07-12 16:06:33",
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
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commits[1:3, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commits[2:4, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commits[4:6, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commits[7:9, ]
        ),
        commit.messages = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commit.messages,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commit.messages,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commit.messages,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commit.messages,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commit.messages
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:41, 45:49), ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 29, 40:41, 45:49), ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$issues[0, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$mails[0, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$pasta
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$synchronicity
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
    expect_equal(results.data, expected.data, info = "Data for ranges (activity.amount).")

})


##
## Tests for split.data.activity.based(..., activity.type = 'mails') using sliding windows
##

test_that("Split a data object activity-based (activity.type = 'mails', sliding.window = TRUE).", {

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
    results = split.data.activity.based(project.data, activity.amount = 3,
                                        activity.type = "mails", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2010-07-12 11:05:35",
        "2005-02-09 18:49:49-2010-07-12 12:05:34",
        "2010-07-12 11:05:35-2010-07-12 12:05:41",
        "2010-07-12 12:05:34-2010-07-12 12:05:42",
        "2010-07-12 12:05:41-2010-07-12 12:05:44",
        "2010-07-12 12:05:42-2010-07-12 12:05:45",
        "2010-07-12 12:05:44-2016-07-12 15:58:40",
        "2010-07-12 12:05:45-2016-07-12 15:58:50",
        "2016-07-12 15:58:40-2016-07-12 16:05:37",
        "2016-07-12 15:58:50-2016-07-12 16:05:38"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 3,
        split.basis = "mails",
        split.sliding.window = TRUE,
        split.revisions = c("2004-10-09 18:38:13", "2005-02-09 18:49:49", "2010-07-12 11:05:35",
                            "2010-07-12 12:05:34", "2010-07-12 12:05:41", "2010-07-12 12:05:42",
                            "2010-07-12 12:05:44", "2010-07-12 12:05:45", "2016-07-12 15:58:40",
                            "2016-07-12 15:58:50", "2016-07-12 16:05:37", "2016-07-12 16:05:38"),
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
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$commits[0, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$commits[0, ],
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$commits[0, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$commits[0, ],
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$commits[0, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$commits[0, ],
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$commits[0, ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$commits[1:2, ],
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        commit.messages = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$commit.messages,
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$commit.messages,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$commit.messages,
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$commit.messages,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$commit.messages,
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$commit.messages,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$commit.messages,
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$commit.messages,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$commit.messages,
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$commit.messages
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$issues[0, ],
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$issues[0, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$issues[0, ],
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$issues[0, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$issues[0, ],
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$issues[0, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$issues[rownames(data$issues) %in% c(1:13, 27:28, 43:44), ],
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$issues[rownames(data$issues) %in% c(1:13, 27:28, 43:44), ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:40, 45:49), ],
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29, 37:40, 45:49), ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$mails[rownames(data$mails) %in% 1:3, ],
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$mails[rownames(data$mails) %in% 2:4, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$mails[rownames(data$mails) %in% 4:6, ],
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$mails[rownames(data$mails) %in% 5:7, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$mails[rownames(data$mails) %in% 7:9, ],
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$mails[rownames(data$mails) %in% 8:10, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$mails[rownames(data$mails) %in% 10:12, ],
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$mails[rownames(data$mails) %in% c(11:12, 14), ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$mails[rownames(data$mails) %in% 14:16, ],
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 15:17, ]
        ),
        pasta = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$pasta,
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$pasta,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$pasta,
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$pasta,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$pasta,
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$pasta,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$pasta,
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$pasta,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$pasta,
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$pasta
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$synchronicity,
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$synchronicity,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$synchronicity,
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$synchronicity,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$synchronicity,
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$synchronicity,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$synchronicity,
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$synchronicity,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$synchronicity,
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$synchronicity
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

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$mails) + 10,
                                        activity.type = "mails", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 26,
        split.basis = "mails",
        split.sliding.window = TRUE,
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
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29, 37:40, 43:49), ]
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (too-large activity amount).")

    ##
    ## split by number of windows (i.e., ignoring sliding windows)
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "mail", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

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
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29, 37:40, 43:49), ]
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows (i.e., ignoring sliding windows)

    expect_error(
        split.data.activity.based(project.data, activity.type = "mails",
                                  number.windows = nrow(project.data$get.mails()) + 10, sliding.window = TRUE),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "mails", number.windows = 0, sliding.window = TRUE),
        info = "Error expected (number.windows) (2)."
    )
})


##
## Tests for split.data.activity.based(..., activity.type = 'issues') using sliding windows
##

test_that("Split a data object activity-based (activity.type = 'issues', sliding.window = TRUE).", {

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
    results = split.data.activity.based(project.data, activity.amount = 9,
                                        activity.type = "issues", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2013-05-25 06:22:23",
        "2013-05-06 01:04:34-2016-07-12 15:30:02",
        "2013-05-25 06:22:23-2016-07-12 15:59:59",
        "2016-07-12 15:30:02-2016-07-12 16:02:02",
        "2016-07-12 15:59:59-2016-07-12 16:06:30",
        "2016-07-12 16:02:02-2016-07-27 20:12:08",
        "2016-07-12 16:06:30-2016-10-05 15:30:02",
        "2016-07-27 20:12:08-2017-05-23 12:31:34",
        "2016-10-05 15:30:02-2017-05-23 12:32:40"
    )
    lapply(results, function(res) {
        expect_equal(res$get.project.conf()$get.value("ranges"), expected, info = "Time ranges.")
    })

    ## This value should not change, so we compare it with the default, which is `c("v1-v2", "v2-v3")`.
    expect_equal(proj.conf$get.value("ranges"), c("v1-v2", "v2-v3"),
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 9,
        split.basis = "issues",
        split.sliding.window = TRUE,
        split.revisions = c("2013-04-21 23:52:09", "2013-05-06 01:04:34", "2013-05-25 06:22:23",
                            "2016-07-12 15:30:02", "2016-07-12 15:59:59", "2016-07-12 16:02:02",
                            "2016-07-12 16:06:30", "2016-07-27 20:12:08", "2016-10-05 15:30:02",
                            "2017-05-23 12:31:34", "2017-05-23 12:32:40"),
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
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$commits[0, ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$commits[1, ],
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$commits[1:2, ],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$commits[2:5, ],
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$commits[3:8, ],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$commits[6:8, ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$commits[0, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$commits[0, ]
        ),
        commit.messages = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$commit.messages,
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$commit.messages,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$commit.messages,
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$commit.messages,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$commit.messages,
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$commit.messages,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$commit.messages,
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$commit.messages,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$commit.messages
        ),
        issues = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$issues[rownames(data$issues) %in% 1:10, ],
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$issues[rownames(data$issues) %in% c(6:13, 43:44), ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$issues[rownames(data$issues) %in% c(11:13, 20:21, 27:28, 37:38, 43:44), ],
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$issues[rownames(data$issues) %in% c(14, 20:22, 27:28, 37:40),],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$issues[rownames(data$issues) %in% c(14:15, 22, 29, 39:41, 45:49), ],
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$issues[rownames(data$issues) %in% c(15:17, 23, 29, 41:42, 45:49),],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$issues[rownames(data$issues) %in% c(16:19, 23:25, 30, 42), ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$issues[rownames(data$issues) %in% c(18:19, 24:26, 30:34), ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(26, 31:36), ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$mails[0, ],
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$mails[0, ],
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$mails[0, ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$mails[0, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$mails[0, ]
        ),
        pasta = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$pasta,
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$pasta,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$pasta,
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$pasta,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$pasta,
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$pasta,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$pasta,
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$pasta,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$pasta
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$synchronicity,
            "2013-05-06 01:04:34-2016-07-12 15:30:02" = data$synchronicity,
            "2013-05-25 06:22:23-2016-07-12 15:59:59" = data$synchronicity,
            "2016-07-12 15:30:02-2016-07-12 16:02:02" = data$synchronicity,
            "2016-07-12 15:59:59-2016-07-12 16:06:30" = data$synchronicity,
            "2016-07-12 16:02:02-2016-07-27 20:12:08" = data$synchronicity,
            "2016-07-12 16:06:30-2016-10-05 15:30:02" = data$synchronicity,
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$synchronicity,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$synchronicity
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

    ##
    ## split by too-large activity amount
    ##

    ## split data
    results = split.data.activity.based(project.data, activity.amount = nrow(data$issues) + 10,
                                        activity.type = "issues", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

    ## test that the config contains the correct splitting information
    expected.config = list(
        split.type = "activity-based",
        split.length = 59,
        split.basis = "issues",
        split.sliding.window = TRUE,
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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (too-large activity amount).")

    ##
    ## split by number of windows (i.e., ignoring sliding windows)
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "issues", sliding.window = TRUE)

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
                 info = "Splitting must not modify the original ProjectConf.")

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
        commits = lapply(results, function(cf.data) cf.data$get.commits.unfiltered()),
        commit.messages = lapply(results, function(cf.data) cf.data$get.commit.messages()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows (i.e., ignoring sliding windows)

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues",
                                  number.windows = nrow(project.data$get.issues()) + 10, sliding.window = TRUE),
        info = "Error expected (number.windows) (1)."
    )

    expect_error(
        split.data.activity.based(project.data, activity.type = "issues", number.windows = 0, sliding.window = TRUE),
        info = "Error expected (number.windows) (2)."
    )
})
