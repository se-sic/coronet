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
## All Rights Reserved.


context("Splitting functionality, using sliding windows.")

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

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.data.time.based(..., split.basis = 'commits'), using sliding windows
##

test_that("Split a data object time-based (split.basis == 'commits', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")

    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commits[1:2, ],
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$commits[2, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$commits[0, ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$commits[3:5, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commits[3:8, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$mails[0, ],
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$mails[0, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[rownames(data$mails) == 16, ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$mails[rownames(data$mails) %in% c(16, 17), ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) == 17, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$issues[rownames(data$issues) %in% c(14, 20:22), ],
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$issues[rownames(data$issues) %in% c(14:15), ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$issues[rownames(data$issues) %in% c(15, 29), ],
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$issues[rownames(data$issues) == 29, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$synchronicity,
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$synchronicity,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$synchronicity,
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$synchronicity,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$pasta,
            "2016-07-12 16:00:29-2016-07-12 16:03:29" = data$pasta,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$pasta,
            "2016-07-12 16:03:29-2016-07-12 16:06:29" = data$pasta,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})


##
## Tests for split.data.time.based(..., split.basis = 'mails'), using sliding windows
##

test_that("Split a data object time-based (split.basis == 'mails', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")

    expect_equal(result, expected, info = "Time ranges.")

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
        mails = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$mails[rownames(data$mails) %in% 1:2, ],
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$mails[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$mails[0, ],
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$mails[0, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        issues = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$issues[0, ],
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$issues[0, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$issues[0, ],
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$issues[0, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 27:29), ]
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$synchronicity,
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$synchronicity,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$synchronicity,
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$synchronicity,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$synchronicity,
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$synchronicity,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$synchronicity
        ),
        pasta = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$pasta,
            "2006-04-10 15:38:13-2009-04-10 09:38:13" = data$pasta,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$pasta,
            "2009-04-10 09:38:13-2012-04-10 03:38:13" = data$pasta,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$pasta,
            "2012-04-10 03:38:13-2015-04-10 21:38:13" = data$pasta,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})


##
## Tests for split.data.time.based(..., split.basis = 'issues'), using sliding windows
##

test_that("Split a data object time-based (split.basis == 'issues', sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")

    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$commits[0, ],
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$commits[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commits,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$commits
        ),
        mails = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$mails[0, ],
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$mails[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$mails[rownames(data$mails) %in% 14:17, ],
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 14:17, ]
        ),
        issues = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$issues[rownames(data$issues) %in% 1:13, ],
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$issues[0, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$issues[rownames(data$issues) %in% 14:34, ],
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% 14:36, ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$synchronicity,
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$synchronicity,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$synchronicity,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$pasta,
            "2014-04-22 05:52:09-2016-04-21 17:52:09" = data$pasta,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$pasta,
            "2016-04-21 17:52:09-2017-05-23 12:32:40" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})

## * * bins ----------------------------------------------------------------

##
## Tests for split.data.time.based(..., bins = ...), sliding windows parameter ignored
##

test_that("Split a data object time-based (bins == ... , sliding.window = TRUE).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("issues.only.comments", FALSE)
    net.conf = NetworkConf$new()

    ## data object
    project.data = ProjectData$new(proj.conf)
    data = list(
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commits,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$commits[0, ]
        ),
        mails = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$mails[rownames(data$mails) %in% 13:17, ],
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$mails[0, ]
        ),
        issues = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$issues[rownames(data$issues) %in% 14:34, ],
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$issues[rownames(data$issues) %in% 35:36, ]
        ),
        synchronicity = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$synchronicity,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$synchronicity
        ),
        pasta = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$pasta,
            "2016-12-31 23:59:59-2017-06-03 03:03:03" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})

## * activity-based --------------------------------------------------------

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
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (activity.amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commits[1:3, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commits[2:4, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commits[4:6, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$mails[0, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29), ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 29), ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (too-large activity amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$commits
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) %in% 16:17, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% c(14:15, 20:23, 29), ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$commits[1:4, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29), ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges (number.windows).")

    ## too large number of windows (i.e., ignoring sliding windows)

    expect_error(
        split.data.activity.based(project.data, activity.type = "commits",
                                  number.windows = nrow(project.data$get.commits()) + 10, sliding.window = TRUE),
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
    commit.data = project.data$get.commits()
    latest.commit = commit.data[nrow(commit.data), ]
    latest.commit[1, "commit.id"] = "<commit-added>"
    latest.commit[1, "hash"] = "abcdefghijklmnopqrstuvxyz"
    commit.data = rbind(commit.data, latest.commit)
    project.data$set.commits(commit.data)

    data = list(
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (activity.amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commits[1:3, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$commits[2:4, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$commits[4:6, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$commits[5:8, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commits[7:9, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$mails[0, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$mails[0, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$mails[0, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29), ],
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$issues[rownames(data$issues) %in% c(14:15, 29), ],
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$issues[rownames(data$issues) == 23, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$issues[0, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$synchronicity,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:00:45-2016-07-12 16:06:20" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:32" = data$pasta,
            "2016-07-12 16:06:20-2016-07-12 16:06:33" = data$pasta,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

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
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$issues[0, ],
            "2005-02-09 18:49:49-2010-07-12 12:05:34" = data$issues[0, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$issues[0, ],
            "2010-07-12 12:05:34-2010-07-12 12:05:42" = data$issues[0, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$issues[0, ],
            "2010-07-12 12:05:42-2010-07-12 12:05:45" = data$issues[0, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$issues[rownames(data$issues) %in% c(1:13, 27:28), ],
            "2010-07-12 12:05:45-2016-07-12 15:58:50" = data$issues[rownames(data$issues) %in% c(1:13, 27:28), ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29), ],
            "2016-07-12 15:58:50-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(14:15, 20:22, 29), ]
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
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (too-large activity amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$mails
        ),
        issues = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29), ]
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$synchronicity
        ),
        pasta = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$commits[0, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$commits[1:2, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$mails[rownames(data$mails) %in% 1:8, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 9:17, ]
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$issues[0, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:15, 20:22, 27:29), ]
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$synchronicity,
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$synchronicity
        ),
        pasta = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$pasta,
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
        commits = project.data$get.commits(),
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 9,
                                        activity.type = "issues", sliding.window = TRUE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2013-05-25 06:22:23",
        "2013-05-06 01:04:34-2016-07-12 15:59:25",
        "2013-05-25 06:22:23-2016-07-12 16:03:59",
        "2016-07-12 15:59:25-2016-07-27 20:12:08",
        "2016-07-12 16:03:59-2016-10-05 15:30:02",
        "2016-07-27 20:12:08-2017-05-23 12:31:34",
        "2016-10-05 15:30:02-2017-05-23 12:32:40"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$commits[0, ],
            "2013-05-06 01:04:34-2016-07-12 15:59:25" = data$commits[1, ],
            "2013-05-25 06:22:23-2016-07-12 16:03:59" = data$commits[1:2, ],
            "2016-07-12 15:59:25-2016-07-27 20:12:08" = data$commits[2:8, ],
            "2016-07-12 16:03:59-2016-10-05 15:30:02" = data$commits[3:8, ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$commits[0, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$commits[0, ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$mails[0, ],
            "2013-05-06 01:04:34-2016-07-12 15:59:25" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2013-05-25 06:22:23-2016-07-12 16:03:59" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 15:59:25-2016-07-27 20:12:08" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:03:59-2016-10-05 15:30:02" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$mails[0, ],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$mails[0, ]
        ),
        issues = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$issues[rownames(data$issues) %in% 1:10, ],
            "2013-05-06 01:04:34-2016-07-12 15:59:25" = data$issues[rownames(data$issues) %in% c(6:13, 27:28), ],
            "2013-05-25 06:22:23-2016-07-12 16:03:59" = data$issues[rownames(data$issues) %in% c(11:15, 20:22, 27:28), ],
            "2016-07-12 15:59:25-2016-07-27 20:12:08" = data$issues[rownames(data$issues) %in% c(14:17, 20:23, 29),],
            "2016-07-12 16:03:59-2016-10-05 15:30:02" = data$issues[rownames(data$issues) %in% c(16:19, 23:25, 29:30), ],
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$issues[rownames(data$issues) %in% c(18:19, 24:26, 30:34),],
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(26, 31:36), ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$synchronicity,
            "2013-05-06 01:04:34-2016-07-12 15:59:25" = data$synchronicity,
            "2013-05-25 06:22:23-2016-07-12 16:03:59" = data$synchronicity,
            "2016-07-12 15:59:25-2016-07-27 20:12:08" = data$synchronicity,
            "2016-07-12 16:03:59-2016-10-05 15:30:02" = data$synchronicity,
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$synchronicity,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2013-05-25 06:22:23" = data$pasta,
            "2013-05-06 01:04:34-2016-07-12 15:59:25" = data$pasta,
            "2013-05-25 06:22:23-2016-07-12 16:03:59" = data$pasta,
            "2016-07-12 15:59:25-2016-07-27 20:12:08" = data$pasta,
            "2016-07-12 16:03:59-2016-10-05 15:30:02" = data$pasta,
            "2016-07-27 20:12:08-2017-05-23 12:31:34" = data$pasta,
            "2016-10-05 15:30:02-2017-05-23 12:32:40" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (too-large activity amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$commits
        ),
        mails = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 14:17, ]
        ),
        issues = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$issues
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2017-05-23 12:32:40" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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
        "2013-04-21 23:52:09-2016-07-12 16:02:30",
        "2016-07-12 16:02:30-2017-05-23 12:32:40"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:30" = data$commits[1:2, ],
            "2016-07-12 16:02:30-2017-05-23 12:32:40" = data$commits[3:8, ]
        ),
        mails = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:30" = data$mails[rownames(data$mails) %in% 14:15, ],
            "2016-07-12 16:02:30-2017-05-23 12:32:40" = data$mails[rownames(data$mails) %in% 16:17, ]
        ),
        issues = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:30" = data$issues[rownames(data$issues) %in% c(1:14, 20:22, 27:28), ],
            "2016-07-12 16:02:30-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(15:19, 23:26, 29:36), ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:30" = data$synchronicity,
            "2016-07-12 16:02:30-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2016-07-12 16:02:30" = data$pasta,
            "2016-07-12 16:02:30-2017-05-23 12:32:40" = data$pasta
        )
    )
    results.data = list(
        commits = lapply(results, function(cf.data) cf.data$get.commits()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        issues = lapply(results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(results, function(cf.data) cf.data$get.pasta())
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


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split network -----------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

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

## * activity-based ------------------------------------------------------------

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
