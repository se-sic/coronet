## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de

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

## - sliding.window = TRUE
## - net.conf$update.values(list(pasta = TRUE, synchronicity = TRUE))


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split data --------------------------------------------------------------

## * time-based ------------------------------------------------------------

## * * time period ---------------------------------------------------------

##
## Tests for split.data.time.based(..., split.basis = 'commits')
##

test_that("Split a data object time-based (split.basis == 'commits').", {

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
                                    split.basis = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:01:59",
        "2016-07-12 16:01:59-2016-07-12 16:04:59",
        "2016-07-12 16:04:59-2016-07-12 16:06:33"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commits[1:4, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data.frame(),
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commits[5:9, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data.frame(),
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[rownames(data$mails) == 16, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) == 17, ]
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$issues[rownames(data$issues) %in% 18:20, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$issues[rownames(data$issues) %in% 21, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% 22, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$synchronicity,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$synchronicity,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$pasta,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$pasta,
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
## Tests for split.data.time.based(..., split.basis = 'mails')
##

test_that("Split a data object time-based (split.basis == 'mails').", {

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
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2007-10-10 12:38:13",
        "2007-10-10 12:38:13-2010-10-10 06:38:13",
        "2010-10-10 06:38:13-2013-10-10 00:38:13",
        "2013-10-10 00:38:13-2016-07-12 16:05:38"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data.frame(),
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data.frame(),
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data.frame(),
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$commits[1:4, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$mails[rownames(data$mails) %in% 1:2, ],
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data.frame(),
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        issues = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data.frame(),
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data.frame(),
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$issues[rownames(data$issues) %in% 1:6, ],
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(8:9, 18:21), ]
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$synchronicity,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$synchronicity,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$synchronicity,
            "2013-10-10 00:38:13-2016-07-12 16:05:38" = data$synchronicity
        ),
        pasta = list(
            "2004-10-09 18:38:13-2007-10-10 12:38:13" = data$pasta,
            "2007-10-10 12:38:13-2010-10-10 06:38:13" = data$pasta,
            "2010-10-10 06:38:13-2013-10-10 00:38:13" = data$pasta,
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
## Tests for split.data.time.based(..., split.basis = 'issues')
##

test_that("Split a data object time-based (split.basis == 'issues').", {

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
                                    split.basis = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2015-04-22 11:52:09",
        "2015-04-22 11:52:09-2017-04-21 23:52:09",
        "2017-04-21 23:52:09-2017-05-23 12:32:40"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data.frame(),
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$commits,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data.frame()
        ),
        mails = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data.frame(),
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$mails[rownames(data$mails) %in% 14:17, ],
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data.frame()
        ),
        issues = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$issues[rownames(data$issues) %in% 1:6, ],
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$issues[rownames(data$issues) %in% 7:33, ],
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% 34:36, ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$synchronicity,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$synchronicity,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2015-04-22 11:52:09" = data$pasta,
            "2015-04-22 11:52:09-2017-04-21 23:52:09" = data$pasta,
            "2017-04-21 23:52:09-2017-05-23 12:32:40" = data$pasta
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
## Tests for split.data.time.based(..., bins = ...)
##

test_that("Split a data object time-based (bins == ... ).", {

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
    results = split.data.time.based(project.data, bins = c("2016-01-01 00:00:00", "2016-12-31 23:59:59"),
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-01-01 00:00:00-2016-12-31 23:59:59"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$commits
        ),
        mails = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        issues = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$issues[rownames(data$issues) %in% 7:31, ]
        ),
        synchronicity = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$synchronicity
        ),
        pasta = list(
            "2016-01-01 00:00:00-2016-12-31 23:59:59" = data$pasta
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

## * * ranges --------------------------------------------------------------

##
## Test splitting data by network names.
##

test_that("Test splitting data by networks", {
    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
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
        complete = c("2004-10-09 18:38:13-2017-05-23 12:32:39",
                     "2004-10-09 18:38:13-2017-05-23 12:32:39",
                     "2004-10-09 18:38:13-2017-05-23 12:32:39")
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
    proj.conf$update.value("artifact.filter.base", FALSE)
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
        mails = lapply(expected.results, function(cf.data) cf.data$get.mails()),
        issues = lapply(expected.results, function(cf.data) cf.data$get.issues()),
        synchronicity = lapply(expected.results, function(cf.data) cf.data$get.synchronicity()),
        pasta = lapply(expected.results, function(cf.data) cf.data$get.pasta())
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
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 2,
                                    activity.type = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:05:41",
        "2016-07-12 16:05:41-2016-07-12 16:06:32",
        "2016-07-12 16:06:32-2016-07-12 16:06:33"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (activity.amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:05:41" = data$commits[1:4, ],
            "2016-07-12 16:05:41-2016-07-12 16:06:32" = data$commits[5:7, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$commits[8:9, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:05:41" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:05:41-2016-07-12 16:06:32" = data.frame(),
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data.frame()
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:05:41" = data$issues[rownames(data$issues) %in% 18:21, ],
            "2016-07-12 16:05:41-2016-07-12 16:06:32" = data$issues[rownames(data$issues) %in% 22, ],
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data.frame()
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:05:41" = data$synchronicity,
            "2016-07-12 16:05:41-2016-07-12 16:06:32" = data$synchronicity,
            "2016-07-12 16:06:32-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:05:41" = data$pasta,
            "2016-07-12 16:05:41-2016-07-12 16:06:32" = data$pasta,
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
            "2016-07-12 15:58:59-2016-07-12 16:06:33" = data$issues[rownames(data$issues) %in% 18:22, ]
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
    ## split by number of windows
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "commits", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-07-12 15:58:59-2016-07-12 16:06:10",
        "2016-07-12 16:06:10-2016-07-12 16:06:33"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$commits[1:6, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:33" = data$commits[7:9, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$mails[rownames(data$mails) %in% 16:17, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:33" = data.frame()
        ),
        issues = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$issues[rownames(data$issues) %in% 18:22, ],
            "2016-07-12 16:06:10-2016-07-12 16:06:33" = data.frame()
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$synchronicity,
            "2016-07-12 16:06:10-2016-07-12 16:06:33" = data$synchronicity
        ),
        pasta = list(
            "2016-07-12 15:58:59-2016-07-12 16:06:10" = data$pasta,
            "2016-07-12 16:06:10-2016-07-12 16:06:33" = data$pasta
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
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data.frame(),
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data.frame(),
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data.frame(),
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data.frame(),
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$commits[1:4, ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data.frame()
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$mails[rownames(data$mails) %in% 1:3, ],
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$mails[rownames(data$mails) %in% 4:6, ],
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$mails[rownames(data$mails) %in% 7:9, ],
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$mails[rownames(data$mails) %in% 10:12, ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$mails[rownames(data$mails) %in% 14:16, ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 17, ]
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data.frame(),
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data.frame(),
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data.frame(),
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$issues[rownames(data$issues) %in% c(1:6, 8:9), ],
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$issues[rownames(data$issues) %in% 18:21, ],
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data.frame()
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$synchronicity,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$synchronicity,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$synchronicity,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$synchronicity,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$synchronicity,
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$synchronicity
        ),
        pasta = list(
            "2004-10-09 18:38:13-2010-07-12 11:05:35" = data$pasta,
            "2010-07-12 11:05:35-2010-07-12 12:05:41" = data$pasta,
            "2010-07-12 12:05:41-2010-07-12 12:05:44" = data$pasta,
            "2010-07-12 12:05:44-2016-07-12 15:58:40" = data$pasta,
            "2016-07-12 15:58:40-2016-07-12 16:05:37" = data$pasta,
            "2016-07-12 16:05:37-2016-07-12 16:05:38" = data$pasta
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
                                        activity.type = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2016-07-12 16:05:38"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (too-large activity amount).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$commits[1:4, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$mails
        ),
        issues = list(
            "2004-10-09 18:38:13-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:6,8:9,18:21), ]
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
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data.frame(),
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$commits[1:4, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data$mails[rownames(data$mails) %in% 1:8, ],
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 9:17, ]
        ),
        issues = list(
            "2004-10-09 18:38:13-2010-07-12 12:05:43" = data.frame(),
            "2010-07-12 12:05:43-2016-07-12 16:05:38" = data$issues[rownames(data$issues) %in% c(1:6,8:9,18:21), ]
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
        mails = project.data$get.mails(),
        issues = project.data$get.issues(),
        synchronicity = project.data$get.synchronicity(),
        pasta = project.data$get.pasta()
    )

    ## split data
    results = split.data.activity.based(project.data, activity.amount = 8,
                                        activity.type = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2016-07-12 16:05:47",
        "2016-07-12 16:05:47-2016-08-31 18:21:48",
        "2016-08-31 18:21:48-2017-02-20 22:25:41",
        "2017-02-20 22:25:41-2017-05-23 12:32:40"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2016-07-12 16:05:47" = data$commits[1:6, ],
            "2016-07-12 16:05:47-2016-08-31 18:21:48" = data$commits[7:9, ],
            "2016-08-31 18:21:48-2017-02-20 22:25:41" = data.frame(),
            "2017-02-20 22:25:41-2017-05-23 12:32:40" = data.frame()
        ),
        mails = list(
            "2013-04-21 23:52:09-2016-07-12 16:05:47" = data$mails[rownames(data$mails) %in% 14:17, ],
            "2016-07-12 16:05:47-2016-08-31 18:21:48" = data.frame(),
            "2016-08-31 18:21:48-2017-02-20 22:25:41" = data.frame(),
            "2017-02-20 22:25:41-2017-05-23 12:32:40" = data.frame()
        ),
        issues = list(
            "2013-04-21 23:52:09-2016-07-12 16:05:47" = data$issues[rownames(data$issues) %in% c(1:6, 8:9, 18:21), ],
            "2016-07-12 16:05:47-2016-08-31 18:21:48" = data$issues[rownames(data$issues) %in% c(7, 10:17, 22), ],
            "2016-08-31 18:21:48-2017-02-20 22:25:41" = data$issues[rownames(data$issues) %in% 23:31, ],
            "2017-02-20 22:25:41-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% 32:36, ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2016-07-12 16:05:47" = data$synchronicity,
            "2016-07-12 16:05:47-2016-08-31 18:21:48" = data$synchronicity,
            "2016-08-31 18:21:48-2017-02-20 22:25:41" = data$synchronicity,
            "2017-02-20 22:25:41-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2016-07-12 16:05:47" = data$pasta,
            "2016-07-12 16:05:47-2016-08-31 18:21:48" = data$pasta,
            "2016-08-31 18:21:48-2017-02-20 22:25:41" = data$pasta,
            "2017-02-20 22:25:41-2017-05-23 12:32:40" = data$pasta
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
                                        activity.type = "issues", sliding.window = FALSE)

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
    ## split by number of windows
    ##

    ## split data
    results = split.data.activity.based(project.data, number.windows = 2,
                                        activity.type = "issues", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2013-04-21 23:52:09-2016-07-27 22:25:25",
        "2016-07-27 22:25:25-2017-05-23 12:32:40"
    )
    result = proj.conf$get.value("ranges")
    expect_equal(result, expected, info = "Time ranges (number.windows).")

    ## check data for all ranges
    expected.data = list(
        commits = list(
            "2013-04-21 23:52:09-2016-07-27 22:25:25" = data$commits,
            "2016-07-27 22:25:25-2017-05-23 12:32:40" = data.frame()
        ),
        mails = list(
            "2013-04-21 23:52:09-2016-07-27 22:25:25" = data$mails[rownames(data$mails) %in% 14:17, ],
            "2016-07-27 22:25:25-2017-05-23 12:32:40" = data.frame()
        ),
        issues = list(
            "2013-04-21 23:52:09-2016-07-27 22:25:25" = data$issues[rownames(data$issues) %in% c( 1:14, 18:22), ],
            "2016-07-27 22:25:25-2017-05-23 12:32:40" = data$issues[rownames(data$issues) %in% c(15:17, 23:36), ]
        ),
        synchronicity = list(
            "2013-04-21 23:52:09-2016-07-27 22:25:25" = data$synchronicity,
            "2016-07-27 22:25:25-2017-05-23 12:32:40" = data$synchronicity
        ),
        pasta = list(
            "2013-04-21 23:52:09-2016-07-27 22:25:25" = data$pasta,
            "2016-07-27 22:25:25-2017-05-23 12:32:40" = data$pasta
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
    proj.conf$update.value("artifact.filter.base", FALSE)
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

test_that("Split a list of networks time-based.", {

    ## time period
    time.period = "2 years"

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
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
        sliding.window = FALSE
    )

    ## check whether the splitting information of the two split networks are identical
    expect_identical(attributes(net.split[[1]]), attributes(net.split[[2]]), info = "Splitting information.")

})

## * * bins ----------------------------------------------------------------

##
## Tests for split.network.time.based(..., bins = ...)
##

test_that("Split a network time-based (bins = ...).", {

    ## bins
    bins = c("2016-07-12 15:58:00", "2016-07-12 16:00:59", "2016-07-12 16:02:59",
             "2016-07-12 16:04:59", "2016-07-12 17:21:43")

    ## configuration and data objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    proj.conf$update.value("artifact.filter.base", FALSE)
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
    results = split.network.time.based(author.net, bins = bins)

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

    expect_error(split.network.time.based(author.net, bins = bins), info = "Illegal split.")

})

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
    proj.conf$update.value("artifact.filter.base", FALSE)
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
    proj.conf$update.value("artifact.filter.base", FALSE)
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
    dates = c('2000-01-25', '2000-01-23', '2000-01-15', '2000-01-27', '2000-01-13',
              '2000-01-03', '2000-01-05', '2000-01-29', '2000-01-19', '2000-01-01',
              '2000-01-11', '2000-01-07', '2000-01-21', '2000-01-09', '2000-01-17')
    ## ## generated with:
    sprintf("c('%s')", paste(
        get.date.string(sample(
            seq.POSIXt(get.date.from.string("2000-01-01"), get.date.from.string("2000-02-01"), by = "1 days"),
            length.dates,
            replace = FALSE
        )), collapse = "', '"))

    ## generate bins
    bins = seq_len(length.bins)
    bins.vector = c('1', '3', '5', '4', '1', '3', '1', '3', '2', '5', '4', '2', '4', '3', '5')
    ## ## generated with:
    ## sprintf("c('%s')", paste( sample(bins, size = length.dates, replace = TRUE), collapse = "', '") )

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
    dates = c('2000-01-25', '2000-01-23', '2000-01-15', '2000-01-27', '2000-01-13',
              '2000-01-03', '2000-01-05', '2000-01-29', '2000-01-19', '2000-01-01',
              '2000-01-11', '2000-01-07', '2000-01-21', '2000-01-09', '2000-01-17')
    dates.posixct = get.date.from.string(dates)
    ## ## generated with:
    ## sprintf("c('%s')", paste(
    ##     get.date.string(sample(
    ##         seq.POSIXt(get.date.from.string("2000-01-01"), get.date.from.string("2000-02-01"), by = "1 days"),
    ##         length.dates,
    ##         replace = FALSE
    ##     )), collapse = "', '"))

    ##
    ## split.get.bins.time.based (1)
    ##

    ## results
    expected.bins  = c('2000-01-01 00:00:00', '2000-01-11 00:00:00', '2000-01-21 00:00:00', '2000-01-29 00:00:01')
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
    expected.bins  = c('2000-01-01 00:00:00', '2000-01-29 00:00:01')
    expected = list(
        vector = factor(head(expected.bins, -1))[ rep(1, length.dates) ],
        bins = expected.bins
    )
    results = split.get.bins.time.based(dates.posixct, "1 year")

    ## check result
    expect_equal(results, expected, info = "split.get.bins.time.based (2)")

    ##
    ## split.get.bins.activity.based (1)
    ##

    ## construct data.frame
    df = data.frame(date = dates.posixct, id = seq_len(length.dates))
    df = df[ order(df$date), ]

    ## results
    expected = list(
        vector = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4),
        bins = c('2000-01-01 00:00:00', '2000-01-09 00:00:00', '2000-01-17 00:00:00', '2000-01-25 00:00:00', '2000-01-29 00:00:01')
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
        bins = c('2000-01-01 00:00:00', '2000-01-29 00:00:01')
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
    proj.conf$update.value("artifact.filter.base", FALSE)
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
