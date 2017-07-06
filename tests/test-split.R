## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de


context("Splitting functionality.")

##
## Context
##

CF.DATA = file.path(".", "codeface-data")
CF.SELECTION.PROCESS = "testing"
CASESTUDY = "test"
ARTIFACT = "feature" # function, feature, file, featureexpression

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

## - activity-based
## - low-level functions
## - sliding.window = TRUE


##
## Tests for split.data.time.based(..., split.basis = 'commits')
##

test_that("Split a data object time-based (split.basis == 'commits').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    net.conf = NetworkConf$new()

    ## data object
    project.data = CodefaceProjectData$new(proj.conf, net.conf)
    data = list(
        commits.raw = project.data$get.commits.raw(),
        mails = project.data$get.mails(),
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
    result = proj.conf$get.entry("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits.raw = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$commits.raw[1:4, ],
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data.frame(),
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$commits.raw[5:9, ]
        ),
        mails = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data.frame(),
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$mails[rownames(data$mails) == 16, ],
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$mails[rownames(data$mails) == 17, ]
        ),
        synchronicity = list(
            "2016-07-12 15:58:59-2016-07-12 16:01:59" = data$synchronicity,
            "2016-07-12 16:01:59-2016-07-12 16:04:59" = data$synchronicity,
            "2016-07-12 16:04:59-2016-07-12 16:06:33" = data$synchronicity
        )
    )
    results.data = list(
        commits.raw = lapply(results, function(cf.data) cf.data$get.commits.raw()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})



##
## Tests for split.data.time.based(..., split.basis = 'mails')
##

test_that("Split a data object time-based (split.basis == 'mails').", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    net.conf = NetworkConf$new()

    ## data object
    project.data = CodefaceProjectData$new(proj.conf, net.conf)
    data = list(
        commits.raw = project.data$get.commits.raw(),
        mails = project.data$get.mails(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, time.period = "3 years",
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2004-10-09 18:38:13-2007-10-09 18:38:13",
        "2007-10-09 18:38:13-2010-10-09 18:38:13",
        "2010-10-09 18:38:13-2013-10-09 18:38:13",
        "2013-10-09 18:38:13-2016-07-12 16:05:38"
    )
    result = proj.conf$get.entry("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits.raw = list(
            "2004-10-09 18:38:13-2007-10-09 18:38:13" = data.frame(),
            "2007-10-09 18:38:13-2010-10-09 18:38:13" = data.frame(),
            "2010-10-09 18:38:13-2013-10-09 18:38:13" = data.frame(),
            "2013-10-09 18:38:13-2016-07-12 16:05:38" = data$commits.raw[1:4, ]
        ),
        mails = list(
            "2004-10-09 18:38:13-2007-10-09 18:38:13" = data$mails[rownames(data$mails) %in% 1:2, ],
            "2007-10-09 18:38:13-2010-10-09 18:38:13" = data$mails[rownames(data$mails) %in% 3:12, ],
            "2010-10-09 18:38:13-2013-10-09 18:38:13" = data.frame(),
            "2013-10-09 18:38:13-2016-07-12 16:05:38" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        synchronicity = list(
            "2004-10-09 18:38:13-2007-10-09 18:38:13" = data$synchronicity,
            "2007-10-09 18:38:13-2010-10-09 18:38:13" = data$synchronicity,
            "2010-10-09 18:38:13-2013-10-09 18:38:13" = data$synchronicity,
            "2013-10-09 18:38:13-2016-07-12 16:05:38" = data$synchronicity
        )
    )
    results.data = list(
        commits.raw = lapply(results, function(cf.data) cf.data$get.commits.raw()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})


##
## Tests for split.data.time.based(..., bins = ...)
##

test_that("Split a data object time-based (bins == ... ).", {

    ## configuration objects
    proj.conf = ProjectConf$new(CF.DATA, CF.SELECTION.PROCESS, CASESTUDY, ARTIFACT)
    net.conf = NetworkConf$new()

    ## data object
    project.data = CodefaceProjectData$new(proj.conf, net.conf)
    data = list(
        commits.raw = project.data$get.commits.raw(),
        mails = project.data$get.mails(),
        synchronicity = project.data$get.synchronicity()
    )

    ## split data
    results = split.data.time.based(project.data, bins = c("2016-01-01 00:00:01", "2016-12-31 23:59:59"),
                                    split.basis = "mails", sliding.window = FALSE)

    ## check time ranges
    expected = c(
        "2016-01-01 00:00:01-2016-12-31 23:59:59"
    )
    result = proj.conf$get.entry("ranges")
    expect_equal(result, expected, info = "Time ranges.")

    ## check data for all ranges
    expected.data = list(
        commits.raw = list(
            "2016-01-01 00:00:01-2016-12-31 23:59:59" = data$commits.raw
        ),
        mails = list(
            "2016-01-01 00:00:01-2016-12-31 23:59:59" = data$mails[rownames(data$mails) %in% 13:17, ]
        ),
        synchronicity = list(
            "2016-01-01 00:00:01-2016-12-31 23:59:59" = data$synchronicity
        )
    )
    results.data = list(
        commits.raw = lapply(results, function(cf.data) cf.data$get.commits.raw()),
        mails = lapply(results, function(cf.data) cf.data$get.mails()),
        synchronicity = lapply(results, function(cf.data) cf.data$get.synchronicity())
    )
    expect_equal(results.data, expected.data, info = "Data for ranges.")

})
