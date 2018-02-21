## (c) Felix Prasse, 2017
## prassefe@fim.uni-passau.de
## (c) Claus Hunsen, 2017, 2018
## hunsen@fim.uni-passau.de
## (c) Thomas Bock, 2017
## bockthom@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Date handling -----------------------------------------------------------


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Range construction and handling -----------------------------------------

##
## Construct consecutive and overlapping ranges.
##

test_that("Construct consecutive and overlapping ranges.", {

    start = ("2018-01-01 00:00:00")
    start.date = get.date.from.string(start)
    end = ("2018-01-01 06:05:00")
    end.date = get.date.from.string(end)
    end.including = end.date + 1

    ## standard overlapping ranges:
    ## 1) expected results
    expected.formatted = c(
        "2018-01-01 00:00:00-2018-01-01 02:00:00",
        "2018-01-01 01:30:00-2018-01-01 03:30:00",
        "2018-01-01 03:00:00-2018-01-01 05:00:00",
        "2018-01-01 04:30:00-2018-01-01 06:05:01"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes", raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Standard overlapping ranges (formatted).")
    ## 3) raw
    result.raw = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes", raw = TRUE)
    expect_equal(result.raw, expected.raw, info = "Standard overlapping ranges (raw).")
    ## TODO use expect_identical here? why failing?

    ## non-overlapping/consecutive ranges:
    ## 1) expected results
    expected.formatted = c(
        "2018-01-01 00:00:00-2018-01-01 02:00:00",
        "2018-01-01 02:00:00-2018-01-01 04:00:00",
        "2018-01-01 04:00:00-2018-01-01 06:00:00",
        "2018-01-01 06:00:00-2018-01-01 06:05:01"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.overlapping.ranges(start.date, end.date, time.period = "2 hours", overlap = 0, raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Non-overlapping ranges (formatted).")
    ## 3) raw
    result.raw = construct.overlapping.ranges(start.date, end.date, time.period = "2 hours", overlap = 0, raw = TRUE)
    expect_equal(result.raw, expected.raw, info = "Non-overlapping ranges (raw).")
    ## TODO use expect_identical here? why failing?
    ## 4) matching with consecutive ranges
    results.raw = construct.consecutive.ranges(start.date, end.date, time.period = "2 hours", raw = FALSE)
    expect_equal(result.raw, expected.raw, info = "Non-overlapping ranges (consecutive).")

    ## illegal overlap
    expect_error(
        construct.overlapping.ranges(start.date, end.date, time.period = "2 hours", overlap = "1 year", raw = FALSE),
        info = "Error expected (illegal overlap)."
    )
})

##
## Construct cumulative ranges.
##

test_that("Construct cumulative ranges.", {

    start = ("2018-01-01 00:00:00")
    start.date = get.date.from.string(start)
    end = ("2018-01-01 06:05:00")
    end.date = get.date.from.string(end)
    end.including = end.date + 1

    ## standard overlapping ranges:
    ## 1) expected results
    expected.formatted = c(
        "2018-01-01 00:00:00-2018-01-01 02:00:00",
        "2018-01-01 00:00:00-2018-01-01 04:00:00",
        "2018-01-01 00:00:00-2018-01-01 06:00:00",
        "2018-01-01 00:00:00-2018-01-01 06:05:01"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.cumulative.ranges(start, end, time.period = "2 hours", raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Cumulative ranges (formatted).")
    ## 3) raw
    result.raw = construct.cumulative.ranges(start, end, time.period = "2 hours", raw = TRUE)
    expect_equal(result.raw, expected.raw, info = "Cumulative ranges (raw).")
    ## TODO use expect_identical here? why failing?
})

##
## Aggregate ranges.
##

test_that("Aggregate ranges.", {

    aggregation.level = c("range", "cumulative", "all.ranges",
                          "project.cumulative", "project.all.ranges",
                          "complete")

    project.start = get.date.from.string("2017-12-01 00:00:00")
    start = ("2018-01-01 00:00:00")
    start.date = get.date.from.string(start)
    end = ("2018-01-01 06:05:00")
    end.date = get.date.from.string(end)
    end.including = end.date + 1
    project.end = get.date.from.string("2019-04-04 00:00:00")

    ## construct ranges to aggregate
    ranges = construct.consecutive.ranges(start.date, end.date, time.period = "2 hours", raw = FALSE)
    ranges.raw =  construct.consecutive.ranges(start.date, end.date, time.period = "2 hours", raw = FALSE)

    ## get results
    results = lapply(aggregation.level, function(level)
        aggregate.ranges(ranges, project.start, project.end, level, raw = FALSE) ## FIXME raw = TRUE
    )
    names(results) = aggregation.level

    ## expected results
    expected = list(
        range = c(
            "2018-01-01 00:00:00-2018-01-01 02:00:00",
            "2018-01-01 02:00:00-2018-01-01 04:00:00",
            "2018-01-01 04:00:00-2018-01-01 06:00:00",
            "2018-01-01 06:00:00-2018-01-01 06:05:01"
        ),
        cumulative = c(
            "2018-01-01 00:00:00-2018-01-01 02:00:00",
            "2018-01-01 00:00:00-2018-01-01 04:00:00",
            "2018-01-01 00:00:00-2018-01-01 06:00:00",
            "2018-01-01 00:00:00-2018-01-01 06:05:01"
        ),
        all.ranges = c(
            "2018-01-01 00:00:00-2018-01-01 06:05:01",
            "2018-01-01 00:00:00-2018-01-01 06:05:01",
            "2018-01-01 00:00:00-2018-01-01 06:05:01",
            "2018-01-01 00:00:00-2018-01-01 06:05:01"
        ),
        project.cumulative = c(
            "2017-12-01 00:00:00-2018-01-01 02:00:00",
            "2017-12-01 00:00:00-2018-01-01 04:00:00",
            "2017-12-01 00:00:00-2018-01-01 06:00:00",
            "2017-12-01 00:00:00-2018-01-01 06:05:01"
        ),
        project.all.ranges = c(
            "2017-12-01 00:00:00-2018-01-01 06:05:01",
            "2017-12-01 00:00:00-2018-01-01 06:05:01",
            "2017-12-01 00:00:00-2018-01-01 06:05:01",
            "2017-12-01 00:00:00-2018-01-01 06:05:01"
        ),
        complete = c(
            "2017-12-01 00:00:00-2019-04-04 00:00:01",
            "2017-12-01 00:00:00-2019-04-04 00:00:01",
            "2017-12-01 00:00:00-2019-04-04 00:00:01",
            "2017-12-01 00:00:00-2019-04-04 00:00:01"
        )
    )

    lapply(aggregation.level, function(aggregation.level) {
        expected.ranges = expected[[aggregation.level]]
        results.ranges = results[[aggregation.level]]

        expect_identical(results.ranges, expected.ranges)
    })
})

##
## Parse ranges.
##

test_that("Parse range", {

    range.input = c("2012-07-10 15:58:00-2012-07-15 16:02:00",
                    "2012-07-10-2012-07-15 16:02:00",
                    "2012-07-10 15:58:00-2012-07-15",
                    "f86391e7d7eaf4234fc742c1b61f32cb8a65782e-63654c1b089b8abe9a52d21fd1b53b1631539e10",
                    "v1.0.0-v2.0.0_alpha")

    expected.output = list(
        get.date.from.string(c("2012-07-10 15:58:00", "2012-07-15 16:02:00")),
        get.date.from.string(c("2012-07-10 00:00:00", "2012-07-15 16:02:00")),
        get.date.from.string(c("2012-07-10 15:58:00", "2012-07-15 00:00:00")),
        c("f86391e7d7eaf4234fc742c1b61f32cb8a65782e", "63654c1b089b8abe9a52d21fd1b53b1631539e10"),
        c("v1.0.0", "v2.0.0_alpha")
    )

    actual.output = lapply(range.input, get.range.bounds)

    expect_equal(actual.output, expected.output, "Parsed Range")
})

