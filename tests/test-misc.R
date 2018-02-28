## This file is part of codeface-extraction-r, which is free software: you
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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Thomas Bock <bockthom@fim.uni-passau.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Date handling -----------------------------------------------------------

##
## Parse date from a string.
##

test_that("Parse date from a string.", {

    ## full date:
    date.string = "2018-02-22 10:44:27"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d %H:%M:%S"))
    ## 1) from string
    expect_equal(get.date.from.string(date.string), date.posixct, info = "From string.")
    ## 2) from POSIXct
    expect_equal(get.date.from.string(date.posixct), date.posixct, info = "From POSIXct.")

    ## partial date:
    ## 1) no seconds
    date.string = "2018-02-22 10:22"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d %H:%M"))
    expect_equal(get.date.from.string(date.string), date.posixct, info = "Partial date (no seconds).")
    ## 2) no seconds, no minutes
    date.string = "2018-02-22 10"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d %H"))
    expect_equal(get.date.from.string(date.string), date.posixct, info = "Partial date (no seconds, no minutes).")
    ## 3) no time
    date.string = "2018-02-22"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d"))
    expect_equal(get.date.from.string(date.string), date.posixct, info = "Partial date (no time).")

})

##
## Parse date from a UNIX timestamp.
##

test_that("Parse date from a UNIX timestamp.", {

    date.numeric = 1519296267
    date.string = "2018-02-22 10:44:27"
    date.posixct = get.date.from.string(date.string)

    expect_equal(get.date.from.unix.timestamp(date.numeric), date.posixct, info = "From string.")

})

##
## Format a POSIXct object.
##

test_that("Format a POSIXct object.", {

    ## full date:
    date.string = "2018-02-22 10:44:27"
    date.posixct = get.date.from.string(date.string)
    ## 1) from string
    expect_identical(get.date.string(date.string), date.string, info = "From string.")
    ## 2) from POSIXct
    expect_identical(get.date.string(date.posixct), date.string, info = "From POSIXct.")

    ## partial date:
    ## 1) no seconds
    date.string = "2018-02-22 10:44"
    date.string.formatted = "2018-02-22 10:44:00"
    date.posixct = get.date.from.string(date.string)
    expect_equal(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no seconds).")
    ## 2) no seconds, no minutes
    date.string = "2018-02-22 10"
    date.string.formatted = "2018-02-22 10:00:00"
    date.posixct = get.date.from.string(date.string)
    expect_equal(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no seconds, no minutes).")
    ## 3) no time
    date.string = "2018-02-22"
    date.string.formatted = "2018-02-22 00:00:00"
    date.posixct = get.date.from.string(date.string)
    expect_equal(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no time).")

})

##
## Generate a date sequence.
##

test_that("Generate a date sequence.", {

    ## parameter configuration
    time.period = "2 hours"
    time.period.duration = lubridate::duration(time.period)

    start.date = "2018-02-22 00:00:00"
    start.date.posixct = get.date.from.string(start.date)

    ## short last range:
    end.date.short = "2018-02-22 06:05:01"
    end.date.short.posixct = get.date.from.string(end.date.short)
    ## 1) expected results
    expected = get.date.from.string(c("2018-02-22 00:00:00", "2018-02-22 02:00:00",
                                      "2018-02-22 04:00:00", "2018-02-22 06:00:00",
                                      "2018-02-22 06:05:01"))
    ## 2) From string.
    result = generate.date.sequence(start.date, end.date.short, time.period)
    expect_equal(result, expected, info = "Date sequence from strings.")
    ## 3) From POSIXct.
    result = generate.date.sequence(start.date.posixct, end.date.short.posixct, time.period)
    expect_equal(result, expected, info = "Date sequence from dates.")
    ## 4) With lubridate::duration.
    result = generate.date.sequence(start.date.posixct, end.date.short.posixct, time.period.duration)
    expect_equal(result, expected, info = "Date sequence with lubridate::duration")

    ## precise last range:
    end.date.precise = "2018-02-22 06:00:00"
    end.date.precise.posixct = get.date.from.string(end.date.precise)
    ## 1) expected results
    expected = get.date.from.string(c("2018-02-22 00:00:00", "2018-02-22 02:00:00",
                                      "2018-02-22 04:00:00", "2018-02-22 06:00:00"))
    ## 2) From string.
    result = generate.date.sequence(start.date, end.date.precise, time.period)
    expect_equal(result, expected, info = "Date sequence from strings.")
    ## 3) From POSIXct.
    result = generate.date.sequence(start.date.posixct, end.date.precise.posixct, time.period)
    expect_equal(result, expected, info = "Date sequence from dates.")
    ## 4) With lubridate::duration.
    result = generate.date.sequence(start.date.posixct, end.date.precise.posixct, time.period.duration)
    expect_equal(result, expected, info = "Date sequence with lubridate::duration")

})


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

