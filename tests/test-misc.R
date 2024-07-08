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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2023 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2022-2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network data ------------------------------------------------------------

test_that("Get edgelist augmented with timestamps", {

    ## construct network
    edges = list(list("A", "A"), list("D", "C"), list("C", "A"), list("B", "C"))
    timestamps = c("2016-12-07 15:30:02", "2016-08-07 15:37:02", "2016-07-12 15:59:25", "2016-07-12 15:59:59")
    network =
        igraph::make_empty_graph(n = 0, directed = TRUE) +
        igraph::vertices("A", "B", "C", "D") +
        igraph::edges(edges, relation = "mail", date = timestamps)


    ## get edgelist augmented with timestamps
    edgelist = get.edgelist.with.timestamps(network)

    ## check correctness
    expect_equal(names(edgelist), c("from", "to", "date"))
    expect_equal(nrow(edgelist), 4)
    lapply(1:4, function(i) {
        actual = edgelist[i, ]
        expect_equal(actual[["from"]], edges[[i]][[1]])
        expect_equal(actual[["to"]], edges[[i]][[2]])
        expect_equal(actual[["date"]], timestamps[i])
    })
})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Parameter verification --------------------------------------------------

##
## Match argument or take default.
##

test_that("Match argument or take default.", {

    ## 1) tests for single choice without default
    test.function = function(param = c("choice1", "choice2", "choice3")) {
        param = match.arg.or.default(param)
        return(param)
    }

    actual.result = test.function()
    expected.result = "choice1"
    expect_equal(actual.result, expected.result, info = "Single choice without default, no choice")

    actual.result = test.function("choice3")
    expected.result = "choice3"
    expect_equal(actual.result, expected.result, info = "Single choice without default, one choice")

    expect_error(test.function(c("choice3", "choice1")), info = "Single choice with default, two choices")

    ## 2) tests for single choice with default
    test.function = function(param = c("choice1", "choice2", "choice3")) {
        param = match.arg.or.default(param, default = "choice2")
        return(param)
    }

    actual.result = test.function()
    expected.result = "choice2"
    expect_equal(actual.result, expected.result, info = "Single choice with default, no choice")

    actual.result = test.function("choice3")
    expected.result = "choice3"
    expect_equal(actual.result, expected.result, info = "Single choice with default, one choice")

    actual.result = test.function(c("choice3", "choice1"))
    expected.result = "choice2"
    expect_equal(actual.result, expected.result, info = "Single choice without default, two choices")

    ## 3) tests for single choice with illegal default
    test.function = function(param = c("choice1", "choice2", "choice3")) {
        param = match.arg.or.default(param, default = "c2")
        return(param)
    }

    expect_error(test.function(), info = "Single choice with illegal default, no choice")
    expect_error(test.function(c("choice3", "choice1")), info = "Single choice with illegal default, one choice")
    expect_error(test.function(c("choice3", "choice1")), info = "Single choice with illegal default, two choices")

    ## 4) tests for multiple choices
    test.function = function(param = c("choice1", "choice2", "choice3")) {
        param = match.arg.or.default(param, several.ok = TRUE)
        return(param)
    }

    actual.result = test.function()
    expected.result = c("choice1", "choice2", "choice3")
    expect_equal(actual.result, expected.result, info = "Multiple choices, no choice")

    actual.result = test.function("choice3")
    expected.result = "choice3"
    expect_equal(actual.result, expected.result, info = "Multiple choices, one choice")

    actual.result = test.function(c("choice3", "choice1"))
    expected.result = c("choice3", "choice1")
    expect_equal(actual.result, expected.result, info = "Multiple choices, two choices")

    ## 5) tests for multiple choices with ignored default
    test.function = function(param = c("choice1", "choice2", "choice3")) {
        param = match.arg.or.default(param, default = "choice2", several.ok = TRUE)
        return(param)
    }

    actual.result = test.function()
    expected.result = c("choice1", "choice2", "choice3")
    expect_equal(actual.result, expected.result, info = "Multiple choices with ignored default, no choice")

    actual.result = test.function("choice3")
    expected.result = "choice3"
    expect_equal(actual.result, expected.result, info = "Multiple choices with ignored default, one choice")

    actual.result = test.function(c("choice3", "choice1"))
    expected.result = c("choice3", "choice1")
    expect_equal(actual.result, expected.result, info = "Multiple choices with ignored default, two choices")
})

##
## Check presence and datatype of data frame columns.
##

test_that("Check presence and datatype of data frame columns.", {

    user.names = c("John", "Peter", "Maria", "Susanne")

    ## contains NaN to verify functionality does not break
    age = c(42, 50, NaN, 66)

    ## contains NA to verify functionality does not break
    is.male = c(TRUE, TRUE, FALSE, NA)

    ## construct simple testing dataframe
    data.frame = data.frame(user.names, age, is.male)

    ## 1) Check base functionality (benign use-case)
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age", "is.male"), c("character", "numeric", "logical")),
        NA,
        message = "All columns present and well-typed.")
    ## Expect no error

    ## 2) Base test with reordered columns
    expect_error(verify.data.frame.columns(
        data.frame, c("is.male", "age", "user.names"), c("logical", "numeric", "character")),
        NA,
        message = "Order of columns does not matter.")
    ## Expect no error

    ## 3) Specify less columns than present (Allow optional columns)
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age"), c("character", "numeric")),
        NA,
        message = "Optional columns are allowed.")
    ## Expect no error

    ## 4) Unequal amount of column names and datatypes
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age", "is.male"), c("character", "numeric")),
        message = "More column names specified than datatypes.")
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age"), c("character", "numeric", "logical")),
        message = "More column names specified than datatypes.")

    ## 5) Datatypes do not match column names
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age", "is.male"), c("logical", "character", "numeric")),
        message = "Column names do not match datatypes.")

    ## 6) Invalid column / Column not present in dataframe (Typo)
    expect_error(verify.data.frame.columns(
        data.frame, c("user.name"), c("character")),
        message = "Column name 'user.name' should not be in dataframe.")

    ## 7) No datatypes specified and column names are present
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age", "is.male")),
        NA,
        message = "Column names do not match datatypes.")
    ## Expect no error

    ## 8) No datatypes specified and column names are not specified correctly (Typo)
    expect_error(verify.data.frame.columns(
        data.frame, c("user.name")),
        message = "Column name 'user.name' should not be in dataframe.")

    ## 9) Too many column names and no datatypes specified
    expect_error(verify.data.frame.columns(
        data.frame, c("user.names", "age", "is.male", "job.orientation")),
        message = "More column names specififed than present in the dataframe.")

})


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vector misc--------------------------------------------------------------

##
## Check if a value is a single NA value.
##

test_that("Check if a value is a single NA value", {

    ## 1) Tests for single NA
    expect_true(is.single.na(NA))
    expect_true(is.single.na(list(NA)))
    expect_true(is.single.na(data.frame(NA)))

    ## 2) Tests for values other than a single NA
    expect_false(is.single.na(0))
    expect_false(is.single.na("na"))
    expect_false(is.single.na(NULL))
    expect_false(is.single.na(logical(0)))
    expect_false(is.single.na(FALSE))
    expect_false(is.single.na(c(NA, NA)))
    expect_false(is.single.na(c(3, NA)))
    expect_false(is.single.na(list(NA, NA)))
    expect_false(is.single.na(data.frame(NA, NA)))
})


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
    expect_identical(get.date.from.string(date.string), date.posixct, info = "From string.")
    ## 2) from POSIXct
    expect_identical(get.date.from.string(date.posixct), date.posixct, info = "From POSIXct.")

    ## partial date:
    ## 1) no seconds
    date.string = "2018-02-22 10:22"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d %H:%M"))
    expect_identical(get.date.from.string(date.string), date.posixct, info = "Partial date (no seconds).")
    ## 2) no seconds, no minutes
    date.string = "2018-02-22 10"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d %H"))
    expect_identical(get.date.from.string(date.string), date.posixct, info = "Partial date (no seconds, no minutes).")
    ## 3) no time
    date.string = "2018-02-22"
    date.posixct = as.POSIXct(strptime(date.string, format = "%Y-%m-%d"))
    expect_identical(get.date.from.string(date.string), date.posixct, info = "Partial date (no time).")

    ## date string with time zone (which will be ignored as we treat it like UTC anyway)
    date.string = "2018-02-22 10:02:03 CET"
    date.posixct = as.POSIXct(strptime("2018-02-22 10:02:03", tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
    expect_identical(get.date.from.string(date.string), date.posixct, info = "Date with (ignored) time-zone")

})

##
## Parse date from a UNIX timestamp.
##

test_that("Parse date from a UNIX timestamp.", {

    date.numeric = 1519296267
    date.string = "2018-02-22 10:44:27"
    date.posixct = get.date.from.string(date.string)

    expect_identical(get.date.from.unix.timestamp(date.numeric), date.posixct, info = "From string.")

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
    expect_identical(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no seconds).")
    ## 2) no seconds, no minutes
    date.string = "2018-02-22 10"
    date.string.formatted = "2018-02-22 10:00:00"
    date.posixct = get.date.from.string(date.string)
    expect_identical(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no seconds, no minutes).")
    ## 3) no time
    date.string = "2018-02-22"
    date.string.formatted = "2018-02-22 00:00:00"
    date.posixct = get.date.from.string(date.string)
    expect_identical(get.date.string(date.posixct), date.string.formatted, info = "Partial date (no time).")

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
    expect_identical(result, expected, info = "Date sequence from strings.")
    ## 3) From POSIXct.
    result = generate.date.sequence(start.date.posixct, end.date.short.posixct, time.period)
    expect_identical(result, expected, info = "Date sequence from dates.")
    ## 4) With lubridate::duration.
    result = generate.date.sequence(start.date.posixct, end.date.short.posixct, time.period.duration)
    expect_identical(result, expected, info = "Date sequence with lubridate::duration")

    ## precise last range:
    end.date.precise = "2018-02-22 06:00:00"
    end.date.precise.posixct = get.date.from.string(end.date.precise)
    ## 1) expected results
    expected = get.date.from.string(c("2018-02-22 00:00:00", "2018-02-22 02:00:00",
                                      "2018-02-22 04:00:00", "2018-02-22 06:00:00"))
    ## 2) From string.
    result = generate.date.sequence(start.date, end.date.precise, time.period)
    expect_identical(result, expected, info = "Date sequence from strings.")
    ## 3) From POSIXct.
    result = generate.date.sequence(start.date.posixct, end.date.precise.posixct, time.period)
    expect_identical(result, expected, info = "Date sequence from dates.")
    ## 4) With lubridate::duration.
    result = generate.date.sequence(start.date.posixct, end.date.precise.posixct, time.period.duration)
    expect_identical(result, expected, info = "Date sequence with lubridate::duration")

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
    expect_identical(result.raw, expected.raw, info = "Standard overlapping ranges (raw).")

    ## overlapping ranges without imperfect ranges:
    ## 1) expected results
    expected.formatted = c(
      "2018-01-01 00:00:00-2018-01-01 02:00:00",
      "2018-01-01 01:30:00-2018-01-01 03:30:00",
      "2018-01-01 03:00:00-2018-01-01 06:05:01"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes",
                                                    imperfect.range.ratio = 1.0, raw = FALSE)
    expect_identical(result.formatted, expected.formatted,
                     info = "Overlapping ranges without imperfect ranges (formatted).")
    ## 3) raw
    result.raw = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes",
                                              imperfect.range.ratio = 1.0, raw = TRUE)
    expect_identical(result.raw, expected.raw,
                     info = "Overlapping ranges without imperfect ranges (raw).")

    ## overlapping ranges with imperfect ranges that last at least 8% of the time period:
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
    result.formatted = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes",
                                                    imperfect.range.ratio = 0.08, raw = FALSE)
    expect_identical(result.formatted, expected.formatted,
                     info = "Overlapping ranges with imperfect ranges (at least 8% of the time period) (formatted).")
    ## 3) raw
    result.raw = construct.overlapping.ranges(start, end, time.period = "2 hours", overlap = "30 minutes",
                                              imperfect.range.ratio = 0.08, raw = TRUE)
    expect_identical(result.raw, expected.raw,
                     info = "Overlapping ranges with imperfect ranges (at least 8% of the time period) (raw).")

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
    expect_identical(result.raw, expected.raw, info = "Non-overlapping ranges (raw).")
    ## 4) matching with consecutive ranges
    results.raw = construct.consecutive.ranges(start.date, end.date, time.period = "2 hours", raw = FALSE)
    expect_identical(result.raw, expected.raw, info = "Non-overlapping ranges (consecutive).")

    ## non-overlapping/consecutive ranges without imperfect ranges and exclusive end:
    ## 1) expected results
    expected.formatted = c(
      "2018-01-01 00:00:00-2018-01-01 02:00:00",
      "2018-01-01 02:00:00-2018-01-01 04:00:00",
      "2018-01-01 04:00:00-2018-01-01 06:05:00"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.overlapping.ranges(start.date, end.date, time.period = "2 hours", overlap = 0,
                                                    imperfect.range.ratio = 1.0, include.end.date = FALSE, raw = FALSE)
    expect_identical(result.formatted, expected.formatted,
                     info = "Non-overlapping ranges without imperfect ranges and exclusive end (formatted).")
    ## 3) raw
    result.raw = construct.overlapping.ranges(start.date, end.date, time.period = "2 hours", overlap = 0,
                                              imperfect.range.ratio = 1.0, include.end.date = FALSE, raw = TRUE)
    expect_identical(result.raw, expected.raw,
                     info = "Non-overlapping ranges without imperfect ranges and exclusive end (raw).")
    ## 4) matching with consecutive ranges
    results.raw = construct.consecutive.ranges(start.date, end.date, time.period = "2 hours",
                                               imperfect.range.ratio = 1.0, include.end.date = FALSE, raw = FALSE)
    expect_identical(result.raw, expected.raw,
                     info = "Non-overlapping ranges without imperfect ranges and exclusive end (consecutive).")

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

    ## standard cumulative ranges:
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
    expect_identical(result.raw, expected.raw, info = "Cumulative ranges (raw).")

    ## cumulative ranges without imperfect ranges:
    ## 1) expected results
    expected.formatted = c(
      "2018-01-01 00:00:00-2018-01-01 02:00:00",
      "2018-01-01 00:00:00-2018-01-01 04:00:00",
      "2018-01-01 00:00:00-2018-01-01 06:05:01"
    )
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted
    ## 2) formatted
    result.formatted = construct.cumulative.ranges(start, end, time.period = "2 hours",
                                                   imperfect.range.ratio = 1.0, raw = FALSE)
    expect_identical(result.formatted, expected.formatted,
                     info = "Cumulative ranges without imperfect ranges (formatted).")
    ## 3) raw
    result.raw = construct.cumulative.ranges(start, end, time.period = "2 hours",
                                             imperfect.range.ratio = 1.0, raw = TRUE)
    expect_identical(result.raw, expected.raw,
                     info = "Cumulative ranges without imperfect ranges (raw).")

})

##
## Construct ranges when difference between start and end is smaller than time period.
##

test_that("Construct ranges when difference between start and end is smaller than time period.", {

    start = ("2015-05-01 01:01:01")
    start.date = get.date.from.string(start)
    end = ("2015-05-02 02:02:02")
    end.date = get.date.from.string(end)
    end.including = end.date + 1

    ## expected results (equal for consecutive, cumulative, and overlapping range construction)
    expected.formatted = c("2015-05-01 01:01:01-2015-05-02 02:02:03")
    expected.raw = lapply(expected.formatted, get.range.bounds)
    names(expected.raw) = expected.formatted

    ## consecutive
    ## 1) formatted
    result.formatted = construct.consecutive.ranges(start, end, time.period = "1 week", imperfect.range.ratio = 0.2, raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Consecutive range (formatted).")
    ## 2) raw
    result.raw = construct.consecutive.ranges(start, end, time.period = "1 week", raw = TRUE)
    expect_identical(result.raw, expected.raw, info = "Consecutive range (raw).")

    ## cumulative
    ## 1) formatted
    result.formatted = construct.cumulative.ranges(start, end, time.period = "1 week", imperfect.range.ratio = 0.2, raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Cumulative range (formatted).")
    ## 2) raw
    result.raw = construct.cumulative.ranges(start, end, time.period = "1 week", raw = TRUE)
    expect_identical(result.raw, expected.raw, info = "Cumulative range (raw).")

    ## overlapping
    ## 1) formatted
    result.formatted = construct.overlapping.ranges(start, end, time.period = "1 week", imperfect.range.ratio = 0.2,
                                                    overlap = "1 day", raw = FALSE)
    expect_identical(result.formatted, expected.formatted, info = "Overlapping range (formatted).")
    ## 2) raw
    result.raw = construct.overlapping.ranges(start, end, time.period = "1 week",
                                              overlap = "5 days", raw = TRUE)
    expect_identical(result.raw, expected.raw, info = "Overlapping range (raw).")
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

    expect_identical(actual.output, expected.output, "Parsed Range")
})

