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
## Copyright 2016-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020-2021 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2018-2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2022-2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("plyr") # for rbind.fill and dlply
requireNamespace("parallel") # for parallel computation
requireNamespace("igraph") # networks
requireNamespace("logging") # for logging
requireNamespace("lubridate") # for date conversion


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network data ------------------------------------------------------------

#' Construct an edge list for the given network, with timestamps as an extra attribute column.
#'
#' The 'date' attribute has to be added during network construction as default edge attribute
#' in order to avoid problems accessing it.
#'
#' @param net the given network
#'
#' @return the new edgelist
get.edgelist.with.timestamps = function(net) {
  ## get edge list as data.frame
  edges = as.data.frame(igraph::get.edgelist(net))
  colnames(edges) = c("from", "to")
  ## get timestamps
  dates = igraph::get.edge.attribute(net, "date")
  ## bind everything together
  edges = cbind(edges, date = dates)

  return(edges)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Parameter verification --------------------------------------------------

#' Verify the actual arguments given for parameters, checking for two things:
#' - Is the argument missing?
#' - Does the argument inherit from the wrong class?
#'
#' If the checks fail for any reason, the program is stopped with an error message.
#'
#' @param argument the actual argument given to a function
#' @param allowed.classes the allowed classes of the argument
#' @param reference the reference string where this function is called from
#'                  (e.g., a function or class name)
#'
#' @return the argument if all checks are passed
verify.argument.for.parameter = function(argument, allowed.classes, reference) {

    ## get variable name of 'argument'
    argument.variable = as.character(match.call())[2]

    ## check if the argument is missing
    if (missing(argument)) {
        error.message = sprintf("The parameter '%s' must not be missing when calling the function '%s'.",
                                argument.variable, reference)
        logging::logerror(error.message)
        stop(error.message)
    }

    ## check if the argument inherits from the correct classes
    if (!inherits(argument, allowed.classes)) {
        error.message = sprintf(paste("The specified parameter '%s' of class [%s] inherits from the wrong class.",
                                "When calling '%s' the parameter must inherit from one of the following classes: %s"),
                                argument.variable, paste(class(argument), collapse = ", "), reference,
                                paste(allowed.classes, collapse = ", "))
        logging::logerror(error.message)
        stop(error.message)
    }

    return(argument)
}

#' Check whether an argument passed to a function partially matches the specified candidate values of this function.
#' If no argument is passed to that function or the passed argument consists of too many elements, a specified
#' \code{default} value for the argument is returned.
#' If \code{several.ok} is \code{TRUE}, then multiple elements are allowed and the \code{default} value is ignored.
#'
#' Notice: If *no* \code{default} value is specified, this function simply calls the \code{match.arg} function of
#' R-base. See further details in the documentation of \code{match.arg}: \code{?match.arg}
#'
#' @param arg the argument to check (has to be a character vector or \code{NULL})
#' @param choices the candidate values for \code{arg} (has to be a character vector)
#'                (if this parameter is not passed, the candidate values are retrieved from the parent function)
#' @param default a valid default value for \code{arg} (used if \code{arg} is not passed to this function or
#'                \code{arg} contains more than one element, unless \code{several.ok} is \code{TRUE})
#'                or \code{NULL} (in case of \code{NULL}, the first element of \code{choices} is chosen)
#'                [default: NULL]
#' @param several.ok logical indicating whether \code{arg} is allowed to have more than one element [default: FALSE]
#'
#' @return the unabbreviated match(es) out of \code{choices} or the \code{default} value
match.arg.or.default = function(arg, choices, default = NULL, several.ok = FALSE) {

    ## if no choices are given, extract them from the formal signature of the parent function
    ## (the following if-block is taken from https://svn.r-project.org/R/tags/R-3-4-4/src/library/base/R/match.R,
    ##  which is also licensed under GPLv2 (or later))
    if (missing(choices)) {
        formal.args <- formals(sys.function(sys.parent()))
        choices <- eval(formal.args[[as.character(substitute(arg))]])
    }

    ## check whether default value is a valid choice
    if (!is.null(default) && (length(default) != 1 || !default %in% choices)) {
        stop(paste("'default' is not a valid choice. Valid choices: ", paste(dQuote(choices), collapse = ", ")))
    }

    ## check whether to return the default value
    if (length(arg) != 1 && !several.ok && !is.null(default)) {
        return(default)
    } else {
        return(match.arg(arg, choices, several.ok))
    }
}

#' Check if a dataframe matches a given structure. This includes the dataframe to contain columns
#' which must match the column names in \code{columns} and the datatypes in \code{data.types}.
#'
#' @param data the data frame under investigation for structural conformity
#' @param columns a character vector containing the column names the data frame should include
#' @param data.types an ordered vector containing the data types corresponding to the columns.
#'                   If this parameter is \code{NULL} only the existence of \code{columns} is checked
#'                   without regarding column types. Otherwise this vector must be of the
#'                   same length as the vector of \code{columns}
#'                   [default: NULL]
verify.data.frame.columns = function(data, columns, data.types = NULL) {

    ## every column of the data frame must be one to one mapped to a datatype expected in the column
    ## therefore if there aren't as many datatypes provided in \code{data.types} as column names have
    ## been provided in \code{columns} we can stop here already
    if (!is.null(data.types) && length(columns) != length(data.types)) {
        error.message = sprintf(paste("If specified, the length of the two given vectors",
        "'columns' and 'data.types' must match."))
        logging::logerror(error.message)
        stop(error.message)
    }

    ## obtain vector of all column names included in the data frame to ease further checks
    data.frame.columns = colnames(data)

    ## iterate over all columns in \code{columns}
    for (i in seq_along(columns)) {

        ## obtain the column.
        column = columns[i]

        ## stop verification process early if column is not present in the data frame
        if (!(column %in% data.frame.columns)) {
            error.message = sprintf("Column '%s' is missing from the dataframe", column)
            logging::logerror(error.message)
            stop(error.message)
        }

        if (!is.null(data.types)) {

            ## obtain the datatype that should be present in the data frame column
            ## which is currently under investigation
            expected.type = data.types[i]

            ## necessary case distinction for special case list where calling \code{base::class}
            ## removes the information whether or not \code{data[[column]]} is a list
            if (expected.type == "list()") {

                ## column is not a list
                if (!is.list(data[[column]])) {
                    error.message = sprintf("Column '%s' is expected to be a list but is '%s'",
                                            column, class(received.type))
                    logging::logerror(error.message)
                    stop(error.message)
                }

            } else {
                ## obtain the datatype that elements of the current column hold in the data frame
                received.type = class(data[[column]])

                ## stop verification process early if column type in the data frame is not matching
                ## the expected datatype
                if (!(expected.type %in% received.type)) {
                    error.message = sprintf("Column '%s' has type '%s' in dataframe, expected '%s'",
                                            column, received.type, expected.type)
                    logging::logerror(error.message)
                    stop(error.message)
                }
            }
        }
    }
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Empty dataframe creation-------------------------------------------------

#' Create an empty dataframe with the specified columns. Unless all columns should have the default datatype
#' \code{logical}, the second parameter \code{data.types} should specify the datatypes.
#'
#' @param columns a character vector containing all the column names
#' @param data.types a character vector of the same length as \code{columns}; the datatypes can be \code{integer},
#'                   \code{numeric}, \code{POSIXct}, \code{character}, \code{factor}, \code{logical}, and \code{list()}
#'
#' @return the newly created empty dataframe
create.empty.data.frame = function(columns, data.types = NULL) {

    ## if the vector data.types is specified, its length must match the length of the corresponding column names
    if (!is.null(data.types) && length(data.types) != length(columns)) {
        stop("If specified, the length of the two given vectors columns and data.types must be the same.")
    }

    ## create the empty data frame (with zero rows), but the given number of columns
    data.frame = data.frame(matrix(nrow = 0, ncol = length(columns)))
    colnames(data.frame) = columns

    ## assign the datatypes to the data frame columns by indivdually swapping the columns with new columns that possess
    ## the correct data type
    for (i in seq_along(data.types)) {

        ## get the column
        column = data.frame[[i]]

        ## replace column with column of correct type
        switch(tolower(data.types[i]),
               "posixct" = {
                   column = lubridate::with_tz(as.POSIXct(column), tzone = TIMEZONE)
               },
               "integer" = {
                   column = as.integer(column)
               },
               "numeric" = {
                   column = as.numeric(column)
               },
               "logical" = {
                   column = as.logical(column)
               },
               "character" = {
                   column = as.character(column)
               },
               "factor" = {
                   column = as.factor(column)
               },
               "list()" = {
                   column = I(as.list(column))
               },
               {
                   stop(paste("Unknown datatype specified:", data.types[[i]]))
               }
        )

        ## set the column back into the dataframe
        data.frame[[i]] = column
    }

    return(data.frame)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Vector misc--------------------------------------------------------------

#' Get the second last element of a vector, if it has, at least, two elements.
#'
#' @param v the vector of which to retrieve the last element but one
#'
#' @return the second last element of \code{v}. If the vector has less than two elements, return \code{NA}.
get.second.last.element = function(v) {
    if (length(v) >= 2) {
        return(tail(v, n = 2)[[1]])
    } else {
        return(NA)
    }
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Stacktrace --------------------------------------------------------------

#' Get the stacktrace.
#'
#' @param calls the calls of the stacktrace
#'
#' @return the built stacktrace
get.stacktrace = function(calls) {
    lapply(calls, deparse)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Intermediate data -------------------------------------------------------

#' This function is for repetitive runs of the same script: It saves intermediate data to disk and
#' loads it from a previous runs if possible. This way, computation time can be saved.
#'
#' In detail, there are two possibilities:
#' - The file with the path \code{dump.path} *does not exist*.
#'
#'   Save the return value of \code{if.not.found} under the variable name \code{variable}
#'   in the environment from which this function is called and, additionally, save the variable's
#'   value in the file \code{dump.path}.
#'
#' - The file with the path \code{dump.path} *exists already*.
#'
#'   Load the saved file and, thus, the inherently saved object and store the variable in the
#'   calling environment.
#'
#' In both cases, the saved/loaded value is assigned to a variable named \code{variable}
#' in the parent frame, i.e., in the calling environment. This means, the return value of this function
#' does not need to be stored manually in a variable. However, for compatibility reasons, the
#' value is returned invisibly so that assignment is possible (although, this does not disable the
#' automatic storage in the parent environment!).
#'
#' Important: With the parameter \code{skip} set to \code{TRUE}, the check for data existance can be
#' skipped (i.e., force a re-save to disk).
#'
#' @param variable a character naming the data variable to be saved to disk or loaded
#' @param dump.path the path where the data is to be saved to or loaded from
#' @param if.not.found if the data does not exist on disk, run this function whose return value is to be
#'                     saved to disk into the file \code{dump.path}
#' @param skip re-save although data exists on the disk? [default: FALSE]
#'
#' @return the data named \code{variable}, either computed by \code{if.not.found} or loaded
#'         from \code{dump.path}
save.and.load = function(variable, dump.path, if.not.found, skip = FALSE) {
    if (!skip && file.exists(dump.path)) {
        logging::logdebug("Load %s from previously dumped object: %s.", variable, dump.path)
        ## load the dumped object into the environment calling this very function
        load(file = dump.path, envir = parent.frame())
    } else {
        res = if.not.found()

        assign(variable, res, envir = parent.frame()) # rewrite to variable name
        rm(res) # clear memory

        logging::logdebug("Dumping object %s to %s.", variable, dump.path)
        save(list = variable, file = dump.path, envir = parent.frame()) # save automatically
    }

    return(invisible(get0(variable, envir = parent.frame())))
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Date handling -----------------------------------------------------------

#' Parse a date with optional time
#'
#' Notice: Time-zone suffixes are ignored. The \code{input} is expected to be in UTC,
#' even if the \code{input} contains another time-zone suffix.
#'
#' @param input The date string, a vector of date strings, or a list of date strings
#'
#' @return The parsed date(s) as POSIXct object, without changing the underlying data structure
get.date.from.string = function(input) {

    ## re-usable function to parse date strings with lubridate
    convert.text.to.date = function(text) {
        date = lubridate::ymd_hms(text, truncated = 3)
        return(date)
    }

    ## Handle list manually as lubridate would
    ## emit warnings on lists containing NA
    if (is.list(input)) {
        result = lapply(input, convert.text.to.date)
    } else {
        result = convert.text.to.date(input)
    }

    return(result)
}

#' Convert UNIX timestamp to POSIXct
#'
#' @param timestmap The timestamp
#'
#' @return The parsed date as POSIXct object
get.date.from.unix.timestamp = function(timestamp) {
    date = lubridate::as_datetime(timestamp)
    return(date)
}

#' Formats a given date as string using the format "%Y-%m-%d %H:%M:%S"
#'
#' @param input The POSIXct object, a vector of such, or a list of such
#'
#' @return The formatted date(s), without changing the underlying data structure
get.date.string = function(input) {

    ## re-usable function to parse date strings with lubridate
    convert.date.to.text = function(date) {

        ## if we do not have a POSIXct object here, do not convert
        if (!lubridate::is.POSIXct(date)) {
            return(date)
        }

        text = strftime(date, format = "%Y-%m-%d %H:%M:%S")
        return(text)
    }

    ## Handle list manually to not change the underlying
    ## data structure
    if (is.list(input)) {
        result = lapply(input, convert.date.to.text)
    } else {
        result = convert.date.to.text(input)
    }

    return(result)
}

#' Construct a date sequence on the given start time and end time, by either applying the given
#' time period between the sequentially generated dates or by creating the given number of
#' sequential windows.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object
#' @param by The time period describing the length of time between dates, a character
#'           string, e.g., "3 mins" or "15 days"
#' @param length.out The desired length of the sequence (an integer). If set, the
#'                   'time.period' parameter is ignored. [default: NULL]
#'
#' @return the sequential dates as a vector
generate.date.sequence = function(start.date, end.date, by, length.out = NULL) {

    ## convert dates
    start.date = get.date.from.string(start.date)
    end.date = get.date.from.string(end.date)

    ## convert time.period to duration
    if (is.null(length.out)) {
        time.period = lubridate::duration(by)
    } else {
        time.period = get.time.period.by.amount(start.date, end.date, length.out)
    }

    ## convenience function for next step
    get.next.step = function(date) {
        return(date + time.period)
    }

    ## generate dates before end date:
    ## 1) initialize date sequence with first date
    dates = c(start.date)
    ## 2) current date
    current.date = start.date
    ## 3) iterate while smaller than end date
    while (get.next.step(current.date) < end.date) {
        ## get next step
        next.step = get.next.step(current.date)
        ## add next-step date to sequence
        dates = c(dates, next.step)
        current.date = next.step
    }
    ## 4) add end date to sequence
    dates = c(dates, end.date)
    ## 5) explicitly re-add time-zone attribute 'tzone' (as 'c.POSIXct' loses it)
    dates = lubridate::with_tz(dates, tzone = TIMEZONE)

    return(dates)
}

#' Calculate the time period for splitting a time period into equally sized windows.
#'
#' @param start.date The start time as string or POSIXct object
#' @param end.date The end time as string or POSIXct object
#' @param amount The desired amount of windows
#'
#' @return The duration of a single window
get.time.period.by.amount = function(start.date, end.date, amount) {
    time.complete = lubridate::as.duration(lubridate::interval(start.date, end.date))
    time.period =  time.complete / amount
    ## to avoid rounding differences, we round the time period up
    ## (otherwise, we may end up with another unwanted date in the sequence)
    time.period = ceiling(time.period)
    return(time.period)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Range construction and handling -----------------------------------------

#' Construct ranges from the given list/vector of revisions. If \code{raw} is
#' \code{FALSE} (the default), the ranges are constructed in the format "rev[n]-rev[n+1]".
#' Otherwise, pairs of range bounds are returned in list.
#'
#' @param revs the revisions
#' @param sliding.window whether sliding window splitting is enabled or not
#'                       [default: FALSE]
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.ranges = function(revs, sliding.window = FALSE, raw = FALSE) {

    ## make sure that, at least, two revisions are provided
    if (length(revs) < 2) {
        error.message = "Cannot construct ranges from less than 2 revisions."
        logging::logerror(error.message)
        stop(error.message)
    }

    ## setting offset to construct ranges, i.e.,
    ## combine each $offset revisions
    offset = 1

    ## with sliding window, we combine each second revision
    if (sliding.window)
        offset = 2

    ## extract sequences of revisions
    seq1 = revs[ 1:(length(revs) - offset) ]
    if ((offset + 1) <= length(revs)) {
        seq2 = revs[ (offset + 1):length(revs) ]
    } else {
        seq2 = revs[ length(revs) ]
    }

    ## construct ranges
    ranges = mapply(seq1, seq2, SIMPLIFY = FALSE, FUN = function(start, end) {
        start.string = get.date.string(start)
        end.string = get.date.string(end)
        range = paste(start.string, end.string, sep = "-")
        return(range)
    })
    ranges = unlist(ranges, use.names = FALSE)

    ## if raw is enabled, we need to compose seq1 and
    ## seq2 to appropriate tuples
    if (raw) {
        ## compose tuples of range start and range end
        ranges.raw = mapply(seq1, seq2, FUN = c, SIMPLIFY = FALSE)
        ## add formatted ranges as names
        names(ranges.raw) = ranges
        ## set as return value
        ranges = ranges.raw
    }

    return(ranges)
}

#' Construct consecutive ranges based on the given start time, end time, and time period for
#' each range. The ranges do not overlap, i.e., the end of any range is the start of the next one.
#'
#' With this function, it is possible to construct ranges like this:
#' > ++...
#' > ..++.
#' > ....+
#'
#' When the time difference between \code{start} and \code{end} is smaller than
#' \code{time.period}, just one range from \code{start} to \code{end} is constructed.
#'
#' Important: As the start of each range is supposed to be inclusive and the end of each range
#' exclusive, 1 second is added to \code{end}. This way, the date \code{end} will be *included*
#' in the last range. To avoid that behavior, set parameter \code{include.end.date} to \code{FALSE}.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object; the last time to be *included* in the
#'            last range (see above)
#' @param time.period The time period describing the length of the ranges, a character
#'                    string, e.g., "3 mins" or "15 days"
#' @param imperfect.range.ratio The ratio of the \code{time.period} the last range has to last at least.
#'                              That is, if the last range would be shorter than ratio * \code{time.period},
#'                              the last range will be combined with the second last range.
#'                              A ratio of 0.0 means that the last range could be as small as possible.
#'                              A ratio of 1.0 means that the last range has to last at least \code{time.period}.
#'                              [default: 0.0]
#' @param include.end.date whether to include the end date or not [default: TRUE]
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.consecutive.ranges = function(start, end, time.period, imperfect.range.ratio = 0.0,
                                        include.end.date = TRUE, raw = FALSE) {

    ## just construct overlapping ranges without any overlap ;)
    ranges = construct.overlapping.ranges(start, end, time.period, overlap = 0,
                                          imperfect.range.ratio = imperfect.range.ratio,
                                          include.end.date = include.end.date, raw)
    return(ranges)
}

#' Construct ranges based on the given start time, end time, time period, and overlap.
#'
#' With this function, it is possible to construct ranges like this:
#' > ++++
#' > .++++
#' > ..++++
#'
#' With \code{overlap} being the half of \code{time.period}, we basically obtain half-
#' overlapping ranges as in the function \code{construct.ranges} when \code{sliding.window}
#' is set to \code{TRUE}.
#'
#' When the time difference between \code{start} and \code{end} is smaller than
#' \code{time.period}, just one range from \code{start} to \code{end} is constructed.
#'
#' Important: As the start of each range is supposed to be inclusive and the end of each range
#' exclusive, 1 second is added to \code{end}. This way, the date \code{end} will be *included*
#' in the last range. To avoid that behavior, set parameter \code{include.end.date} to \code{FALSE}.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object; the last time to be *included* in the
#'            last range (see above)
#' @param time.period The time period describing the length of the ranges, a character
#'                    string, e.g., "3 mins" or "15 days"
#' @param overlap The time period describing the length of the overlap, a character string
#'                (e.g., "3 mins" or "15 days") or a numeric indication the percentage of
#'                overlap (e.g., 1/4). Should be more than 0 seconds and must not be larger
#'                than the given \code{time.period}.
#' @param imperfect.range.ratio The ratio of the \code{time.period} the last range has to last at least.
#'                              That is, if the last range would be shorter than ratio * \code{time.period},
#'                              the last range will be combined with the second last range.
#'                              A ratio of 0.0 means that the last range could be as small as possible.
#'                              A ratio of 1.0 means that the last range has to last at least \code{time.period}.
#'                              [default: 0.0]
#' @param include.end.date whether to include the end date or not [default: TRUE]
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.overlapping.ranges = function(start, end, time.period, overlap, imperfect.range.ratio = 0.0,
                                        include.end.date = TRUE, raw = FALSE) {

    ## convert given periods to lubridate stuff:
    ## 1) time period
    time.period = lubridate::duration(time.period)
    ## 2) overlap as character string or percent of time.period
    if (is.character(overlap)) {
        overlap = lubridate::duration(overlap)
    } else {
        overlap = time.period * overlap
    }
    ## 3) the dates for theirselves
    start.date = get.date.from.string(start)
    if (include.end.date) {
        end.date = get.date.from.string(end) + 1 ## add 1 for inclusion of end.date
    } else {
        end.date = get.date.from.string(end)
    }

    ## check the breaking case
    if (overlap >= time.period) {
        logging::logerror("The overlap (%s) is exceeding the given time period (%s).",
                          overlap, time.period)
        stop("Stopping due to illegally specified overlap for overlapping ranges.")
    }

    ## compute overall duration
    bins.duration = lubridate::as.duration(lubridate::interval(start.date, end.date))
    ## compute negative overlap
    overlap.negative = time.period - overlap
    ## compute number of complete bins
    bins.number = floor(bins.duration / overlap.negative)
    if (bins.number < 1) {
        bins.number = 1
    }

    ## generate a approximate sequence of dates which can be streamlined later
    seq.start = start.date + overlap
    seq.end = seq.start + (bins.number) * overlap.negative
    ranges.approx = generate.date.sequence(seq.start, seq.end, by = overlap.negative)

    ## handle end date properly
    if (end.date > seq.end) {
        bins.number = bins.number + 1

        if (seq.end == seq.start) {
            ranges.approx = c(seq.start, end.date)
        } else {
            ranges.approx = c(ranges.approx, end.date)
        }
    }

    ## construct the raw ranges from the approximate ones
    ranges.raw = lapply(seq_len(bins.number), function(bin.index) {
        ## combine start and end dates
        bin.start = ranges.approx[[bin.index]] - overlap
        bin.end = ranges.approx[[bin.index + 1]]

        ## check if we hit the end already
        if (bin.end > end.date) {
            bin.end = end.date
        }

        ## construct current bin as the tuple of bin start and bin end
        current.bin = c(bin.start, bin.end)
        ## explicitly set time-zone attribute 'tzone' again (as 'c.POSIXct' loses it)
        current.bin = lubridate::with_tz(current.bin, tzone = TIMEZONE)
        return(current.bin)
    })

    # if wanted, check for imperfect range in the end:
    if (imperfect.range.ratio > 0) {
        ## 1) get the last range
        last.range = ranges.raw[[bins.number]]
        ## 2) get the last range's duration
        last.range.duration = lubridate::as.duration(lubridate::interval(last.range[1], last.range[2]))
        ## 3) check if the last range is too short
        is.too.short = last.range.duration < imperfect.range.ratio * time.period
        ## 4) combine the last range with the second-last one, if the last one is too short
        if (bins.number > 1 && is.too.short) {
            ## extend second-last range until end.date
            ranges.raw[[bins.number - 1]][2] = end.date
            ## remove last range
            ranges.raw = ranges.raw[-bins.number]
        }
    }

    ## construct actual range strings (without names)
    ranges = sapply(ranges.raw, construct.ranges, sliding.window = FALSE, raw = FALSE)
    ranges = unname(ranges)

    ## if raw is enabled, we need to attach proper names
    if (raw) {
        ## add formatted ranges as names
        names(ranges.raw) = ranges
        ## set as return value
        ranges = ranges.raw
    }

    return(ranges)
}

#' Construct cumulative ranges based on the given start time, end time, and time period.
#' Each range starts at \code{start}; the first range lasts exactly \code{time.period}-long,
#' the second two times as long, etc.
#'
#' With this function, it is possible to construct ranges like this:
#' > +...
#' > ++..
#' > +++.
#' > ++++
#'
#' When the time difference between \code{start} and \code{end} is smaller than
#' \code{time.period}, just one range from \code{start} to \code{end} is constructed.
#'
#' Important: As the start of each range is supposed to be inclusive and the end of each range
#' exclusive, 1 second is added to \code{end}. This way, the date \code{end} will be *included*
#' in the last range. To avoid that behavior, set parameter \code{include.end.date} to \code{FALSE}.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object; the last time to be *included* in the
#'            last range (see above)
#' @param time.period The time period describing the length of the ranges, a character
#'                    string, e.g., "3 mins" or "15 days"
#' @param imperfect.range.ratio The ratio of the \code{time.period} the last range has to last at least.
#'                              That is, if the last range would be shorter than ratio * \code{time.period},
#'                              the last range will be combined with the second last range.
#'                              A ratio of 0.0 means that the last range could be as small as possible.
#'                              A ratio of 1.0 means that the last range has to last at least \code{time.period}.
#'                              [default: 0.0]
#' @param include.end.date whether to include the end date or not [default: TRUE]
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.cumulative.ranges = function(start, end, time.period, imperfect.range.ratio = 0.0,
                                       include.end.date = TRUE, raw = FALSE) {

    ## get the consecutive ranges to alter them afterwards
    ranges.consecutive = construct.overlapping.ranges(start, end, time.period, overlap = 0,
                                                      imperfect.range.ratio = imperfect.range.ratio,
                                                      include.end.date = include.end.date, raw = TRUE)

    ## set the start of each range to global start date
    ranges.raw = lapply(ranges.consecutive, function(range.bounds) {
        ## start of each range is the global start date
        range.bounds[1] = start
        return(range.bounds)
    })

    ## construct actual range strings (without names)
    ranges = sapply(ranges.raw, construct.ranges, sliding.window = FALSE, raw = FALSE)
    ranges = unname(ranges)

    ## if raw is enabled, we need to attach proper names
    if (raw) {
        ## add formatted ranges as names
        names(ranges.raw) = ranges
        ## set as return value
        ranges = ranges.raw
    }

    return(ranges)
}

#' Aggregate a given list/vector of ranges to specific levels, configurable through the
#' the parameter \code{aggregation.level} (see below for more details).
#'
#' Using different aggregation levels given by the parameter \code{aggregation.level},
#' it is possible to configure the exact treatment of range bounds and, thus, the
#' re-arrangement of the given list of ranges. The various aggregation levels work
#' as follows:
#' - \code{"range"}: The ranges will be kept exactly as given.
#' - \code{"cumulative"}: The ranges will be re-arranged in a cumulative manner.
#' - \code{"all.ranges"}: The ranges will be re-arranged to exactly to the time range
#'                   specified by the start of the first range and end of the last
#'                   range. All ranges will be exactly the same.
#' - \code{"project.cumulative"}: The same re-arrangement as for \code{"cumulative"}, but
#'                   all ranges will start at \code{project.start} and *not* at the
#'                   beginning of the first range.
#' - \code{"project.all.ranges"}: The same re-arrangement as for \code{"all.ranges"}, but
#'                   all ranges will start at \code{project.start} and *not* at
#'                   the beginning of the first range. All ranges will be exactly the same.
#' - \code{"complete"}: The same re-arrangement as for \code{"all.ranges"}, but all ranges
#'                   will start at \code{project.start} and end at \code{project.end}. All
#'                   ranges will be exactly the same.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here, to pass proper values for \code{project.start} and \code{project.end}.
#'
#' Important: As the start of each range is supposed to be inclusive and the end of each range
#' exclusive, 1 second is added to \code{project.end}. All other range bounds are supposed to
#' be correctly constructed upfront, but if \code{project.end} comes from the function
#' \code{ProjectData$get.data.timestamps}, this is not respected directly. This way, the date
#' \code{project.end} will be *included* in the last range for the aggregation level
#' \code{"complete"}.
#'
#' @param ranges the list or vector of ranges to aggregate
#' @param project.start the project start time as string or POSIXct object
#' @param project.end the project end time as string or POSIXct object
#' @param aggregation.level One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See above for more details. [default: "range"]
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the ranges from \code{ranges} are the names
aggregate.ranges = function(ranges, project.start, project.end,
                            aggregation.level = c("range", "cumulative", "all.ranges",
                                                  "project.cumulative", "project.all.ranges",
                                                  "complete"),
                            raw = FALSE) {

    ## get the chosen aggregation level
    aggregation.level = match.arg(aggregation.level)

    ## get the timestamp data from the project data (needed for some aggr. levels)
    project.start = get.date.from.string(project.start)
    project.end = get.date.from.string(project.end) + 1 ## add 1 for inclusion of project.end
                                                        ## with aggregation level "complete"

    ## loop over all ranges and split the data for each range accordingly:
    list.of.range.bounds = lapply(ranges, get.range.bounds)
    ranges.raw = lapply(ranges, function(range) {
        ## 1) get the range bounds to work with
        start.end = get.range.bounds(range)

        ## 2) adjust the range bounds for the respective aggregation levels
        ##    (if nothing else is stated below, the respective range bounds stay unchanged)
        switch(aggregation.level,

               range = {
                   ## use the exact range bounds
               },
               cumulative = {
                   ## the start is always at the first network's start bound
                   start.end[1] = list.of.range.bounds[[1]][1]
               },
               all.ranges = {
                   ## the start is always at the first network's start bound
                   start.end[1] =list.of.range.bounds[[1]][1]
                   ## the end is always at the last network's ending bound
                   start.end[2] = list.of.range.bounds[[length(ranges)]][2]
               },
               project.cumulative = {
                   ## the start is always at the project data's start
                   start.end[1] = project.start
               },
               project.all.ranges = {
                   ## the start is always at the project data's start
                   start.end[1] = project.start
                   ## the end is always at the last network's ending bound
                   start.end[2] = list.of.range.bounds[[length(ranges)]][2]
               },
               complete = {
                   ## the start is always at the project data's start
                   start.end[1] = project.start
                   ## the start is always at the project data's ending
                   start.end[2] = project.end
               }
        )

        return(start.end)
    })

    ## construct actual range strings (without names)
    ranges.new = sapply(ranges.raw, construct.ranges, sliding.window = FALSE, raw = FALSE)
    ranges.new = unname(ranges.new)

    ## if raw is enabled, we need to attach proper names
    if (raw) {
        ## add formatted original(!) ranges as names
        if (is.list(ranges)) {
            names(ranges.raw) = names(ranges)
        } else {
            names(ranges.raw) = ranges
        }
        ## set as return value
        ranges.new = ranges.raw
    }

    return(ranges.new)
}

#' Calculate the bounds of a range from its name.
#'
#' @param range The range name
#'
#' @return Returns a vector with two entries (start, end) of type POSIXct if input was a date;
#'         or of type character if input was a commit hash or version;
#'         or the unaltered given range if the string could not be parsed
get.range.bounds = function(range) {

    ## the patterns to test with appropriate conversions (if any)
    tests = list(
        ## date format (assuming dates are GMT)
        c("\\d{4}-\\d{2}-\\d{2}(\\s\\d{2}:\\d{2}:\\d{2})?", get.date.from.string),

        ## commit format
        c("[A-F0-9a-f]{40}", identity),

        ## version format
        c("([A-Za-z0-9]+[\\._]?)+", identity)
    )

    for (pattern in tests) {
        start.end = regmatches(range, gregexpr(pattern = pattern[[1]], range))[[1]]

        if (length(start.end) == 2) {
            return (pattern[[2]](start.end))
        }
    }

    return (range)
}


#' Get the data from a data frame in a specific range.
#'
#' @param range The range object that specifies the range
#' @param data The data frame that the data shall be extracted from. It must have a 'date' column.
#'
#' @return A data frame holding the data corresponding to the given range
get.data.from.range = function(range, data) {
    range.bounds = get.range.bounds(range)
    range.bounds.date = get.date.from.string(range.bounds)
    df.bins = findInterval(data[["date"]], range.bounds.date, all.inside = FALSE)

    ## split data by this bin; this gives a list of three data frames, "0" contains the data before the range, "1" the
    ## data within the range and "2" the holds the data after the range
    split.data = split.data.by.bins(data, df.bins)

    ## look for the element with name "1", as we are interested in the data within the range
    ## if there is no data, return an empty data frame corresponding to the data we want to cut
    data.between = split.data["1"][[1]]
    if (is.null(data.between)) {
        ## in order to get an empty data frame, get the data with all rows removed
        empty.data = data[0, ]
        return(empty.data)
    } else {
        return(data.between)
    }
}
