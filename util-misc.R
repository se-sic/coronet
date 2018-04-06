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
## Copyright 2016-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
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

    ## check argument if it is missing
    if (missing(argument)) {
        logging::logerror(paste("The parameter '%s' is missing in %s constructor ('%s' wanted)."),
                          argument.variable, reference, allowed.classes)
        stop(sprintf("Parameter '%s' missing in %s constructor.", argument.variable, reference))
    }

    ## check argument if it is not allowed
    if (!inherits(argument, allowed.classes)) {
        logging::logerror(paste("The given parameter '%s' inherits from the wrong class in %s constructor",
                                "(actual class is '%s', '%s' wanted)."),
                          argument.variable, reference, class(argument), allowed.classes)
        stop(sprintf("Parameter '%s' inherits from wrong class in %s constructor.", argument.variable, reference))
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

#' Save the given 'variable' on the file system (in 'dump.path') if it does not exist already,
#' and load the saved data if it exists.
#'
#' With skip, the check for data existance can be skipped (i.e., force a re-save).
#'
#' This function is for repetitive runs of the same script: It saves intermediate data to disk and
#' loads it from a previous runs if possible. This way, computation time can be saved.
#'
#' @param variable a character naming the data variable to be saved to disk
#' @param dump.path the path where the data is to be saved
#' @param if.not.found if the data does not exist on disk, run this function ('variable' must be a variable there!)
#' @param skip re-save although data exists on the disk? [default: FALSE]
#'
#' @return the data computed by 'if.not.found' or loaded from 'dump.path'
save.and.load = function(variable, dump.path, if.not.found, skip = FALSE) {
    if (!skip && file.exists(dump.path)) {
        logging::logdebug("Load %s from previously dumped object: %s.", variable, dump.path)
        load(file = dump.path) # load the list named "variable" into the current environment
    } else {
        res = if.not.found()

        assign(variable, res) # rewrite to variable name
        rm(res) # clear memory

        logging::logdebug("Dumping object %s to %s.", variable, dump.path)
        save(list = variable, file = dump.path) # save automatically
    }

    return(get0(variable))
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

#' Construct a date sequence on the given start time, end time, and time period between the
#' sequentially generated dates.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object
#' @param by The time period describing the length of time between dates, a character
#'           string, e.g., "3 mins" or "15 days"
#'
#' @return the sequential dates as a vector
generate.date.sequence = function(start.date, end.date, by) {

    ## convert dates
    start.date = get.date.from.string(start.date)
    end.date = get.date.from.string(end.date)

    ## convert time.period to duration
    time.period = lubridate::duration(by)

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

    return(dates)
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
    ## setting offset to construct ranges, i.e.,
    ## combine each $offset revisions
    offset = 1

    ## with sliding window, we combine each second revision
    if (sliding.window)
        offset = 2

    ## extract sequences of revisions
    seq1 = revs[ 1:(length(revs) - offset) ]
    seq2 = revs[ (offset + 1):length(revs) ]

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
#' in the last range.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object; the last time to be *included* in the
#'            last range (see above)
#' @param time.period The time period describing the length of the ranges, a character
#'                    string, e.g., "3 mins" or "15 days"
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.consecutive.ranges = function(start, end, time.period, raw = FALSE) {

    ## just construct overlapping ranges without any overlap ;)
    ranges = construct.overlapping.ranges(start, end, time.period, overlap = 0, raw)
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
#' in the last range.
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
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.overlapping.ranges = function(start, end, time.period, overlap, raw = FALSE) {

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
    end.date = get.date.from.string(end) + 1 ## add 1 for inclusion of end.date

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
    bins.number = round(bins.duration / overlap.negative)

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

        ## return the tuple of bin start and bin end
        return(c(bin.start, bin.end))
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
#' in the last range.
#'
#' Note: You may want to use the function \code{ProjectData$get.data.timestamps} with this
#' function here.
#'
#' @param start The start time as string or POSIXct object
#' @param end The end time as string or POSIXct object; the last time to be *included* in the
#'            last range (see above)
#' @param time.period The time period describing the length of the ranges, a character
#'                    string, e.g., "3 mins" or "15 days"
#' @param raw whether to return pairs of POSIXct objects or strings rather than
#'            formatted strings [default: FALSE]
#'
#' @return the constructed ranges, either formatted or raw; the raw ranges are a named list,
#'         for which the formatted ranges are the names
construct.cumulative.ranges = function(start, end, time.period, raw = FALSE) {

    ## get the consecutive ranges to alter them afterwards
    ranges.consecutive = construct.overlapping.ranges(start, end, time.period, overlap = 0, raw = TRUE)

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
