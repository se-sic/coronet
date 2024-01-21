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
## Copyright 2017-2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2017 by Raphael Nömmer <noemmer@fim.uni-passau.de>
## Copyright 2017-2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2020 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2017-2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2020 by Thomas Bock <bockthom@cs.uni-saarland.de>
## Copyright 2021 by Niklas Schneider <s8nlschn@stud.uni-saarland.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## Copyright 2022 by Jonathan Baumann <joba00002@stud.uni-saarland.de>
## Copyright 2023 by Maximilian Löffler <s8maloef@stud.uni-saarland.de>
## All Rights Reserved.


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("igraph") # networks
requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("lubridate") # for date conversion

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split data --------------------------------------------------------------

#' Split project data in time-based ranges as specified
#'
#' Important: For given 'time.period' parameters (e.g., 3-month windows), the last bin may be a lot smaller
#' than the specified time period.
#'
#' @param project.data the *Data object from which the data is retrieved
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days" [default: "3 months"]
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range, in an
#'             *exclusive* manner). If set, the \code{time.period} parameter is ignored; consequently, \code{split.basis} and
#'             \code{sliding.window} do not make sense then either. [default: NULL]
#' @param number.windows the number of consecutive data objects to get from this function, implying equally
#'                       time-sized windows for all ranges. If set, the \code{time.period} and \code{bins} parameters are ignored;
#'                       consequently, \code{sliding.window} does not make sense then either.
#'                       [default: NULL]
#' @param split.basis the data name to use as the basis for split bins, either 'commits', 'mails', or 'issues'
#'                    [default: "commits"]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#' @param project.conf.new the new project config to construct the \code{RangeData} objects.
#'                         If \code{NULL}, a clone of \code{project.data$get.project.conf()} will be used.
#'                         [default: NULL]
#'
#' @return the list of RangeData objects, each referring to one time period
split.data.time.based = function(project.data, time.period = "3 months", bins = NULL,
                                 number.windows = NULL, split.basis = c("commits", "mails", "issues"),
                                 sliding.window = FALSE, project.conf.new = NULL) {

    # validate existence and type of the 'bins' parameter
    if (!is.null(bins) && !lubridate::is.POSIXct(bins)) {
        dates = parallel::mclapply(unlist(bins), get.date.from.string)
        if (any(is.na(dates))) {
            logging::logerror(paste("The bins parameter, if present, needs to be a vector",
                                    "whose elements represent dates"))
            stop("Stopped due to incorrect parameter types")
        }
    }

    split = split.data.by.time.or.bins(project.data, splitting.length = time.period, bins, split.by.time = TRUE,
                                        number.windows, split.basis, sliding.window, project.conf.new)
    return(split)
}

#' Split project data in activity-bin-based ranges as specified
#'
#' @param project.data the project data object from which the data is retrieved
#' @param activity.amount the amount of data elements with unique ids to be considered in a bin, an integer.
#' @param bins the bins by which data should be split. Comprises of two components:
#'             \code{vector}: Assigns elements of the \code{split.basis} column of \code{project.data} to bins.
#'             \code{bins}: Dates defining the start of bins (the last date defines the end of the last bin, in an
#'             *exclusive* manner).
#'             The expected format of \code{bins} is produced by \code{split.get.bins.activity.based}.
#' @param split.basis the data name to use as the basis for split bins, either 'commits', 'mails', or 'issues'
#'                    [default: "commits"]
#' @param sliding.window logical indicating whether a sliding-window approach was used when obtaining the \code{bins}.
#'
#' @return the list of RangeData objects, each referring to one bin
#'
#' @seealso split.get.bins.activity.based
split.data.by.bins = function(project.data, activity.amount, bins, split.basis = c("commits", "mails", "issues"),
                                     sliding.window) {

    # validate type of the 'bins' parameter
    if (is.null(bins) || !is.list(bins)) {
        logging::logerror("The bins parameter needs to be of type list, (is %s)", class(bins))
        stop("Stopped due to incorrect parameter types")
    }

    # validate existence and type of the 'bins' component of the 'bins' parameter
    if (!("bins" %in% names(bins))) {
        logging::logerror("The 'bins' parameter needs to include a component 'bins'")
        stop("Stopped due to incorrect parameter types")
    }

    dates = parallel::mclapply(bins[["bins"]], get.date.from.string)
    if (any(is.na(dates))) {
        logging::logerror(paste("The 'bins' component of the 'bins' parameter, needs to be a vector",
                                "whose elements represent dates"))
        stop("Stopped due to incorrect parameter types")
    }

    # validate existence and type of the 'vector' component of the 'bins' parameter
    if (!inherits(bins[["vector"]], "numeric")) {
        logging::logerror("The 'vector' component of the bins parameter needs to be a numeric vector")
        stop("Stopped due to incorrect parameter types")
    }

    split = split.data.by.time.or.bins(project.data, activity.amount, bins, split.by.time = FALSE,
                                       sliding.window = sliding.window, split.basis = split.basis)
    return(split)
}

#' Split project data by timestamps
#'
#' Splits project data into ranges, where the first range starts with the first timestamp
#' and the last range ends with the last timestamp.
#'
#' If timestamps are not provided, the custom event timestamps in \code{project.data} are
#' used instead.
#'
#' @param project.data the *Data object from which the data is retrieved
#' @param bins a vector of timestamps [default: NULL]
#' @param project.conf.new the new project config to construct the \code{RangeData} objects.
#'                         If \code{NULL}, a clone of \code{project.data$get.project.conf()} will be used.
#'                         [default: NULL]
#'
#' @return the list of RangeData objects, each referring to one time period
split.data.time.based.by.timestamps = function(project.data, bins = NULL, project.conf.new = NULL) {

    if (is.null(bins)) { # bins were not provided, use custom timestamps from project
        bins = unlist(project.data$get.custom.event.timestamps())
    }

    return (split.data.time.based(project.data, bins = bins, project.conf.new));
}

#' Split project data in activity-based ranges as specified
#'
#' Important: For a given amount of activity, the last set of data may be a lot smaller
#' than the specified amount.
#'
#' @param project.data the *Data object from which the data is retrieved
#' @param activity.type the type of activity used for splitting, either 'commits', 'mails', or 'issues'
#'                      [default: "commits"]
#' @param activity.amount the amount of activity describing the size of the ranges, a numeric, further
#'                        specified by 'activity.type' [default: 5000]
#' @param number.windows the number of consecutive data objects to get from this function
#'                       (implying an equally distributed amount of data in each range and
#'                       'sliding.window = FALSE') [default: NULL]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#' @param project.conf.new the new project config to construct the \code{RangeData} objects.
#'                         If \code{NULL}, a clone of \code{project.data$get.project.conf()} will be used.
#'                         [default: NULL]
#'
#' @return the list of RangeData objects, each referring to one time period
split.data.activity.based = function(project.data, activity.type = c("commits", "mails", "issues"),
                                     activity.amount = 5000, number.windows = NULL,
                                     sliding.window = FALSE, project.conf.new = NULL) {

    ## get basis for splitting process
    activity.type = match.arg(activity.type)

    ## get actual raw data
    data.sources = project.data$get.cached.data.sources("only.unfiltered")
    data = lapply(data.sources, function(ds) {
        ## build the name of the respective getter and call it
        function.name = DATASOURCE.TO.UNFILTERED.ARTIFACT.FUNCTION[[ds]]
        return(project.data[[function.name]]())
    })
    names(data) = data.sources

    ## if the data used by the split basis is not present, load it automatically
    if (!(activity.type %in% project.data$get.cached.data.sources("only.unfiltered"))) {
        function.name = DATASOURCE.TO.UNFILTERED.ARTIFACT.FUNCTION[[activity.type]]
        project.data[[function.name]]()
    }

    ## define ID columns for mails and commits
    id.column = list(
        commits = "hash",
        mails = "message.id",
        issues = "event.id"
    )

    ## get amount of available activity
    activity = length(unique(data[[activity.type]][[ id.column[[activity.type]] ]]))

    if (is.null(project.conf.new)) {
        ## Clone the project configuration, so that splitting repeatedly does not interfere
        ## with the same configuration.
        project.conf.new = project.data$get.project.conf()$clone()
    }

    ## activity amount given (number of windows NOT given)
    if (is.null(number.windows)) {
        if (activity < 1) {
            logging::logerror("The given amount of activity has to be strictly positive (given: %s).", activity)
            stop("Stopping due to missing data.")
        }
        ## compute the number of time windows according to the activity amount
        number.windows = ceiling(activity / activity.amount)
        if (activity < activity.amount) {
            activity.type.pretty = list(
                commits = "commits",
                mails = "mails",
                issues = "issue events"
            )[[activity.type]]
            logging::logwarn("Can not form bins of %s %s for splitting data %s, as there are only %s %s.",
                             activity.amount, activity.type.pretty, project.data$get.class.name(), activity, activity.type.pretty)
        }
    }
    ## number of windows given (ignoring amount of activity)
    else {
        ## check the breaking case
        if (number.windows < 1 || number.windows > activity) {
            logging::logerror("The given number of windows is not suitable for this
                              data object (given: %s).", number.windows)
            stop("Stopping due to illegally specified amount of windows to create.")
        }
        ## compute the amount of activity according to the number of specified windows
        activity.amount = ceiling(activity / number.windows)
        ## remove sliding windows as they do not make sense here
        sliding.window = FALSE
    }

    logging::loginfo("Splitting data '%s' into activity ranges of %s %s (%s windows).",
                     project.data$get.class.name(), activity.amount, activity.type, number.windows)

    ## get bins based on 'split.basis'. Here the 'include.duplicate.ids' parameter flag must be set, to
    ## retrieve bins which map every event to a bin including events with non-unique ids. This is important
    ## to ensure that every range really has 'activity.amount' many entries after splitting
    logging::logdebug("Getting activity-based bins.")
    bins.data = split.get.bins.activity.based(data[[activity.type]], id.column[[activity.type]],
                                              activity.amount, remove.duplicate.bins = TRUE, include.duplicate.ids = TRUE)
    bins = bins.data[["bins"]]
    bins.date = get.date.from.string(bins)

    ## split the data based on the extracted timestamps
    logging::logdebug("Splitting data based on time windows arising from activity bins.")
    cf.data = split.data.by.bins(project.data, bins = bins.data, activity.amount = activity.amount,
                                     sliding.window = sliding.window, split.basis = activity.type)

    ## perform additional steps for sliding-window approach:
    ## for activity-based sliding-window bins to work, we need to crop the data appropriately and,
    ## then, compute bins on the cropped data
    ## (only if there is more than one range until here)
    if (sliding.window && length(bins.date) <= 2) {
        logging::logwarn("Sliding-window approach does not apply for one range or less.")
    } else if (sliding.window) {
        ## get the list of unique items that are used for the bin computation and, thus, also the
        ## cropping of data
        items.unique = unique(data[[activity.type]][[ id.column[[activity.type]] ]])
        items.unique.count = length(items.unique)

        ## offsets used for cropping (half of the first bin)
        offset.start = floor(activity.amount / 2)
        items.cut = items.unique[seq_len(offset.start)]

        ## store the data again
        data.to.cut = data[[activity.type]][[ id.column[[activity.type]] ]] %in% items.cut
        data[[activity.type]] = data[[activity.type]][ !data.to.cut, ]

        ## clone the project data and update raw data to split it again
        project.data.clone = project.data$clone()
        project.data.clone$set.commits(data[["commits"]])
        project.data.clone$set.mails(data[["mails"]])
        project.data.clone$set.issues(data[["issues"]])

        ## split data for sliding windows
        cf.data.sliding = split.data.activity.based(project.data.clone, activity.type = activity.type,
                                                    activity.amount = activity.amount, sliding.window = FALSE,
                                                    project.conf.new = project.conf.new)

        ## extract bins
        bins.date.middle = attr(cf.data.sliding, "bins")

        ## Both, the last sliding range and the last regular range end at the very last item.
        ## This is the case because the end of the data is never cropped (like the beginning is).
        ## split.data.activity.based, which is invoked to obtain both set of ranges, creates
        ## ranges until all elements are in one.
        ##
        ## The conditional below inspects whether the very last item is in the first or the second
        ## half of the last regular range. If it is in the first half, there will be a sliding
        ## window which covers all items of the last regular range which makes the last regular
        ## range obsolete.
        ## Similarely if the last item is in the second half of the last regular range, there
        ## will be a sliding range (which started at the half of the last regular range) which
        ## contains only items also included in the last regular range, which makes the sliding
        ## range obsolete.
        length.of.last.range = items.unique.count %% activity.amount
        if (length.of.last.range > offset.start || length.of.last.range == 0) {
            cf.data.sliding = cf.data.sliding[-length(cf.data.sliding)]
            bins.date.middle = bins.date.middle[-length(bins.date.middle)]
        } else {
            cf.data = cf.data[-length(cf.data)]
            bins.date = bins.date[-length(bins.date)]
            bins = bins[-length(bins)]
        }

        ## append data to normally-split data
        cf.data = append(cf.data, cf.data.sliding)

        ## sort data object properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        cf.data = cf.data[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
        bins = get.date.string(bins.date)

        ## update project configuration
        project.conf.new$set.revisions(bins, bins.date, sliding.window = TRUE)
        for (cf in cf.data) {
            ## re-set project configuration due to object duplication
            cf.conf = cf$set.project.conf(project.conf.new, reset.environment = FALSE)
        }
    }

    ## add splitting information to project configuration
    project.conf.new$set.splitting.info(
        type = "activity-based",
        length = activity.amount,
        basis = activity.type,
        sliding.window = sliding.window,
        revisions = bins,
        revisions.dates = bins.date
    )

    ## set bin attribute
    attr(cf.data, "bins") = bins.date

    return(cf.data)
}

#' Map a list of networks to their corresponding range data, after splitting the
#' given project data (\code{project.data}) to the time ranges given by the networks'
#' names. The splitting can be more specifically configured with the parameter
#' \code{aggregation.level}, see below for more details.
#'
#' For this function to work properly, the list of networks needs to be named with
#' timestamp-ranges, which can be splitted using \code{get.range.bounds}. The easiest
#' way to achieve this is to use one of the \code{split.*} functions in this very file.
#' For example, the time ranges have a format like this:
#' "2017-01-01 23:57:01-2017-02-15 12:19:37", which can be split by the utility
#' function \code{get.range.bounds}, obtaining the range bounds as timestamps.
#'
#' Using different aggregation levels given by the parameter \code{aggregation.level},
#' it is possible to configure the exact treatment of range bounds and, thus, the
#' splitting of the given project data. The various aggregation levels work as follows:
#' - \code{"range"}: The project data will be split exactly to the time ranges specified
#'                   by the networks' names.
#' - \code{"cumulative"}: The project data will be split exactly to the time ranges
#'                   specified by the networks' names, but in a cumulative manner.
#' - \code{"all.ranges"}: The project data will be split exactly to the time range
#'                   specified by the start of the first network and end of the last
#'                   network. All data instances will contain the same data.
#' - \code{"project.cumulative"}: The same splitting as for \code{"cumulative"}, but all
#'                   data will start at the beginning of the project data and *not* at
#'                   the beginning of the first network.
#' - \code{"project.all.ranges"}: The same splitting as for \code{"all.ranges"}, but all
#'                   data will start at the beginning of the project data and *not* at
#'                   the beginning of the first network. All data instances will contain
#'                   the same data.
#' - \code{"complete"}: The same splitting as for \code{"all.ranges"}, but all data will
#'                   start at the beginning of the project data and end at the end of
#'                   the project data. All data instances will contain the same data.
#'
#' @param list.of.networks The network list
#' @param project.data The entire project data
#' @param aggregation.level One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See above for more details. [default: "range"]
#'
#' @return A list containing tuples with the keys "network" and "data", where, under "network", are
#'         the respective networks passed via \code{list.of.networks} and, under "data", are the
#'         split data instances of type \code{RangeData}.
#'
#' @seealso \code{aggregate.ranges}
split.data.by.networks = function(list.of.networks, project.data,
                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                        "project.cumulative", "project.all.ranges",
                                                        "complete")) {
    ## get the chosen aggregation level
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    ## get the timestamp data from the project data (needed for some aggr. levels)
    project.timestamps = project.data$get.data.timestamps(outermost = TRUE)

    ## get the list of ranges
    list.of.ranges = names(list.of.networks)
    ## aggregate ranges
    ranges.bounds = aggregate.ranges(
        list.of.ranges, project.start = project.timestamps[["start"]], project.end = project.timestamps[["end"]],
        aggregation.level = aggregation.level, raw = TRUE
    )

    ## split the data by the computed (and aggregated) ranges
    list.of.data = split.data.time.based.by.ranges(project.data, ranges.bounds)

    ## zip networks and range data
    net.to.range.list = mapply(
        list.of.networks, list.of.data, SIMPLIFY = FALSE,
        FUN = function(net, range.data) {
            net.to.range.entry = list(
                "network" = net,
                "data" = range.data
            )
            return(net.to.range.entry)
        }
    )

    ## properly set names for the result list
    names(net.to.range.list) = list.of.ranges

    return(net.to.range.list)
}

#' Split the given data to the given ranges and return the resulting list.
#'
#' Note: You may want to use any function \code{construct.*.ranges} to obtain
#' an appropriate sequence of ranges to pass to this function.
#'
#' @param project.data the \code{ProjectData} instance to be split
#' @param ranges the ranges to be used for splitting
#'
#' @return a list of \code{RangeData} instances, each representing one of the
#'         given ranges; the ranges are used as names for the list
split.data.time.based.by.ranges = function(project.data, ranges) {

    ## check whether all ranges are identical (then we only need to split the data once)
    if (length(ranges) > 1 && length(unique(ranges)) == 1) {
        ## aggregate range
        range.bounds = get.range.bounds(ranges[[1]])

        ## split data accordingly
        range.data = split.data.time.based(project.data, bins = range.bounds, sliding.window = FALSE)[[1]]

        ## clone range data objects (as all ranges are identical)
        data.split = lapply(ranges, function(x) range.data$clone())
    } else {
        ## aggregate ranges
        ranges.bounds = lapply(ranges, get.range.bounds)

        ## loop over all ranges and split the data accordingly:
        data.split = mapply(ranges, ranges.bounds, SIMPLIFY = FALSE, FUN = function(range, start.end) {
            ## 1) split the data to the current range
            range.data = split.data.time.based(project.data, bins = start.end, sliding.window = FALSE)[[1]]

            ## 2) return the data
            return (range.data)
        })
    }
    return(data.split)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split networks ----------------------------------------------------------

#' Discretizes a network (using the edge attribute "date") according to the given 'time.period',
#' to the given hard 'bins', or the given number of windows ('number.windows').
#'
#' Important: For given 'time.period' parameters (e.g., 3-month windows), the last bin may be a lot smaller
#' than the specified time period.
#'
#' Important notice: This function only works for unsimplified networks, where no edges have been
#' contracted, which would combine edge attributes, especially the "date" attribute.
#'
#' @param network the igraph network to split, needs to have an edge attribute named "date"
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days" [default: "3 months"]
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range, in an
#'             *exclusive* manner). If set, the 'time.period' and 'sliding.window' parameters are ignored.
#'             [default: NULL]
#' @param number.windows the number of consecutive networks to get from this function, implying equally
#'                       time-sized windows for all ranges. If set, the 'time.period' and 'bins' parameters are ignored;
#'                       consequently, 'sliding.window' does not make sense then either.
#'                       [default: NULL]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#' @param remove.isolates whether to remove isolates in the resulting split networks [default: TRUE]
#'
#' @return a list of igraph networks, each referring to one time period
split.network.time.based = function(network, time.period = "3 months", bins = NULL,
                                    number.windows = NULL,  sliding.window = FALSE,
                                    remove.isolates = TRUE) {
    ## extract date attributes from edges
    dates = get.date.from.unix.timestamp(igraph::get.edge.attribute(network, "date"))

    ## number of windows given (ignoring time period and bins)
    if (!is.null(number.windows)) {
        ## reset bins for the later algorithm
        bins = NULL
        ## ignore sliding windows
        sliding.window = FALSE
    }

    ## get bin information for all edges
    if (is.null(bins)) {
        ## get bins
        bins.info = split.get.bins.time.based(dates, time.period, number.windows)
        bins.vector = bins.info[["vector"]]
        bins.date = get.date.from.string(bins.info[["bins"]])
        bins = head(bins.info[["bins"]], -1)
    } else {
        ## specific bins are given, do not use sliding windows
        sliding.window = FALSE
        ## find bins for dates
        bins.date = get.date.from.string(bins)
        bins.vector = findInterval(dates, bins.date, all.inside = FALSE)
        bins = seq_len(length(bins.date) - 1) # the last item just closes the last bin
    }

    ## perform additional steps for sliding-window approach
    if (sliding.window) {
        ranges = construct.overlapping.ranges(start = min(bins.date), end = max(bins.date),
                                              time.period = time.period, overlap = 0.5, raw = FALSE,
                                              include.end.date = FALSE)
        logging::loginfo("Splitting network into overlapping time ranges [%s].",
                         paste(ranges, collapse = ", "))
        nets = split.network.time.based.by.ranges(network, ranges, remove.isolates)
    } else {
        revs = get.date.string(bins.date)
        ranges = construct.ranges(revs, sliding.window = FALSE)
        logging::loginfo("Splitting network into non-overlapping time ranges [%s].",
                         paste(ranges, collapse = ", "))
        nets = split.network.by.bins(network, bins, bins.vector, bins.date, remove.isolates)
    }

    ## set ranges as names
    names(nets) = ranges
    return(nets)
}

#' Discretizes a list of networks (using the edge attribute "date") according to the given 'time.period',
#' using the very same bins for all networks. The procedure is as follows:
#' 1) Use the earliest timestamp of all networks and the latest timestamp of all networks
#'    to compute the bins for splitting.
#' 2) All networks are then split using the computed and, thus, very same bins using the
#'    function \code{split.network.time.based}.
#' 3) The list of split networks is returned.
#'
#' For further information, see the documentation of \code{split.network.time.based}.
#'
#' Important notice: This function only works for unsimplified networks, where no edges have been
#' contracted, which would combine edge attributes, especially the "date" attribute.
#'
#' @param networks the igraph networks to split, needs to have an edge attribute named "date"
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days" [default: "3 months"]
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range, in an
#'             *exclusive* manner). If set, the 'time.period' and 'sliding.window' parameters are ignored.
#'             [default: NULL]
#' @param number.windows the number of consecutive networks to get for each network, implying equally
#'                       time-sized windows for all ranges. If set, the 'time.period' and 'bins' parameters are ignored;
#'                       consequently, 'sliding.window' does not make sense then either.
#'                       [default: NULL]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#' @param remove.isolates whether to remove isolates in the resulting split networks [default: TRUE]
#'
#' @return a list of network-splitting results (of length \code{length(networks)}), each item referring to a list
#'         of networks, each itself referring to one time period
split.networks.time.based = function(networks, time.period = "3 months", bins = NULL,
                                     number.windows = NULL, sliding.window = FALSE,
                                     remove.isolates = TRUE) {

    ## number of windows given (ignoring time period and bins)
    if (!is.null(number.windows)) {
        ## reset bins for the later algorithm
        bins = NULL
        ## ignore sliding windows
        sliding.window = FALSE
    }

    if (is.null(bins)) {
        ## get base network and obtain splitting information:
        ## 1) extract date attributes from edges
        networks.dates = lapply(networks, function(net) {
            dates = igraph::E(net)$date
            return(dates)
        })
        dates = unlist(networks.dates, recursive = FALSE)
        dates = get.date.from.unix.timestamp(dates)

        ## 2) get bin information
        if (sliding.window) {
            ranges = construct.overlapping.ranges(start = min(dates), end = max(dates),
                                                  time.period = time.period, overlap = 0.5, raw = FALSE,
                                                  include.end.date = TRUE)
        } else {
            bins.info = split.get.bins.time.based(dates, time.period, number.windows)
            bins.date = get.date.from.string(bins.info[["bins"]])
        }
    } else {
        ## specific bins are given, do not use sliding windows
        sliding.window = FALSE
        ## set the bins to use
        bins.date = bins
    }

    ## split all networks to the extracted bins
    networks.split = lapply(networks, function(net) {

        if (sliding.window) {
            nets = split.network.time.based.by.ranges(network = net, ranges = ranges,
                                                      remove.isolates = remove.isolates)
        } else {
            nets = split.network.time.based(network = net, bins = bins.date, sliding.window = sliding.window,
                                            remove.isolates = remove.isolates)
        }
        return(nets)
    })

    ## return the split networks
    return(networks.split)
}


#' Discretizes a network according to the given 'number.edges' or by a predefined 'number.windows'.
#'
#' Important: For a given amount of edges, the last set of data may be a lot smaller
#' than the specified amount.
#'
#' Important notice: This function only works for unsimplified networks, where no edges have been
#' contracted, which would combine edge attributes, especially the "date" attribute.
#'
#' @param network the igraph network to split
#' @param number.edges the amount of edges describing the size of the ranges
#'                     (implying an open number of resulting ranges) [default: 5000]
#' @param number.windows the number of consecutive networks to get from this function
#'                       (implying an equally distributed amount of edges in each range and
#'                       'sliding.window = FALSE) [default: NULL]
#' @param sliding.window logical indicating whether the splitting should be performed using
#'                       a sliding-window approach [default: FALSE]
#' @param remove.isolates whether to remove isolates in the resulting split networks [default: TRUE]
#'
#' @return a list of igraph networks, each referring to one period of activity
split.network.activity.based = function(network, number.edges = 5000, number.windows = NULL,
                                        sliding.window = FALSE, remove.isolates = TRUE) {
    ## get total edge count
    edge.count = igraph::ecount(network)

    ## number of edges given (number of windows NOT given)
    if (is.null(number.windows)) {
        if (edge.count < 1) {
            logging::logerror("The number of edges in the given network has to be
                              strictly positive (given: %s).", edge.count)
            stop("Stopping due to missing edges in given network.")
        }
        ## compute the number of time windows according to the number of edges per network
        number.windows = ceiling(edge.count / number.edges)
    }
    ## number of windows given (ignoring number of edges)
    else {
        ## check the breaking case
        if (number.windows < 1 || number.windows > edge.count) {
            logging::logerror("The given number of windows is not suitable for this
                              network (given: %s).", number.windows)
            stop("Stopping due to illegally specified amount of windows to create.")
        }
        ## compute the amount of activity according to the number of specified windows
        number.edges = ceiling(edge.count / number.windows)
        ## remove sliding windows as they do not make sense here
        sliding.window = FALSE
    }

    logging::loginfo("Splitting network into activity ranges of %s edges, yielding %s windows.",
                     number.edges, number.windows)

    ## get dates in a data.frame for splitting purposes
    df = data.frame(
        date = get.date.from.unix.timestamp(igraph::get.edge.attribute(network, "date")),
        my.unique.id = seq_len(edge.count) # as a unique identifier only
    )
    ## sort by date
    df = df[ with(df, order(date)), ]

    ## identify bins
    logging::logdebug("Getting bins for activity-based splitting based on amount of edges.")
    bins.data = split.get.bins.activity.based(df, "my.unique.id", activity.amount = number.edges,
                                              remove.duplicate.bins = FALSE)
    bins.date = bins.data[["bins"]]
    bins.vector = bins.data[["vector"]]
    bins.vector = bins.vector[ with(df, order(my.unique.id)) ] # re-order to get igraph ordering
    bins = sort(unique(bins.vector))
    ## split network by bins
    networks = split.network.by.bins(network, bins, bins.vector, remove.isolates = remove.isolates)

    if (number.edges >= edge.count) {
        logging::logwarn("Sliding-window approach does not apply: not enough edges (%s) for number of edges %s",
                         edge.count, number.edges)
        sliding.window = FALSE
    }

    ## perform additional steps for sliding-window approach
    ## for activity-based sliding-window bins to work, we need to crop edges appropriately and,
    ## then, compute bins on the cropped networks
    if (sliding.window) {

        ## get edge ids ordered by date
        edges.by.date = df[["my.unique.id"]]

        ## offsets used for cropping (half the first/last bin)
        offset.start = floor(number.edges / 2)
        edges.cut = edges.by.date[seq_len(offset.start)]

        ## delete edges from the network and create a new network
        network.cut = igraph::delete.edges(network, igraph::E(network)[edges.cut])

        ## split network for sliding windows
        networks.sliding = split.network.activity.based(network.cut, number.edges = number.edges,
                                                        sliding.window = FALSE)

        ## compute bins for sliding windows: pairwise middle between dates
        bins.date.middle = attr(networks.sliding, "bins")

        ## Both, the last sliding network and the last regular network end at the very last edge.
        ## This is the case because the end of the edges is never cropped (like the beginning is).
        ## Both split.network.activity.based, and split.network.by.bins, which are invoked to obtain
        ## the two set of networks, creates networks until all edges are contained.
        ##
        ## The conditional below inspects whether the very last edge is in the first or the second
        ## half of the last regular network. If it is in the first half, there will be a sliding
        ## network which covers all edges of the last regular network which makes the last regular
        ## network obsolete.
        ## Similarely if the last edge is in the second half of the last regular network, there
        ## will be a sliding network (which started at the half of the last regular network) which
        ## contains only edges also included in the last regular network, which makes the sliding
        ## network obsolete.
        length.of.last.range = edge.count %% number.edges
        if (length.of.last.range > offset.start || length.of.last.range == 0) {
            networks.sliding = networks.sliding[-length(networks.sliding)]
            bins.date.middle = bins.date.middle[-length(bins.date.middle)]
        } else {
            networks = networks[-length(networks)]
            bins.date = bins.date[-length(bins.date)]
            bins = bins[-length(bins)]
        }

        ## append sliding networks to normally-split networks
        networks = append(networks, networks.sliding)

        ## sort networks properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        networks = networks[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
    }

    ## set bin attribute
    attr(networks, "bins") = bins.date

    ## set ranges as names
    revs = get.date.string(bins.date)
    names(networks) = construct.ranges(revs, sliding.window = sliding.window)

    ## issue warning if ranges are not unique
    if (any(duplicated(names(networks)))) {
        logging::logwarn(
            paste("Due to the splitting, there are duplicated range names.",
                  "You can correct these by calling the function 'split.unify.range.names()'",
                  "and providing the range names.")
        )
    }

    return(networks)
}

#' Split the given network to the given ranges and return the resulting list.
#'
#' Note: You may want to use any function \code{construct.*.ranges} to obtain
#' an appropriate sequence of ranges to pass to this function.
#'
#' @param network the network to be split
#' @param ranges the ranges to be used for splitting
#' @param remove.isolates whether to remove isolates in the resulting split networks [default: TRUE]
#'
#' @return a list of networks, each representing one of the given ranges; the
#'         ranges are used as names for the list
split.network.time.based.by.ranges = function(network, ranges, remove.isolates = TRUE) {

    ## aggregate ranges
    ranges.bounds = lapply(ranges, get.range.bounds)

    ## loop over all ranges and split the network accordingly:
    nets.split = lapply(ranges.bounds, function(bounds) {
            ## 1) split the network to the current range
            range.net = split.network.time.based(network, bins = bounds, sliding.window = FALSE,
                                                 remove.isolates = remove.isolates)[[1]]

            ## 2) return the network
            return (range.net)
        }
    )

    return(nets.split)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Split raw data ----------------------------------------------------------

#' Split the given datafame by the given bins.
#'
#' @param df a data.frame to be split
#' @param bins a vector with the length of 'nrow(df)' assigning a bin for each row of 'df'
#'
#' @return a list of data.frames, with the length of 'unique(bins)'
split.dataframe.by.bins = function(df, bins) {
    logging::logdebug("split.dataframe.by.bins: starting.")
    df.split = split(df, bins)
    logging::logdebug("split.dataframe.by.bins: finished.")
    return(df.split)
}

#' Split the given data by the given bins, in increasing order of the bin identifiers.
#'
#' @param network a network
#' @param bins a vector with the unique bin identifiers, describing the order in which the bins are created
#' @param bins.vector a vector of length 'ecount(network)' assigning a bin for each edge of 'network'
#' @param bins.date a vector of dates representing the start of each bin. If present, then the dates will be set
#'                  as an attribute on the returned networks [default: NULL]
#' @param remove.isolates whether to remove isolates in the resulting split networks [default: TRUE]
#'
#' @return a list of networks, with the length of 'unique(bins.vector)'
split.network.by.bins = function(network, bins, bins.vector, bins.date = NULL, remove.isolates = TRUE) {
    logging::logdebug("split.network.by.bins: starting.")
    ## create a network for each bin of edges
    nets = parallel::mclapply(bins, function(bin) {
        logging::logdebug("Splitting network: bin %s", bin)
        ## identify edges in the current bin
        edges = igraph::E(network)[ bins.vector == bin ]
        ## create network based on the current set of edges
        g = igraph::subgraph.edges(network, edges, delete.vertices = remove.isolates)
        return(g)
    })
    ## set 'bins' attribute, if specified
    if (!is.null(bins.date)) {
        attr(nets, "bins") = bins.date
    }
    logging::logdebug("split.network.by.bins: finished.")
    return(nets)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Internal helper functions for data splitting ----------------------------

#' Split project data in time-based or activity-bin-based ranges as specified
#'
#' @param project.data the *Data object from which the data is retrieved
#' @param splitting.length either \code{time.period} from \code{split.data.time.based}
#'                         or \code{activity.amount} from \code{split.data.by.bins}
#' @param bins either formatted as the \code{bins} parameter of \code{split.data.time.based}
#'             or as the \code{bins} parameter of \code{split.data.by.bins}
#' @param split.by.time logical indicating whether splitting is done time-based or activity-bins-based
#' @param number.windows see \code{number.windows} from \code{split.data.time.based}
#'                       [default: NULL]
#' @param split.basis the data source to use as the basis for split bins, either 'commits', 'mails', or 'issues'
#'                    [default: "commits"]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#' @param project.conf.new the new project config to construct the \code{RangeData} objects.
#'                         If \code{NULL}, a clone of \code{project.data$get.project.conf()} will be used.
#'                         [default: NULL]
#'
#' @return the list of RangeData objects, each referring to one time period
#'
#' @seealso split.data.time.based
#' @seealso split.data.by.bins
split.data.by.time.or.bins = function(project.data, splitting.length, bins, split.by.time,
                                      number.windows = NULL, split.basis = c("commits", "mails", "issues"),
                                      sliding.window = FALSE, project.conf.new = NULL) {

    ## get basis for splitting process
    split.basis = match.arg(split.basis)

    ## if the data used by the split basis is not present, load it automatically
    if (!(split.basis %in% project.data$get.cached.data.sources("only.unfiltered"))) {
        function.name = DATASOURCE.TO.UNFILTERED.ARTIFACT.FUNCTION[[split.basis]]
        project.data[[function.name]]()
    }

    ## get actual raw data
    data.to.split = project.data$get.cached.data.sources("only.unfiltered")

    data = lapply(data.to.split, function(ds) {
        ## build the name of the respective getter and call it
        function.name = DATASOURCE.TO.UNFILTERED.ARTIFACT.FUNCTION[[ds]]
        return(project.data[[function.name]]())
    })
    names(data) = data.to.split

    ## load available additional data sources
    additional.data.sources = project.data$get.cached.data.sources("only.additional")
    additional.data = lapply(additional.data.sources, function(ds) {
        ## build the name of the respective getter and call it
        function.name = DATASOURCE.TO.ADDITIONAL.ARTIFACT.FUNCTION[[ds]]
        return(project.data[[function.name]]())
    })
    names(additional.data) = additional.data.sources

    ## number of windows given (ignoring time period and bins)
    if (!is.null(number.windows)) {
        ## reset bins for the later algorithm
        bins = NULL
        ## remove sliding windows
        sliding.window = FALSE
    }

    ## indicates if time-based splitting is performed using bins
    split.time.based.with.bins = FALSE

    ## if bins are NOT given explicitly
    if (is.null(bins)) {
        ## get bins based on split.basis
        bins = split.get.bins.time.based(data[[split.basis]][["date"]], splitting.length, number.windows)$bins
        bins.labels = head(bins, -1)
        ## logging
        logging::loginfo("Splitting data '%s' into time ranges of %s based on '%s' data.",
                         project.data$get.class.name(), splitting.length, split.basis)
    }
    ## when bins are given explicitly, get bins based on parameter
    else {
        if (split.by.time) {
            split.time.based.with.bins = TRUE
            split.basis = NULL
            bins = get.date.from.string(bins)
            bins = get.date.string(bins)
            ## remove sliding windows
            sliding.window = FALSE
        } else {
            ## sliding windows do not need to be removed here, as sliding windows and bins
            ## are not contradicting in activity-based splitting
            bins.vector = bins[["vector"]]
            bins = bins[["bins"]]
        }
        bins.labels = head(bins, -1)
        ## logging
        logging::loginfo("Splitting data '%s' into time ranges [%s].",
                         project.data$get.class.name(), paste(bins, collapse = ", "))
    }
    bins.date = get.date.from.string(bins)

    ## construct ranges
    bins.ranges = construct.ranges(bins)
    names(bins.ranges) = bins.ranges

    if ((length(bins.ranges) <= 1) && sliding.window) {
        logging::logwarn("Sliding-window approach does not apply for one range or less.")
        sliding.window = FALSE
    }

    if (is.null(project.conf.new)) {
        ## Clone the project configuration, so that splitting repeatedly does not interfere
        ## with the same configuration.
        project.conf.new = project.data$get.project.conf()$clone()
    }

    if (!sliding.window || !split.by.time) {
        ## split data
        data.split = parallel::mclapply(data.to.split, function(df.name) {
            logging::logdebug("Splitting %s.", df.name)
            ## identify bins for data
            df = data[[df.name]]
            df.bins = if (!split.by.time && (df.name == split.basis))
                        bins.vector
                      else
                        findInterval(df[["date"]], bins.date, all.inside = FALSE)
            ## split data according to df.bins
            df.split = split(df, df.bins)
            ## add proper labels/names
            names(df.split) = sapply(as.integer(names(df.split)), function(bin) bins[bin])
            return(df.split)
        })
        ## set the names to the data sources obtained earlier
        names(data.split) = data.to.split

        ## re-arrange data to get the proper list of data per range
        logging::logdebug("Re-arranging data.")
        data.split = parallel::mclapply(bins.labels, function(bin) lapply(data.split, `[[`, bin))
        names(data.split) = bins.ranges

        ## adapt project configuration
        project.conf.new$set.revisions(bins, bins.date)

        ## construct RangeData objects
        logging::logdebug("Constructing RangeData objects.")

        cf.data = parallel::mclapply(bins.ranges, function(range) {
            logging::logdebug("Constructing data for range %s.", range)
            ## construct object for current range
            cf.range.data = RangeData$new(project.conf.new, range)
            ## get data for current range
            df.list = data.split[[range]]

            ## set main data sources: commits, mails, issues
            for (data.source in data.to.split) {
                setter.name = sprintf("set.%s", data.source)
                cf.range.data[[setter.name]](df.list[[data.source]])
            }
            ## set additional data sources: authors, commit.messages, pasta, synchronicity
            for (data.source in additional.data.sources) {
                setter.name = sprintf("set.%s", data.source)
                cf.range.data[[setter.name]](additional.data[[data.source]])
            }

            return(cf.range.data)
        })

    } else {
        ## perform different steps for sliding-window approach of time-based splitting

        ranges = construct.overlapping.ranges(start = min(bins.date), end = max(bins.date),
                                              time.period = splitting.length, overlap = 0.5, raw = FALSE,
                                              include.end.date = FALSE) # bins have already been prepared correctly
        bins.info = construct.overlapping.ranges(start = min(bins.date), end = max(bins.date),
                                                 time.period = splitting.length, overlap = 0.5, raw = TRUE,
                                                 include.end.date = FALSE) # bins have already been prepared correctly
        bins.date = get.bin.dates.from.ranges(bins.info)
        bins = get.date.string(bins.date)

        logging::loginfo("Splitting data '%s' into time ranges using sliding windows [%s].",
                         project.data$get.class.name(), ranges)
        cf.data = split.data.time.based.by.ranges(project.data, ranges)

        ## update project configuration
        project.conf.new$set.revisions(bins, bins.date, sliding.window = TRUE)
        for (cf in cf.data) {
            ## re-set project configuration due to object duplication
            cf.conf = cf$set.project.conf(project.conf.new)
        }
    }

    ## add splitting information to project configuration
    project.conf.new$set.splitting.info(
        type = if (split.by.time) "time-based" else "activity-based",
        length = if (split.time.based.with.bins) {
                    bins
                 }
                 else {
                    if (!is.null(number.windows)) {
                        as.character(lubridate::as.period(
                            get.time.period.by.amount(
                                min(data[[split.basis]][["date"]]),
                                max(data[[split.basis]][["date"]]),
                                number.windows
                            )
                        ))
                    }
                    else splitting.length
                },
        basis = split.basis,
        sliding.window = sliding.window,
        revisions = bins,
        revisions.dates = bins.date
    )

    ## set bin attribute
    attr(cf.data, "bins") = bins.date

    ## return list of RangeData objects
    return(cf.data)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Unification of range names ----------------------------------------------

#' Unify range names, i.e., add numbering suffixes to duplicate range names.
#'
#' To avoid duplicate ranges, any duplicate range in the given list of ranges are suffixed
#' with the pattern ' (#)', where '#' is a number. Also the first duplicate is renamed,
#' which results in the existence of the suffix ' (1)'.
#'
#' Note: The ranges need to be sorted properly, unsorted ranges will not work with this
#' function as expected. For example, consider the following example:
#' c("A-B", "A-B", "B-C", "A-B", "B-C") --> c("A-B (1)", "A-B (2)", "B-C (1)", "A-B (1)", "B-C (1)")
#'
#' @param ranges the range names to unify
#'
#' @return the unified ranges, suffixed by ' (#)' if duplicated
split.unify.range.names = function(ranges) {

    ## identify duplicated ranges
    ranges.dup = duplicated(ranges) | duplicated(ranges, fromLast = TRUE)
    ranges.numbers.raw = rle(ranges)
    ranges.numbers = unlist(lapply(ranges.numbers.raw$lengths, seq_len))

    ## transform ranges
    ranges.corrected = mapply(ranges, ranges.dup, ranges.numbers, USE.NAMES = FALSE,
           FUN = function(range, dup, number) {
               ifelse(
                   dup,
                   sprintf("%s (%s)", range, number),
                   range
               )
           }
    )

    return(ranges.corrected)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Bin identification ------------------------------------------------------

#' Compute bin information for a time-based splitting based on the given time period.
#'
#' Note: As the last bound of a bin is exclusive, the end of the last bin is always
#' set to \code{max(dates) + 1} to include the last date!
#'
#' @param dates the dates that are to be split into several bins
#' @param time.period the time period each bin lasts
#' @param number.windows the number of consecutive time windows to get from this function. If set,
#'                       the 'time.period' parameter is ignored. [default: NULL]
#'
#' @return a list,
#'         the item 'vector': the bins each item in 'dates' belongs to,
#'         the item 'bins': the bin labels, each spanning the length of 'time.period';
#'             each item in the vector indicates the start of a bin, although the last
#'             item indicates the end of the last bin
split.get.bins.time.based = function(dates, time.period, number.windows = NULL) {
    logging::logdebug("split.get.bins.time.based: starting.")

    ## generate date bins from given dates
    if (is.null(number.windows)) {
        dates.breaks = generate.date.sequence(min(dates), max(dates), time.period)
    } else {
        dates.breaks = generate.date.sequence(min(dates), max(dates), length.out = number.windows)
    }
    ## as the last bin bound is exclusive, we need to add a second to it
    dates.breaks[length(dates.breaks)] = max(dates) + 1
    ## generate charater strings for bins
    dates.breaks.chr = get.date.string(head(dates.breaks, -1))

    ## find bins for given dates
    dates.bins = findInterval(dates, dates.breaks, all.inside = FALSE)
    ## convert to character factor and set factor's levels appropriately
    dates.bins = factor(dates.breaks.chr[dates.bins], levels = dates.breaks.chr)

    logging::logdebug("split.get.bins.time.based: finished.")

    ## return properly
    return(list(
        vector = dates.bins,
        bins = get.date.string(dates.breaks)
    ))
}

#' Compute bin information for a activity-based splitting based on the given amount of activity
#' based on the actual order of the rows in the given data.frame 'df'.
#'
#' @param df the sorted data.frame representing the data
#' @param id a character string denoting the ID column of the data.frame 'df'
#' @param activity.amount the amount of activity denoting the number of unique items
#'                        in each split bin [default: 5000]
#' @param remove.duplicate.bins remove duplicate bin borders? [default: FALSE]
#' @param include.duplicate.ids include entries of the \code{df} with non-unique ids
#'                              in the creation of the bins. This should! not change bin borders
#'                              as entries with the same id should! share the same \code{date} attribute.
#'                              [default: FALSE]
#'
#' @return a list,
#'         the item 'vector': the bins each row in 'df' belongs to (increasing integers),
#'         the item 'bins': the bin labels,  described by dates, each bin containing
#'         'activity.amount' many unique items; each item in the vector indicates
#'         the start of a bin, although the last item indicates the end of the last bin
split.get.bins.activity.based = function(df, id, activity.amount, remove.duplicate.bins = FALSE,
                                         include.duplicate.ids = FALSE) {
    logging::logdebug("split.get.bins.activity.based: starting")
    ## get the unique integer IDs for each item in 'id' column
    ids = df[[id]]
    ids.unique = unique(ids)
    ## compute split bins
    bins.number.complete = length(ids.unique) %/% activity.amount
    bins.number.incomplete = length(ids.unique) %% activity.amount
    bins.activity = c(
        if (bins.number.complete != 0) rep(seq_len(bins.number.complete), each = activity.amount),
        rep(bins.number.complete + 1, bins.number.incomplete)
    )

    ## pad bins with entries for all duplicate ids
    if (include.duplicate.ids) {
        bins.activity.padded = c()
        for (i in seq_along(ids)) {
            ## create an extra entry for every duplicate id in the same bin as
            ## the first occurance of the id
            current.bin = bins.activity[ which(ids.unique == ids[i]) ]
            bins.activity.padded = c(bins.activity.padded, current.bin)
        }
        bins.activity = bins.activity.padded
    }
    bins.number = max(bins.activity)

    ## join ids and bin numbers
    bins.mapping = data.frame(
        id = if (include.duplicate.ids) ids else ids.unique,
        bin = bins.activity
    )

    ## get the start (and end) date for all bins
    bins.date = parallel::mclapply(seq_len(bins.number), function(bin) {
        ## get the ids in the bin
        ids = bins.mapping[ bins.mapping[["bin"]] == bin, "id"]
        ## grab dates for the ids
        dates = df[df[[id]] %in% ids, "date"]

        ## get min and max date
        dates.min = min(dates)
        dates.max = max(dates)

        ## add 1 second to last range's dates.max to ensure
        ## that the last items are included by cut(..., right = TRUE)
        dates.max = dates.max + 1

        ## return earliest date and, if in last bin, most recent date
        return(c(
            dates.min,
            if (bin == bins.number) dates.max
        ))
    })
    ## unlist bins
    bins.date = do.call(base::c, bins.date)
    ## convert to character strings
    bins.date.char = get.date.string(bins.date)

    ## if we have a duplicate bin border, merge the two things
    if (remove.duplicate.bins && any(duplicated(bins.date))) {
        ## find all duplicate bins (decreasing order)
        duplicated.idx = sort(which(duplicated(bins.date)), decreasing = TRUE)
        ## for each duplicate, we modify the current results appropriately
        for (idx in duplicated.idx) {
            ## 1) remove the date from the bins
            bins.date = bins.date[-idx]
            ## 2) remove the corresponding string
            bins.date.char = bins.date.char[-idx]
            ## 3) decrease all indices by 1 that are higher than the current
            bins.activity = sapply(bins.activity, function(b) {
                if (b >= idx) b - 1 else b
            })
        }
    }

    logging::logdebug("split.get.bins.activity.based: finished.")
    return(list(
        vector = bins.activity,
        bins = bins.date.char
    ))
}

