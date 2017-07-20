## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Sofie Kemper, 2017
## kemperso@fim.uni-passau.de
## (c) Raphael Nömmer, 2017
## noemmer@fim.uni-passau.de
## (c) Christian Hechtl, 2017
## hechtl@fim.uni-passau.de


## libraries
requireNamespace("igraph")
requireNamespace("logging")
requireNamespace("parallel")


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## High-level functionality
##

#' Split project data in time-based ranges as specified
#'
#' Important: For given 'time.period' parameters (e.g., 3-month windows), the last bin may be a lot smaller
#' than the specified time period.
#'
#' @param project.data the Codeface*Data object from which the data is retrieved
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days"
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range).
#'             If set, the 'time.period' parameter is ignored; consequently, 'split.basis' does not make sense then.
#' @param split.basis the data name to use as the basis for split bins, either 'commits' or 'mails'
#'                    [default: commits]
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#'
#' @return the list of RangeData objects, each referring to one time period
split.data.time.based = function(project.data, time.period = "3 months", bins = NULL,
                                 split.basis = c("commits", "mails"), sliding.window = FALSE) {
    ## get actual raw data
    data = list(
        commits = project.data$get.commits.raw(),
        mails = project.data$get.mails()
    )
    split.data = names(data)
    names(split.data) = split.data

    ## get basis for splitting process
    split.basis = match.arg(split.basis)

    ## if bins are NOT given explicitly
    if (is.null(bins)) {
        ## get bins based on split.basis
        bins = split.get.bins.time.based(data[[split.basis]][["date"]], time.period)$bins
        bins.labels = head(bins, -1)
        split.by.bins = FALSE
        ## logging
        logging::loginfo("Splitting data '%s' into time ranges of %s based on '%s' data.",
                         project.data$get.class.name(), time.period, split.basis)
    }
    ## when bins are given explicitly
    else {
        ## get bins based on parameter
        split.basis = NULL
        bins = strftime(bins)
        bins.labels = head(bins, -1)
        split.by.bins = TRUE
        ## logging
        logging::loginfo("Splitting data '%s' into time ranges [%s].",
                         project.data$get.class.name(), paste(bins, collapse = ", "))
    }
    bins.date = as.POSIXct(bins)

    ## construct ranges
    bins.ranges = construct.ranges(bins)
    names(bins.ranges) = bins.ranges

    ## split data
    data.split = parallel::mclapply(split.data, function(df.name) {
        logging::logdebug("Splitting %s.", df.name)
        ## identify bins for data
        df = data[[df.name]]
        df.bins = findInterval(df[["date"]], bins.date, all.inside = FALSE)
        ## split data according to df.bins
        df.split = split(df, df.bins)
        ## add proper labels/names
        names(df.split) = sapply(as.integer(names(df.split)), function(bin) bins[bin])
        return(df.split)
    })

    ## re-arrange data to get the proper list of data per range
    logging::logdebug("Re-arranging data.")
    data.split = parallel::mclapply(bins.labels, function(bin) lapply(data.split, `[[`, bin))
    names(data.split) = bins.ranges

    ## adapt project configuration
    project.data$get.project.conf()$set.revisions(bins, bins.date)

    ## construct RangeData objects
    logging::logdebug("Constructing RangeData objects.")
    cf.data = parallel::mclapply(bins.ranges, function(range) {
        logging::logdebug("Constructing data for range %s.", range)
        ## construct object for current range
        cf.range.data = RangeData$new(project.data$get.project.conf(), range)
        ## TODO add revision.callgraph parameter
        ## get data for current range
        df.list = data.split[[range]]

        ## set data
        ## 1) commits
        cf.range.data$set.commits.raw(df.list[["commits"]])
        ## 2) mails
        cf.range.data$set.mails(df.list[["mails"]])
        ## 3) synchronicity data
        cf.range.data$set.synchronicity(project.data$get.synchronicity())
        ## 4) ID--author mapping
        cf.range.data$set.authors(project.data$get.authors())
        ## 5) PaStA data
        cf.range.data$set.pasta(project.data$get.pasta())

        return(cf.range.data)
    })

    ## perform additional steps for sliding-window approach
    ## (only if there is more than one range until here)
    if (sliding.window && length(bins.ranges) <= 1) {
        logging::logwarn("Sliding-window approach does not apply for one range or less.")
    } else if (sliding.window) {
        ## compute bins for sliding windows: pairwise middle between dates
        bins.date.middle = mapply(
            bins.date[1:(length(bins.date) - 1)],
            bins.date[2:length(bins.date)],
            FUN = function(d1, d2) d1 + ((d2 - d1) / 2)
        )
        bins.date.middle = as.POSIXct(bins.date.middle, origin = "1970-01-01")

        ## split data for sliding windows
        cf.data.sliding = split.data.time.based(project.data, bins = bins.date.middle,
                                                split.basis = split.basis, sliding.window = FALSE)

        ## append data to normally-split data
        cf.data = append(cf.data, cf.data.sliding)

        ## sort data object properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        cf.data = cf.data[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
        bins = strftime(bins.date)

        ## update project configuration
        project.data$get.project.conf()$set.revisions(bins, bins.date, sliding.window = TRUE)
        for (cf in cf.data) {
            ## re-set project configuration due to object duplication
            cf.conf = cf$set.project.conf(project.data$get.project.conf())
        }
    }

    ## add splitting information to project configuration
    project.data$get.project.conf()$set.splitting.info(
        type = "time-based",
        length = if (split.by.bins) bins else time.period,
        basis = split.basis,
        sliding.window = sliding.window,
        revisions = bins,
        revisions.dates = bins.date
    )

    ## return list of RangeData objects
    return(cf.data)
}

#' Split project data in activity-based ranges as specified
#'
#' Important: For a given amount of activity, the last set of data may be a lot smaller
#' than the specified amount.
#'
#' @param project.data the Codeface*Data object from which the data is retrieved
#' @param activity.type the type of activity used for splitting, either 'commits' or 'mails' [default: commits]
#' @param activity.amount the amount of activity describing the size of the ranges, a numeric, further
#'                        specified by 'activity.type'
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#'
#' @return the list of RangeData objects, each referring to one time period
split.data.activity.based = function(project.data, activity.type = c("commits", "mails"),
                                     activity.amount, sliding.window = FALSE) {

    ## get basis for splitting process
    activity.type = match.arg(activity.type)
    logging::loginfo("Splitting data '%s' into activity ranges of %s %s.",
                     project.data$get.class.name(), activity.amount, activity.type)

    ## get actual raw data
    data = list(
        commits = project.data$get.commits.raw(),
        mails = project.data$get.mails()
    )

    ## define ID columns for mails and commits
    id.column = list(
        commits = "hash",
        mails = "message.id"
    )

    ## get bins based on split.basis
    logging::logdebug("Getting activity-based bins.")
    bins.data = split.get.bins.activity.based(data[[activity.type]], id.column[[activity.type]], activity.amount)
    bins = bins.data[["bins"]]
    bins.date = as.POSIXct(bins)

    ## split the data based on the extracted timestamps
    logging::logdebug("Splitting data based on time windows arising from activity bins.")
    cf.data = split.data.time.based(project.data, bins = bins.date, split.basis = activity.type)

    ## perform additional steps for sliding-window approach:
    ## for activity-based sliding-window bins to work, we need to crop the data appropriately and,
    ## then, compute bins on the cropped data
    ## (only if there is more than one range until here)
    if (sliding.window && length(bins.date) <= 2) {
        logging::logwarn("Sliding-window approach does not apply for one range or less.")
    } else if (sliding.window) {
        ## get the list of unique items that are used for the bin computation and, thus, also the
        ## cropping of data
        items.unique = unique(data[[ activity.type ]][[ id.column[[activity.type]] ]])
        items.unique.count = length(items.unique)

        ## offsets used for cropping (half the first/last bin)
        offset.start = floor(activity.amount / 2)
        offset.end = floor((items.unique.count %% activity.amount) / 2)
        ## cut the data appropriately
        items.cut = c(
            items.unique[1:offset.start],
            items.unique[(items.unique.count - offset.end):items.unique.count]
        )

        ## store the data again
        data.to.cut = data[[ activity.type ]][[ id.column[[activity.type]] ]] %in% items.cut
        data[[ activity.type ]] = data[[ activity.type ]][ !data.to.cut, ]

        ## clone the project data and update raw data to split it again
        project.data.clone = project.data$clone()
        project.data.clone$set.commits.raw(data[["commits"]])
        project.data.clone$set.mails(data[["mails"]])

        ## split data for sliding windows
        cf.data.sliding = split.data.activity.based(project.data.clone, activity.type = activity.type,
                                                    activity.amount = activity.amount, sliding.window = FALSE)

        ## append data to normally-split data
        cf.data = append(cf.data, cf.data.sliding)

        ## compute bins for sliding windows: pairwise middle between dates
        bins.date.middle = attr(cf.data.sliding, "bins")

        ## sort data object properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        cf.data = cf.data[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
        bins = strftime(bins.date)

        ## update project configuration
        project.data$get.project.conf()$set.revisions(bins, bins.date, sliding.window = TRUE)
        for (cf in cf.data) {
            ## re-set project configuration due to object duplication
            cf.conf = cf$set.project.conf(project.data$get.project.conf(), reset.environment = FALSE)
        }
    }

    ## add splitting information to project configuration
    project.data$get.project.conf()$set.splitting.info(
        type = "activity-based",
        length = activity.amount,
        basis = activity.type,
        sliding.window = sliding.window,
        revisions = bins,
        revisions.dates = bins.date
    )

    ## set bin attribute for sliding-window functionality
    attr(cf.data, "bins") = bins.date

    return(cf.data)
}

#' Discretizes a network (using the edge attribute "date") according to the given 'time.period'
#' or to the given hard 'bins'.
#'
#' Important: For given 'time.period' parameters (e.g., 3-month windows), the last bin may be a lot smaller
#' than the specified time period.
#'
#' Important notice: This function only works for unsimplified networks, where no edges have been
#' contracted, which would combine edge attributes, especially the "date" attribute.
#'
#' @param network the igraph network to split, needs to have an edge attribute named "date"
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days"
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range).
#'             If set, the 'time.period' parameter is ignored.
#' @param sliding.window logical indicating whether the splitting should be performed using a sliding-window approach
#'                       [default: FALSE]
#'
#' @return a list of igraph networks, each referring to one time period
split.network.time.based = function(network, time.period = "3 months", bins = NULL,
                                    sliding.window = FALSE) {
    ## extract date attributes from edges
    dates = as.POSIXct(igraph::get.edge.attribute(network, "date"), origin="1970-01-01")

    ## get bin information for all edges
    if (!is.null(bins)) {
        bins.date = as.POSIXct(bins)
        bins.vector = findInterval(dates, bins.date, all.inside = FALSE)
        bins = 1:(length(bins.date) - 1) # the last item just closes the last bin
        ## logging
        logging::loginfo("Splitting network into bins [%s].", paste(bins.date, collapse = ", "))
    } else {
        bins.info = split.get.bins.time.based(dates, time.period)
        bins.vector = bins.info[["vector"]]
        bins.date = as.POSIXct(bins.info[["bins"]])
        bins = head(bins.info[["bins"]], -1)
        ## logging
        logging::loginfo("Splitting network into time ranges [%s].",
                         paste(bins.info[["bins"]], collapse = ", "))
    }

    nets = split.network.by.bins(network, bins, bins.vector)

    ## perform additional steps for sliding-window approach
    if (sliding.window) {
        ## compute bins for sliding windows: pairwise middle between dates
        bins.date.middle = mapply(
            bins.date[1:(length(bins.date) - 1)],
            bins.date[2:length(bins.date)],
            FUN = function(d1, d2) d1 + ((d2 - d1) / 2)
        )
        bins.date.middle = as.POSIXct(bins.date.middle, origin = "1970-01-01")

        ## order edges by date
        edges.all = igraph::E(network)
        edges.dates = igraph::get.edge.attribute(network, "date")

        ## identify edges to cut for sliding-window approach
        edges.cut = sapply(edges.dates, function(date) {
            date < bins.date.middle[1] || date > bins.date.middle[length(bins.date.middle)]
        })

        ## delete edges from the network and create a new network
        network.cut = igraph::delete.edges(network, edges.all[edges.cut])

        ## split network for sliding windows
        nets.sliding = split.network.time.based(network.cut, bins = bins.date.middle, sliding.window = FALSE)

        ## append data to normally-split data
        nets = append(nets, nets.sliding)

        ## sort data object properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        nets = nets[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
    }

    ## set ranges as names
    names(nets) = construct.ranges(bins.date, sliding.window = sliding.window)

    return(nets)
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
#'                     (implying an open number of resulting ranges)
#' @param number.windows the number of consecutive networks to get from this function
#'                       (implying an equally distributed amount of edges in each range)
#' @param sliding.window logical indicating whether the splitting should be performed using
#'                       a sliding-window approach (increases 'number.windows' accordingly)
#'                       [default: FALSE]
#'
#' @return a list of igraph networks, each referring to one period of activity
split.network.activity.based = function(network, number.edges = 5000, number.windows = NULL,
                                        sliding.window = FALSE) {
    ## get total edge count
    edge.count = igraph::ecount(network)

    ## number of edges given (number of windows NOT given)
    if (is.null(number.windows)) {
        if (edge.count < 1) {
            logging::logerror("The number of edges in the given network has to be
                              strictly positive (given: %s).", edge.count)
            stop("Stopping due to missing edges in given network.")
        }
        # compute the number of time windows according to the number of edges per network
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
        # compute the number of time windows according to the number of edges per network
        number.edges = ceiling(edge.count / number.windows)
    }

    logging::loginfo("Splitting network into activity ranges of %s edges, yielding %s windows.",
                     number.edges, number.windows)

    ## get dates in a data.frame for splitting purposes
    df = data.frame(
        date = as.POSIXct( # use POSIXct as other functions need it
            igraph::get.edge.attribute(network, "date"), origin = "1970-01-01"),
        my.unique.id = 1:edge.count # as a unique identifier only
    )
    ## sort by date
    df = df[ with(df, order(date)), ]

    ## identify bins
    logging::logdebug("Getting bins for activity-based splitting based on amount of edges.")
    bins.data = split.get.bins.activity.based(df, "my.unique.id", activity.amount = number.edges)
    bins.date = bins.data[["bins"]]
    bins.vector = bins.data[["vector"]]
    bins.vector = bins.vector[ with(df, order(my.unique.id)) ] # re-order to get igraph ordering
    bins = sort(unique(bins.vector))
    ## split network by bins
    networks = split.network.by.bins(network, bins, bins.vector)

    ## perform additional steps for sliding-window approach
    ## for activity-based sliding-window bins to work, we need to crop edges appropriately and,
    ## then, compute bins on the cropped networks
    if (sliding.window) {
        ## order edges by date
        edges.by.date = igraph::E(network)[ order(df[["date"]]) ]

        ## offsets used for cropping (half the first/last bin)
        offset.start = floor(number.edges / 2)
        offset.end = floor((edge.count %% number.edges) / 2)
        ## cut the data appropriately
        edges.cut = c(
            edges.by.date[1:offset.start],
            edges.by.date[(edge.count - offset.end):edge.count]
        )

        ## delete edges from the network and create a new network
        network.cut = igraph::delete.edges(network, edges.cut)

        ## split network for sliding windows
        networks.sliding = split.network.activity.based(network.cut, number.edges = number.edges, sliding.window = FALSE)

        ## append data to normally-split data
        networks = append(networks, networks.sliding)

        ## compute bins for sliding windows: pairwise middle between dates
        bins.date.middle = attr(networks.sliding, "bins")

        ## sort data object properly by bin starts
        bins.ranges.start = c(head(bins.date, -1), head(bins.date.middle, -1))
        networks = networks[ order(bins.ranges.start) ]

        ## construct proper bin vectors for configuration
        bins.date = sort(c(bins.date, bins.date.middle))
    }

    ## set bin attribute for sliding-window functionality
    attr(networks, "bins") = bins.date

    ## set ranges as names
    names(networks) = construct.ranges(bins.date, sliding.window = sliding.window)

    return(networks)
}

#' Split the given data by the given bins.
#'
#' @param df a data.frame to be split
#' @param bins a vector with the length of 'ncol(df)' assigning a bin for each row of 'df'
#'
#' @return a list of data.frames, with the length of 'unique(bins)'
split.data.by.bins = function(df, bins) {
    logging::logdebug("split.data.by.bins: starting.")
    df.split = split(df, bins)
    logging::logdebug("split.data.by.bins: finished.")
    return(df.split)
}

#' Split the given data by the given bins, in increasing order of the bin identifiers.
#'
#' @param network a network
#' @param bins a vector with the unique bin identifiers, describing the order in which the bins are created
#' @param bins.vector a vector of length 'ecount(network)' assigning a bin for each edge of 'network'
#'
#' @return a list of networks, with the length of 'unique(bins.vector)'
split.network.by.bins = function(network, bins, bins.vector) {
    logging::logdebug("split.data.time.based: starting.")
    ## create a network for each bin of edges
    nets = parallel::mclapply(bins, function(bin) {
        logging::logdebug("Splitting network: bin %s", bin)
        ## identify edges in the current bin
        edges = igraph::E(network)[ bins.vector == bin ]
        ## create network based on the current set of edges
        g = igraph::subgraph.edges(network, edges, delete.vertices = TRUE)
        return(g)
    })
    logging::logdebug("split.data.time.based: finished.")
    return(nets)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Low-level functionality
##

#' Compute bin information for a time-based splitting based on the given time period.
#'
#' @param dates the dates that are to be split into several bins
#' @param time.period the time period each bin lasts
#'
#' @return a list,
#'         the item 'vector': the bins each item in 'dates' belongs to,
#'         the item 'bins': the bin labels, each spanning the length of 'time.period';
#'             each item in the vector indicates the start of a bin, although the last
#'             item indicates the end of the last bin
split.get.bins.time.based = function(dates, time.period) {
    logging::logdebug("split.get.bins.time.based: starting.")
    ## find date bins from given dates
    dates.breaks = c(
        ## time periods of length 'time.period'
        seq.POSIXt(from = min(dates), to = max(dates), by = time.period),
        ## add last bin
        max(dates) + 1
    )
    dates.breaks.chr = strftime(head(dates.breaks, -1))
    ## find bins for given dates
    dates.bins = findInterval(dates, dates.breaks, all.inside = FALSE)
    dates.bins = factor(dates.bins)
    ## set factor's levels appropriately
    levels(dates.bins) = dates.breaks.chr[ as.integer(levels(dates.bins)) ] # get the dates
    levels(dates.bins) = c( # append all missing dates
        levels(dates.bins),
        dates.breaks.chr[ !(dates.breaks.chr %in% levels(dates.bins)) ]
    )

    logging::logdebug("split.get.bins.time.based: finished.")

    ## return properly
    return(list(
        vector = dates.bins,
        bins = strftime(dates.breaks)
    ))
}

#' Compute bin information for a activity-based splitting based on the given amount of activity
#' based on the actual order of the rows in the given data.frame 'df'.
#'
#' @param df the sorted data.frame representing the data
#' @param id a character string denoting the ID column of the data.frame 'df'
#' @param activity.amount the amount of activity denoting the number of unique items
#'                        in each split bin [default: 5000]
#'
#' @return a list,
#'         the item 'vector': the bins each row in 'df' belongs to (increasing integers),
#'         the item 'bins': the bin labels,  described by dates, each bin containing
#'         'acitivity.amount' many unique items; each item in the vector indicates
#'         the start of a bin, although the last item indicates the end of the last bin
split.get.bins.activity.based = function(df, id, activity.amount) {
    logging::logdebug("split.get.bins.activity.based: starting")
    ## get the unique integer IDs for each item in 'id' column
    ids = df[[id]]
    ids.unique = unique(ids)
    ## compute split bins
    bins.number.complete = length(ids.unique) %/% activity.amount
    bins.number.incomplete = length(ids.unique) %% activity.amount
    bins.activity = c(
        if (bins.number.complete != 0) rep(1:bins.number.complete, each = activity.amount),
        rep(bins.number.complete + 1, bins.number.incomplete)
    )
    bins.number = max(bins.activity)

    ## join ids and bin numbers
    bins.mapping = data.frame(
        id = ids.unique,
        bin = bins.activity
    )

    ## get the start (and end) date for all bins
    bins.date = parallel::mclapply(1:bins.number, function(bin) {
        ## get the ids in the bin
        ids = bins.mapping[ bins.mapping$bin == bin, "id"]
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
    bins.date = do.call(c, bins.date)
    ## convert to character strings
    bins.date.char = strftime(bins.date)

    logging::logdebug("split.get.bins.activity.based: finished.")
    return(list(
        vector = bins.activity,
        bins = bins.date.char
    ))
}

