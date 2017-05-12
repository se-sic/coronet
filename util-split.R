## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Sofie Kemper, 2017
## kemperso@fim.uni-passau.de


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## High-level functionality
##

#' Split project data in time-based ranges as specified
#'
#' @param project.data the Codeface*Data object from which the data is retrieved
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days"
#' @param bins the date objects defining the start of ranges (the last date defines the end of the last range).
#'             If set, the 'time.period' parameter is ignored; consequently, 'split.basis' does not make sense then.
#' @param split.basis the data name to use as the basis for split bins, either 'commits' or 'mails'
#'                    [default: commits]
#'
#' @return the list of CodefaceRangeData objects, each referring to one time period
split.data.time.based = function(project.data, time.period = "3 months", bins = NULL,
                                 split.basis = c("commits", "mails")) {

    ## get basis for splitting process
    split.basis = match.arg(split.basis)
    logging::loginfo(split.basis)

    ## get actual raw data
    data = list(
        commits = project.data$get.commits.raw(),
        mails = project.data$get.mails()
    )
    split.data = names(data)
    names(split.data) = split.data
    logging::loginfo(paste(split.data, collapse = ", "))

    ## if bins are NOT given explicitly
    if (is.null(bins)) {
        ## get bins based on split.basis
        bins = split.get.bins.time.based(data[[split.basis]][["date"]], time.period)$bins
        bins.labels = head(bins, -1)
    }
    ## when bins are given explicitly
    else {
        split.basis = NULL
        bins = strftime(bins)
        bins.labels = head(bins, -1)
    }
    bins.date = as.POSIXct(bins)

    ## construct ranges
    bins.ranges = paste(
        bins[1:(length(bins) - 1)],
        bins[2:length(bins)],
        sep = "-"
    )
    names(bins.ranges) = bins.ranges

    ## split data
    data.split = lapply(split.data, function(df.name) {
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
    data.split = lapply(bins.labels, function(bin) lapply(data.split, `[[`, bin))
    names(data.split) = bins.ranges

    ## construct CodefaceRangeData objects
    cf.data = lapply(bins.ranges, function(range) {
        ## construct object for current range
        cf.range.data = CodefaceRangeData$new(conf, range)
        ## FIXME add revision.callgraph parameter
        ## get data for current range
        df.list = data.split[[range]]

        ## set data
        ## 1) commits
        cf.range.data$set.commits.raw(df.list[["commits"]])
        ## 2) mails
        cf.range.data$set.mails(df.list[["mails"]])
        ## 3) synchronicity data
        cf.range.data$set.synchronicity(project.data$get.synchronicity()) ## FIXME what about parameters?!
        ## 4) ID--author mapping
        cf.range.data$set.authors(project.data$get.authors())

        return(cf.range.data)
    })

    ## return list of CodefaceRangeData objects
    return(cf.data)
}


#' Split project data in activity-based ranges as specified
#'
#' @param project.data the Codeface*Data object from which the data is retrieved
#' @param activity.type the type of activity used for splitting, either 'commits' or 'mails' [default: commits]
#' @param activity.amount the amount of activity describing the size of the ranges, a numeric, further
#'                        specified by 'activity.type'
#'
#' @return the list of CodefaceRangeData objects, each referring to one time period
split.data.activity.based = function(project.data, activity.type = c("commits", "mails"),
                                     activity.amount) {

    ## get basis for splitting process
    activity.type = match.arg(activity.type)
    logging::loginfo(activity.type)

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
    bins = split.get.bins.activity.based(data[[activity.type]], id.column[[activity.type]], activity.amount)[["bins"]]
    bins.date = as.POSIXct(bins)

    ## split the data based on the extracted timestamps
    data.split = split.data.time.based(project.data, bins = bins.date, split.basis = activity.type)
    return(data.split)
}


#' This function takes a global network (igraph) with a edge attribute "date"
#' and discretizes them by sectioning the network according to the given 'time_period'.
#' The resulting networks does NOT get simplified.
#'
#' Important notice: This function only works for unsimplified networks, where no edges have been
#' contracted, which would combine edge attributes.
#'
#' @param network the igraph network to split, needs to have an edge attribute named "date"
#' @param time.period the time period describing the length of the ranges, a character string,
#'                    e.g., "3 mins" or "15 days"
#'
#' @return a list of igraph networks, each referring to one time period
split.network.time.based = function(network, time.period) {
    ## extract date attributes from edges
    dates = as.POSIXct(igraph::get.edge.attribute(network, "date"), origin="1970-01-01")

    ## get bin information for all edges
    bins.info = split.get.bins.time.based(dates, time.period)
    bins.vector = bins.info[["vector"]]
    bins = head(bins.info[["bins"]], -1)

    ## create a network for each bin of edges
    nets = lapply(bins, function(bin) {
        ## identify edges in the current bin
        edges = igraph::E(network)[ bins.vector == bin ]
        ## create network based on the current set of edges
        g = igraph::subgraph.edges(network, edges, delete.vertices = TRUE)
        return(g)
    })

    return(nets)
}


#' Split the given data by the given bins.
#'
#' @param df a data.frame to be split
#' @param bins a vector with the length of 'ncol(df)' assigning a bin for each row of 'df'
#'
#' @return a list of data.frames, with the length of 'unique(bins)'
split.data.by.bins = function(df, bins) {
    df.split = split(df, bins)
    return(df.split)
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
    ## find bins for given dates
    dates.bins = cut(dates, breaks = time.period)
    ## get bins for returning
    bins = levels(dates.bins)
    ## add end date for last bin
    bins = c(
        bins,
        strftime(seq(
            as.POSIXct(tail(bins, 1)),
            by = time.period,
            length = 2)[2]
        )
    )
    return(list(
        vector = dates.bins,
        bins = bins
    ))
}

#' Compute bin information for a activity-based splitting based on the given amount of activity.
#'
#' @param df the data.frame representing the data
#' @param id a character string denoting the ID column of the data.frame 'df'
#' @param activity.amount the amount of activity denoting the number of unique items
#'                        in each split bin [default: 5000]
#'
#' @return a list,
#'         the item 'vector': the bins each row in 'df' belongs to,
#'         the item 'bins': the bin labels,  described by dates, each bin containing
#'         'acitivity.amount' many unique items; each item in the vector indicates
#'         the start of a bin, although the last item indicates the end of the last bin
split.get.bins.activity.based = function(df, id, activity.amount) {
    ## get the unique integer IDs for each item in 'id' column
    ids = df[[id]]
    ids.unique = unique(ids)
    ## compute split bins
    bins.number.complete = length(ids.unique) %/% activity.amount
    bins.number.incomplete = length(ids.unique) %% activity.amount
    bins.activity = c(
        rep(1:bins.number.complete, each = activity.amount),
        rep(bins.number.complete + 1, bins.number.incomplete)
    )
    bins.number = max(bins.activity)

    ## join ids and bin numbers
    bins.mapping = data.frame(
        id = ids.unique,
        bin = bins.activity
    )

    ## get the start (and end) date for all bins
    bins.date = lapply(1:bins.number, function(bin) {
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
    bins.date = unique(bins.date)
    ## convert to character strings
    bins.date.char = strftime(bins.date)

    return(list(
        vector = bins.activity,
        bins = bins.date.char
    ))
}

