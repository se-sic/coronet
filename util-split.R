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
        bins = split.get.bins.time.based(data[[split.basis]][["date"]], time.period)
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

#' Compute the bins for a time-based splitting based on the given time period.
#'
#' @param dates the dates that are to be split into several bins
#' @param time.period the time period each bin lasts
#'
#' @return a vector of bins, each spanning the length of 'time.period';
#'         each item in the vector indicates the start of a bin, although the last
#'         item indicates the end of the last bin
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
    return(bins)
}

