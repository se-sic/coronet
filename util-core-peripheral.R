## (c) Ferdinand Frank, 2017
## frankfer@fim.uni-passau.de
## (c) Claus Hunsen, 2017
## hunsen@fim.uni-passau.de
## (c) Mitchell Joblin, 2017
## mitchell.joblin@uni-passau.de
## (c) Sofie Kemper, 2017
## kemperso@fim.uni-passau.de

## This file is derived from following Codeface script:
## https://github.com/siemens/codeface/blob/master/codeface/R/developer_classification.r



## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("sqldf") # for SQL-selections on data.frames
requireNamespace("igraph") # for calculation of network metrics (degree, eigen-centrality)
requireNamespace("markovchain") # for role stability analysis
requireNamespace("logging") # for logging


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Thresholds --------------------------------------------------------------

## Defines at which percentage of the work load authors will
## be classified as core
CORE.THRESHOLD = 0.8

## Defines the percentage of version development ranges in which
## a author has to be classified as core to be stated as a
## longterm core author
LONGTERM.CORE.THRESHOLD = 0.5


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author-class wrappers and overviews -------------------------------------

## Classify the authors of the specified version range into core and peripheral
## based on the classification metric indicated by "type".
##
## For count-based network metrics, the raw data has to be given. For network-based metrics,
## the network has to be given.
get.author.class.by.type = function(network = NULL, data = NULL,
                                    type = c("network.degree", "network.eigen", "commit.count", "loc.count")) {
    logging::logdebug("get.author.class.by.type: starting.")

    type = match.arg(type)

    if(is.null(network) && is.null(data)) {
        logging::logerror("Neither network nor raw data were given.")
        stop("Either network or raw data needs to be given.")
    }

    result = switch(type,
                    "network.degree" = get.author.class.network.degree(network = network),
                    "network.eigen" = get.author.class.network.eigen(network = network),
                    "commit.count" = get.author.class.commit.count(codeface.range.data = data),
                    "loc.count"= get.author.class.loc.count(codeface.range.data = data))

    logging::logdebug("get.author.class.by.type: finished.")
    return(result)
}

## Classify the authors of all specified version ranges into core and peripheral
## based on the specified classification function.
##
## The data can either be given as list of raw range data (for the count-based metrics)
## or as list of networks for the network-based metrics).
get.author.class.overview = function(network.list = NULL, codeface.range.data.list = NULL,
                                     type = c("network.degree", "network.eigen", "commit.count", "loc.count")) {
    logging::logdebug("get.author.class.overview: starting.")

    type = match.arg(type)

    if(is.null(codeface.range.data.list) && (type == "commit.count" || type == "loc.count")) {
        logging::logerror("For count-based metric evolution, a list of codeface range-data objects is needed.")
        stop("For the count-based metrics, the raw data has to be given.")

    } else if(is.null(network.list) && (type == "network.degree" || type == "network.eigen")) {
        logging::logerror("For the network-based metric evolution, a list of networks as igraph-objects is needed.")
        stop("For the network-based metrics, the network list has to be given.")
    }

    res = list()
    if(!is.null(codeface.range.data.list)) {
        for (i in 1:length(codeface.range.data.list)) {
            range.data = codeface.range.data.list[[i]]
            range.name = names(codeface.range.data.list)[[i]]

            ## Get classification data of the current range
            range.class =
                get.author.class.by.type(data = range.data, type = type)

            ## Save in list of classifications
            if(!is.null(range.name)) {
                res[[range.name]] = range.class
            } else {
                res <- c(res, list(range.class))
            }
        }
    } else { # use network list as data
        for (i in 1:length(network.list)) {
            range.network = network.list[[i]]
            range.name = names(network.list)[[i]]

            ## Get classification data of the current range
            range.class =
                get.author.class.by.type(network = range.network, type = type)

            ## save in list of clasifications
            if(!is.null(range.name)) {
                res[[range.name]] = range.class
            } else {
                res <- c(res, list(range.class))
            }
        }
    }

    logging::logdebug("get.author.class.overview: finished.")
    return(res)
}

## Get the number/activity of core and peripheral authors based on the specified classification function
## for each specified split range and for each version development range.
##
## An individual split range can be set for each version as a list of vectors with the version name as the key.
## An integer value can be set as "sliding.window.core" to specify, that the core authors of the last
## version ranges (according to the value) shall be included in the core author set of the current range.
get.author.class.activity.overview = function(codeface.range.data.list = NULL,
                                              author.class.overview = NULL,
                                              split = c(),
                                              sliding.window.core = NULL,
                                              longterm.cores = c(),
                                              activity.measure = c("commit.count", "loc.count")) {
    logging::logdebug("get.author.class.activity.overview: starting.")

    activity.measure = match.arg(activity.measure)

    if(is.null(codeface.range.data.list)) {
        logging::logerror("A list of Codeface range-data objects is needed for the activity analysis.")
        stop("Raw data is needed for the activity analysis.")
    }

    if(is.null(author.class.overview)) {
        logging::logerror("An author.class.overview has to be given for the activity overview analysis.")
        stop("Author classification has to be given.")
    }

    if(length(codeface.range.data.list) != length(author.class.overview)) {
        logging::logerror("The raw data and the author classification use a different number of ranges.")
        stop("Raw data and author classification have to match.")
    }

    res = list()
    for (i in 1:length(codeface.range.data.list)) {

        ## Check if an individual split for each version range is set
        if (class(split) == "list") {
            range.split = split[[i]]
        } else {
            range.split = split
        }

        additional.core = longterm.cores

        ## Add the additional core authors according to the specified sliding window length,
        ## if one is specified
        if (!is.null(sliding.window.core) && i > 1) {
            sliding.window.start = ifelse(i - sliding.window.core > 1, i - sliding.window.core, 1)

            for (j in sliding.window.start:(i-1)) {
                additional.core = c(additional.core, author.class.overview[[j]]$core$author.name)
            }
        }

        res[[i]] = get.author.class.activity(codeface.range.data.list[[i]],
                                             author.class = author.class.overview[[i]],
                                             activity.measure = activity.measure,
                                             split = range.split,
                                             additional.cores = additional.core)
    }

    logging::logdebug("get.author.class.activity.overview: finished.")
    return(res)
}

## Get the author turnover values measured as the proportion of authors in the
## specified version range classes which were not active, i.e. do not exist,
## in the previous version range classes (saturation).
get.class.turnover.overview = function(author.class.overview, saturation = 1) {
    logging::logdebug("get.class.turnover.overview: starting.")

    if(!is.null(names(author.class.overview))) {
        versions = names(author.class.overview)
    } else {
        versions = 1:length(author.class.overview)
    }

    ## Set up the data.frame for the analysis results
    turnover.overview = data.frame(
        versions = versions,
        row.names = 1,
        turnover = 0,
        turnover.core = 0,
        turnover.peripheral = 0,
        dev.count = 0,
        dev.count.core = 0,
        dev.count.peripheral = 0
    )

    ## Get all active authors for each version range in the different classes (and both)
    devs = sapply(author.class.overview, function(author.class) {
        return(c(author.class$core$author.name, author.class$peripheral$author.name))
    })
    devs.core = sapply(author.class.overview, function(author.class) {
        return(author.class$core$author.name)
    })
    devs.peripheral = sapply(author.class.overview, function(author.class) {
        return(author.class$peripheral$author.name)
    })

    ## The author turnover measured as the proportion of devs in the current version
    ## range which were not active in the previous version range
    devs.new = devs[[1]]
    devs.core.new = devs.core[[1]]
    devs.peripheral.new = devs.peripheral[[1]]
    turnover.overview$dev.count[1] = length(devs.new)
    turnover.overview$dev.count.core[1] = length(devs.core.new)
    turnover.overview$dev.count.peripheral[1] = length(devs.peripheral.new)
    for (i in 2:length(author.class.overview)) {
        devs.old = devs.new
        devs.core.old = devs.core.new
        devs.peripheral.old = devs.peripheral.new

        j = 1
        while (j <= saturation) {
            if ((i-j) > 0) {
                devs.old = igraph::union(devs.old, devs[[i-j]])
                devs.core.old = igraph::union(devs.core.old, devs.core[[i-j]])
                devs.peripheral.old = igraph::union(devs.peripheral.old, devs.peripheral[[i-j]])
            }
            j = j + 1
        }

        ## Find the authors which are active in the current period
        devs.new = devs[[i]]
        devs.core.new = devs.core[[i]]
        devs.peripheral.new = devs.peripheral[[i]]

        ## Calculate the turnover values
        turnover.overview$turnover[i] = sum(!(devs.new %in% devs.old)) / length(devs.new)
        turnover.overview$turnover.core[i] = sum(!(devs.core.new %in% devs.core.old)) / length(devs.core.new)
        turnover.overview$turnover.peripheral[i] = sum(!(devs.peripheral.new %in% devs.peripheral.old)) / length(devs.peripheral.new)

        turnover.overview$dev.count[i] = length(devs.new)
        turnover.overview$dev.count.core[i] = length(devs.core.new)
        turnover.overview$dev.count.peripheral[i] = length(devs.peripheral.new)
    }

    logging::logdebug("get.class.turnover.overview: finished.")
    return(turnover.overview)
}

## Gets a data frame to show the proportion of
## the authors which are either only active in the current version range but not in the previous ones (new) or
## which are only active in the previous ranges (as specified in saturation) but not in the current one (gone) in
## relation to all authors of the current and the previous ranges.
get.unstable.authors.overview = function(author.class.overview, saturation = 1) {
    logging::logdebug("get.unstable.authors.overview: starting.")

    if(!is.null(names(author.class.overview))) {
        versions = names(author.class.overview)
    } else {
        versions = 1:length(author.class.overview)
    }

    ## Set up the data.frame for the analysis results
    turnover.overview = data.frame(
        versions = versions,
        row.names = 1,
        unstable = 0,
        unstable.core = 0,
        unstable.peripheral = 0
    )

    ## Get all active authors for each version range in the different classes (and both)
    devs = sapply(author.class.overview, function(author.class) {
        return(c(author.class$core$author.name, author.class$peripheral$author.name))
    })
    devs.core = sapply(author.class.overview, function(author.class) {
        return(author.class$core$author.name)
    })
    devs.peripheral = sapply(author.class.overview, function(author.class) {
        return(author.class$peripheral$author.name)
    })

    devs.current = devs[[1]]
    devs.core.current = devs.core[[1]]
    devs.peripheral.current = devs.peripheral[[1]]
    for (i in 2:length(author.class.overview)) {
        devs.prev = devs.current
        devs.core.prev = devs.core.current
        devs.peripheral.prev = devs.peripheral.current

        j = 1
        while (j <= saturation) {
            if ((i-j) > 0) {
                devs.prev = igraph::union(devs.prev, devs[[i-j]])
                devs.core.prev = igraph::union(devs.core.prev, devs.core[[i-j]])
                devs.peripheral.prev = igraph::union(devs.peripheral.prev, devs.peripheral[[i-j]])
            }
            j = j + 1
        }

        ## Find the authors which are active in the current period
        devs.current = devs[[i]]
        devs.core.current = devs.core[[i]]
        devs.peripheral.current = devs.peripheral[[i]]

        ## Find the union of the devs which are active in the current period and in the prev periods
        devs.union = igraph::union(devs.current, devs.prev)
        devs.core.union = igraph::union(devs.core.current, devs.core.prev)
        devs.peripheral.union = igraph::union(devs.peripheral.current, devs.peripheral.prev)

        ## Find the devs which are only active in the current range but not in the previous ones
        devs.new = sum(!(devs.current %in% devs.prev))
        devs.core.new = sum(!(devs.core.current %in% devs.core.prev))
        devs.peripheral.new = sum(!(devs.peripheral.current %in% devs.peripheral.prev))

        ## Find the devs which are only active in the previous ranges but not in the current one
        devs.gone = sum(!(devs.prev %in% devs.current))
        devs.core.gone = sum(!(devs.core.prev %in% devs.core.current))
        devs.peripheral.gone = sum(!(devs.peripheral.prev %in% devs.peripheral.current))

        ## Calculate the ratio values
        turnover.overview$unstable[i] = (devs.new + devs.gone) / length(devs.union)
        turnover.overview$unstable.core[i] = (devs.core.new + devs.core.gone) / length(devs.core.union)
        turnover.overview$unstable.peripheral[i] = (devs.peripheral.new + devs.peripheral.gone) / length(devs.peripheral.union)

    }

    logging::logdebug("get.unstable.authors.overview: finished.")
    return(turnover.overview)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network-based classification --------------------------------------------

## * Degree-based classification -------------------------------------------

## Classify the authors of the specified version range into core and peripheral
## based on the degree centrality.
##
## This function takes an igraph object.
get.author.class.network.degree = function(network = NULL, result.limit = NULL) {
    logging::logdebug("get.author.class.network.degree: starting.")

    if(is.null(network)) {
        logging::logerror("For the network-based degree-centrality analysis, the network is needed.")
        stop("The network has to be given for this analysis.")
    } else if(igraph::vcount(network) == 0) {
        logging::logwarn("The given network is empty. Returning empty classification...")
        ## return an empty classification
        return(list("core" = data.frame("author.name" = character(0), "centrality" = numeric(0)),
                    "peripheral" = data.frame("author.name" = character(0), "centrality" = numeric(0))))
    }

    ## Get node degrees for all authors
    centrality.vec = sort(igraph::degree(network), decreasing = TRUE)
    centrality.df = data.frame(author.name = names(centrality.vec),
                               centrality = as.vector(centrality.vec))

    ## Get the author classification based on the centrality
    res = get.author.class(centrality.df, "centrality", result.limit = result.limit)

    logging::logdebug("get.author.class.network.degree: finished.")
    return(res)
}

## * Eigenvector-based classification --------------------------------------

## Classify the authors of the specified version range into core and peripheral
## based on the eigenvector centrality.
##
## This function takes either a network OR the raw range data. In case both are given, the network is used.
get.author.class.network.eigen = function(network = NULL, codeface.range.data = NULL, result.limit = NULL) {
    logging::logdebug("get.author.class.network.eigen: starting.")

    if(is.null(network)) {
        logging::logerror("For the network-based eigen-centrality analysis, the network has to be given.")
        stop("The network has to be given for this analysis.")

    } else if(igraph::vcount(network) == 0) {
        logging::logwarn("The given network is empty. Returning empty classification...")
        ## return an empty classification
        return(list("core" = data.frame("author.name" = character(0), "centrality" = numeric(0)),
                    "peripheral" = data.frame("author.name" = character(0), "centrality" = numeric(0))))
    }

    ## Get eigenvectors for all authors
    centrality.vec = sort(igraph::eigen_centrality(network)$vector, decreasing= TRUE)

    ## In case no collaboration occured, all centrality values are set to 0
    if(igraph::ecount(network) == 0) {
        centrality.vec[1:length(centrality.vec)] = rep(0, length(centrality.vec))
    }
    centrality.df = data.frame(author.name = names(centrality.vec),
                               centrality = as.vector(centrality.vec))

    ## Get the author classification based on the centrality
    res = get.author.class(centrality.df, "centrality", result.limit = result.limit)

    logging::logdebug("get.author.class.network.eigen: finished.")
    return(res)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-based classification ---------------------------------------------

## * Count-based classification --------------------------------------------

## Classify the authors of the specified version range into core and peripheral
## based on the number of commits made withing a version range.
get.author.class.commit.count = function(codeface.range.data, result.limit = NULL) {
    logging::logdebug("get.author.class.commit.count: starting.")

    ## Get the commit counts per author
    author.commit.count = get.author.commit.count(codeface.range.data)

    ## Get the author classification based on the commit counts
    res = get.author.class(author.commit.count, "freq", result.limit = result.limit)

    logging::logdebug("get.author.class.commit.count: finished.")
    return(res)
}

## Get the commit count threshold  of the specified version range
## on which a author can be classified as core.
get.commit.count.threshold = function(codeface.range.data) {
    logging::logdebug("get.commit.count.threshold: starting.")

    ## Get the commit counts per author
    author.commit.count = get.author.commit.count(codeface.range.data)
    threshold = get.threshold(author.commit.count$freq)

    logging::logdebug("get.commit.count.threshold: finished.")
    return(threshold)
}

## Get the commit count per author of the specified version range
## as a data frame ordered by the commit count.
get.author.commit.count = function(codeface.range.data) {
    logging::logdebug("get.author.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(codeface.range.data)[[1]]

    ## Return NA in case no commit data is available
    if(all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the commit count per author
    res = sqldf::sqldf("select *, COUNT(*) as `freq` from `commits.df` group by `author.name` order by `freq` desc")

    logging::logdebug("get.author.commit.count: finished.")
    return(res)
}

## * LOC-based classification ----------------------------------------------

## Classify the authors of the specified version range into core and peripheral
## based on the sum of added and deleted lines of code a author has committed within a version range.
get.author.class.loc.count = function(codeface.range.data, result.limit = NULL) {
    logging::logdebug("get.author.class.loc.count: starting.")

    ## Get the changed lines (loc counts) per author
    author.loc.count = get.author.loc.count(codeface.range.data)

    ## Get the author classification based on the loc counts
    res = get.author.class(author.loc.count, "loc", result.limit = result.limit)

    logging::logdebug("get.author.class.loc.count: finished.")
    return(res)
}

## Get the loc count threshold of the specified version range
## on which a author can be classified as core.
get.loc.count.threshold = function(codeface.range.data) {
    logging::logdebug("get.loc.count.threshold: starting.")

    ## Get the loc per author
    author.loc.count = get.author.loc.count(codeface.range.data)

    threshold = get.threshold(author.loc.count$loc)

    logging::logdebug("get.loc.count.threshold: finished.")
    return(threshold)
}

## Get the changed lines per author of the specified version range
## as a data frame ordered by the changed lines.
get.author.loc.count = function(codeface.range.data) {
    logging::logdebug("get.author.loc.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(codeface.range.data,
                                 columns = c("author.name", "author.email", "added.lines", "deleted.lines"))[[1]]

    ## Return NA in case no commit data is available
    if(all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the changed lines per author
    res = sqldf::sqldf("select `author.name`, `author.email`, SUM(`added.lines`) + SUM(`deleted.lines`) as `loc`
               from `commits.df` group by `author.name` order by `loc` desc")

    logging::logdebug("get.author.loc.count: finished.")
    return(res)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Activity collection -----------------------------------------

## Get the number/activity of core and peripheral authors based on the specified classification function
## for each split range of the specified version development range.
## A split interval can be set by defining the number of weeks for each requested range as a vector,
## e.g., c(1, 1, 1) for a three-week range that shall be treated as three one-week ranges.
## A vector of addition core authors can be specified which will always be set as core in each range.
get.author.class.activity = function(codeface.range.data = NULL,
                                     author.class = NULL,
                                     activity.measure = c("commit.count", "loc.count"),
                                     split = c(),
                                     additional.cores = NULL) {
    logging::logdebug("get.author.class.activity: starting.")

    activity.measure = match.arg(activity.measure)

    if(is.null(codeface.range.data)) {
        logging::logerror("A Codeface range-data object is needed for the activity analysis.")
        stop("Raw data is needed for the activity analysis.")
    }
    if(is.null(author.class)) {
        logging::logerror("An author classification is needed for the activity analysis.")
        stop("Author classification has to be given by the user")
    }

    ## Return NA in case no classification information is available
    if(all(is.na(author.class))
       || (nrow(author.class$core) + nrow(author.class$peripheral) == 0)) {
        return(NA)
    }

    author.core = unique(c(author.class$core$author.name, additional.cores))

    ## Get the splitted commit data with all necessary columns
    commits.data = get.commit.data(codeface.range.data,
                                   columns = c("author.name", "added.lines", "deleted.lines"),
                                   split = split)

    ## Build the query string to group commits by the author name
    commits.query = "select `author.name`, SUM(`added.lines`) + SUM(`deleted.lines`) as `loc.count`,
    COUNT(*) as `commit.count` from `commits.df` group by `author.name`"

    ## Get the authors with their commit count and corresponding class for each splitted range
    commits.dev.list = list()
    for (i in 1:length(commits.data)) {
        commits.df = commits.data[[i]]
        commits.dev.list[[names(commits.data)[i]]] = sqldf::sqldf(commits.query)

        ## Classify authors in splitted range according to the overall classification
        core.test = commits.dev.list[[names(commits.data)[i]]]$author.name %in% author.core
        commits.dev.core = commits.dev.list[[names(commits.data)[i]]][core.test,]
        commits.dev.peripheral = commits.dev.list[[names(commits.data)[i]]][!core.test,]

        commits.dev.list[[names(commits.data)[i]]] = list(core = commits.dev.core, peripheral = commits.dev.peripheral)
    }

    res.range = c()
    res.devs = c()
    res.core = c()
    res.peripheral = c()

    res.activity.count = c()
    res.activity.count.core = c()
    res.activity.count.peripheral = c()
    res.activity.count.avg.core = c()
    res.activity.count.avg.peripheral = c()
    res.activity.count.med.core = c()
    res.activity.count.med.peripheral = c()
    res.activity.count.norm.weeks.core = c()
    res.activity.count.norm.weeks.peripheral = c()

    ## Count the number of core and peripheral authors for each splitted range
    for (i in 1:length(commits.dev.list)) {
        commits.dev = commits.dev.list[[i]]
        res.range[i] = names(commits.dev.list)[i]

        res.core[i] = nrow(commits.dev$core)
        res.peripheral[i] = nrow(commits.dev$peripheral)
        res.devs[i] = res.core[i] + res.peripheral[i]

        res.activity.count.core[i] = sum(commits.dev$core[[activity.measure]])
        res.activity.count.peripheral[i] = sum(commits.dev$peripheral[[activity.measure]])
        res.activity.count[i] = res.activity.count.core[i] + res.activity.count.peripheral[i]

        ## Get average activity count
        num.core.dev = ifelse(res.core[i] > 0, res.core[i], 1)
        num.peripheral.dev = ifelse(res.peripheral[i] > 0, res.peripheral[i], 1)
        res.activity.count.avg.core[i] = res.activity.count.core[i] / num.core.dev
        res.activity.count.avg.peripheral[i] = res.activity.count.peripheral[i] / num.peripheral.dev

        ## Get median activity count
        activity.count.core.ordered = commits.dev$core[order(commits.dev$core[[activity.measure]]),][[activity.measure]]
        activity.count.peripheral.ordered = commits.dev$peripheral[order(commits.dev$peripheral[[activity.measure]]),][[activity.measure]]
        res.activity.count.med.core[i] = ifelse(length(activity.count.core.ordered) > 0, median(activity.count.core.ordered), 0)
        res.activity.count.med.peripheral[i] = ifelse(length(activity.count.peripheral.ordered) > 0, median(activity.count.peripheral.ordered), 0)

        ## Normalize activity count by weeks
        res.range.splitted = attr(commits.data, "bins")[c(i, i + 1)]
        res.range.start = as.Date(res.range.splitted[1])
        res.range.end = as.Date(res.range.splitted[2])
        res.range.length.days = as.numeric(res.range.end - res.range.start)
        res.range.length.weeks = round(res.range.length.days / 7)

        res.activity.count.norm.weeks.core[i] = res.activity.count.core[i] / res.range.length.weeks
        res.activity.count.norm.weeks.peripheral[i] = res.activity.count.peripheral[i] / res.range.length.weeks
    }

    ## Build the data frame as the result
    res = data.frame(
        range = res.range,
        devs = res.devs,
        devs.core = res.core,
        devs.peripheral = res.peripheral,
        activity.count = res.activity.count,
        activity.count.core = res.activity.count.core,
        activity.count.peripheral = res.activity.count.peripheral,
        activity.count.avg.core = res.activity.count.avg.core,
        activity.count.avg.peripheral = res.activity.count.avg.peripheral,
        activity.count.med.core = res.activity.count.med.core,
        activity.count.med.peripheral = res.activity.count.med.peripheral,
        activity.count.norm.weeks.core = res.activity.count.norm.weeks.core,
        activity.count.norm.weeks.peripheral = res.activity.count.norm.weeks.peripheral
    )

    logging::logdebug("get.author.class.activity: finished.")
    return(res)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Role stability ----------------------------------------------

## Get a data frame with the authors and their occurence count in the specified class for
## the specified author classification list.
get.recurring.authors = function(author.class.overview, class = c("both", "core", "peripheral")) {
    logging::logdebug("get.recurring.authors: starting.")

    class = match.arg(class)

    authors = c()
    freq = c()

    ## Iterate over each version development range
    for(i in 1:length(author.class.overview)) {

        ## skip range in case no classification is available
        if(all(is.na(author.class.overview[[i]]))) {
            next
        }

        if (class == "both") {

            ## skip range in case no classification is available
            if(nrow(author.class.overview[[i]]$core) == 0
               && nrow(author.class.overview[[i]]$peripheral) == 0) {
                next
            }

            author.class.authors = c(author.class.overview[[i]]$core$author.name,
                                     author.class.overview[[i]]$peripheral$author.name)
        } else {

            ## skip range in case no classification for the given class is available
            if(nrow(author.class.overview[[i]][[class]])==0) {
                next
            }

            author.class.authors = author.class.overview[[i]][[class]]$author.name
        }

        ## Iterate over each author in the specified class and increase his occurence count
        for(j in 1:length(author.class.authors)) {
            author.class.author.name = author.class.authors[j]

            ## Check if the author already exists in previous ranges
            author.class.author.index = which(authors == author.class.author.name)
            if (length(author.class.author.index) > 0) {

                ## Increase the occurence count as the author already exists in previous ranges
                freq[author.class.author.index] = freq[author.class.author.index] + 1
            } else {

                ## Save the author and its first occurence
                authors = c(authors, author.class.author.name)
                freq = c(freq, 1)
            }
        }
    }

    data = data.frame(
        author.name = authors,
        freq = freq
    )

    ## Sort the authors by occurence count
    data = data[order(data$freq, decreasing = TRUE),]

    logging::logdebug("get.recurring.authors: finished.")
    return(data)
}

## Retrieves all authors which will be classified as core by the specified
## classification in more than a certain number of version ranges
## -> see: "LONGTERM.CORE.THRESHOLD".
get.longterm.core.authors = function(author.class = NULL) {
    logging::logdebug("get.longterm.core.authors: starting.")

    if(is.null(author.class)) {
        logging::logerror("For the analysis of longterm-core authors, the author classification has to be given.")
        stop("The author classification has to be given.")
    }

    ## Get a list with the occurence freq for each core author
    recurring.authors = get.recurring.authors(author.class, class = "core")

    ## Calculate the num of occurences at which a dev gets stated as longterm core
    longterm.threshold = length(author.class) * LONGTERM.CORE.THRESHOLD

    ## Get the longterm core authors
    longterm.core = recurring.authors[recurring.authors$freq >= longterm.threshold,]$author.name

    logging::logdebug("get.longterm.core.authors: finished.")
    return(longterm.core)
}

## Get a markov chain object representing the role stability of the
## specified classification overview.
get.role.stability = function(author.class.overview) {
    logging::logdebug("get.role.stability: starting.")

    core.core = 0 # core in prev version and core in current version
    core.peripheral = 0 # core in prev version and peripheral in current version
    core.absent = 0 # core in prev version and absent in current version
    peripheral.core = 0 # peripheral in prev version and core in current version
    peripheral.peripheral =0 # peripheral in prev version and peripheral in current version
    peripheral.absent = 0 # peripheral in prev version and absent in current version
    absent.core = 0 # absent in prev version and core in current version
    absent.peripheral = 0 # absent in prev version and peripheral in current version
    absent.absent = 0 # absent in prev version and absent in current version

    dev.current.absent = c()

    ## Run through each version range author classification
    for (i in 2:length(author.class.overview)) {

        ## Get core and peripheral devs from previous version
        class.prev = author.class.overview[[i-1]]
        dev.prev.core = class.prev$core$author.name
        dev.prev.peripheral = class.prev$peripheral$author.name
        dev.prev = c(dev.prev.core, dev.prev.peripheral)
        dev.prev.absent = dev.current.absent

        ## Get core and peripheral devs from current version
        class.current = author.class.overview[[i]]
        dev.current.core = class.current$core$author.name
        dev.current.peripheral = class.current$peripheral$author.name
        dev.current = c(dev.current.core, dev.current.peripheral)
        dev.current.absent = unique(c(dev.prev.absent[!(dev.prev.absent %in% dev.current)], dev.prev[!(dev.prev %in% dev.current)]))

        ## Add the transition numbers of core authors
        core.core = core.core + sum(dev.prev.core %in% dev.current.core)
        core.peripheral = core.peripheral + sum(dev.prev.core %in% dev.current.peripheral)
        core.absent = core.absent + sum(dev.prev.core %in% dev.current.absent)

        ## Add the transition numbers of peripheral authors
        peripheral.core = peripheral.core + sum(dev.prev.peripheral %in% dev.current.core)
        peripheral.peripheral = peripheral.peripheral + sum(dev.prev.peripheral %in% dev.current.peripheral)
        peripheral.absent = peripheral.absent + sum(dev.prev.peripheral %in% dev.current.absent)

        ## Add the transition numbers of absent authors
        absent.core = absent.core + sum(dev.prev.absent %in% dev.current.core)
        absent.peripheral = absent.peripheral + sum(dev.prev.absent %in% dev.current.peripheral)
        absent.absent = absent.absent + sum(dev.prev.absent %in% dev.current.absent)
    }

    ## Calculate the percentage of the core author transitions
    core.transition.sum = sum(core.core, core.peripheral, core.absent)
    core.core.rel = core.core / core.transition.sum
    core.peripheral.rel = core.peripheral / core.transition.sum
    core.absent.rel = core.absent / core.transition.sum

    ## Calculate the percentage of the peripheral author transitions
    peripheral.transition.sum = sum(peripheral.core, peripheral.peripheral, peripheral.absent)
    peripheral.core.rel = peripheral.core / peripheral.transition.sum
    peripheral.peripheral.rel = peripheral.peripheral / peripheral.transition.sum
    peripheral.absent.rel = peripheral.absent / peripheral.transition.sum

    ## Calculate the percentage of the absent author transitions
    absent.transition.sum = sum(absent.core, absent.peripheral, absent.absent)
    absent.core.rel = ifelse(absent.transition.sum > 0, absent.core / absent.transition.sum, 0)
    absent.peripheral.rel = ifelse(absent.transition.sum > 0, absent.peripheral / absent.transition.sum, 0)
    ## set to 1 because if all absent authors remain absent if there are never transition from absent-state
    absent.absent.rel = ifelse(absent.transition.sum > 0, absent.absent / absent.transition.sum, 1)

    ## Build the markov chain
    roles = c("Core", "Peripheral", "Absent")
    roles.matrix = matrix(data = c(core.core.rel, core.peripheral.rel, core.absent.rel,
                                   peripheral.core.rel, peripheral.peripheral.rel, peripheral.absent.rel,
                                   absent.core.rel, absent.peripheral.rel, absent.absent.rel),
                          byrow = TRUE, nrow = 3, dimnames = list(roles, roles))

    roles.stability = new("markovchain", states = roles, byrow = TRUE,
                          transitionMatrix = roles.matrix, name = "Role Stability")

    logging::logdebug("get.role.stability: finished.")
    return(roles.stability)
}

## Calculates the cohen's kappa to measure the agreement of the specified author classifications.
calculate.cohens.kappa = function(author.classification.list, other.author.classification.list) {
    logging::logdebug("calculate.cohens.kappa: starting.")

    num.core.core = 0 # core in first, core in second
    num.core.peripheral = 0 # core in first, peripheral in second
    num.peripheral.core = 0 # peripheral in first, core in second
    num.peripheral.peripheral = 0 # peripheral in first, peripheral in second

    ## Calculate the sums of equal classifications
    for(i in 1:length(author.classification.list)) {
        author.class = author.classification.list[[i]]
        author.class.compare = other.author.classification.list[[i]]

        num.core.core = num.core.core +
            sum(author.class$core$author.name %in% author.class.compare$core$author.name == TRUE)

        num.core.peripheral = num.core.peripheral +
            sum(author.class$core$author.name %in% author.class.compare$peripheral$author.name == TRUE)

        num.peripheral.core = num.peripheral.core +
            sum(author.class$peripheral$author.name %in% author.class.compare$core$author.name == TRUE)

        num.peripheral.peripheral = num.peripheral.peripheral +
            sum(author.class$peripheral$author.name %in% author.class.compare$peripheral$author.name == TRUE)
    }

    num.sum = num.core.core + num.peripheral.peripheral + num.core.peripheral + num.peripheral.core

    po = (num.core.core + num.peripheral.peripheral) / num.sum
    pe.core = ((num.core.core + num.core.peripheral) / num.sum) * ((num.core.core + num.peripheral.core) / num.sum)
    pe.peripheral = ((num.peripheral.peripheral + num.core.peripheral) / num.sum) * ((num.peripheral.peripheral + num.peripheral.core) / num.sum)
    pe = pe.core + pe.peripheral

    kappa = (po - pe) / (1 - pe)

    logging::logdebug("calculate.choens.kappa: finished.")
    return(kappa)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Generic helper functions ------------------------------------------------

## Classify the specified set of authors into core and peripheral based on the data
## of the specified data frame column name (calc.base.name).
## Core authors are those which are responsible for a given
## percentage of the work load with a default threshold set at 80% according
## to Ref: Terceiro A, Rios LR, Chavez C (2010) An empirical study on
##         the structural complexity introduced by core and peripheral
##         developers in free software projects.
get.author.class = function(author.data.frame, calc.base.name, result.limit = NULL) {
    logging::logdebug("get.author.class: starting.")

    ## Return empty classification in case no data is available
    if(all(is.na(author.data.frame))) {
        logging::logwarn("There is no data to use for the classification. Returning empty classification...")

        empty.df <- data.frame(character(0), numeric(0))
        names(empty.df) <- c("author.name", calc.base.name)
        return(list("core" = empty.df,
                    "peripheral" = empty.df))
    }

    ## Make sure the provided data is ordered correctly by the calculation base
    author.data = author.data.frame[order(author.data.frame[[calc.base.name]], decreasing = TRUE), , drop = FALSE]

    ## Remove rows with invalid calculation base values
    if(any(is.na(author.data[[calc.base.name]]))) {
        logging::logwarn("Some authors' activity indicator (%s) is NA. Setting the activity to 0...", calc.base.name)
        author.data[is.na(author.data[[calc.base.name]]), calc.base.name, drop = FALSE] = 0
    }

    ## Get the threshold depending on all calculation base values
    author.class.threshold = get.threshold(author.data[[calc.base.name]])

    ## Check if the result shall be limited
    if (!is.null(result.limit)) {
        author.data = head(author.data, result.limit)
    }

    ## Check which authors can be treated as core based on the calculation base values:
    ## (1) check which positions are over the threshold
    ## (2) check which positions are equal to the threshold (use all.equal due to rounding errors)
    author.cumsum = cumsum(author.data[[calc.base.name]])
    author.class.threshold.idx = min(which(
        author.cumsum > author.class.threshold |
            sapply(author.cumsum, function(x) isTRUE(all.equal(x, author.class.threshold)))
    ))

    ## classify developers according to threshold
    core.classification = rep(FALSE, nrow(author.data))
    core.classification[1:author.class.threshold.idx] = TRUE

    ## If we have not found a core author, the author with the highest calculation base value
    ## will be treated as core, to return at least one core author. The only exception is the
    ## case that no activity/collaboration occured. Then, all authors are classified as peripheral.
    if(author.class.threshold == 0) {
        logging::logwarn("No collaboration/activity occured, thus, all developer's classification is set to peripheral.")
        core.classification = rep(FALSE, length(core.classification))
        # } else if (!any(core.classification)) {
        #     core.classification = c(TRUE, rep(FALSE, length(core.classification) - 1))
    }

    ## Cut core and peripheral authors from base data and construct return value
    core.authors = author.data[core.classification, , drop = FALSE]
    peripheral.authors = author.data[!core.classification, , drop = FALSE]
    res = list(core = core.authors, peripheral = peripheral.authors)

    logging::logdebug("get.author.class: finished.")
    return(res)
}

## Get the threshold based on the specified integer data list
## on which a author can be classified as core.
get.threshold = function(data.list) {
    logging::logdebug("get.threshold: starting.")

    ## Calculate the sum of the provided data as base for the threshold calculation
    data.threshold.base = sum(data.list)

    ## Check which authors can be treated as core based on the data
    data.threshold = round(CORE.THRESHOLD * data.threshold.base)

    logging::logdebug("get.threshold: finished.")
    return(data.threshold)
}

## Get the commit data with the specified columns for the specified version range as a data frame
## for each specified split range.
## A split interval can be set by defining the number of weeks for each requested range as a vector.
get.commit.data = function(codeface.range.data, columns = c("author.name", "author.email"), split = c()) {
    logging::logdebug("get.commit.data: starting.")

    ## Get commit data
    commits.df = codeface.range.data$get.commits.raw()

    ## In case no commit data is available, return NA
    if(nrow(commits.df) == 0) {
        return(NA)
    }

    ## Make sure the hash is included in the cut columns vector for grouping
    cut.columns = columns
    if (!("hash" %in% cut.columns)) {
        cut.columns = c(cut.columns, "hash")
    }

    ## Make sure the date is included in the cut columns vector for splitting
    if (!("date" %in% cut.columns)) {
        cut.columns = c(cut.columns, "date")
    }

    ## Cut down data to needed minimum
    commits.df = commits.df[cut.columns]

    ## Group by hash to get a line per commit
    commits.df = sqldf::sqldf("select * from `commits.df` group by `hash`")

    ## Remove hash column if not wanted as it now contains nonsensical data
    if (!("hash" %in% columns)) {
        commits.df["hash"] = NULL
    }

    ## Order commits by date column
    commits.df = commits.df[order(commits.df$date),]

    ## Fetch the date range info
    date.first = as.Date(commits.df$date[1])
    date.last = as.Date(commits.df$date[nrow(commits.df)]) + 1 # +1 since findInterval is right-exclusive

    ## Calc the split dates depending on the specified intervals
    date.split = c(date.last)
    if (!is.null(split)) {
        for (i in 1:length(split)) {
            ## substract split[i] number of weeks (i.e., split[i] * 7 days)
            ## TODO use lubridate package here to substract a week from POSIXct?
            date.calc = date.split[i] - (split[i] * 7)

            ## Check if calculated date is still after the first commit date of the range
            if (date.calc > date.first) {
                date.split = c(date.split, date.calc)
            } else {
                date.split = c(date.split, date.first)
                break
            }
        }
    } else {
        date.split = c(date.split, date.first)
    }

    date.split = rev(date.split)

    ## Only keep the commits which were made within the specified split ranges
    ## TODO https://github.com/se-passau/codeface-extraction-r/pull/51#discussion_r132924711
    commits.df = commits.df[as.Date(commits.df$date) >= date.split[1],]

    ## Calc group numbers for the commits by the split dates
    intervals = findInterval(as.Date(commits.df[["date"]]), date.split, all.inside = FALSE)

    ## Remove date column if not wanted
    if (!("date" %in% columns)) {
        commits.df["date"] = NULL
    }

    ## Split the commits by the calculated groups
    res = split.data.by.bins(commits.df, intervals)
    names(res) = construct.ranges(date.split)
    attr(res, "bins") = date.split

    logging::logdebug("get.commit.data: finished.")
    return(res)
}
