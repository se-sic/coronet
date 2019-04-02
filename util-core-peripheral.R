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
## Copyright 2017 by Mitchell Joblin <mitchell.joblin@uni-passau.de>
## Copyright 2017 by Ferdinand Frank <frankfer@fim.uni-passau.de>
## Copyright 2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2017-2019 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2018 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## All Rights Reserved.
##
## This file is derived from following Codeface script:
## https://github.com/siemens/codeface/blob/master/codeface/R/developer_classification.r

## TODO see https://github.com/se-passau/codeface-extraction-r/issues/70
## TODO adjust coding style regarding bracket notation


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

#' Classify authors into the classes "core" and "peripheral".
#'
#' The classification algorithm works by considering a numerical value for each author that denotes their centrality and
#' can be imagined to work in the following way:
#'   1. Order the authors by their centrality value and put them into a stack in which the most central authors reside
#'      on the top
#'   2. Initialize an empty set of "core"-authors
#'   3. Pop the author residing at the very top of the stack and add them to the set of "core"-authors
#'   4. Check if the combined centrality of the set of "core"-authors is greater than or equal to \code{CORE.THRESHOLD}
#'      times the total centrality value (sum over the centrality values of all authors)
#'      - If so, terminate. Consider everyone in the set of "core"-authors as "core" and everyone else as "peripheral".
#'      - If not, go to step 3
#'
#' @param network the network containing the authors to classify (parameter is required if the parameter \code{type}
#'                specifies a network-based classification metric) [default: NULL]
#' @param proj.data the \code{ProjectData} containing the authors to classify (parameter is required if the parameter
#'                  \code{type} specifies a count-based classification metric) [default: NULL]
#' @param type a character string declaring the classification metric. The classification metric determines which
#'             numerical characteristic of authors should be chosen as their centrality.
#'             The parameter currently supports the following five options:
#'             Network-based options/metrics (parameter \code{network} has to be specified):
#'              - "network.degree"
#'              - "network.eigen"
#'              - "network.hierarchy"
#'             Commit-based options/metrics (parameter \code{proj.data} has to be specified):
#'              - "commit.count"
#'              - "loc.count"
#'             [default: "network.degree"]
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which holding the authors classified as core or peripheral, respectively. Both entries in this list ("core"
#'         and "peripheral") are dataframes containing the authors' names in the first column and their centrality
#'         values in the second column.
get.author.class.by.type = function(network = NULL,
                                    proj.data = NULL,
                                    type = c("network.degree", "network.eigen", "network.hierarchy",
                                             "commit.count", "loc.count"),
                                    result.limit = NULL,
                                    restrict.classification.to.authors = NULL) {

    logging::logdebug("get.author.class.by.type: starting.")

    type = match.arg(type)

    ## Get a reasonable metric name for each classification type
    metric.name = switch(type,
                         "network.degree" = "node.degree",
                         "network.eigen" = "eigen.centrality",
                         "network.hierarchy" = "hierarchy",
                         "commit.count" = "commit.count",
                         "loc.count" = "loc.count")

    if (startsWith(type, "network")) {
        if (is.null(network)) {
            logging::logerror("For network-based classifications the parameter 'network' must not be null.")
            stop("For network-based classifications the parameter 'network' must not be null.")
        }

        ## Ensure that the parameter 'network' is of type 'igraph'
        verify.argument.for.parameter(network, "igraph", "get.author.class.by.type")

        if (igraph::vcount(network) == 0) {
            logging::logwarn("The specified network is empty. Returning an empty classification.")
            return(list("core" = create.empty.data.frame(c("author.name", metric.name), c("character", "numeric")),
                        "peripheral" = create.empty.data.frame(c("author.name", metric.name), c("character", "numeric"))))
        }
    } else {
        if (is.null(proj.data)) {
            logging::logerror("For commit-based classifications the parameter 'proj.data' must not be null.")
            stop("For commit-based classifications the parameter 'proj.data' must not be null.")
        }

        ## Ensure that the parameter 'proj.data' is of type 'ProjectData'
        verify.argument.for.parameter(proj.data, "ProjectData", "get.author.class.by.type")
    }


    ## The centrality dataframe has two columns:
    ## 1. a column of author names
    ## 2. a column for the centrality type (which is given by the parameter 'type'),
    ##    e.g. the node degree when type == network.degree or the commit count when type == commit.count
    centrality.dataframe = NULL

    if (type == "network.degree") {
        ## Get node degrees for all authors
        node.degree.vec = igraph::degree(network)

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = names(node.degree.vec), centrality = as.vector(node.degree.vec))
    } else if (type == "network.eigen") {
        ## Get eigenvectors for all authors
        ## The method ignores the directed parameter if the given network is undirected
        eigen.centrality.vec = tryCatch(
            igraph::eigen_centrality(network, directed = TRUE),
            error = function(e) {
                logging::logwarn(e)
                logging::logwarn("As of the error above, adjust ARPACK options 'maxiter' and 'tol'...")
                adjusted.options = list(maxiter = 5000, tol = 0.1)
                adjusted.computation = igraph::eigen_centrality(network, directed = TRUE,
                                                                options = adjusted.options)
                logging::loginfo("eigen_centrality with adjusted ARPACK options finished successfully.")
                return(adjusted.computation)
            }
        )

        eigen.centrality.vec = eigen.centrality.vec[["vector"]]

        ## In case no collaboration occured, all centrality values are set to 0
        if (igraph::ecount(network) == 0) {
            eigen.centrality.vec[1:length(eigen.centrality.vec)] = rep(0, length(eigen.centrality.vec))
        }

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = names(eigen.centrality.vec),
                                          centrality = as.vector(eigen.centrality.vec))
    } else if (type == "network.hierarchy") {
        hierarchy.base.df = metrics.hierarchy(network)
        hierarchy.calculated = hierarchy.base.df$deg / hierarchy.base.df$cc

        ## fix all authors whose clustering coefficient is '0', since their hierarchy value
        ## is 'Inf'. We do not get any complications here because there are no authors with
        ## degree == 0 and a CC > 0 (i.e., the hierarchy value would really be 0). Authors with
        ## a CC == NaN (degree < 2) will stay with their hierarchy value of NaN, accordingly.
        hierarchy.calculated[is.infinite(hierarchy.calculated)] = 0

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = row.names(hierarchy.base.df), centrality = hierarchy.calculated)
    } else if (type == "commit.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.commit.count(proj.data)
    } else {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.loc.count(proj.data)
    }

    # rename the second column of the centrality dataframe to the correct name with respect to the classification type
    names(centrality.dataframe)[2] = metric.name

    ## If the parameter 'restrict.classification.to.authors' is 'NULL', no restriction is made.
    ## Therefore, the restriction is defined as to include all authors (i.e., there is no restriction in the next step)
    if (is.null(restrict.classification.to.authors)) {
        restrict.classification.to.authors = centrality.dataframe$author.name
    }

    ## Restrict authors as given by the parameter 'restrict.classification.to.authors' before classification
    centrality.dataframe = centrality.dataframe[centrality.dataframe$author.name %in% restrict.classification.to.authors, ]

    ## Retrieve classification results
    classification = get.author.class(centrality.dataframe, metric.name, result.limit = result.limit)

    ## Authors who are specified in the parameter 'restrict.classification.to.authors' but were not considered in the
    ## classification will be appended with a value of 'NA' to the classification as peripheral authors:
    ## 1) Prepare a list of author names who are specified in the parameter 'restrict.classification.to.authors' but are
    ##    not part of the classification
    remaining.authors = setdiff(restrict.classification.to.authors, centrality.dataframe$author.name)
    ## 2) Create a dataframe for those authors, all of which getting 'NA' as centrality value
    remaining.authors.df = data.frame(author.name = remaining.authors, temp = rep(NA, length(remaining.authors)))
    ## 3) In preparition to the coming 'rbind', adjust the column names to be the same for both dataframes
    names(remaining.authors.df) = c("author.name", metric.name)
    ## 4) Append the newly created dataframe of authors to the classification as peripheral authors
    classification$peripheral = rbind(classification$peripheral, remaining.authors.df)

    logging::logdebug("get.author.class.by.type: finished.")
    return(classification)
}

#' Classify authors into "core" and "peripheral" independently for multiple ranges.
#'
#' @param network.list a list of networks, each of which containing the authors to classify (parameter is required if
#'                     the parameter \code{type} specifies a network based classification metric) [default: null]
#' @param range.data.list a list of \code{RangeData} objects, each of which containing the authors to
#'                        classify (parameter is required if the parameter \code{type} specifies a count based
#'                        classification metric) [default: null]
#' @param type a character string declaring the classification metric. The classification metric determines which
#'             numerical characteristic of authors should be chosen as their centrality.
#'             The parameter currently supports the following five options:
#'             Network-based options/metrics (parameter \code{network} has to be specified):
#'              - "network.degree"
#'              - "network.eigen"
#'              - "network.hierarchy"
#'             Commit-based options/metrics (parameter \code{proj.data} has to be specified):
#'              - "commit.count"
#'              - "loc.count"
#'             [default: "network.degree"]
#' @param restrict.classification.to.authors a vector of author names or a list of vectors of author names.
#'                                           When choosing the first option (i.e., when passing a vector of author
#'                                           names), the group of authors that is about to be classified in each range
#'                                           is restricted to those that are contained within the vector. Authors that
#'                                           appear in the vector but would not be part of the classification result of
#'                                           a range will also be added to it afterwards but with a centrality value of
#'                                           \code{NA}.
#'                                           Alternatively, a list containing vectors of author names can be passed. The
#'                                           list must have the same length as the list of networks specified in the
#'                                           parameter \code{network.list} or the list of \code{RangeData} objects
#'                                           specified in the parameter \code{range.data.list}, respectively. For each
#'                                           range, the corresponding author group in the list will then be used for
#'                                           restriction instead of using the same group for all ranges. \code{NULL}
#'                                           means that no restriction is made. [default: NULL]
#'
#' @return a list of classification results. Each classification result is a list containing two named list members
#'         "core" and "peripheral", each of which holding the authors classified as core or peripheral, respectively.
#'         Both entries in this list ("core" and "peripheral") are dataframes containing the authors' names in the first
#'         column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.overview = function(network.list = NULL, range.data.list = NULL,
                                     type = c("network.degree", "network.eigen", "network.hierarchy",
                                              "commit.count", "loc.count"),
                                     restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.overview: starting.")

    type = match.arg(type)

    if (startsWith(type, "network")) {
        if (is.null(network.list)) {
            logging::logerror("For network-based classifications the parameter 'network.list' must not be null.")
            stop("For network-based classifications the parameter 'network.list' must not be null.")
        }

        data.list = network.list
    } else {
        if (is.null(range.data.list)) {
            logging::logerror("For commit-based classifications the parameter 'range.data.list' must not be null.")
            stop("For commit-based classifications the parameter 'range.data.list' must not be null.")
        }
        data.list = range.data.list
    }

    ## If the parameter 'restrict.classification.to.authors' is no list but simply a vector of authors, this vector is
    ## replicated for each range so that each range is restricted to same group of authors
    if (!is.list(restrict.classification.to.authors)) {
        restrict.classification.to.authors = rep(list(restrict.classification.to.authors), length(data.list))
    } else if (length(restrict.classification.to.authors) != length(data.list)) {
        stop("If a list is specified in the parameter 'restrict.classification.to.authors', its length must match the
              length of either the parameter 'network.list' or 'range.data.list', depending on the classification type
              specified in the parameter 'type'.")
    }

    res = list()
    for (i in seq_along(data.list)) {

        ## Get classification data of the current range
        data = data.list[[i]]
        range.name = names(data.list)[[i]]

        if (startsWith(type, "network")) {
            range.class = get.author.class.by.type(network = data, type = type,
                                                   restrict.classification.to.authors =
                                                       restrict.classification.to.authors[[i]])
        } else {
            range.class = get.author.class.by.type(proj.data = data, type = type,
                                                   restrict.classification.to.authors =
                                                        restrict.classification.to.authors[[i]])
        }

        ## Save in list of classifications
        if (!is.null(range.name)) {
            res[[range.name]] = range.class
        } else {
            res = c(res, list(range.class))
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
get.author.class.activity.overview = function(range.data.list = NULL,
                                              author.class.overview = NULL,
                                              split = c(),
                                              sliding.window.core = NULL,
                                              longterm.cores = c(),
                                              activity.measure = c("commit.count", "loc.count")) {
    logging::logdebug("get.author.class.activity.overview: starting.")

    activity.measure = match.arg(activity.measure)

    if (is.null(range.data.list)) {
        logging::logerror("A list of RangeData objects is needed for the activity analysis.")
        stop("Raw data is needed for the activity analysis.")
    }

    if (is.null(author.class.overview)) {
        logging::logerror("An author.class.overview has to be given for the activity overview analysis.")
        stop("Author classification has to be given.")
    }

    if (length(range.data.list) != length(author.class.overview)) {
        logging::logerror("The raw data and the author classification use a different number of ranges.")
        stop("Raw data and author classification have to match.")
    }

    res = list()
    for (i in 1:length(range.data.list)) {

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

        res[[i]] = get.author.class.activity(range.data.list[[i]],
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

    if (!is.null(names(author.class.overview))) {
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

    if (!is.null(names(author.class.overview))) {
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

#' Classify authors into "core" and "peripheral" based on the node degree of author nodes in the network and return the
#' classification result.
#'
#' The functioning of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the authors' names in the first column and their
#'         centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.network.degree = function(network, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.network.degree: starting.")

    result = get.author.class.by.type(network = network, type = "network.degree", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.network.degree: finished.")
    return(result)
}

## * Eigenvector-based classification --------------------------------------

#' Classify authors into "core" and "peripheral" based on the eigenvector-centrality of author nodes in the network and
#' return the classification result.
#'
#' The functioning of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the authors' names in the first column and their
#'         centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.network.eigen = function(network, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.network.eigen: starting.")

    result = get.author.class.by.type(network = network, type = "network.eigen", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.network.eigen: finished.")
    return(result)
}

## * Hierarchy-based classification ----------------------------------------

#' Classify authors into "core" and "peripheral" based on the hierarchy-value of author nodes in the network and
#' return the classification result.
#'
#' The functioning of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the authors' names in the first column and their
#'         centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.network.hierarchy = function(network, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.network.hierarchy: starting.")

    result = get.author.class.by.type(network = network, type = "network.hierarchy", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.network.hierarchy: finished.")
    return(result)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Commit-based classification ---------------------------------------------

## * Count-based classification --------------------------------------------

#' Classify authors into "core" and "peripheral" based on authors' commit-counts and return the classification result.
#'
#' The functioning of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' commit data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the authors' names in the first column and their
#'         centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.commit.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL) {

    logging::logdebug("get.author.class.commit.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "commit.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.commit.count: finished.")
    return(result)
}

#' Get the commit count per comitter in the given range data, where the committer
#' does not match the author of the respective commits
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.not.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.not.author.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(range.data, columns = c("committer.name", "author.name"))[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the commit count per author
    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `commits.df`
                       WHERE `committer.name` <> `author.name`
                       GROUP BY `committer.name`, `author.name`
                       ORDER BY `freq` DESC")

    logging::logdebug("get.committer.not.author.commit.count: finished.")
    return(res)
}

#' Get the commit count per person in the given range data for commits where the author equals the committer.
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.and.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.and.author.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(range.data, columns = c("committer.name", "author.name"))[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the commit count per person
    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `commits.df`
                       WHERE `committer.name` = `author.name`
                       GROUP BY `committer.name`, `author.name`
                       ORDER BY `freq` DESC")

    logging::logdebug("get.committer.and.author.commit.count: finished.")
    return(res)
}

#' Get the commit count per person in the given range data where the person is committer or author or both.
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count
get.committer.or.author.commit.count = function(range.data) {
    logging::logdebug("get.committer.or.author.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(range.data, columns = c("committer.name", "author.name"))[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute queries to get the commit count per person
    ungrouped = sqldf::sqldf("SELECT `committer.name` AS `name` FROM `commits.df`
                             WHERE `committer.name` = `author.name`
                                UNION ALL
                             SELECT `author.name` AS `name` FROM `commits.df`
                             WHERE `author.name` <> `committer.name`
                                UNION ALL
                             SELECT `committer.name` AS `name` FROM `commits.df`
                             WHERE `author.name` <> `committer.name`")

    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `ungrouped`
                       GROUP BY `name`
                       ORDER BY `freq` DESC")

    logging::logdebug("get.committer.or.author.commit.count: finished.")
    return(res)
}

#' Get the commit count per committer in the given range data, where the committer
#' may match the author of the respective commits
#'
#' @param range.data The data to count on
#'
#' @return A data frame in descending order by the commit count.
get.committer.commit.count = function(range.data) {
    logging::logdebug("get.committer.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(range.data, columns = c("committer.name", "committer.email"))[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the commit count per author
    res = sqldf::sqldf("SELECT *, COUNT(*) AS `freq` FROM `commits.df`
                       GROUP BY `committer.name` ORDER BY `freq` DESC")

    logging::logdebug("get.committer.commit.count: finished.")
    return(res)
}

#' Get the commit count for each author based on the commit data contained in the specified \code{ProjectData}.
#'
#' @param proj.data the \code{ProjectData} containing the commit data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective commit counts
get.author.commit.count = function(proj.data) {
    logging::logdebug("get.author.commit.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(proj.data)[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the commit count per author
    res = sqldf::sqldf("SELECT `author.name`, COUNT(*) AS `freq` FROM `commits.df`
                       GROUP BY `author.name` ORDER BY `freq` DESC")

    logging::logdebug("get.author.commit.count: finished.")
    return(res)
}

## * LOC-based classification ----------------------------------------------

#' Classify authors into "core" and "peripheral" based on authors' lines-of-code (LOC) and return the classification
#' result. LOC are calculated independtly for each author by looking at the sum of added and deleted lines of code in
#' all their commits.
#'
#' The functioning of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' commit data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. The group of authors that is about to be
#'                                           classified is restricted to those that are contained within this vector.
#'                                           Authors that appear in the vector but would not be part of the
#'                                           classification result will also be added to it afterwards but with a
#'                                           centrality value of \code{NA}. \code{NULL} means that no restriction is
#'                                           made. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the authors' names in the first column and their
#'         centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.loc.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.loc.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "loc.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.loc.count: finished.")
    return(result)
}

#' Get the number of changed lines-of-code (LOC) for each author based on the commit data contained in the specified
#' \code{ProjectData}. The number is calculated by taking the sum of added and deleted lines-of-code for each commit.
#'
#' @param proj.data the \code{ProjectData} containing the commit data
#'
#' @return a dataframe consisting of two columns, the first of which holding the authors' names and the second holding
#'         their respective LOC counts
get.author.loc.count = function(proj.data) {
    logging::logdebug("get.author.loc.count: starting.")

    ## Get commit data
    commits.df = get.commit.data(
        proj.data,
        columns = c("author.name", "added.lines", "deleted.lines")
    )[[1]]

    ## Return NA in case no commit data is available
    if (all(is.na(commits.df))) {
        return(NA)
    }

    ## Execute a query to get the changed lines per author
    res = sqldf::sqldf("SELECT `author.name`, SUM(`added.lines`) + SUM(`deleted.lines`) AS `loc`
                        FROM `commits.df`
                        GROUP BY `author.name` ORDER BY `loc` DESC")

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
get.author.class.activity = function(range.data = NULL,
                                     author.class = NULL,
                                     activity.measure = c("commit.count", "loc.count"),
                                     split = c(),
                                     additional.cores = NULL) {
    logging::logdebug("get.author.class.activity: starting.")

    activity.measure = match.arg(activity.measure)

    if (is.null(range.data)) {
        logging::logerror("A RangeData object is needed for the activity analysis.")
        stop("Raw data is needed for the activity analysis.")
    }
    if (is.null(author.class)) {
        logging::logerror("An author classification is needed for the activity analysis.")
        stop("Author classification has to be given by the user")
    }

    ## Return NA in case no classification information is available
    if (all(is.na(author.class))
       || (nrow(author.class$core) + nrow(author.class$peripheral) == 0)) {
        return(NA)
    }

    author.core = unique(c(author.class$core$author.name, additional.cores))

    ## Get the splitted commit data with all necessary columns
    commits.data = get.commit.data(range.data,
                                   columns = c("author.name", "added.lines", "deleted.lines"),
                                   split = split)

    ## Build the query string to group commits by the author name
    commits.query = "SELECT `author.name`, SUM(`added.lines`) + SUM(`deleted.lines`) AS `loc.count`,
                        COUNT(*) AS `commit.count`
                     FROM `commits.df`
                     GROUP BY `author.name`"

    ## Get the authors with their commit count and corresponding class for each splitted range
    commits.dev.list = list()
    for (i in 1:length(commits.data)) {
        commits.df = commits.data[[i]]
        commits.dev.list[[names(commits.data)[i]]] = sqldf::sqldf(commits.query)

        ## Classify authors in splitted range according to the overall classification
        core.test = commits.dev.list[[names(commits.data)[i]]]$author.name %in% author.core
        commits.dev.core = commits.dev.list[[names(commits.data)[i]]][core.test, ]
        commits.dev.peripheral = commits.dev.list[[names(commits.data)[i]]][!core.test, ]

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
        activity.count.core.ordered = commits.dev$core[order(commits.dev$core[[activity.measure]]), ][[activity.measure]]
        activity.count.peripheral.ordered = commits.dev$peripheral[order(commits.dev$peripheral[[activity.measure]]), ][[activity.measure]]
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
    for (i in 1:length(author.class.overview)) {

        ## skip range in case no classification is available
        if (all(is.na(author.class.overview[[i]]))) {
            next
        }

        if (class == "both") {

            ## skip range in case no classification is available
            if (nrow(author.class.overview[[i]]$core) == 0
               && nrow(author.class.overview[[i]]$peripheral) == 0) {
                next
            }

            author.class.authors = c(author.class.overview[[i]]$core$author.name,
                                     author.class.overview[[i]]$peripheral$author.name)
        } else {

            ## skip range in case no classification for the given class is available
            if (nrow(author.class.overview[[i]][[class]]) == 0) {
                next
            }

            author.class.authors = author.class.overview[[i]][[class]]$author.name
        }

        ## Iterate over each author in the specified class and increase his occurence count
        for (j in 1:length(author.class.authors)) {
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
    data = data[order(data$freq, decreasing = TRUE), ]

    logging::logdebug("get.recurring.authors: finished.")
    return(data)
}

## Retrieves all authors which will be classified as core by the specified
## classification in more than a certain number of version ranges
## -> see: "LONGTERM.CORE.THRESHOLD".
get.longterm.core.authors = function(author.class = NULL) {
    logging::logdebug("get.longterm.core.authors: starting.")

    if (is.null(author.class)) {
        logging::logerror("For the analysis of longterm-core authors, the author classification has to be given.")
        stop("The author classification has to be given.")
    }

    ## Get a list with the occurence freq for each core author
    recurring.authors = get.recurring.authors(author.class, class = "core")

    ## Calculate the num of occurences at which a dev gets stated as longterm core
    longterm.threshold = length(author.class) * LONGTERM.CORE.THRESHOLD

    ## Get the longterm core authors
    longterm.core = recurring.authors[recurring.authors$freq >= longterm.threshold, ]$author.name

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
    for (i in 1:length(author.classification.list)) {
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


#' Classify authors into "core" and "peripheral".
#'
#' Core authors are those which are responsible for a given percentage of the work load with a default threshold set at
#' 80% according to Ref:
#' Terceiro A, Rios LR, Chavez C (2010) An empirical study on the structural complexity introduced by core and
#' peripheral developers in free software projects.
#'
#' @param author.data.frame a dataframe containing at least two columns. The column called "author.name" should store
#'                          the author's names, the other one, whose name must be specified in the parameter
#'                          \code{calc.base.name}, should store the authors' centrality values.
#' @param calc.base.name the name of the second column of the dataframe specified in the parameter
#'                       \code{author.data.frame}
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each of
#'         which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the author's names in the first column and their
#'         centrality values in the second column.
get.author.class = function(author.data.frame, calc.base.name, result.limit = NULL) {
    logging::logdebug("get.author.class: starting.")

    ## Make sure the provided data is ordered correctly by the calculation base
    author.data = author.data.frame[order(author.data.frame[[calc.base.name]], decreasing = TRUE), , drop = FALSE]

    ## Remove rows with invalid calculation base values
    if (any(is.na(author.data[[calc.base.name]]))) {
        logging::logwarn("Some authors' activity indicator (%s) is NA. Setting the activity to 0...", calc.base.name)
        author.data[is.na(author.data[[calc.base.name]]), calc.base.name] = 0
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
    buffer.value = which(
        author.cumsum > author.class.threshold |
            sapply(author.cumsum, function(x) isTRUE(all.equal(x, author.class.threshold)))
    )
    if (is.infinite(min(buffer.value))) {
        author.class.threshold.idx = nrow(author.data)
    } else {
        author.class.threshold.idx = min(buffer.value)
    }

    ## classify authors according to threshold
    core.classification = rep(FALSE, nrow(author.data))
    core.classification[1:author.class.threshold.idx] = TRUE

    ## With no activity/collaboration occurring, all authors are classified as peripheral.
    if (author.class.threshold == 0) {
        logging::logwarn("No collaboration/activity occured, thus, all authors' classification is set to peripheral.")
        core.classification = rep(FALSE, length(core.classification))
        # ## old code: if we found no core author (should not happen anymore)
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

#' Retrieve the classification threshold of a list containing centrality values. The threshold is determined by taking
#' the sum over all centrality values and multiplying this sum with \code{CORE.THRESHOLD}.
#'
#' Example: Consider three authors A, B and C that have centrality values of 1, 2 and 2, respectively. By taking the sum
#' and multiplying with \code{CORE.THRESHOLD} which, for instance, could be 0.8, we end up with a classification
#' threshold of (1 + 2 + 2) * 0.8 = 4. The smallest group of authors that together has a centrality sum of 4 or more
#' could now be considered core.
#'
#' @param centrality.list a list containing centrality values
#'
#' @return the threshold used within classifications
get.threshold = function(centrality.list) {
    logging::logdebug("get.threshold: starting.")

    ## Calculate the sum of the provided data as base for the threshold calculation
    data.threshold.base = sum(centrality.list)

    ## Check which authors can be treated as core based on the data
    data.threshold = CORE.THRESHOLD * data.threshold.base

    logging::logdebug("get.threshold: finished.")
    return(data.threshold)
}
