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
## Copyright 2017 by Mitchell Joblin <mitchell.joblin@uni-passau.de>
## Copyright 2017 by Ferdinand Frank <frankfer@fim.uni-passau.de>
## Copyright 2017 by Sofie Kemper <kemperso@fim.uni-passau.de>
## Copyright 2017-2020 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018-2019 by Christian Hechtl <hechtl@fim.uni-passau.de>
## Copyright 2021 by Christian Hechtl <hechtl@cs.uni-saarland.de>
## Copyright 2018 by Klara Schlüter <schluete@fim.uni-passau.de>
## Copyright 2019 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2019 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
## Copyright 2021 by Johannes Hostert <s8johost@stud.uni-saarland.de>
## All Rights Reserved.
##
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
## an author has to be classified as core to be stated as a
## longterm core author
LONGTERM.CORE.THRESHOLD = 0.5


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Classification Type Categories ----------------------------------------

## Mapping of the classification type to its category which can either
## be 'network' or 'count' based categories
CLASSIFICATION.TYPE.TO.CATEGORY = list(
    "network.degree"            = "network",
    "network.eigen"             = "network",
    "network.hierarchy"         = "network",
    "commit.count"              = "count",
    "loc.count"                 = "count",
    "mail.count"                = "count",
    "mail.thread.count"         = "count",
    "issue.count"               = "count",
    "issue.comment.count"       = "count",
    "issue.commented.in.count"  = "count",
    "issue.created.count"       = "count"
)


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author-class wrappers and overviews -------------------------------------

#' Classify authors into the classes "core" and "peripheral".
#'
#' The classification algorithm works by considering a numerical value for each author that denotes their centrality
#' and can be imagined to work in the following way:
#'   1. Order the authors by their centrality value and put them into a stack in which the most central authors reside
#'      on the top
#'   2. Initialize an empty set of "core" authors
#'   3. Pop the author residing at the very top of the stack and add them to the set of "core" authors
#'   4. Check if the combined centrality of the set of "core" authors is greater than or equal to \code{CORE.THRESHOLD}
#'      times the total centrality value (sum over the centrality values of all authors)
#'      - If so, terminate. Consider everyone in the set of "core" authors as "core" and everyone else as "peripheral".
#'      - If not, go to step 3
#'
#' @param network the network containing the authors to classify (parameter is required if the parameter \code{type}
#'                specifies a network-based classification metric) [default: NULL]
#' @param proj.data the \code{ProjectData} containing the authors to classify (parameter is required if the parameter
#'                  \code{type} specifies a count-based classification metric) [default: NULL]
#' @param type a character string declaring the classification metric. The classification metric determines which
#'             numerical characteristic of authors is chosen as their centrality value.
#'             The parameter currently supports the following eleven options:
#'             Network-based options/metrics (parameter \code{network} has to be specified):
#'              - "network.degree"
#'              - "network.eigen"
#'              - "network.hierarchy"
#'             Count-based options/metrics (parameter \code{proj.data} has to be specified):
#'              - "commit.count"
#'              - "loc.count"
#'              - "mail.count"
#'              - "mail.thread.count"
#'              - "issue.count"
#'              - "issue.comment.count"
#'              - "issue.commented.in.count"
#'              - "issue.created.count"
#'             [default: "network.degree"]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}. This parameter is ignored for
#'                   non-issue-related classification metrics. [default: "all"]
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are data frames containing the authors' names in the
#'         first column and their centrality values in the second column.
get.author.class.by.type = function(network = NULL,
                                    proj.data = NULL,
                                    type = c("network.degree", "network.eigen", "network.hierarchy",
                                             "commit.count", "loc.count", "mail.count", "mail.thread.count",
                                             "issue.count", "issue.comment.count", "issue.commented.in.count",
                                             "issue.created.count"),
                                    issue.type = c("all", "issues", "pull.requests"),
                                    result.limit = NULL,
                                    restrict.classification.to.authors = NULL) {

    logging::logdebug("get.author.class.by.type: starting.")

    type = match.arg(type)
    issue.type = match.arg(issue.type)

    ## Get a reasonable metric name for each classification type
    metric.name = switch(type,
                         "network.degree" = "vertex.degree",
                         "network.eigen" = "eigen.centrality",
                         "network.hierarchy" = "hierarchy",
                         "commit.count" = "commit.count",
                         "loc.count" = "loc.count",
                         "mail.count" = "mail.count",
                         "mail.thread.count" = "mail.thread.count",
                         "issue.count" = "issue.count",
                         "issue.comment.count" = "issue.comment.count",
                         "issue.commented.in.count" = "issue.commented.in.count",
                         "issue.created.count" = "issue.created.count")

    if (CLASSIFICATION.TYPE.TO.CATEGORY[[type]] == "network") {
        if (is.null(network)) {
            logging::logerror("For network-based classifications the parameter 'network' must not be null.")
            stop("For network-based classifications the parameter 'network' must not be null.")
        }

        ## Ensure that the parameter 'network' is of type 'igraph'
        verify.argument.for.parameter(network, "igraph", "get.author.class.by.type")

        if (igraph::vcount(network) == 0) {
            logging::logwarn("The specified network is empty. Returning an empty classification.")
            return(list("core" = create.empty.data.frame(c("author.name", metric.name), c("character", "numeric")),
                        "peripheral" = create.empty.data.frame(c("author.name", metric.name),
                                                               c("character", "numeric"))))
        }
    } else {
        if (is.null(proj.data)) {
            logging::logerror("For count-based classifications the parameter 'proj.data' must not be null.")
            stop("For count-based classifications the parameter 'proj.data' must not be null.")
        }

        ## Ensure that the parameter 'proj.data' is of type 'ProjectData'
        verify.argument.for.parameter(proj.data, "ProjectData", "get.author.class.by.type")
    }


    ## The centrality dataframe has two columns:
    ## 1. a column of author names
    ## 2. a column for the centrality type (which is given by the parameter 'type'),
    ##    e.g. the vertex degree when type == network.degree or the commit count when type == commit.count
    centrality.dataframe = NULL

    if (type == "network.degree") {
        ## Get vertex degrees for all authors
        vertex.degree.vec = igraph::degree(network)

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = names(vertex.degree.vec),
                                          centrality = as.vector(vertex.degree.vec))
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
            eigen.centrality.vec[] = 0
        }

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = names(eigen.centrality.vec),
                                          centrality = as.vector(eigen.centrality.vec))
    } else if (type == "network.hierarchy") {
        hierarchy.base.df = metrics.hierarchy(network)
        hierarchy.calculated = hierarchy.base.df[["deg"]] / hierarchy.base.df[["cc"]]

        ## fix all authors whose clustering coefficient is '0', since their hierarchy value
        ## is 'Inf'. We do not get any complications here because there are no authors with
        ## degree == 0 and a CC > 0 (i.e., the hierarchy value would really be 0). Authors with
        ## a CC == NaN (degree < 2) will stay with their hierarchy value of NaN, accordingly.
        hierarchy.calculated[is.infinite(hierarchy.calculated)] = 0

        ## Construct centrality dataframe
        centrality.dataframe = data.frame(author.name = row.names(hierarchy.base.df),
                                          centrality = hierarchy.calculated)
    } else if (type == "commit.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.commit.count(proj.data)
    } else if (type == "loc.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.loc.count(proj.data)
    } else if (type == "mail.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.mail.count(proj.data)
    } else if (type == "mail.thread.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.mail.thread.count(proj.data)
    }else if (type == "issue.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.issue.count(proj.data, issue.type)
    } else if (type == "issue.comment.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.issue.comment.count(proj.data, issue.type)
    } else if (type == "issue.commented.in.count") {
        ## Construct centrality dataframe
        centrality.dataframe = get.author.issues.commented.in.count(proj.data, issue.type)
    } else { # type == 'issue.created.count'
        ## Construct centrality dataframe
        centrality.dataframe = get.author.issues.created.count(proj.data, issue.type)
    }

    # rename the second column of the centrality data frame to the correct name with respect to the classification type
    names(centrality.dataframe)[2] = metric.name

    ## If the parameter 'restrict.classification.to.authors' is 'NULL', no restriction is made.
    ## Therefore, the restriction is defined as to include all authors (i.e., there is no restriction in the next step)
    if (is.null(restrict.classification.to.authors)) {
        restrict.classification.to.authors = centrality.dataframe[["author.name"]]
    }

    ## Restrict authors as given by the parameter 'restrict.classification.to.authors' before classification
    centrality.dataframe = centrality.dataframe[centrality.dataframe[["author.name"]] %in%
                                                    restrict.classification.to.authors, ]

    ## Retrieve classification results
    classification = get.author.class(centrality.dataframe, metric.name, result.limit = result.limit,
                                      classification.category = CLASSIFICATION.TYPE.TO.CATEGORY[[type]])

    ## Authors who are specified in the parameter 'restrict.classification.to.authors' but were not considered in the
    ## classification will be appended with a value of 'NA' to the classification as peripheral authors:
    ## 1) Prepare a list of author names who are specified in the parameter 'restrict.classification.to.authors' but
    ##    are not part of the classification
    remaining.authors = setdiff(restrict.classification.to.authors, centrality.dataframe[["author.name"]])
    ## 2) Create a dataframe for those authors, all of which getting 'NA' as centrality value
    remaining.authors.df = data.frame(author.name = remaining.authors, temp = rep(NA, length(remaining.authors)))
    ## 3) In preparition to the coming 'rbind', adjust the column names to be the same for both dataframes
    names(remaining.authors.df) = c("author.name", metric.name)
    ## 4) Append the newly created dataframe of authors to the classification as peripheral authors
    classification[["peripheral"]] = rbind(classification[["peripheral"]], remaining.authors.df)

    logging::logdebug("get.author.class.by.type: finished.")
    return(classification)
}

#' Classify authors into "core" and "peripheral" independently for a list of networks or \code{ProjectData} objects.
#'
#' @param network.list a list of networks, each of which containing the authors to classify (parameter is required if
#'                     the parameter \code{type} specifies a network-based classification metric) [default: NULL]
#' @param range.data.list a list of \code{ProjectData} and/or \code{RangeData} objects, each of which containing the
#'                        authors to classify (parameter is required if the parameter \code{type} specifies a
#'                        count-based classification metric) [default: NULL]
#'@param type a character string declaring the classification metric. The classification metric determines which
#'             numerical characteristic of authors is chosen as their centrality value.
#'             The parameter currently supports the following eleven options:
#'             Network-based options/metrics (parameter \code{network} has to be specified):
#'              - "network.degree"
#'              - "network.eigen"
#'              - "network.hierarchy"
#'             Count-based options/metrics (parameter \code{proj.data} has to be specified):
#'              - "commit.count"
#'              - "loc.count"
#'              - "mail.count"
#'              - "mail.thread.count"
#'              - "issue.count"
#'              - "issue.comment.count"
#'              - "issue.commented.in.count"
#'              - "issue.created.count"
#'             [default: "network.degree"]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}. This parameter is ignored for
#'                   non-issue-related classification metrics. [default: "all"]
#' @param restrict.classification.to.authors a vector of author names or a list of vectors of author names.
#'                                           When choosing the first option (i.e., when passing a vector of author
#'                                           names), only authors that are contained within this vector are to be
#'                                           classified. Authors that appear in the vector but are not part of the
#'                                           classification result (i.e., they are not present in the underlying data)
#'                                           will be added to it afterwards (with a centrality value of \code{NA}).
#'                                           Alternatively, a list containing vectors of author names can be passed.
#'                                           The list must have the same length as the list of networks specified in
#'                                           the parameter \code{network.list} or the list of \code{RangeData} objects
#'                                           specified in the parameter \code{range.data.list}, respectively. For each
#'                                           range, the corresponding author group in the list will then be used for
#'                                           restriction instead of using the same group for all ranges. \code{NULL}
#'                                           means that no restriction is made. [default: NULL]
#'
#' @return a list of classification results. Each classification result is a list containing two named list members
#'         \code{core} and \code{peripheral}, each of which holding the authors classified as core or peripheral,
#'         respectively. Both entries in this list (\code{core} and \code{peripheral}) are dataframes containing the
#'         authors' names in the first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.overview = function(network.list = NULL, range.data.list = NULL,
                                     type = c("network.degree", "network.eigen", "network.hierarchy",
                                              "commit.count", "loc.count", "mail.count", "mail.thread.count",
                                              "issue.count", "issue.comment.count", "issue.commented.in.count",
                                              "issue.created.count"),
                                     issue.type = c("all", "issues", "pull.requests"),
                                     restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.overview: starting.")

    type = match.arg(type)
    issue.type = match.arg(issue.type)

    if (CLASSIFICATION.TYPE.TO.CATEGORY[[type]] == "network") {
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

    result = mapply(data.list, restrict.classification.to.authors, SIMPLIFY = FALSE,
                    FUN = function(data, restrict.classification.to.authors) {
        if (startsWith(type, "network")) {
            return(get.author.class.by.type(network = data, type = type,
                                            restrict.classification.to.authors = restrict.classification.to.authors))
        } else {
            return(get.author.class.by.type(proj.data = data, type = type, issue.type = issue.type,
                                            restrict.classification.to.authors = restrict.classification.to.authors))
        }
    })

    logging::logdebug("get.author.class.overview: finished.")
    return(result)
}

#' Get the author turnover values measured as the proportion of authors in the
#' specified range classes which were not active, i.e. do not exist,
#' in the previous range classes (saturation).
#'
#' @param author.class.overview the list of author classifications
#' @param saturation the number of ranges to look in the past [default: 1]
#'
#' @return a data.frame with information about developer turnover
get.class.turnover.overview = function(author.class.overview, saturation = 1) {
    logging::logdebug("get.class.turnover.overview: starting.")

    if (!is.null(names(author.class.overview))) {
        versions = names(author.class.overview)
    } else {
        versions = seq_along(author.class.overview)
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

    ## Get all active authors for each range in the different classes (and both)
    devs = sapply(author.class.overview, function(author.class) {
        return(c(author.class[["core"]][["author.name"]], author.class[["peripheral"]][["author.name"]]))
    })
    devs.core = sapply(author.class.overview, function(author.class) {
        return(author.class[["core"]][["author.name"]])
    })
    devs.peripheral = sapply(author.class.overview, function(author.class) {
        return(author.class[["peripheral"]][["author.name"]])
    })

    ## The author turnover measured as the proportion of devs in the current version
    ## range which were not active in the previous range
    devs.new = devs[[1]]
    devs.core.new = devs.core[[1]]
    devs.peripheral.new = devs.peripheral[[1]]
    turnover.overview[["dev.count"]][1] = length(devs.new)
    turnover.overview[["dev.count.core"]][1] = length(devs.core.new)
    turnover.overview[["dev.count.peripheral"]][1] = length(devs.peripheral.new)
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
        turnover.overview[["turnover"]][i] = sum(!(devs.new %in% devs.old)) / length(devs.new)
        turnover.overview[["turnover.core"]][i] = sum(!(devs.core.new %in% devs.core.old)) / length(devs.core.new)
        turnover.overview[["turnover.peripheral"]][i] = sum(!(devs.peripheral.new %in% devs.peripheral.old)) /
                                                              length(devs.peripheral.new)

        turnover.overview[["dev.count"]][i] = length(devs.new)
        turnover.overview[["dev.count.core"]][i] = length(devs.core.new)
        turnover.overview[["dev.count.peripheral"]][i] = length(devs.peripheral.new)
    }

    logging::logdebug("get.class.turnover.overview: finished.")
    return(turnover.overview)
}

#' Gets a data frame to show the proportion of
#' the authors which are either only active in the current range but not in the previous ones (new) or
#' which are only active in the previous ranges (as specified in saturation) but not in the current one (gone) in
#' relation to all authors of the current and the previous ranges.
#'
#' @param author.class.overview the list of author classifications
#' @param saturation the number of ranges to look in the past [default: 1]
#'
#' @return a data.frame with information about author stability
get.unstable.authors.overview = function(author.class.overview, saturation = 1) {
    logging::logdebug("get.unstable.authors.overview: starting.")

    if (!is.null(names(author.class.overview))) {
        versions = names(author.class.overview)
    } else {
        versions = seq_along(author.class.overview)
    }

    ## Set up the data.frame for the analysis results
    turnover.overview = data.frame(
        versions = versions,
        row.names = 1,
        unstable = 0,
        unstable.core = 0,
        unstable.peripheral = 0
    )

    ## Get all active authors for each range in the different classes (and both)
    devs = sapply(author.class.overview, function(author.class) {
        return(c(author.class[["core"]][["author.name"]], author.class[["peripheral"]][["author.name"]]))
    })
    devs.core = sapply(author.class.overview, function(author.class) {
        return(author.class[["core"]][["author.name"]])
    })
    devs.peripheral = sapply(author.class.overview, function(author.class) {
        return(author.class[["peripheral"]][["author.name"]])
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
        turnover.overview[["unstable"]][i] = (devs.new + devs.gone) / length(devs.union)
        turnover.overview[["unstable.core"]][i] = (devs.core.new + devs.core.gone) / length(devs.core.union)
        turnover.overview[["unstable.peripheral"]][i] =
            (devs.peripheral.new + devs.peripheral.gone) / length(devs.peripheral.union)

    }

    logging::logdebug("get.unstable.authors.overview: finished.")
    return(turnover.overview)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Network-based classification --------------------------------------------

## * Degree-based classification -------------------------------------------

#' Classify authors into "core" and "peripheral" based on the vertex degree of author vertices in the network and
#' return the classification result.
#'
#' The details of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
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

#' Classify authors into "core" and "peripheral" based on the eigenvector-centrality of author vertices in the network
#' and return the classification result.
#'
#' The details of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
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

#' Classify authors into "core" and "peripheral" based on the hierarchy value of author vertices in the network and
#' return the classification result.
#'
#' The details of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param network the network containing the authors to classify
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.network.hierarchy = function(network, result.limit = NULL,
                                              restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.network.hierarchy: starting.")

    result = get.author.class.by.type(network = network, type = "network.hierarchy", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.network.hierarchy: finished.")
    return(result)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Count-based classification ---------------------------------------------

## * Commit-based classification --------------------------------------------


#' Classify authors into "core" and "peripheral" based on authors' commit-counts and return the classification result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' commit data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.commit.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL) {

    logging::logdebug("get.author.class.commit.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "commit.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.commit.count: finished.")
    return(result)
}


## * LOC-based classification ----------------------------------------------

#' Classify authors into "core" and "peripheral" based on authors' lines of code (LOC) and return the classification
#' result. LOC are calculated independently for each author by looking at the sum of added and deleted lines of code in
#' all their commits.
#'
#' The details of the classification algorithm is explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' commit data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.loc.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.loc.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "loc.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.loc.count: finished.")
    return(result)
}


## * Mail-based classification --------------------------------------------

#' Classify authors into "core" and "peripheral" based on authors' mail-counts and return the classification result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' mail data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.mail.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.mail.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "mail.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.mail.count: finished.")
    return(result)
}

#' Classify authors into "core" and "peripheral" based on authors' mail-thread counts and return the
#' classification result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' mail data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.mail.thread.count = function(proj.data, result.limit = NULL,
                                              restrict.classification.to.authors = NULL) {
    logging::logdebug("get.author.class.mail.thread.count: starting.")

    result = get.author.class.by.type(proj.data = proj.data, type = "mail.thread.count", result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.mail.thread.count: finished.")
    return(result)
}

## * Issue-based classification --------------------------------------------

#' Classify authors into "core" and "peripheral" based on authors' issue-counts and return the classification result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' issue data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}.
#'                   [default: "all"]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.issue.count = function(proj.data, result.limit = NULL, restrict.classification.to.authors = NULL,
                                        issue.type = c("all", "issues", "pull.requests")) {
    logging::logdebug("get.author.class.issue.count: starting.")

    issue.type = match.arg(issue.type)

    result = get.author.class.by.type(proj.data = proj.data, type = "issue.count", issue.type = issue.type,
                                      result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.issue.count: finished.")
    return(result)
}

#' Classify authors into "core" and "peripheral" based on authors' issue-comment counts and return the classification
#' result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' issue data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}.
#'                   [default: "all"]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.issue.comment.count = function(proj.data, result.limit = NULL,
                                                restrict.classification.to.authors = NULL,
                                                issue.type = c("all", "issues", "pull.requests")) {
    logging::logdebug("get.author.class.issue.comment.count: starting.")

    issue.type = match.arg(issue.type)

    result = get.author.class.by.type(proj.data = proj.data, type = "issue.comment.count", issue.type = issue.type,
                                      result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.issue.comment.count: finished.")
    return(result)
}

#' Classify authors into "core" and "peripheral" based on authors' issue-commented-in counts and return the
#' classification result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' issue data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}.
#'                   [default: "all"]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.issue.commented.in.count = function(proj.data, result.limit = NULL,
                                                restrict.classification.to.authors = NULL,
                                                issue.type = c("all", "issues", "pull.requests")) {
    logging::logdebug("get.author.class.issue.commented.in.count: starting.")

    issue.type = match.arg(issue.type)

    result = get.author.class.by.type(proj.data = proj.data, type = "issue.commented.in.count",
                                      issue.type = issue.type,
                                      result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.issue.commented.in.count: finished.")
    return(result)
}

#' Classify authors into "core" and "peripheral" based on authors' issue-created counts and return the classification
#' result.
#'
#' The details of the classification algorithm are explained in the documentation of \code{get.author.class.by.type}.
#'
#' @param proj.data the \code{ProjectData} containing the authors' issue data
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param restrict.classification.to.authors a vector of author names. Only authors that are contained within this
#'                                           vector are to be classified. Authors that appear in the vector but are not
#'                                           part of the classification result (i.e., they are not present in the
#'                                           underlying data) will be added to it afterwards (with a centrality value
#'                                           of \code{NA}). \code{NULL} means that no restriction is made.
#'                                           [default: NULL]
#' @param issue.type which issue type to consider for count-based metrics using issues
#'                   (see \code{preprocess.issue.data}). One of \code{"issues"},
#'                   \code{"pull.requests"} or \code{"all"}.
#'                   [default: "all"]
#'
#' @return the classification result, that is, a list containing two named list members \code{core} and
#'         \code{peripheral}, each of which holding the authors classified as core or peripheral, respectively. Both
#'         entries in this list (\code{core} and \code{peripheral) are dataframes containing the authors' names in the
#'         first column and their centrality values in the second column.
#'
#' @seealso get.author.class.by.type
get.author.class.issue.created.count = function(proj.data, result.limit = NULL,
                                                restrict.classification.to.authors = NULL,
                                                issue.type = c("all", "issues", "pull.requests")) {
    logging::logdebug("get.author.class.issue.created.count: starting.")

    issue.type = match.arg(issue.type)

    result = get.author.class.by.type(proj.data = proj.data, type = "issue.created.count", issue.type = issue.type,
                                      result.limit = result.limit,
                                      restrict.classification.to.authors = restrict.classification.to.authors)

    logging::logdebug("get.author.class.issue.created.count: finished.")
    return(result)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Role stability ----------------------------------------------

#' Get a data frame with the authors and their occurence count in the specified class for
#' the specified author classification list.
#'
#' @param author.class.overview the list of classifications in which to find the number
#'                              of occurences
#' @param class the type of class to look for. Can either be \code{"both"}, \code{"core},
#'              or \code{"peripheral}. [default: "both"]
#'
#' @return a data frame with the authors and their occurence count in the specified class
get.recurring.authors = function(author.class.overview, class = c("both", "core", "peripheral")) {
    logging::logdebug("get.recurring.authors: starting.")

    class = match.arg(class)

    authors = c()
    freq = c()

    ## Iterate over each version development range
    for (i in seq_along(author.class.overview)) {

        ## skip range in case no classification is available
        if (all(is.na(author.class.overview[[i]]))) {
            next
        }

        if (class == "both") {

            ## skip range in case no classification is available
            if (nrow(author.class.overview[[i]][["core"]]) == 0
               && nrow(author.class.overview[[i]][["peripheral"]]) == 0) {
                next
            }

            author.class.authors = c(author.class.overview[[i]][["core"]][["author.name"]],
                                     author.class.overview[[i]][["peripheral"]][["author.name"]])
        } else {

            ## skip range in case no classification for the given class is available
            if (nrow(author.class.overview[[i]][[class]]) == 0) {
                next
            }

            author.class.authors = author.class.overview[[i]][[class]][["author.name"]]
        }

        ## Iterate over each author in the specified class and increase his occurence count
        for (j in seq_along(author.class.overview)) {
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
    data = data[order(data[["freq"]], decreasing = TRUE), ]

    logging::logdebug("get.recurring.authors: finished.")
    return(data)
}

#' Retrieves all authors which will be classified as core by the specified
#' classification in more than a certain number of ranges.
#'
#' @param author.class the author classification to use for finding the
#'                     longterm core-authors [default: NULL]
#'
#' @return the names of the longterm core-authors
#'
#' @seealso LONGTERM.CORE.THRESHOLD
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
    longterm.core = recurring.authors[recurring.authors[["freq"]] >= longterm.threshold, ][["author.name"]]

    logging::logdebug("get.longterm.core.authors: finished.")
    return(longterm.core)
}

#' Get a markov chain object representing the role stability of the
#' specified classification overview.
#'
#' @param author.class.overview the list of author classifications to
#'                              use for the role stability calculation
#'
#' @return a markov chain object representing the role stability
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

    ## Run through each range author classification
    for (i in 2:length(author.class.overview)) {

        ## Get core and peripheral devs from previous version
        class.prev = author.class.overview[[i-1]]
        dev.prev.core = class.prev[["core"]][["author.name"]]
        dev.prev.peripheral = class.prev[["peripheral"]][["author.name"]]
        dev.prev = c(dev.prev.core, dev.prev.peripheral)
        dev.prev.absent = dev.current.absent

        ## Get core and peripheral devs from current version
        class.current = author.class.overview[[i]]
        dev.current.core = class.current[["core"]][["author.name"]]
        dev.current.peripheral = class.current[["peripheral"]][["author.name"]]
        dev.current = c(dev.current.core, dev.current.peripheral)
        dev.current.absent = unique(c(dev.prev.absent[!(dev.prev.absent %in% dev.current)],
                                      dev.prev[!(dev.prev %in% dev.current)]))

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

#' Calculates the cohen's kappa to measure the agreement of the specified author classifications.
#'
#' @param author.classification.list the first author classification list to compare
#' @param other.author.classification.list the second author classification list to compare
#'
#' @return the cohens kappa coefficient describing the agreement of the two classification lists
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
            sum(author.class[["core"]][["author.name"]] %in% author.class.compare[["core"]][["author.name"]])

        num.core.peripheral = num.core.peripheral +
            sum(author.class[["core"]][["author.name"]] %in%
                    author.class.compare[["peripheral"]][["author.name"]])

        num.peripheral.core = num.peripheral.core +
            sum(author.class[["peripheral"]][["author.name"]] %in%
                    author.class.compare[["core"]][["author.name"]])

        num.peripheral.peripheral = num.peripheral.peripheral +
            sum(author.class[["peripheral"]][["author.name"]] %in%
                    author.class.compare[["peripheral"]][["author.name"]])
    }

    num.sum = num.core.core + num.peripheral.peripheral + num.core.peripheral + num.peripheral.core

    po = (num.core.core + num.peripheral.peripheral) / num.sum
    pe.core = ((num.core.core + num.core.peripheral) / num.sum) * ((num.core.core + num.peripheral.core) / num.sum)
    pe.peripheral = ((num.peripheral.peripheral + num.core.peripheral) / num.sum) *
                    ((num.peripheral.peripheral + num.peripheral.core) / num.sum)
    pe = pe.core + pe.peripheral

    kappa = (po - pe) / (1 - pe)

    logging::logdebug("calculate.cohens.kappa: finished.")
    return(kappa)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Generic helper functions ------------------------------------------------


#' Classify authors into "core" and "peripheral".
#'
#' Core authors are those who are responsible for a given percentage of the work load with a default threshold set at
#' 80% according to Ref:
#' Terceiro A, Rios LR, Chavez C (2010) An empirical study on the structural complexity introduced by core and
#' peripheral developers in free software projects.
#'
#' The actual threshold ratio (i.e., the exemplary 80 %) is set by the constant \code{CORE.THRESHOLD}. See also the
#' function \code{get.threshold}.
#'
#' @param author.data.frame a dataframe containing at least two columns. The column called "author.name" should store
#'                          the author's names, the other one, whose name must be specified in the parameter
#'                          \code{calc.base.name}, should store the authors' centrality values.
#' @param calc.base.name the name of the classification column of the dataframe specified in the parameter
#'                       \code{author.data.frame}
#' @param result.limit the maximum number of authors contained in the classification result. Only the top
#'                     \code{result.limit} authors of the classification stack will be contained within the returned
#'                     classification result. \code{NULL} means that all authors will be returned. [default: NULL]
#' @param classification.category the category of classification metric used for the classification of authors.
#'                                These metrics can either be count- or network-based metrics. [default: NULL]
#'
#' @return the classification result, that is, a list containing two named list members "core" and "peripheral", each
#'         of which containing the authors classified as core or peripheral, respectively. Both entries in this list
#'         ("core" and "peripheral") are dataframes containing the author's names in the first column and their
#'         centrality values in the second column.
get.author.class = function(author.data.frame, calc.base.name, result.limit = NULL,
                            classification.category = NULL) {
    logging::logdebug("get.author.class: starting.")

    if (is.null(classification.category)) {
        logging::logerror("A classification metric type ('count' or 'network') has to be specified! Stopping...")
        stop("Stopped due to missing classification metric type.")
    }

    ## Make sure that we have enough data for a classification
    if (ncol(author.data.frame) < 2) {
        author.data.frame = create.empty.data.frame(
            columns = c("author.name", calc.base.name),
            data.types = list("character", "numeric")
        )
    }

    ## Make sure the provided data is ordered correctly by the calculation base
    author.data = author.data.frame[order(author.data.frame[[calc.base.name]], decreasing = TRUE), , drop = FALSE]

    ## Remove rows with invalid calculation base values
    if (any(is.na(author.data[[calc.base.name]]))) {
        logging::logwarn("Some authors' activity indicator (%s) is NA. Setting the activity to 0...", calc.base.name)
        author.data[is.na(author.data[[calc.base.name]]), calc.base.name] = 0
    }

    ## Get the threshold depending on all calculation base values
    author.class.threshold = get.threshold(author.data[[calc.base.name]], classification.category)

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
            as.logical(sapply(author.cumsum, function(x) isTRUE(all.equal(x, author.class.threshold))))
    )

    # Suppress the warnings since the special case of 'min' returning 'Inf' is handled in the following 'if' statement
    min = suppressWarnings(min(buffer.value))
    if (is.infinite(min)) {
        author.class.threshold.idx = nrow(author.data)
    } else {
        author.class.threshold.idx = min
    }

    ## classify authors according to threshold
    core.classification = rep(FALSE, nrow(author.data))
    core.classification[seq_len(author.class.threshold.idx)] = TRUE

    ## With no activity/collaboration occurring, all authors are classified as peripheral.
    if (author.class.threshold == 0 && length(author.data[["author.name"]]) != 1) {
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
#' the sum over all centrality values and multiplying this sum with \code{CORE.THRESHOLD} when using count-based
#' metrics to calculate the centrality. When using network-based metrics, the threshold is determined by the
#' \code{CORE.THRESHOLD} quantile of the centrality values.
#'
#' Example: Consider three authors A, B and C that have centrality values of 1, 2 and 2, respectively. By taking the
#' sum and multiplying with \code{CORE.THRESHOLD} which, for instance, could be 0.8, we end up with a classification
#' threshold of (1 + 2 + 2) * 0.8 = 4. The smallest group of authors that together has a centrality sum of 4 or more
#' could now be considered core (here, B and C with 2 commits each). Accordingly, when using a network-based approach,
#' the threshold would now be 2 as this is the 80% quantile.
#'
#' @param centrality.list a list containing centrality values
#' @param classification.category the category of centrality metric used as there are two ways to determine
#'                                the threshold based on the type of centrality function. The two types
#'                                are network-based and count-based functions. [default: "network"]
#'
#' @return the threshold used within classifications
get.threshold = function(centrality.list, classification.category = c("network", "count")) {
    logging::logdebug("get.threshold: starting.")

    category = match.arg(classification.category)

    if (category == "network") {
        ## Calculate the quantile specified by CORE.THRESHOLD
        data.threshold = quantile(centrality.list, CORE.THRESHOLD)
    } else { # category == 'count'
        ## Calculate the sum of the provided data as base for the threshold calculation
        data.threshold.base = sum(centrality.list)

        ## Check which authors can be treated as core based on the data
        data.threshold = CORE.THRESHOLD * data.threshold.base
    }

    logging::logdebug("get.threshold: finished.")
    return(data.threshold)
}
