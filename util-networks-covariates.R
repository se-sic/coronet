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
## Copyright 2017 by Felix Prasse <prassefe@fim.uni-passau.de>
## Copyright 2018 by Claus Hunsen <hunsen@fim.uni-passau.de>
## Copyright 2018 by Thomas Bock <bockthom@fim.uni-passau.de>
## Copyright 2018 by Klara Schlüter <schluete@fim.uni-passau.de>
## All Rights Reserved.

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Libraries ---------------------------------------------------------------

requireNamespace("logging") # for logging
requireNamespace("parallel") # for parallel computation
requireNamespace("plyr") # for ldply function
requireNamespace("igraph") # networks


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Utility functions -------------------------------------------------------

#' Utility function to compute vertex attributes for a list of network
#'
#' Important: This function only works for lists of networks which have timestamps used in their range names.
#'
#' This method is a wrapper combining the steps of splitting the project data and calculating the attribute.
#'
#' Important: If a vertex attribute can have multiple values per vertex, the \code{compute.attr} function
#' has to wrap all attribute vectors of a single node into a list. In that case, also the
#' \code{default.value} needs to be put into a list. By that, this function can differentiate between
#' attributes having multiple values and attributes having, at most, one value.
#'
#' @param list.of.networks The list of networks to add vertex attributes to
#' @param project.data The entire project data
#' @param attr.name The name of the attribute to add
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                     with the names being the name of the vertex.
#'
#' @return A list of networks with the added attribute
split.and.add.vertex.attribute = function(list.of.networks, project.data, attr.name,
                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                "project.cumulative", "project.all.ranges",
                                                                "complete"),
                                          default.value, compute.attr, list.attributes = FALSE) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    nets.with.attr = add.vertex.attribute(net.to.range.list, attr.name, default.value, compute.attr, list.attributes)
    return(nets.with.attr)
}

#' Utility function to compute vertex attributes for a list of network-to-range tuples.
#'
#' Important: If a vertex attribute can have multiple values per vertex, the \code{compute.attr} function
#' has to wrap all attribute vectors of a single node into a list. In that case, also the
#' \code{default.value} needs to be put into a list. By that, this function can differentiate between
#' attributes having multiple values and attributes having, at most, one value.
#'
#' @param net.to.range.list A list containing tuples with networks and corresponding range data.
#' @param attr.name The name of the attribute to add
#' @param default.value The default value to add if a vertex has no matching value
#' @param compute.attr The function to compute the attribute to add. Must return a named list
#'                     with the names being the name of the vertex.
#'
#' @return A list of networks with the added attribute
add.vertex.attribute = function(net.to.range.list, attr.name, default.value, compute.attr, list.attributes = FALSE) {

    nets.with.attr = mapply(
        names(net.to.range.list), net.to.range.list,
        SIMPLIFY = FALSE, FUN = function(range, net.to.range) {

            current.network = net.to.range[["network"]]
            range.data = net.to.range[["data"]]

            attr.df = compute.attr(range, range.data, current.network)

            get.or.default = function(name, data, default) {
                if (name %in% names(data)) {
                    return(data[[name]])
                } else {
                    return(default)
                }
            }

            attributes = lapply(igraph::V(current.network)$name,
                                function(x) get.or.default(x, attr.df, default.value))

            ## simplify the list of attributes to a vector if all its elements are just vectors (not lists)
            if (length(attributes) > 0 && !any(lapply(attributes, is.list))) {
                attributes = unlist(attributes)
            }
            ## otherwise, the list of attributes contains lists, so we can only remove the outermost list
            else if (!list.attributes) {
                attributes = unlist(attributes, recursive = FALSE)
            }

            net.with.attr = igraph::set.vertex.attribute(current.network, attr.name, value = attributes)

            return(net.with.attr)
        }
    )

    return (nets.with.attr)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Author network functions ------------------------------------------------

## * Commit count ----------------------------------------------------------

#' Add commit-count attribute based on author name
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author = function(list.of.networks, project.data, name = "commit.count",
                                                    aggregation.level = c("range", "cumulative", "all.ranges",
                                                                          "project.cumulative", "project.all.ranges",
                                                                          "complete"),
                                                    default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.author.commit.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on author name where author is not committer
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.author.not.committer"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author.not.committer = function(list.of.networks, project.data,
                                                                  name = "commit.count.author.not.committer",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.not.author.commit.count, "author.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on comitter name
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer = function(list.of.networks, project.data, name = "commit.count.committer",
                                                       aggregation.level = c("range", "cumulative", "all.ranges",
                                                                             "project.cumulative", "project.all.ranges",
                                                                             "complete"),
                                                       default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on comitter name where committer is not author
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.not.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.not.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.not.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.not.author.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on committer name where the committer equals the author.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.and.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.and.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.and.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.and.author.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on commits where the person represented by the vertex is the committer
#' or the author.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.or.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.or.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.or.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0L) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.or.author.commit.count, "name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based using \code{commit.count.method}
#'
#' Note: This is a helper function for all other functions adding a commit-count-related
#' vertex attribute.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0L]
#' @param commit.count.method The method reference for counting the commits
#' @param name.column The name of the author or committer column
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.helper = function(list.of.networks, project.data, name = "commit.count",
                                             aggregation.level = c("range", "cumulative", "all.ranges",
                                                                   "project.cumulative", "project.all.ranges",
                                                                   "complete"),
                                             default.value = 0L, commit.count.method, name.column) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            commit.count.df = commit.count.method(range.data)[c(name.column, "freq")]

            if (!is.data.frame(commit.count.df)) {
                return(list())
            }

            commit.count.list = structure(commit.count.df[["freq"]], names = commit.count.df[[name.column]])

            return(commit.count.list)
        }
    )

    return(nets.with.attr)
}

## * Meta-data -------------------------------------------------------------

#' Add author email attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "author.email"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.email = function(list.of.networks, project.data, name = "author.email", default.value = NA) {
    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, "complete", default.value,
        function(range, range.data, net) {
            authors = range.data$get.authors()
            author.to.mail = structure(names = authors[["author.name"]],
                                       authors[["author.email"]])

            return(author.to.mail)
        }
    )

    return(nets.with.attr)
}

#' Add unique artifact count attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "artifact.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.count = function(list.of.networks, project.data, name = "artifact.count",
                                               aggregation.level = c("range", "cumulative", "all.ranges",
                                                                     "project.cumulative", "project.all.ranges",
                                                                     "complete"),
                                               default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            ## FIXME we need to implement this also for the other kinds of artifacts
            ## (get.author2artifact() gets only information on source-code artifacts!)
            lapply(range.data$get.author2artifact("commits"), function(x) {
                length(unique(x[["artifact"]]))
            })
        }
    )

    return(nets.with.attr)
}

## * Activity --------------------------------------------------------------

#' Add first activity attribute.
#'
#' @param list.of.networks The network list.
#' @param project.data The project data.
#' @param activity.types The kinds of activity to use as basis: One or more of \code{mails}, \code{commits} and
#'                       \code{issues}. [default: c("mails", "commits", "issues")]
#' @param name The attribute name to add. [default: "first.activity"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "complete"]
#' @param default.value The default value to add if a vertex has no matching value. [default: NA].
#' @param take.first.over.all.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A list of networks with the added attribute.
add.vertex.attribute.first.activity = function(list.of.networks, project.data,
                                               activity.types = c("mails", "commits", "issues"),
                                               name = "first.activity",
                                               aggregation.level = c("range", "cumulative", "all.ranges",
                                                                     "project.cumulative", "project.all.ranges",
                                                                     "complete"),
                                               default.value = NA,
                                               take.first.over.all.activity.types = FALSE) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "complete")
    compute.attr = function(range, range.data, net) {
        data = get.first.activity.data(range.data, activity.types, take.first.over.all.activity.types)
        return(data)
    }

    nets.with.attr = split.and.add.vertex.attribute(list.of.networks, project.data, name, aggregation.level, default.value,
                                                    compute.attr, list.attributes = TRUE)
    return(nets.with.attr)
}

#' Add active-ranges attribute
#'
#' Notice: One vertex can be active in multiple ranges, therefore there may be a vector of ranges as
#' active-ranges attribute.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "active.ranges"]
#' @param default.value The default value to add if a vertex has no matching value [default: list()]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.active.ranges = function(list.of.networks, project.data, name = "active.ranges",
                                              default.value = list()) {
    net.to.range.list = split.data.by.networks(list.of.networks, project.data, "range")

    range.to.authors = lapply(
        net.to.range.list,
        function(net.to.range) {
            ## FIXME support data-source-specific method AND all-sources method
            ## (we only use 'commits' source here)
            unique(net.to.range[["data"]]$get.commits()[["author.name"]])
        }
    )

    author.names = unique(unlist(range.to.authors))

    active.ranges = lapply(
        author.names,
        function(author) {
            filter.by.author = Filter(function(range) author %in% range,
                                      range.to.authors)

            active.ranges.of.author = names(filter.by.author)

            ## vector for one vertex needs to be wrapped into a list due to multiple values per vertex
            return(list(active.ranges.of.author))
        }
    )

    names(active.ranges) = author.names

    ## default value for one vertex needs to be wrapped into a list due to multiple values per vertex
    list.default.value = list(default.value)

    nets.with.attr = add.vertex.attribute(
        net.to.range.list, name, list.default.value,
        function(range, range.data, net) {
            active.ranges
        }
    )

    return(nets.with.attr)
}

## * Role ------------------------------------------------------------------

#' Add author role attribute, while using the classification method
#' \code{get.author.class.by.type} to provide the attributes
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "author.role"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param type The type of author classification. One of \code{"network.degree"},
#'             \code{"network.eigen"}, \code{"commit.count"}, and \code{"loc.count"}.
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role.simple = function(list.of.networks, project.data, name = "author.role",
                                                   aggregation.level = c("range", "cumulative", "all.ranges",
                                                                         "project.cumulative", "project.all.ranges",
                                                                         "complete"),
                                                   type = c("network.degree", "network.eigen",
                                                            "commit.count", "loc.count"),
                                                   default.value = NA) {

    classification.function = function(network, range.data) {
        classification = get.author.class.by.type(network, range.data, type)
        return(classification)
    }

    nets.with.attr = add.vertex.attribute.author.role.function(
        list.of.networks, project.data, classification.function, name,
        aggregation.level, default.value
    )

    return(nets.with.attr)
}

#' Add author role attribute using a specified classification function
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param classification.result A name of an author-classification function. Must return a tuple
#'                              of two lists containing the authors named "core" and "peripheral".
#'                              See the functions \code{get.author.class.*}.
#' @param name The attribute name to add [default: "author.role"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role.function = function(list.of.networks, project.data, classification.function,
                                                     name = "author.role",
                                                     aggregation.level = c("range", "cumulative", "all.ranges",
                                                                          "project.cumulative", "project.all.ranges",
                                                                          "complete"),
                                                     default.value = NA) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    classification.results = lapply(
        net.to.range.list,
        function(net.to.range) {
            author.class = classification.function(net.to.range[["network"]], net.to.range[["data"]])
            return(author.class)
        }
    )

    nets.with.attr = add.vertex.attribute.author.role(
        list.of.networks, classification.results, name, default.value
    )

    return(nets.with.attr)
}

#' Add author role attribute by classification results
#'
#' Important: The lists \code{list.of.networks} and \code{classification.results} needs to be of the same
#' length for this to work properly.
#'
#' @param list.of.networks The network list
#' @param classification.results A list of author-classification results. Each item needs to contain
#'                               a tuple of two lists containing the authors named "core" and "peripheral"
#'                               (see the functions \code{get.author.class.*}).
#'                               The list needs to be of the same length as \code{list.of.networks} and use
#'                               the same names.
#' @param name The attribute name to add [default: "author.role"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.author.role = function(list.of.networks, classification.results,
                                            name = "author.role", default.value = NA) {

    if (length(list.of.networks) != length(classification.results) ||
        !identical(names(list.of.networks), names(classification.results))) {
        logging::logwarn(paste("Adding author-classification vertex attribute: The classification",
                               "results do not match with the list of networks. Please see the",
                               "documentation of the function 'add.vertex.attribute.author.role'."))
    }

    ## provide a list whose elements only contain the network as no data is needed here
    net.list = lapply(list.of.networks, function(net) {
        n = list()
        n[["network"]] = net
        return(n)
    })

    nets.with.attr = add.vertex.attribute(
        net.list, name, default.value,
        function(range, range.data, net) {
            classification = classification.results[[range]]
            author.class = plyr::ldply(classification, .id = NA)

            author.to.role = structure(author.class[[".id"]], names = author.class[["author.name"]])
            return(author.to.role)
        }
    )

    return(nets.with.attr)
}


## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Artifact network functions ----------------------------------------------

## * Change count ----------------------------------------------------------

#' Add the count of unique editors (i.e., authors) that worked on an artifact
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "editor.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.editor.count = function(list.of.networks, project.data, name = "editor.count",
                                                      aggregation.level = c("range", "cumulative", "all.ranges",
                                                                            "project.cumulative", "project.all.ranges",
                                                                            "complete"),
                                                      default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            lapply(range.data$get.artifact2author("commits"), function(x) {
                length(unique(x[["author.name"]]))
            })
        }
    )

    return(nets.with.attr)
}

#' Add the amount of times the artifact was changed
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "change.count"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.change.count = function(list.of.networks, project.data, name = "change.count",
                                                      aggregation.level = c("range", "cumulative", "all.ranges",
                                                                            "project.cumulative", "project.all.ranges",
                                                                            "complete"),
                                                      default.value = 0) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            artifact.to.commit = get.key.to.value.from.df(range.data$get.commits.filtered.empty(), "artifact", "hash")
            artifact.change.count = lapply(artifact.to.commit, function(x) {
                length(unique(x[["hash"]]))
            })

            return(artifact.change.count)
        }
    )

    return(nets.with.attr)
}

## * Activity --------------------------------------------------------------

#' Add the first occurrence of the artifact
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "first.occurrence"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "complete"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.artifact.first.occurrence = function(list.of.networks, project.data, name = "first.occurrence",
                                                          aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                "project.cumulative", "project.all.ranges",
                                                                                "complete"),
                                                          default.value = NA) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "complete")

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            artifact.to.dates = get.key.to.value.from.df(range.data$get.commits.filtered.empty(), "artifact", "date")
            artifact.to.first = lapply(artifact.to.dates, function(a) {
                min(a[["date"]])
            })
            return(artifact.to.first)
        }
    )
    return(nets.with.attr)
}

## / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /
## Helper ------------------------------------------------------------------

#' Helper function for first activity: computing first activity information per person and activity type and returning it as a dataframe.
#'
#' @param activity.types The activity types to compute information for. They determine the columns of the returned data frame.
#'                       [default: c("mails", "commits", "issues")]
#' @param range.data The data to base the computation on.
#' @param take.first.over.all.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A data frame with rows named with persons and columns named with activity types, containing the time of the corresponding
#'         first activity as POSIXct.
get.first.activity.data = function(range.data, activity.types = c("commits", "mails", "issues"),
                                   take.first.over.all.activity.types = FALSE) {

    ## parse given activity types to functions
    parsed.activity.types = match.arg.or.default(activity.types, several.ok = TRUE)

    ## get data for each activity type and extract minimal date for each author
    activity.by.type = lapply(parsed.activity.types, function(type, type.function) {
        ## compute minima
        minima.per.person = lapply(range.data$get.author2artifact(type), function(x) {
            ## get first date
            m = list(min(x[["date"]]))
            ## add activity type as name to the list
            names(m) = type
            return(m)
        })
        return(minima.per.person)
    })

    ## accumulate/fold lists by adding all values of each list to an intermediate list (start with first list)
    result = Reduce(function(x, y) {
        ## get names from both lists
        keys = union(names(x), names(y))
        ## get current step (i.e., "loop" index)
        depth = max(lengths(x), lengths(y))
        ## get the list of names to use on sublists
        names.list = activity.types[seq_len(depth + 1)]

        ## actually combine values for each key
        result = parallel::mclapply(keys, function(key) {
            ## get value from intermediate list (pre-fill with right length if not existing)
            value.x = if (is.null(x[[key]])) rep(list(NA), times = depth) else x[[key]]
            ## get value from current list (use NA as default if not existing)
            value.y = if (is.null(y[[key]])) NA else y[[key]]

            ## combine values and name them apropriately
            combined.values = c(value.x, value.y)
            names(combined.values) = names.list

            ## convert to POSIXct object again (which can be lost if is.null(x[[key]]) == TRUE)
            combined.values = lapply(combined.values, get.date.from.unix.timestamp)

            return(combined.values)
        })
        names(result) = keys
        return(result)

    }, activity.by.type)

    ## find minima over all activity types if configured
    if (take.first.over.all.activity.types) {
        result = parallel::mclapply(result, function(item.list) {
            min.value = min(unlist(item.list), na.rm = TRUE)
            ## convert to POSIXct object again (gets lost by unlist)
            min.value = get.date.from.unix.timestamp(min.value)
            return(list(all.activities = min.value))
        })
    }

    return(result)
}
