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
## Copyright 2018 by Jakob Kronawitter <kronawij@fim.uni-passau.de>
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
#' has to wrap all attribute vectors of a single vertex into a list. In that case, also the
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
#' has to wrap all attribute vectors of a single vertex into a list. In that case, also the
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
            lapply(range.data$group.artifacts.by.data.column("commits", "author.name"),
                   function(x) {
                       length(unique(x[["artifact"]]))
                   }
            )
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
    parsed.activity.types = match.arg.or.default(activity.types, several.ok = TRUE)

    compute.attr = function(range, range.data, net) {
        data = get.first.activity.data(range.data, parsed.activity.types, take.first.over.all.activity.types)
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
#' @param activity.types The kinds of activity to use as basis: One or more of \code{mails}, \code{commits} and
#'                       \code{issues}. [default: c("mails", "commits", "issues")]
#' @param default.value The default value to add if a vertex has no matching value [default: list()]
#' @param take.first.over.all.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.active.ranges = function(list.of.networks, project.data, name = "active.ranges",
                                              activity.types = c("mails", "commits", "issues"),
                                              default.value = NA,
                                              take.first.over.all.activity.types = FALSE) {
    net.to.range.list = split.data.by.networks(list.of.networks, project.data, "range")
    parsed.activity.types = match.arg.or.default(activity.types, several.ok = TRUE)

    compute.attr = function(range, range.data, net) {
        data = get.active.ranges.data(parsed.activity.types, net.to.range.list)
        return(data)
    }

    ## default value for one vertex needs to be wrapped into a list due to multiple values per vertex
    list.default.value = list(default.value)

    nets.with.attr = add.vertex.attribute(net.to.range.list, name, list.default.value, compute.attr)
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
            lapply(range.data$group.authors.by.data.column("commits", "artifact"),
                   function(x) {
                       length(unique(x[["author.name"]]))
                   }
            )
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
            artifact.to.commit = get.key.to.value.from.df(range.data$get.commits.filtered(), "artifact", "hash")
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
            artifact.to.dates = get.key.to.value.from.df(range.data$get.commits.filtered(), "artifact", "date")
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

#' Helper function for first activity: computes first activity information per person and activity type.
#'
#' @param activity.types The activity types to compute information for. [default: c("mails", "commits", "issues")]
#' @param range.data The data to base the computation on.
#' @param take.first.over.all.activity.types Flag indicating that one value, computed over all given
#'                                           \code{activity.types} is of interest (instead of one value per type).
#'                                           [default: FALSE]
#'
#' @return A list with authors as keys and a POSIXct list in the following format as value:
#'         - if \code{take.first.over.all.activity.types}, a one-element list named 'all.activities'
#'         - otherwise, a list with length \code{length(activity.types)} and corresponding names
get.first.activity.data = function(range.data, activity.types = c("commits", "mails", "issues"),
                                   take.first.over.all.activity.types = FALSE) {

    ## get data for each activity type and extract minimal date for each author in each type,
    ## resulting in a list of activity types with each item containing a list of authors
    ## mapped to their first activity for the current activity type; for example:
    ##    list(
    ##        commits = list(authorA = list(commits = 1), authorB = list(commits = 0)),
    ##        mails   = list(authorB = list(mails = 2), authorC = list(mails = 3)),
    ##        issues  = list(authorA = list(issues = 2), authorD = list(issues = 2))
    ##    )
    activity.by.type = parallel::mclapply(activity.types, function(type) {
        ## compute minima
        minima.per.person = lapply(
            range.data$group.artifacts.by.data.column(type, "author.name"),
            function(x) {
                ## get first date
                m = list(min(x[["date"]]))
                ## add activity type as name to the list
                names(m) = type
                return(m)
            }
        )
        return(minima.per.person)
    })

    ## accumulate/fold lists 'activity.by.type' by adding all values of each list
    ## to an intermediate list (start with first list); for example:
    ##     list(
    ##        authorA = list(commits =  1, mails = NA, issues =  2),
    ##        authorB = list(commits =  0, mails =  2, issues = NA),
    ##        authorC = list(commits = NA, mails =  3, issues = NA),
    ##        authorD = list(commits = NA, mails = NA, issues =  2)
    ##     )
    result = Reduce(function(x, y) {
        ## get names from both lists, representing author names for the current activity types
        keys = union(names(x), names(y))
        ## get current step (i.e., "loop" index or, rather, "how many lists have we merged already?")
        depth = max(lengths(x), lengths(y))
        ## get the list of activity types to use on sublists (after merging next list (i.e., y)!)
        names.list = activity.types[seq_len(depth + 1)]

        ## actually combine values for each key
        result = parallel::mclapply(keys, function(key) {
            ## get value from intermediate list (pre-fill with right length if not existing)
            value.x = if (is.null(x[[key]])) rep(list(NA), times = depth) else x[[key]]
            ## get value from current list (use NA as default if not existing)
            value.y = if (is.null(y[[key]])) NA else y[[key]]

            ## combine values and name them appropriately
            combined.values = c(value.x, value.y)
            names(combined.values) = names.list

            ## convert to POSIXct object again (which can be lost if is.null(x[[key]]) == TRUE)
            combined.values = lapply(combined.values, get.date.from.unix.timestamp)

            return(combined.values)
        })
        names(result) = keys
        return(result)

    }, activity.by.type)

    ## find minima over all activity types if configured; for example:
    ##     list(
    ##        authorA = list(all.activities = 1), authorB = list(all.activities = 0),
    ##        authorC = list(all.activities = 3), authorD = list(all.activities = 2)
    ##     )
    if (take.first.over.all.activity.types) {
        result = parallel::mclapply(result, function(item.list) {
            min.value = min(do.call(c, item.list), na.rm = TRUE)
            return(list(all.activities = min.value))
        })
    }

    return(result)
}

#' Helper function for active ranges: computes the active ranges per author and activity.type.
#'
#' @param activity.types The activity types to compute information for. [default: c("mails", "commits", "issues")]
#' @param net.to.range.list The data to base the computation on, split by networks.
#'
#' @return A list with elements representing the authors, each containing a list of elements representing the activity
#'         types, each containing a list of active ranges.
get.active.ranges.data = function(activity.types = c("mails", "commits", "issues"), net.to.range.list) {

    ## a list with elements representing the parsed activity types, each containing a list of elements
    ## representing the ranges the data was split by, each containing a list of authors who were active
    ## for the corresponding activity type and range.
    range.to.authors.per.activity.type = lapply(activity.types, function(type) {
        type.function = paste0("get.", type)
        lapply(net.to.range.list, function(net.to.range) {

            ## get author information per activity.type
            type.data = net.to.range[["data"]][[type.function]]()

            ## remove unnecessary information and potentially resulting duplicats
            clean.type.data = unique(type.data[["author.name"]])

            return(as.list(clean.type.data))
        })
    })
    names(range.to.authors.per.activity.type) = activity.types

    ## Switch the list levels from "type - range - author" to "author - range - type".
    active.ranges = list.by.inner.level(range.to.authors.per.activity.type)

    ## For every author: if there's no activity data for a type, an empty list named with the type is added instead.
    active.ranges = lapply(active.ranges, function(ranges.per.type) {
        ranges.for.all.types = lapply(activity.types, function(type) {
            if (type %in% names(ranges.per.type)) {
                return(ranges.per.type[[type]])
            } else {
                return(list())
            }
        })
        names(ranges.for.all.types) = activity.types
        return(list(ranges.for.all.types))
    })

    return(active.ranges)
}


#' This function takes a named nested list, for example \code{l = list(outer1 = list(middle11 = list("innerA", "innerB"),
#' middle12 = list("innerB")), outer2 = list("innerA"))}. For every distinct innermost element (every element of the
#' nested list, that isn't itself a list, in the example list l \code{"innerA" and "innerB"}), the whole nested list
#' structure is searched for the specific element. Every part of the list structure, that doesn't contain the specific
#' innermost element, is removed. For \code{"innerB"} in l for example, the whole \code{outer2} is removed. The resulting
#' list structures are combined to a list named with the corresponding (previous) innermost elements and returned. Called
#' with the example list l, this function returns a list like \code{list(innerA=list(outer1=list(middle11 = "middle11"),
#' outer2 = "outer2"), innerB = list(outer1 = list(middle11 = "middle11", middle12 = "middle12")))}.
#'
#' @param nested.list A list nested AT LEAST ONCE, that means: the elements of the outermost list are also lists.
#'
#' @return The nested list with the innermost level as new outermost level.
list.by.inner.level = function(nested.list) {
    list.by = unique(unlist(nested.list, use.names = FALSE))

    ## Returns the given structure of nested lists with all parts removed, that do not contain the given
    ## innerst.element.
    get.structure.for = function(innerst.element, structure, name="default") {

        ## Base case 1: an empty list is returned as it is.
        if (length(structure) == 0) {
            return(list())

        ## Base case 2: if the structure isn't nested itself, it is only returned, if it contains the given innerst.element.
        ##           Otherwise, NA is returned.
        } else if (!is.list(structure[[1]])) {
            if (innerst.element %in% structure) {
                return(name)
            } else {
                return(NA)
            }

        ## Recursive case: for every substructure, the function is called recursively. From the results, the list is
        ##                 reconstructed, named and returned.
        } else {
            result.of.recursive.calls = lapply(names(structure), function(substructure.name) {
                substructure = structure[[substructure.name]]
                call.result = get.structure.for(innerst.element, substructure, substructure.name)
                return(call.result)
            })
            names(result.of.recursive.calls) = names(structure)

            ## remove NA values from recursively obtained structure
            na.removed = result.of.recursive.calls[sapply(result.of.recursive.calls, function(x) any(!is.na(x)))]
            return(na.removed)
        }
    }

    result = lapply(list.by, function(element) get.structure.for(element, nested.list))
    names(result) = list.by
    return(result)
}
