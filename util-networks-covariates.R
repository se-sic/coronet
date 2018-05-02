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
## All Rights Reserved.


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
                                          default.value, compute.attr) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "range")

    net.to.range.list = split.data.by.networks(list.of.networks, project.data, aggregation.level)

    nets.with.attr = add.vertex.attribute(net.to.range.list, attr.name, default.value, compute.attr)
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
add.vertex.attribute = function(net.to.range.list, attr.name, default.value, compute.attr) {

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
            else {
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
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author = function(list.of.networks, project.data, name = "commit.count",
                                                    aggregation.level = c("range", "cumulative", "all.ranges",
                                                                          "project.cumulative", "project.all.ranges",
                                                                          "complete"),
                                                    default.value = 0) {
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
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.author.not.committer = function(list.of.networks, project.data,
                                                                  name = "commit.count.author.not.committer",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0) {
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
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer = function(list.of.networks, project.data, name = "commit.count.committer",
                                                       aggregation.level = c("range", "cumulative", "all.ranges",
                                                                             "project.cumulative", "project.all.ranges",
                                                                             "complete"),
                                                       default.value = 0) {
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
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.not.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.not.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.not.author.commit.count, "committer.name"
    )

    return(nets.with.attr)
}

#' Add commit-count attribute based on comitter name where the committer equals the author.
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param name The attribute name to add [default: "commit.count.committer.and.author"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "range"]
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.committer.and.author = function(list.of.networks, project.data,
                                                                  name = "commit.count.committer.and.author",
                                                                  aggregation.level = c("range", "cumulative", "all.ranges",
                                                                                        "project.cumulative", "project.all.ranges",
                                                                                        "complete"),
                                                                  default.value = 0) {
    nets.with.attr = add.vertex.attribute.commit.count.helper(
        list.of.networks, project.data, name, aggregation.level,
        default.value, get.committer.and.author.commit.count, "committer.name"
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
#' @param default.value The default value to add if a vertex has no matching value [default: 0]
#' @param commit.count.method The method reference for counting the commits
#' @param name.column The name of the author or committer column
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.commit.count.helper = function(list.of.networks, project.data, name = "commit.count",
                                             aggregation.level = c("range", "cumulative", "all.ranges",
                                                                   "project.cumulative", "project.all.ranges",
                                                                   "complete"),
                                             default.value = 0, commit.count.method, name.column) {
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
            lapply(range.data$get.author2artifact(), function(x) {
                length(unique(x[["artifact"]]))
            })
        }
    )

    return(nets.with.attr)
}

## * Activity --------------------------------------------------------------

#' Add first activity attribute
#'
#' @param list.of.networks The network list
#' @param project.data The project data
#' @param activity.type The kind of activity to use as basis.
#'                      One of \code{mails}, \code{commits}, and \code{issues}. [default: "mails"]
#' @param name The attribute name to add [default: "first.activity"]
#' @param aggregation.level Determines the data to use for the attribute calculation.
#'                          One of \code{"range"}, \code{"cumulative"}, \code{"all.ranges"},
#'                          \code{"project.cumulative"}, \code{"project.all.ranges"}, and
#'                          \code{"complete"}. See \code{split.data.by.networks} for
#'                          more details. [default: "complete"]
#' @param default.value The default value to add if a vertex has no matching value [default: NA]
#'
#' @return A list of networks with the added attribute
add.vertex.attribute.first.activity = function(list.of.networks, project.data,
                                               activity.type = c("mails", "commits", "issues"),
                                               name = "first.activity",
                                               aggregation.level = c("range", "cumulative", "all.ranges",
                                                                     "project.cumulative", "project.all.ranges",
                                                                     "complete"),
                                               default.value = NA) {
    aggregation.level = match.arg.or.default(aggregation.level, default = "complete")
    activity.type = match.arg.or.default(activity.type)
    ## TODO support getting first activity over all available activity types
    function.suffix = substr(activity.type, 1, nchar(activity.type) - 1)
    activity.type.function = paste0("get.author2", function.suffix)

    nets.with.attr = split.and.add.vertex.attribute(
        list.of.networks, project.data, name, aggregation.level, default.value,
        function(range, range.data, net) {
            lapply(range.data[[activity.type.function]](),
                   function(x) min(x[["date"]])
            )
        }
    )

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
            names(net.to.range[["data"]]$get.author2commit())
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
            lapply(range.data$get.artifact2author(), function(x) {
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
